module Facet
  module Compiler
    class Parser
      CALL_METHOD_NAME_TOKENS = "IDENT, CONST, +, -, *, /, //, %, |, &, ^, ~, !, **, <<, <, <=, ==, !=, =~, !~, >>, >, >=, <=>, ===, [], []=, []?, [, &+, &-, &*, &**"

      getter diagnostics : Array(Diagnostic)

      def initialize(@source : Source)
        @lexer = Lexer.new(@source)
        @lexer.parser_mode = true
        @tokens = TokenStream.new(@lexer)
        @arena = AstArena.new
        @diagnostics = [] of Diagnostic
        @macro_depth = 0
        @macro_def_depth = 0
        @macro_expr_depth = 0
        @def_depth = 0
        @type_depth = 0
        @lib_depth = 0
        @enum_depth = 0
        @local_assigns = [] of String
      end

      def parse_file : AstFile
        exprs = parse_expressions([TokenKind::Eof])

        # Validate top-level statements for standalone splats
        @arena.children(exprs).each { |stmt| validate_no_standalone_splat(stmt) }
        @arena.children(exprs).each { |stmt| validate_comma_tuple(stmt) }
        @arena.children(exprs).each { |stmt| validate_named_tuple_duplicates(stmt) }
        @arena.children(exprs).each { |stmt| validate_case_when_clauses(stmt) }
        validate_dynamic_constant_assignments(exprs, false, true, true)
        validate_void_value_expressions(exprs)

        @lexer.diagnostics.each { |diag| @diagnostics << diag }

        root_span = Span.new(0, @source.size)
        root = @arena.add_node(NodeKind::File, root_span, [exprs])
        AstFile.new(@source, root, @arena, @diagnostics)
      end

      private def parse_expressions(terminators : Array(TokenKind), stop : Proc(Bool)? = nil, expr_stop : Proc(Bool)? = nil) : NodeId
        children = [] of NodeId
        expr_stop ||= stop
        skip_separators
        while !current.eof?
          break if terminator?(current.kind, terminators, stop)
          break if stop && macro_control_start?
          node = parse_statement(terminators, expr_stop)
          children << node
          end_pos = node_span(node).finish
          if @macro_def_depth == 0 &&
             current.span.start > end_pos &&
             expression_start_token?(current.kind) &&
             !newline_between?(end_pos, current.span.start) &&
             !terminator?(current.kind, terminators, stop) &&
             !macro_control_start?
            @diagnostics << Diagnostic.new(current.span, "unexpected token: \"#{crystal_diagnostic_token_text(current)}\"")
          end
          skip_separators
        end

        span = if children.empty?
                 Span.new(current.span.start, current.span.start)
               else
                 span_from_nodes(children.first, children.last)
               end
        @arena.add_node(NodeKind::Expressions, span, children)
      end

      private def parse_statement(terminators : Array(TokenKind), expr_stop : Proc(Bool)? = nil) : NodeId
        diagnose_declaration_inside_def
        if macro_control_start?
          return parse_macro_control
        end
        if current.kind == TokenKind::Annotation
          annotations = [] of NodeId
          while current.kind == TokenKind::Annotation
            annotations << parse_annotation
            skip_separators
          end
          if current.kind == TokenKind::Semicolon || current.kind == TokenKind::KeywordEnd ||
             terminator?(current.kind, terminators, expr_stop)
            nop = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
            return attach_annotations(annotations, nop)
          end
          target = parse_statement(terminators, expr_stop)
          return attach_annotations(annotations, target)
        end
        if var_decl_start?(current.kind) && (
             peek1.kind == TokenKind::Colon ||
             (peek1.kind == TokenKind::Comma && var_decl_with_comma_ahead?) ||
             (@lib_depth > 0 && current.kind == TokenKind::GlobalVar && peek1.kind == TokenKind::Assign && var_decl_with_assign_ahead?)
           )
          return parse_var_decl(expr_stop)
        end
        node = case current.kind
               when TokenKind::KeywordIf
                 parse_if
               when TokenKind::KeywordUnless
                 parse_unless
               when TokenKind::KeywordWhile
                 parse_while
               when TokenKind::KeywordUntil
                 parse_until
               when TokenKind::KeywordAbstract
                 advance
                 case current.kind
                 when TokenKind::KeywordDef
                   parse_abstract_def
                 when TokenKind::KeywordClass
                   parse_type_block(NodeKind::Class, "expected 'end' to close class")
                 when TokenKind::KeywordStruct
                   parse_type_block(NodeKind::Struct, "expected 'end' to close struct")
                 when TokenKind::KeywordModule
                   parse_type_block(NodeKind::Module, "expected 'end' to close module")
                 else
                   @diagnostics << Diagnostic.new(current.span, "unexpected token after 'abstract'")
                   @arena.add_node(NodeKind::Error, current.span)
                 end
               when TokenKind::KeywordReturn
                 parse_control(NodeKind::Return, expr_stop)
               when TokenKind::KeywordBreak
                 parse_control(NodeKind::Break, expr_stop)
               when TokenKind::KeywordNext
                 parse_control(NodeKind::Next, expr_stop)
               when TokenKind::KeywordYield
                 parse_control(NodeKind::Yield, expr_stop)
               when TokenKind::KeywordSelect
                 parse_select
               when TokenKind::KeywordRequire
                 parse_require
               when TokenKind::KeywordDef
                 parse_def(NodeKind::Def, TokenKind::KeywordEnd, "expected 'end' to close def")
               when TokenKind::KeywordMacro
                 parse_def(NodeKind::MacroDef, TokenKind::KeywordEnd, "expected 'end' to close macro")
               when TokenKind::KeywordClass
                 parse_type_block(NodeKind::Class, "expected 'end' to close class")
               when TokenKind::KeywordModule
                 parse_type_block(NodeKind::Module, "expected 'end' to close module")
               when TokenKind::KeywordStruct
                 parse_type_block(NodeKind::Struct, "expected 'end' to close struct")
               when TokenKind::KeywordEnum
                 parse_type_block(NodeKind::Enum, "expected 'end' to close enum")
               when TokenKind::KeywordUnion
                 if @def_depth > 0 || peek1.kind == TokenKind::Assign || @local_assigns.includes?("union")
                   parse_expression(0, expr_stop)
                 else
                   parse_type_block(NodeKind::Struct, "expected 'end' to close union")
                 end
               when TokenKind::KeywordLib
                 parse_type_block(NodeKind::Lib, "expected 'end' to close lib")
               when TokenKind::KeywordAnnotation
                 parse_annotation_def
               when TokenKind::KeywordPrivate, TokenKind::KeywordProtected
                 parse_visibility(terminators, expr_stop)
               when TokenKind::KeywordFun
                 parse_fun
               when TokenKind::KeywordAlias
                 parse_alias
               when TokenKind::KeywordType
                 if peek1.kind == TokenKind::Identifier && peek2.kind == TokenKind::Assign
                   parse_type_def
                 else
                   parse_expression(0, expr_stop)
                 end
               when TokenKind::KeywordFor
                 parse_for
               when TokenKind::KeywordCase
                 parse_case
               when TokenKind::KeywordProperty, TokenKind::KeywordGetter, TokenKind::KeywordSetter
                 if peek1.kind == TokenKind::Assign || peek1.kind == TokenKind::Dot || peek1.kind == TokenKind::SafeNav ||
                    @local_assigns.includes?(token_text(current))
                   parse_expression(0, expr_stop)
                 else
                   parse_property_like
                 end
               else
                 parse_expression(0, expr_stop)
               end
        apply_trailing_modifier(node)
      rescue ex : Exception
        @diagnostics << Diagnostic.new(current.span, "parse error: #{ex.message}")
        synchronize(terminators)
        @arena.add_node(NodeKind::Error, current.span)
      end

      private def parse_proc_pointer_target : NodeId
        root_qualified = current.kind == TokenKind::DoubleColon
        root_span = current.span
        if root_qualified
          advance
        end

        base = case current.kind
               when TokenKind::KeywordSelf
                 tok = advance
                 sym = @arena.symbols.intern("self")
                 @arena.add_ident(tok.span, sym)
               when TokenKind::Identifier
                 parse_proc_pointer_ident
               when TokenKind::InstanceVar
                 tok = advance
                 sym = @arena.symbols.intern(token_text(tok))
                 @arena.add_node(NodeKind::InstanceVar, tok.span, payload_index: sym)
               when TokenKind::ClassVar
                 tok = advance
                 sym = @arena.symbols.intern(token_text(tok))
                 @arena.add_node(NodeKind::ClassVar, tok.span, payload_index: sym)
               when TokenKind::GlobalVar
                 tok = advance
                 sym = @arena.symbols.intern(token_text(tok))
                 @arena.add_node(NodeKind::Global, tok.span, payload_index: sym)
               else
                 tok = advance
                 @diagnostics << Diagnostic.new(tok.span, "unexpected token in proc pointer")
                 @arena.add_node(NodeKind::Error, tok.span)
               end

        while current.kind == TokenKind::Dot || current.kind == TokenKind::DoubleColon
          sep = advance
          if root_qualified && sep.kind == TokenKind::Dot && !const_like?(base)
            variable_kind = case @arena.node(base).kind
                            when NodeKind::InstanceVar then "instance variable"
                            when NodeKind::ClassVar    then "class variable"
                            else                            "local variable"
                            end
            @diagnostics << Diagnostic.new(root_span, "ProcPointer of #{variable_kind} cannot be global")
          end
          rhs = if current.kind == TokenKind::Identifier
                  parse_proc_pointer_ident
                elsif current.kind == TokenKind::KeywordSelf
                  tok = advance
                  sym = @arena.symbols.intern("self")
                  @arena.add_ident(tok.span, sym)
                elsif current.kind == TokenKind::LBracket
                  lb = advance
                  rb = expect(TokenKind::RBracket, "expected ']' in proc pointer")
                  span = Span.new(lb.span.start, rb.span.finish)
                  name = "[]"
                  if current.kind == TokenKind::Assign && current.span.start == span.finish
                    assign = advance
                    name = "[]="
                    span = Span.new(span.start, assign.span.finish)
                  end
                  sym = @arena.symbols.intern(name)
                  @arena.add_ident(span, sym)
                elsif op_name = operator_method_name(current.kind)
                  tok = advance
                  sym = @arena.symbols.intern(op_name)
                  @arena.add_ident(tok.span, sym)
                else
                  parse_identifier_or_error
                end
          span = Span.new(node_span(base).start, node_span(rhs).finish)
          base = @arena.add_node(NodeKind::Path, span, [base, rhs])
        end
        if current.kind == TokenKind::LParen
          args = parse_type_args
          span = Span.new(node_span(base).start, node_span(args).finish)
          base = @arena.add_node(NodeKind::Call, span, [base, args])
        end
        base
      end

      private def parse_proc_pointer_ident : NodeId
        tok = advance
        name = token_text(tok)
        span = tok.span
        loop do
          if current.kind == TokenKind::Assign && current.span.start == span.finish
            eq = advance
            name += "="
            span = Span.new(span.start, eq.span.finish)
          elsif (current.kind == TokenKind::Question || current.kind == TokenKind::Bang) && current.span.start == span.finish
            suf = advance
            name += token_text(suf)
            span = Span.new(span.start, suf.span.finish)
          else
            break
          end
        end
        sym = @arena.symbols.intern(name)
        @arena.add_ident(span, sym)
      end

      private def const_like?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        case node.kind
        when NodeKind::Ident
          name = @arena.symbols[node.payload_index]
          return false if name.empty?
          name[0].uppercase?
        when NodeKind::Path
          children = @arena.children(node_id)
          return false if children.empty?
          const_like?(children.last)
        else
          false
        end
      end

      private def adjacent?(node_id : NodeId, token : Token) : Bool
        node_span(node_id).finish == token.span.start
      end

      private def parse_annotation : NodeId
        at = advance
        expect(TokenKind::LBracket, "expected '[' after annotation")
        arg_node = if current.kind == TokenKind::RBracket
                     @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
                   else
                     values = [] of NodeId
                     values << parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket }, allow_type_apply: false)
                     while match(TokenKind::Comma)
                       break if current.kind == TokenKind::RBracket
                       values << parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket }, allow_type_apply: false)
                     end
                     if values.size == 1
                       values.first
                     else
                       span = span_from_nodes(values.first, values.last)
                       @arena.add_node(NodeKind::Expressions, span, values)
                     end
                   end
        end_token = expect(TokenKind::RBracket, "expected ']' to close annotation")
        span = Span.new(at.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Annotation, span, [arg_node])
      end

      private def attach_annotations(annots : Array(NodeId), target : NodeId) : NodeId
        node = target
        annots.reverse_each do |annot|
          arg = @arena.children(annot)[0]?
          arg ||= @arena.add_node(NodeKind::Nop, Span.new(@arena.node(annot).span.start, @arena.node(annot).span.start))
          span = Span.new(@arena.node(annot).span.start, node_span(node).finish)
          node = @arena.add_node(NodeKind::Annotation, span, [arg, node])
        end
        node
      end

      private def parse_annotation_def : NodeId
        start = advance
        name_node = parse_path
        validate_declaration_separator(NodeKind::AnnotationDef, node_span(name_node).finish)
        body = parse_expressions([TokenKind::KeywordEnd])
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close annotation")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::AnnotationDef, span, [name_node, body])
      end

      private def var_decl_start?(kind : TokenKind) : Bool
        soft_identifier_kind?(kind) || keyword_token?(kind) ||
          kind == TokenKind::InstanceVar || kind == TokenKind::ClassVar || kind == TokenKind::GlobalVar
      end

      private def var_decl_with_comma_ahead? : Bool
        i = 1
        loop do
          tok = @tokens.peek(i)
          case tok.kind
          when TokenKind::Colon
            return true
          when TokenKind::Assign, TokenKind::Semicolon, TokenKind::KeywordEnd, TokenKind::RBrace, TokenKind::RParen, TokenKind::Eof
            return false
          end
          i += 1
        end
      end

      private def var_decl_with_assign_ahead? : Bool
        i = 1
        loop do
          tok = @tokens.peek(i)
          case tok.kind
          when TokenKind::Colon
            return true
          when TokenKind::Semicolon, TokenKind::KeywordEnd, TokenKind::RBrace, TokenKind::RParen, TokenKind::Eof
            return false
          end
          i += 1
        end
      end

      private def parse_var_decl(stop : Proc(Bool)? = nil) : NodeId
        names = [] of NodeId
        names << parse_var_ref
        loop do
          break unless match(TokenKind::Comma)
          break unless var_decl_start?(current.kind)
          names << parse_var_ref
          break if current.kind == TokenKind::Colon
        end
        value_before_type = false
        if names.size == 1 && current.kind == TokenKind::Assign && @arena.node(names.first).kind == NodeKind::Global
          value_before_type = true
          advance
          value_node = parse_expression(0, -> { current.kind == TokenKind::Colon || current.kind == TokenKind::Semicolon || current.kind == TokenKind::KeywordEnd || (stop ? stop.call : false) }, allow_var_decl: false)
        end
        colon = expect(TokenKind::Colon, "expected ':' in declaration")
        type_node = parse_type(-> { current.kind == TokenKind::Assign || current.kind == TokenKind::Semicolon || current.kind == TokenKind::KeywordEnd || (stop ? stop.call : false) })

        # validate type tuple doesn't contain lowercase identifiers (indicates malformed `x : T, a = v`)
        if @arena.node(type_node).kind == NodeKind::Tuple
          @arena.children(type_node).each do |child_id|
            child = @arena.node(child_id)
            if child.kind == NodeKind::Ident
              name = @arena.symbols[child.payload_index]
              if name.size > 0 && name[0].lowercase?
                @diagnostics << Diagnostic.new(node_span(child_id), "unexpected identifier in type position")
              end
            end
          end
        end

        type_finish = node_span(type_node).finish
        value_node = @arena.add_node(NodeKind::Nop, Span.new(type_finish, type_finish))
        if !value_before_type && match(TokenKind::Assign)
          value_node = parse_expression(0, stop)
        elsif value_before_type
          # value already parsed
        end
        decls = names.map do |lhs|
          span = Span.new(node_span(lhs).start, [node_span(value_node).finish, node_span(type_node).finish].max)
          @arena.add_node(NodeKind::VarDecl, span, [lhs, type_node, value_node])
        end
        if decls.size == 1
          register_assigned_locals(names.first)
          decls.first
        else
          names.each { |name| register_assigned_locals(name) }
          span = Span.new(node_span(decls.first).start, node_span(decls.last).finish)
          @arena.add_node(NodeKind::Expressions, span, decls)
        end
      end

      private def parse_var_ref : NodeId
        case current.kind
        when TokenKind::Identifier, TokenKind::KeywordType
          ident = advance
          sym = @arena.symbols.intern(token_text(ident))
          @arena.add_ident(ident.span, sym)
        when TokenKind::InstanceVar
          ident = advance
          sym = @arena.symbols.intern(token_text(ident))
          @arena.add_node(NodeKind::InstanceVar, ident.span, payload_index: sym)
        when TokenKind::ClassVar
          ident = advance
          sym = @arena.symbols.intern(token_text(ident))
          @arena.add_node(NodeKind::ClassVar, ident.span, payload_index: sym)
        when TokenKind::GlobalVar
          ident = advance
          parse_global_var(ident)
        else
          if keyword_token?(current.kind)
            ident = advance
            sym = @arena.symbols.intern(token_text(ident))
            @arena.add_ident(ident.span, sym)
          else
            tok = current
            @diagnostics << Diagnostic.new(tok.span, "expected variable name")
            advance unless tok.eof?
            @arena.add_node(NodeKind::Error, tok.span)
          end
        end
      end

      private def parse_global_var(token : Token) : NodeId
        text = token_text(token)
        sym = @arena.symbols.intern(text)
        node_id = @arena.add_node(NodeKind::Global, token.span, payload_index: sym)
        if @lib_depth == 0 && text.size > 1 && (text[1].ascii_letter? || text[1] == '_')
          position = token.span.finish
          @diagnostics << Diagnostic.new(Span.new(position, position), "$global_variables are not supported, use @@class_variables instead")
        end
        if @lib_depth > 0 && text.size > 1 && text[1].ascii_uppercase?
          @diagnostics << Diagnostic.new(token.span, "external variables must start with lowercase, use for example `$errno = Errno : Int32`")
        end
        if digits = global_match_data_digits(text)
          if digits.size > 10 || (value = digits.to_i64?) && value > Int32::MAX
            position = token.span.finish
            @diagnostics << Diagnostic.new(Span.new(position, position), "Index $#{digits} doesn't fit in an Int32")
          end
        end
        node_id
      end

      private def global_match_data_name?(name : String) : Bool
        !!global_match_data_digits(name)
      end

      private def global_match_data_digits(name : String) : String?
        return nil if name.size < 2 || name[0] != '$'
        bytes = name.to_slice
        i = 1
        while i < bytes.size && bytes[i] >= 48 && bytes[i] <= 57
          i += 1
        end
        return nil if i == 1
        if i == bytes.size
          String.new(bytes[1, i - 1])
        elsif i == bytes.size - 1 && bytes[i] == '?'.ord.to_u8
          String.new(bytes[1, i - 1])
        else
          nil
        end
      end

      private def soft_identifier_kind?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier,
             TokenKind::KeywordType,
             TokenKind::KeywordOf,
             TokenKind::KeywordProperty,
             TokenKind::KeywordGetter,
             TokenKind::KeywordSetter,
             TokenKind::KeywordUninitialized,
             TokenKind::KeywordUnion
          true
        else
          false
        end
      end

      private def keyword_token?(kind : TokenKind) : Bool
        kind >= TokenKind::KeywordAbstract && kind <= TokenKind::KeywordYield
      end

      private def named_arg_name_token?(kind : TokenKind) : Bool
        kind == TokenKind::Identifier || kind == TokenKind::String || keyword_token?(kind)
      end

      private def brace_tuple_literal?(node_id : NodeId) : Bool
        span = node_span(node_id)
        return false if span.start < 0 || span.start >= @source.bytes.size
        @source.bytes[span.start] == '{'.ord.to_u8
      end

      private def implicit_dot_target?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier,
             TokenKind::KeywordIsAQuestion,
             TokenKind::KeywordRespondsToQuestion,
             TokenKind::KeywordNilQuestion,
             TokenKind::KeywordAsQuestion,
             TokenKind::KeywordAs,
             TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash, TokenKind::Percent,
             TokenKind::Caret, TokenKind::Ampersand, TokenKind::Pipe,
             TokenKind::Bang, TokenKind::Tilde,
             TokenKind::EqualEqual, TokenKind::BangEqual, TokenKind::TripleEqual,
             TokenKind::Less, TokenKind::LessEqual, TokenKind::Greater, TokenKind::GreaterEqual,
             TokenKind::Spaceship, TokenKind::Match, TokenKind::NotMatch,
             TokenKind::ShiftLeft, TokenKind::ShiftRight,
             TokenKind::AmpersandStar, TokenKind::AmpersandStarStar,
             TokenKind::LBracket
          true
        else
          false
        end
      end

      private def identifier_like_token?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier,
             TokenKind::KeywordIsAQuestion,
             TokenKind::KeywordRespondsToQuestion,
             TokenKind::KeywordNilQuestion,
             TokenKind::KeywordAsQuestion,
             TokenKind::KeywordAs
          true
        else
          false
        end
      end

      private def apply_trailing_modifier(node : NodeId) : NodeId
        if (current.kind == TokenKind::KeywordIf || current.kind == TokenKind::KeywordUnless) && same_line?(node_span(node), current.span)
          kw = advance
          cond_stop = @macro_depth > 0 ? -> { macro_control_end? } : nil
          cond = parse_expression(0, cond_stop)
          then_body = wrap_expressions(node)
          else_body = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
          span = Span.new(node_span(node).start, node_span(cond).finish)
          return @arena.add_node(
            kw.kind == TokenKind::KeywordIf ? NodeKind::If : NodeKind::Unless,
            span,
            [cond, then_body, else_body]
          )
        end
        if (current.kind == TokenKind::KeywordWhile || current.kind == TokenKind::KeywordUntil) && same_line?(node_span(node), current.span)
          keyword = token_text(current)
          @diagnostics << Diagnostic.new(current.span, "trailing `#{keyword}` is not supported")
        end
        node
      end

      private def same_line?(left : Span, right : Span) : Bool
        start_index = left.finish
        end_index = right.start
        return true if start_index >= end_index

        bytes = @source.bytes
        i = start_index
        while i < end_index && i < bytes.size
          byte = bytes[i]
          return false if byte == 0x0a || byte == 0x0d
          i += 1
        end
        true
      end

      private def wrap_expressions(node_id : NodeId) : NodeId
        @arena.add_node(NodeKind::Expressions, node_span(node_id), [node_id])
      end

      private def parse_if : NodeId
        start = advance
        cond = parse_expression
        if expression_start_token?(current.kind) && !newline_between?(node_span(cond).finish, current.span.start)
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"#{token_text(current)}\"")
        end
        then_body = parse_expressions([TokenKind::KeywordElse, TokenKind::KeywordElsif, TokenKind::KeywordEnd])
        else_body = parse_if_else
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close if")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::If, span, [cond, then_body, else_body])
      end

      private def parse_if_else : NodeId
        case current.kind
        when TokenKind::KeywordElse
          advance
          parse_expressions([TokenKind::KeywordEnd])
        when TokenKind::KeywordElsif
          start = advance
          cond = parse_expression
          then_body = parse_expressions([TokenKind::KeywordElse, TokenKind::KeywordElsif, TokenKind::KeywordEnd])
          else_body = parse_if_else
          span = span_from_nodes(cond, else_body)
          @arena.add_node(NodeKind::If, span, [cond, then_body, else_body])
        else
          @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        end
      end

      private def parse_unless : NodeId
        start = advance
        cond = parse_expression
        if expression_start_token?(current.kind) && !newline_between?(node_span(cond).finish, current.span.start)
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"#{token_text(current)}\"")
        end
        then_body = parse_expressions([TokenKind::KeywordElse, TokenKind::KeywordEnd])
        else_body = case current.kind
                    when TokenKind::KeywordElse
                      advance
                      parse_expressions([TokenKind::KeywordEnd])
                    else
                      @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
                    end
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close unless")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Unless, span, [cond, then_body, else_body])
      end

      private def parse_while : NodeId
        start = advance
        cond = parse_expression
        if expression_start_token?(current.kind) && !newline_between?(node_span(cond).finish, current.span.start)
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"#{token_text(current)}\"")
        end
        body = parse_expressions([TokenKind::KeywordEnd])
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close while")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::While, span, [cond, body])
      end

      private def parse_until : NodeId
        start = advance
        cond = parse_expression
        if expression_start_token?(current.kind) && !newline_between?(node_span(cond).finish, current.span.start)
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"#{token_text(current)}\"")
        end
        body = parse_expressions([TokenKind::KeywordEnd])
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close until")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Until, span, [cond, body])
      end

      private def parse_begin : NodeId
        start = advance
        body = parse_expressions([TokenKind::KeywordRescue, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
        rescue_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        else_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        ensure_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))

        if current.kind == TokenKind::KeywordRescue
          rescue_body = parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          while current.kind == TokenKind::KeywordRescue
            parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          end
          rescue_span = span_from_nodes(rescue_body, rescue_body)
          rescue_node = @arena.add_node(NodeKind::Rescue, rescue_span, [rescue_body])
        end

        if current.kind == TokenKind::KeywordElse
          advance
          else_body = parse_expressions([TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          else_span = span_from_nodes(else_body, else_body)
          else_node = @arena.add_node(NodeKind::Rescue, else_span, [else_body])
        end

        if current.kind == TokenKind::KeywordEnsure
          advance
          ensure_body = parse_expressions([TokenKind::KeywordEnd])
          ensure_span = span_from_nodes(ensure_body, ensure_body)
          ensure_node = @arena.add_node(NodeKind::Ensure, ensure_span, [ensure_body])
        end

        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close begin")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Begin, span, [body, rescue_node, else_node, ensure_node])
      end

      private def parse_rescue_clause(terminators : Array(TokenKind)) : NodeId
        rescue_token = advance
        unless current.eof? || newline_between?(rescue_token.span.finish, current.span.start)
          if current.kind == TokenKind::Identifier && token_text(current)[0].ascii_uppercase?
            parse_type(-> { terminators.includes?(current.kind) || current.kind == TokenKind::KeywordRescue || current.kind == TokenKind::Semicolon })
          elsif var_decl_start?(current.kind) && peek1.kind == TokenKind::Colon
            parse_var_decl(-> { terminators.includes?(current.kind) || current.kind == TokenKind::KeywordRescue })
          elsif var_decl_start?(current.kind)
            local = parse_var_ref
            register_assigned_locals(local)
          end
        end
        parse_expressions([TokenKind::KeywordRescue] + terminators)
      end

      private def parse_visibility(terminators : Array(TokenKind), expr_stop : Proc(Bool)?) : NodeId
        visibility = advance
        if current.eof? || newline_between?(visibility.span.finish, current.span.start)
          @diagnostics << Diagnostic.new(visibility.span, "visibility modifier must be followed by a declaration")
          return @arena.add_node(NodeKind::Nop, visibility.span)
        end
        parse_statement(terminators, expr_stop)
      end

      private def parse_case : NodeId
        start = advance
        skip_separators
        subject = if current.kind == TokenKind::KeywordWhen || current.kind == TokenKind::KeywordIn || current.kind == TokenKind::KeywordEnd
                    @arena.add_node(NodeKind::Nop, Span.new(start.span.finish, start.span.finish))
                  else
                    parse_expression
                  end
        if expression_start_token?(current.kind) && !newline_between?(node_span(subject).finish, current.span.start)
          @diagnostics << Diagnostic.new(current.span, "unexpected token")
        end
        skip_separators

        whens = [] of NodeId
        while current.kind == TokenKind::KeywordWhen || current.kind == TokenKind::KeywordIn || macro_control_start?
          whens << (macro_control_start? ? parse_macro_control : parse_when)
          skip_separators
        end

        else_body = if current.kind == TokenKind::KeywordElse
                      advance
                      parse_expressions([TokenKind::KeywordEnd])
                    else
                      @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
                    end

        skip_separators
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close case")

        whens_span = if whens.empty?
                       Span.new(start.span.finish, start.span.finish)
                     else
                       span_from_nodes(whens.first, whens.last)
                     end
        whens_node = @arena.add_node(NodeKind::Expressions, whens_span, whens)
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Case, span, [subject, whens_node, else_body])
      end

      private def parse_select : NodeId
        start = advance
        whens = [] of NodeId
        skip_separators
        while current.kind == TokenKind::KeywordWhen
          when_node = parse_when
          if conds = @arena.children(when_node)[0]?
            @arena.children(conds).each do |cond|
              kind = @arena.node(cond).kind
              if {NodeKind::LiteralNumber, NodeKind::LiteralString, NodeKind::LiteralChar,
                  NodeKind::LiteralSymbol, NodeKind::LiteralRegex, NodeKind::LiteralNil,
                  NodeKind::LiteralBool}.includes?(kind)
                @diagnostics << Diagnostic.new(node_span(cond), "invalid select when expression: must be an assignment or call")
              end
            end
          end
          whens << when_node
          skip_separators
        end
        else_body = if current.kind == TokenKind::KeywordElse
                      advance
                      parse_expressions([TokenKind::KeywordEnd])
                    else
                      @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
                    end
        skip_separators
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close select")
        whens_span = whens.empty? ? Span.new(start.span.finish, start.span.finish) : span_from_nodes(whens.first, whens.last)
        whens_node = @arena.add_node(NodeKind::Expressions, whens_span, whens)
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Case, span, [@arena.add_node(NodeKind::Nop, Span.new(start.span.finish, start.span.finish)), whens_node, else_body])
      end

      private def parse_when : NodeId
        start = advance
        conds = [] of NodeId

        conds << parse_expression(0, -> { when_condition_stop? })
        if expression_start_token?(current.kind) && !newline_between?(node_span(conds.last).finish, current.span.start)
          @diagnostics << Diagnostic.new(current.span, "unexpected token")
        end
        while match(TokenKind::Comma)
          conds << parse_expression(0, -> { when_condition_stop? })
          if expression_start_token?(current.kind) && !newline_between?(node_span(conds.last).finish, current.span.start)
            @diagnostics << Diagnostic.new(current.span, "unexpected token")
          end
        end

        match(TokenKind::KeywordThen)

        body = parse_expressions([TokenKind::KeywordWhen, TokenKind::KeywordIn, TokenKind::KeywordElse, TokenKind::KeywordEnd])

        cond_span = conds.empty? ? Span.new(start.span.finish, start.span.finish) : span_from_nodes(conds.first, conds.last)
        conds_node = @arena.add_node(NodeKind::Expressions, cond_span, conds)
        when_span = Span.new(start.span.start, node_span(body).finish)
        @arena.add_node(NodeKind::When, when_span, [conds_node, body])
      end

      private def when_condition_stop? : Bool
        case current.kind
        when TokenKind::Comma, TokenKind::KeywordThen, TokenKind::KeywordWhen, TokenKind::KeywordElse, TokenKind::KeywordEnd
          true
        else
          false
        end
      end

      private def parse_for : NodeId
        start = advance
        targets = parse_for_targets
        if match(TokenKind::KeywordIn)
          iter = parse_expression
        else
          @diagnostics << Diagnostic.new(current.span, "expected 'in' in for loop")
          iter = @arena.add_node(NodeKind::Error, current.span)
        end
        body = parse_expressions([TokenKind::KeywordEnd])
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close for")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::For, span, [targets, iter, body])
      end

      private def parse_for_targets : NodeId
        start_token = current
        args = [] of NodeId
        loop do
          if current.kind == TokenKind::Identifier || current.kind == TokenKind::KeywordType
            ident = advance
            sym = @arena.symbols.intern(token_text(ident))
            args << @arena.add_ident(ident.span, sym)
          else
            @diagnostics << Diagnostic.new(current.span, "expected for loop variable")
            args << @arena.add_node(NodeKind::Error, current.span)
            advance unless current.eof?
          end
          break unless match(TokenKind::Comma)
        end
        span = Span.new(start_token.span.start, args.empty? ? start_token.span.finish : node_span(args.last).finish)
        @arena.add_node(NodeKind::Args, span, args)
      end

      private def parse_control(kind : NodeKind, stop : Proc(Bool)? = nil) : NodeId
        start = advance
        children = [] of NodeId
        if !newline_between?(start.span.finish, current.span.start) &&
           expression_follows? &&
           !(kind == NodeKind::Yield && {TokenKind::OrOr, TokenKind::AndAnd}.includes?(current.kind))
          children << parse_expression(0, stop)
        end
        span = if children.empty?
                 start.span
               else
                 span_from(start.span, node_span(children.first))
               end
        @arena.add_node(kind, span, children)
      end

      private def parse_def(kind : NodeKind, end_kind : TokenKind, end_message : String) : NodeId
        start = advance
        name_node, name_span = parse_def_name
        if {TokenKind::BangEqual, TokenKind::AndAnd, TokenKind::OrOr}.includes?(current.kind)
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"#{token_text(current)}\"")
        elsif method_name = def_method_name(name_node)
          if method_name.size > 2 && (method_name.ends_with?("?=") || method_name.ends_with?("!="))
            suffix = Span.new(name_span.finish - 2, name_span.finish - 1)
            @diagnostics << Diagnostic.new(suffix, "unexpected token: \"#{method_name[-2]}\"")
          end
        end
        if current.kind == TokenKind::KeywordEnd && same_line?(name_span, current.span)
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"end\"")
        end
        if kind == NodeKind::MacroDef
          name_info = @arena.node(name_node)
          if name_info.kind == NodeKind::Path
            receiver, member = @arena.children(name_node)
            position = const_like?(receiver) ? node_span(receiver).finish : node_span(member).start
            @diagnostics << Diagnostic.new(Span.new(position, position), "macro can't have a receiver")
          elsif name_info.kind == NodeKind::Ident
            name = @arena.symbols[name_info.payload_index]
            if !name.empty? && name[0].ascii_uppercase?
              position = name_span.finish
              @diagnostics << Diagnostic.new(Span.new(position, position), "macro can't have a receiver")
            end
          end
        end

        params = @arena.add_node(NodeKind::Args, Span.new(name_span.finish, name_span.finish))
        if current.kind == TokenKind::LParen && !newline_between?(name_span.finish, current.span.start)
          params = parse_params
          validate_def_params(params)
          validate_macro_params(params) if kind == NodeKind::MacroDef
        elsif current.kind != TokenKind::Symbol || !span_text(current.span).starts_with?(":")
          diagnose_missing_def_parens(kind, name_span)
        end
        return_type = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if current.kind == TokenKind::Symbol && span_text(current.span).starts_with?(":")
          @diagnostics << Diagnostic.new(current.span, "a space is mandatory between ':' and return type")
        end
        if current.kind == TokenKind::Colon
          colon = advance
          if current.span.start == colon.span.finish
            @diagnostics << Diagnostic.new(current.span, "space required after return type colon")
          end
          return_type = parse_type
        end
        if method_name = def_method_name(name_node)
          if method_name.ends_with?("=") && !operator_method_name_string?(method_name)
            if method_name.ends_with?("?=") || method_name.ends_with?("!=")
              @diagnostics << Diagnostic.new(name_span, "setter method name cannot end with ?= or !=")
            end
            params_children = @arena.children(params)
            invalid = params_children.size > 1
            has_block = false
            params_children.each do |param_id|
              kind = @arena.node(param_id).kind
              if kind == NodeKind::BlockParam
                has_block = true
              elsif kind == NodeKind::Splat || kind == NodeKind::DoubleSplat
                invalid = true
              end
            end
            if has_block
              @diagnostics << Diagnostic.new(current.span, "setter method '#{method_name}' cannot have a block")
            elsif invalid
              @diagnostics << Diagnostic.new(current.span, "setter method '#{method_name}' cannot have more than one parameter")
            end
          end
        end
        if pseudo_method_name?(name_node)
          name = def_method_name(name_node).to_s
          @diagnostics << Diagnostic.new(name_span, "'#{name}' is a pseudo-method and can't be redefined")
        end
        forall_vars = parse_forall_vars
        outer_local_assigns = @local_assigns
        if kind == NodeKind::Def
          @local_assigns = [] of String
          register_param_locals(params)
        end
        @macro_def_depth += 1 if kind == NodeKind::MacroDef
        @def_depth += 1 if kind == NodeKind::Def
        body = parse_expressions([TokenKind::KeywordRescue, TokenKind::KeywordEnsure, end_kind])

        rescue_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        else_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        ensure_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        handlers = false

        if current.kind == TokenKind::KeywordRescue
          handlers = true
          rescue_body = parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, end_kind])
          while current.kind == TokenKind::KeywordRescue
            parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, end_kind])
          end
          rescue_span = span_from_nodes(rescue_body, rescue_body)
          rescue_node = @arena.add_node(NodeKind::Rescue, rescue_span, [rescue_body])
        end

        if current.kind == TokenKind::KeywordElse
          handlers = true
          advance
          else_body = parse_expressions([TokenKind::KeywordEnsure, end_kind])
          else_span = span_from_nodes(else_body, else_body)
          else_node = @arena.add_node(NodeKind::Rescue, else_span, [else_body])
        end

        if current.kind == TokenKind::KeywordEnsure
          handlers = true
          advance
          ensure_body = parse_expressions([end_kind])
          ensure_span = span_from_nodes(ensure_body, ensure_body)
          ensure_node = @arena.add_node(NodeKind::Ensure, ensure_span, [ensure_body])
        end

        body = if handlers
                 tail = if @arena.node(ensure_node).kind != NodeKind::Nop
                          ensure_node
                        elsif @arena.node(else_node).kind != NodeKind::Nop
                          else_node
                        elsif @arena.node(rescue_node).kind != NodeKind::Nop
                          rescue_node
                        else
                          body
                        end
                 span = Span.new(node_span(body).start, node_span(tail).finish)
                 @arena.add_node(NodeKind::Begin, span, [body, rescue_node, else_node, ensure_node])
               else
                 body
               end

        @macro_def_depth -= 1 if kind == NodeKind::MacroDef
        @def_depth -= 1 if kind == NodeKind::Def
        effective_end_message = kind == NodeKind::MacroDef && current.eof? ? "unexpected token: EOF" : end_message
        end_token = expect(end_kind, effective_end_message)
        span = Span.new(start.span.start, end_token.span.finish)
        node = @arena.add_node(kind, span, [name_node, params, return_type, body, forall_vars])
        @local_assigns = outer_local_assigns if kind == NodeKind::Def
        node
      end

      private def parse_abstract_def : NodeId
        start = advance
        name_node, name_span = parse_def_name

        params = @arena.add_node(NodeKind::Args, Span.new(name_span.finish, name_span.finish))
        if current.kind == TokenKind::LParen && !newline_between?(name_span.finish, current.span.start)
          params = parse_params
          validate_def_params(params)
        else
          diagnose_missing_def_parens(NodeKind::Def, name_span)
        end
        return_type = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if match(TokenKind::Colon)
          return_type = parse_type
        end
        if method_name = def_method_name(name_node)
          if method_name.ends_with?("=") && !operator_method_name_string?(method_name)
            if method_name.ends_with?("?=") || method_name.ends_with?("!=")
              @diagnostics << Diagnostic.new(name_span, "setter method name cannot end with ?= or !=")
            end
            params_children = @arena.children(params)
            invalid = params_children.size > 1
            has_block = false
            params_children.each do |param_id|
              kind = @arena.node(param_id).kind
              if kind == NodeKind::BlockParam
                has_block = true
              elsif kind == NodeKind::Splat || kind == NodeKind::DoubleSplat
                invalid = true
              end
            end
            if has_block
              @diagnostics << Diagnostic.new(current.span, "setter method '#{method_name}' cannot have a block")
            elsif invalid
              @diagnostics << Diagnostic.new(current.span, "setter method '#{method_name}' cannot have more than one parameter")
            end
          end
        end
        if pseudo_method_name?(name_node)
          name = def_method_name(name_node).to_s
          @diagnostics << Diagnostic.new(name_span, "'#{name}' is a pseudo-method and can't be redefined")
        end
        forall_vars = parse_forall_vars
        body = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        end_span = if @arena.node(forall_vars).kind != NodeKind::Nop
                     node_span(forall_vars).finish
                   elsif @arena.node(return_type).kind == NodeKind::Nop
                     node_span(params).finish
                   else
                     node_span(return_type).finish
                   end
        span = Span.new(start.span.start, end_span)
        @arena.add_node(NodeKind::Def, span, [name_node, params, return_type, body, forall_vars])
      end

      private def parse_forall_vars : NodeId
        unless current.kind == TokenKind::Identifier && token_text(current) == "forall"
          return @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        end

        start = advance
        vars = [] of NodeId
        seen = [] of String
        loop do
          if current.kind == TokenKind::Identifier
            token = advance
            name = token_text(token)
            if seen.includes?(name)
              @diagnostics << Diagnostic.new(token.span, "duplicated free variable name: #{name}")
            else
              seen << name
            end
            vars << @arena.add_ident(token.span, @arena.symbols.intern(name))
          else
            @diagnostics << Diagnostic.new(current.span, "expecting token 'CONST', not '#{diagnostic_token_text(current)}'")
            advance unless current.eof?
          end
          break unless match(TokenKind::Comma)
        end
        finish = vars.empty? ? start.span.finish : node_span(vars.last).finish
        @arena.add_node(NodeKind::Args, Span.new(start.span.start, finish), vars)
      end

      private def parse_fun : NodeId
        start = advance
        @def_depth += 1
        name_node, name_span = parse_def_name
        external = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if match(TokenKind::Assign)
          if current.kind == TokenKind::String
            str_tok = advance
            sym = @arena.symbols.intern(token_text(str_tok))
            external = @arena.add_ident(str_tok.span, sym)
          else
            external = parse_identifier_or_error
          end
        end
        params = if current.kind == TokenKind::LParen && !newline_between?(name_span.finish, current.span.start)
                   parse_params
                 else
                   @arena.add_node(NodeKind::Args, Span.new(name_span.finish, name_span.finish))
                 end
        if @lib_depth == 0
          if name = def_method_name(name_node)
            if !name.empty? && name[0].ascii_uppercase?
              @diagnostics << Diagnostic.new(name_span, "top-level fun name must start with a lowercase letter")
            end
          end
          @arena.children(params).each do |param_id|
            param = @arena.node(param_id)
            next unless param.kind == NodeKind::Param
            next if param.payload_index < 0
            name = @arena.symbols[param.payload_index]
            children = @arena.children(param_id)
            type_node = children.size >= 2 ? @arena.node(children[children.size - 2]) : nil
            if !name.empty? && name[0].ascii_uppercase? && type_node && type_node.kind == NodeKind::Nop
              @diagnostics << Diagnostic.new(param.span, "top-level fun parameter must have a name")
            end
          end
        end
        validate_param_name_duplicates(params, "duplicated fun parameter name")
        return_type = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if match(TokenKind::Colon)
          return_type = parse_type
        end
        body = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        end_span = node_span(return_type)
        end_span = node_span(external) if @arena.node(external).kind != NodeKind::Nop
        end_span = node_span(params) if @arena.node(return_type).kind == NodeKind::Nop && @arena.node(external).kind == NodeKind::Nop
        if @lib_depth == 0
          if fun_body_start?(current.kind)
            # Only diagnose when a type declaration starts a fun body.
            case current.kind
            when TokenKind::KeywordClass, TokenKind::KeywordModule, TokenKind::KeywordStruct,
                 TokenKind::KeywordEnum, TokenKind::KeywordLib
              @diagnostics << Diagnostic.new(current.span, "can't define class inside fun")
            end
          end
          if current.kind == TokenKind::KeywordEnd || current.kind == TokenKind::Semicolon || fun_body_start?(current.kind)
            body = parse_expressions([TokenKind::KeywordEnd])
            end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close fun")
            end_span = end_token.span
          end
        end
        @def_depth -= 1
        span = Span.new(start.span.start, end_span.finish)
        @arena.add_node(NodeKind::Fun, span, [name_node, params, return_type, external, body])
      end

      private def fun_body_start?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Annotation,
             TokenKind::KeywordAlias,
             TokenKind::KeywordType,
             TokenKind::KeywordFun,
             TokenKind::KeywordDef,
             TokenKind::KeywordMacro,
             TokenKind::KeywordClass,
             TokenKind::KeywordModule,
             TokenKind::KeywordStruct,
             TokenKind::KeywordEnum,
             TokenKind::KeywordLib,
             TokenKind::KeywordAbstract,
             TokenKind::KeywordInclude,
             TokenKind::KeywordExtend,
             TokenKind::KeywordPrivate,
             TokenKind::KeywordProtected
          false
        else
          true
        end
      end

      private def parse_alias : NodeId
        start = advance
        name = parse_path
        name_span = node_span(name)
        if span_text(name_span).ends_with?("?") && current.eof?
          question = Span.new(name_span.finish - 1, name_span.finish)
          @diagnostics << Diagnostic.new(question, "expecting token '=', not '?'")
        else
          expect(TokenKind::Assign, "expecting token '=', not '#{crystal_diagnostic_token_text(current)}'")
        end
        value = parse_type
        span = Span.new(start.span.start, node_span(value).finish)
        @arena.add_node(NodeKind::Alias, span, [name, value])
      end

      private def parse_type_def : NodeId
        start = advance
        name = parse_identifier_or_error
        expect(TokenKind::Assign, "expected '=' in type definition")
        value = parse_type
        span = Span.new(start.span.start, node_span(value).finish)
        @arena.add_node(NodeKind::TypeDef, span, [name, value])
      end

      private def parse_type_block(kind : NodeKind, end_message : String) : NodeId
        start = advance
        name_node = parse_path
        if current.kind == TokenKind::LParen
          args = parse_type_args(-> { current.kind == TokenKind::RParen })
          if {NodeKind::Class, NodeKind::Struct, NodeKind::Module}.includes?(kind)
            args_children = @arena.children(args)
            if args_children.empty?
              finish = node_span(args).finish
              @diagnostics << Diagnostic.new(Span.new(finish, finish), "must specify at least one type var")
            end
            splat_count = 0
            args_children.each do |arg|
              splat_count += 1 if @arena.node(arg).kind == NodeKind::Splat
              if splat_count > 1
                @diagnostics << Diagnostic.new(node_span(arg), "splat type parameter already specified")
                break
              end
            end
            validate_type_param_duplicates(args)
          end
          span = Span.new(node_span(name_node).start, node_span(args).finish)
          name_node = @arena.add_node(NodeKind::TypeApply, span, [name_node, args])
        end
        superclass = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if kind == NodeKind::Enum
          if match(TokenKind::Colon)
            superclass = parse_type
          elsif current.kind == TokenKind::Less
            lt = advance
            @diagnostics << Diagnostic.new(lt.span, "expecting any of these tokens: ;, NEWLINE (not '<')")
            superclass = parse_type
          end
        elsif match(TokenKind::Less)
          superclass = parse_type
        end
        header_finish = @arena.node(superclass).kind == NodeKind::Nop ? node_span(name_node).finish : node_span(superclass).finish
        validate_declaration_separator(kind, header_finish)
        @type_depth += 1 if {NodeKind::Class, NodeKind::Struct, NodeKind::Module, NodeKind::Enum, NodeKind::Lib}.includes?(kind)
        @lib_depth += 1 if kind == NodeKind::Lib
        @enum_depth += 1 if kind == NodeKind::Enum
        body = parse_expressions([TokenKind::KeywordEnd])
        @enum_depth -= 1 if kind == NodeKind::Enum
        validate_enum_body(body) if kind == NodeKind::Enum
        @lib_depth -= 1 if kind == NodeKind::Lib
        @type_depth -= 1 if {NodeKind::Class, NodeKind::Struct, NodeKind::Module, NodeKind::Enum, NodeKind::Lib}.includes?(kind)
        end_token = expect(TokenKind::KeywordEnd, end_message)
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(kind, span, [name_node, superclass, body])
      end

      private def validate_declaration_separator(kind : NodeKind, header_finish : Int32) : Nil
        return if current.eof? || current.kind == TokenKind::KeywordEnd || current.kind == TokenKind::Semicolon
        return if newline_between?(header_finish, current.span.start)
        return if kind != NodeKind::Enum && whitespace_between?(header_finish, current.span.start)

        token = current
        diagnostic_span = token.span
        diagnostic_text = crystal_diagnostic_token_text(token)
        if token.kind == TokenKind::Symbol && diagnostic_text.starts_with?(":") && diagnostic_text.size > 1
          diagnostic_text = diagnostic_text.byte_slice(1, diagnostic_text.bytesize - 1)
          if kind != NodeKind::Enum
            diagnostic_span = Span.new(token.span.start + 1, token.span.finish)
          end
        elsif token.kind == TokenKind::Colon && kind != NodeKind::Enum && peek1.kind != TokenKind::Eof
          token = peek1
          diagnostic_span = token.span
          diagnostic_text = crystal_diagnostic_token_text(token)
        end
        separators = kind == NodeKind::Enum ? ";, NEWLINE" : ";, NEWLINE, SPACE"
        @diagnostics << Diagnostic.new(
          diagnostic_span,
          "expecting any of these tokens: #{separators} (not '#{diagnostic_text}')"
        )
      end

      private def validate_type_param_duplicates(args : NodeId) : Nil
        seen = {} of String => Bool
        @arena.children(args).each do |arg|
          if name = type_param_name(arg)
            if seen.has_key?(name)
              @diagnostics << Diagnostic.new(node_span(arg), "duplicated type parameter name: #{name}")
            else
              seen[name] = true
            end
          end
        end
      end

      private def type_param_name(node_id : NodeId) : String?
        node = @arena.node(node_id)
        case node.kind
        when NodeKind::Ident
          name = @arena.symbols[node.payload_index]
          name.empty? ? nil : name
        when NodeKind::Splat
          child = @arena.children(node_id)[0]?
          child ? type_param_name(child) : nil
        else
          nil
        end
      end

      private def parse_path : NodeId
        left = if current.kind == TokenKind::DoubleColon
                 root = advance
                 root_node = @arena.add_ident(root.span, @arena.symbols.intern("::"))
                 right = parse_identifier_or_error
                 @arena.add_node(NodeKind::Path, Span.new(root.span.start, node_span(right).finish), [root_node, right])
               else
                 parse_identifier_or_error
               end
        while current.kind == TokenKind::DoubleColon
          op = advance
          right = parse_identifier_or_error
          span = span_from_nodes(left, right)
          left = @arena.add_node(NodeKind::Path, span, [left, right])
        end
        left
      end

      private def parse_identifier_or_error : NodeId
        return parse_escaped_macro_literal if escaped_macro_literal_start?
        return parse_macro_expr if macro_expr_start?
        token = current
        if identifier_like_token?(token.kind)
          advance
          symbol_id = @arena.symbols.intern(token_text(token))
          return @arena.add_ident(token.span, symbol_id)
        end
        @diagnostics << Diagnostic.new(token.span, "expected identifier")
        advance unless token.eof?
        @arena.add_node(NodeKind::Error, token.span)
      end

      private def parse_def_name : {NodeId, Span}
        receiver = nil
        if def_receiver_start?(current.kind) &&
           (peek1.kind == TokenKind::Dot || peek1.kind == TokenKind::DoubleColon) &&
           !newline_between?(current.span.finish, peek1.span.start)
          receiver = parse_def_receiver
          advance
        end

        name_node, name_span = parse_def_method_name
        if receiver
          span = Span.new(node_span(receiver).start, name_span.finish)
          name_node = @arena.add_node(NodeKind::Path, span, [receiver, name_node])
          name_span = span
        end
        {name_node, name_span}
      end

      private def def_receiver_start?(kind : TokenKind) : Bool
        kind == TokenKind::Identifier ||
          kind == TokenKind::InstanceVar ||
          kind == TokenKind::ClassVar ||
          kind == TokenKind::GlobalVar ||
          kind == TokenKind::KeywordSelf
      end

      private def parse_def_receiver : NodeId
        node = case current.kind
               when TokenKind::Identifier
                 ident = advance
                 sym = @arena.symbols.intern(token_text(ident))
                 @arena.add_ident(ident.span, sym)
               when TokenKind::InstanceVar
                 ident = advance
                 sym = @arena.symbols.intern(token_text(ident))
                 @arena.add_node(NodeKind::InstanceVar, ident.span, payload_index: sym)
               when TokenKind::ClassVar
                 ident = advance
                 sym = @arena.symbols.intern(token_text(ident))
                 @arena.add_node(NodeKind::ClassVar, ident.span, payload_index: sym)
               when TokenKind::GlobalVar
                 ident = advance
                 sym = @arena.symbols.intern(token_text(ident))
                 @arena.add_node(NodeKind::Global, ident.span, payload_index: sym)
               when TokenKind::KeywordSelf
                 tok = advance
                 sym = @arena.symbols.intern("self")
                 @arena.add_ident(tok.span, sym)
               else
                 tok = current
                 @diagnostics << Diagnostic.new(tok.span, "expected receiver")
                 advance unless tok.eof?
                 @arena.add_node(NodeKind::Error, tok.span)
               end

        while current.kind == TokenKind::DoubleColon && def_receiver_start?(peek1.kind)
          sep = advance
          rhs = parse_def_receiver
          span = Span.new(node_span(node).start, node_span(rhs).finish)
          node = @arena.add_node(NodeKind::Path, span, [node, rhs])
        end
        node
      end

      private def parse_def_method_name : {NodeId, Span}
        if macro_expr_start?
          node = parse_macro_expr
          return {node, node_span(node)}
        end
        token = current
        case token.kind
        when TokenKind::Identifier, TokenKind::KeywordIsAQuestion, TokenKind::KeywordRespondsToQuestion,
             TokenKind::KeywordNilQuestion, TokenKind::KeywordAsQuestion, TokenKind::KeywordAs
          advance
          name = token_text(token)
          span = token.span
          loop do
            if current.kind == TokenKind::Assign && current.span.start == span.finish
              assign = advance
              name += "="
              span = Span.new(span.start, assign.span.finish)
            elsif (current.kind == TokenKind::Question || current.kind == TokenKind::Bang) && current.span.start == span.finish
              suffix = advance
              name += token_text(suffix)
              span = Span.new(span.start, suffix.span.finish)
            else
              break
            end
          end
          sym = @arena.symbols.intern(name)
          node = @arena.add_ident(span, sym)
          while macro_expr_start? && node_span(node).finish == current.span.start
            suffix = parse_macro_expr
            span = Span.new(span.start, node_span(suffix).finish)
            node = @arena.add_node(NodeKind::Path, span, [node, suffix])
          end
          while {TokenKind::Question, TokenKind::Bang, TokenKind::Assign}.includes?(current.kind) && current.span.start == span.finish
            suffix = advance
            suffix_sym = @arena.symbols.intern(token_text(suffix))
            suffix_node = @arena.add_ident(suffix.span, suffix_sym)
            span = Span.new(span.start, suffix.span.finish)
            node = @arena.add_node(NodeKind::Path, span, [node, suffix_node])
          end
          {node, span}
        when TokenKind::KeywordType
          advance
          name = token_text(token)
          span = token.span
          loop do
            if current.kind == TokenKind::Assign && current.span.start == span.finish
              assign = advance
              name += "="
              span = Span.new(span.start, assign.span.finish)
            elsif (current.kind == TokenKind::Question || current.kind == TokenKind::Bang) && current.span.start == span.finish
              suffix = advance
              name += token_text(suffix)
              span = Span.new(span.start, suffix.span.finish)
            else
              break
            end
          end
          sym = @arena.symbols.intern(name)
          node = @arena.add_ident(span, sym)
          {node, span}
        when TokenKind::LBracket
          lb = advance
          rb = expect(TokenKind::RBracket, "expected ']' in operator def")
          span = Span.new(lb.span.start, rb.span.finish)
          name = "[]"
          loop do
            if current.kind == TokenKind::Assign && current.span.start == span.finish
              assign = advance
              name = "[]="
              span = Span.new(span.start, assign.span.finish)
            elsif (current.kind == TokenKind::Question || current.kind == TokenKind::Bang) && current.span.start == span.finish
              suffix = advance
              name += token_text(suffix)
              span = Span.new(span.start, suffix.span.finish)
            else
              break
            end
          end
          sym = @arena.symbols.intern(name)
          node = @arena.add_ident(span, sym)
          {node, span}
        else
          if keyword_token?(token.kind) && !newline_between?(@tokens.peek(-1).span.finish, token.span.start)
            advance
            name = token_text(token)
            span = token.span
            while {TokenKind::Question, TokenKind::Bang, TokenKind::Assign}.includes?(current.kind) && current.span.start == span.finish
              suffix = advance
              name += token_text(suffix)
              span = Span.new(span.start, suffix.span.finish)
            end
            sym = @arena.symbols.intern(name)
            node = @arena.add_ident(span, sym)
            {node, span}
          elsif op_name = operator_method_name(token.kind)
            tok = advance
            span = tok.span
            sym = @arena.symbols.intern(op_name)
            node = @arena.add_ident(span, sym)
            {node, span}
          else
            @diagnostics << Diagnostic.new(token.span, "expected identifier for definition name")
            advance unless token.eof?
            node = @arena.add_node(NodeKind::Error, token.span)
            {node, token.span}
          end
        end
      end

      private def operator_method_name(kind : TokenKind) : String?
        case kind
        when TokenKind::Plus              then "+"
        when TokenKind::Minus             then "-"
        when TokenKind::Star              then "*"
        when TokenKind::Slash             then "/"
        when TokenKind::SlashSlash        then "//"
        when TokenKind::Percent           then "%"
        when TokenKind::Caret             then "^"
        when TokenKind::Ampersand         then "&"
        when TokenKind::Pipe              then "|"
        when TokenKind::Bang              then "!"
        when TokenKind::Tilde             then "~"
        when TokenKind::Backtick          then "`"
        when TokenKind::StarStar          then "**"
        when TokenKind::EqualEqual        then "=="
        when TokenKind::BangEqual         then "!="
        when TokenKind::Less              then "<"
        when TokenKind::LessEqual         then "<="
        when TokenKind::Greater           then ">"
        when TokenKind::GreaterEqual      then ">="
        when TokenKind::Spaceship         then "<=>"
        when TokenKind::TripleEqual       then "==="
        when TokenKind::Match             then "=~"
        when TokenKind::NotMatch          then "!~"
        when TokenKind::ShiftLeft         then "<<"
        when TokenKind::ShiftRight        then ">>"
        when TokenKind::AmpersandPlus     then "&+"
        when TokenKind::AmpersandMinus    then "&-"
        when TokenKind::AmpersandStar     then "&*"
        when TokenKind::AmpersandStarStar then "&**"
        else
          nil
        end
      end

      private OPERATOR_METHOD_NAMES = {
        "+", "-", "*", "/", "//", "%", "^", "&", "|", "!", "~", "`", "**",
        "==", "!=", "<", "<=", ">", ">=", "<=>", "===", "=~", "!~",
        "<<", ">>", "&+", "&-", "&*", "&**", "[]", "[]=", "[]?",
      }

      private def operator_method_name_string?(name : String) : Bool
        OPERATOR_METHOD_NAMES.includes?(name)
      end

      private def assignable_method_name?(name : String) : Bool
        return false if name.empty?
        return true if name == "[]"
        return false if name.ends_with?("?") || name.ends_with?("!")
        return false if operator_method_name_string?(name)
        name.each_char do |ch|
          case ch
          when '?', '!', '+', '-', '*', '/', '%', '<', '>', '=', '&', '|', '^', '~', '[', ']'
            return false
          end
        end
        true
      end

      private def def_method_name(name_node : NodeId) : String?
        node = @arena.node(name_node)
        case node.kind
        when NodeKind::Ident
          @arena.symbols[node.payload_index]
        when NodeKind::Path
          children = @arena.children(name_node)
          return nil if children.empty?
          def_method_name(children.last)
        else
          nil
        end
      end

      private def valid_simple_assignment_target?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        case node.kind
        when NodeKind::Ident
          name = @arena.symbols[node.payload_index]
          return false if name.ends_with?("?") || name.ends_with?("!")
          true
        when NodeKind::InstanceVar, NodeKind::ClassVar
          true
        when NodeKind::Global
          name = @arena.symbols[node.payload_index]
          return false if global_match_data_name?(name)
          true
        when NodeKind::Index
          true
        when NodeKind::Binary
          op = @arena.operator_kind(node.payload_index)
          return false unless op == TokenKind::Dot || op == TokenKind::SafeNav
          rhs = @arena.children(node_id)[1]?
          return false unless rhs
          rhs_node = @arena.node(rhs)
          return false unless rhs_node.kind == NodeKind::Ident
          name = @arena.symbols[rhs_node.payload_index]
          assignable_method_name?(name)
        else
          false
        end
      end

      private def parse_expression(min_bp : Int32 = 0, stop : Proc(Bool)? = nil, allow_var_decl : Bool = true, allow_type_apply : Bool = true) : NodeId
        left = parse_prefix(stop, allow_var_decl, allow_type_apply)
        if {NodeKind::Break, NodeKind::Return, NodeKind::Next}.includes?(@arena.node(left).kind) &&
           newline_between?(node_span(left).finish, current.span.start)
          return left
        end
        left = parse_postfix(left, allow_type_apply)
        loop do
          break if stop && stop.call
          token = current
          if token.kind == TokenKind::Plus && peek1.kind == TokenKind::Plus && adjacent?(token, peek1)
            @diagnostics << Diagnostic.new(peek1.span, "postfix increment is not supported, use `exp += 1`")
          elsif token.kind == TokenKind::Minus && peek1.kind == TokenKind::Minus && adjacent?(token, peek1)
            @diagnostics << Diagnostic.new(peek1.span, "postfix decrement is not supported, use `exp -= 1`")
          end
          if token.kind == TokenKind::DoubleColon && newline_between?(node_span(left).finish, token.span.start)
            break
          end
          if token.kind == TokenKind::DoubleColon &&
             (!adjacent?(token, peek1) || (!adjacent?(left, token) && !command_callee?(left)))
            @diagnostics << Diagnostic.new(token.span, "unexpected token: \"::\"")
          end
          if token.kind == TokenKind::KeywordOf && @arena.node(left).kind == NodeKind::CallWithBlock &&
             !newline_between?(node_span(left).finish, token.span.start)
            @diagnostics << Diagnostic.new(token.span, "unexpected token: \"of\"")
            break
          end
          if (macro_control_start? || macro_expr_start?) && !command_callee?(left)
            break
          end
          if command_callee?(left) && (command_call_start_here? || command_named_arg_start?)
            if newline_between?(node_span(left).finish, token.span.start)
              break
            end
            args = parse_command_args
            register_out_argument(left, args)
            span = Span.new(node_span(left).start, node_span(args).finish)
            left = @arena.add_node(NodeKind::Call, span, [left, args])
            left = parse_postfix(left, allow_type_apply)
            next
          end
          if token.kind == TokenKind::Dot && peek1.kind == TokenKind::LBracket && peek2.kind == TokenKind::RBracket
            dot = advance
            lb = advance
            rb = expect(TokenKind::RBracket, "expected ']' in operator call")
            name_span = Span.new(lb.span.start, rb.span.finish)
            name = "[]"
            if current.kind == TokenKind::Question && current.span.start == name_span.finish
              q = advance
              name = "[]?"
              name_span = Span.new(name_span.start, q.span.finish)
            end
            if current.kind == TokenKind::Assign && current.span.start == name_span.finish
              assign = advance
              name = "[]="
              name_span = Span.new(name_span.start, assign.span.finish)
            end
            sym = @arena.symbols.intern(name)
            rhs = @arena.add_ident(name_span, sym)
            span = Span.new(node_span(left).start, name_span.finish)
            left = @arena.add_binary(dot.kind, span, left, rhs)
            left = parse_postfix(left, allow_type_apply)
            next
          end
          if token.kind == TokenKind::Dot && peek1.kind == TokenKind::LBracket && peek2.kind != TokenKind::RBracket
            advance
            advance
            indices = [] of NodeId
            if current.kind != TokenKind::RBracket
              loop do
                indices << parse_argument
                if current.kind == TokenKind::Comma
                  advance
                  break if current.kind == TokenKind::RBracket
                  next
                end
                break
              end
            end
            end_token = expect(TokenKind::RBracket, "expected ']' to close index")
            flags = 0_u16
            if current.kind == TokenKind::Question && adjacent?(end_token, current)
              end_token = advance
              flags = 1_u16
            end
            span = Span.new(node_span(left).start, end_token.span.finish)
            left = @arena.add_node(NodeKind::Index, span, [left] + indices, flags: flags)
            left = parse_postfix(left, allow_type_apply)
            next
          end
          # Comma forms tuples only at very low precedence (below assignment at 10)
          # This prevents `a.foo, a.bar` from being parsed as `a.(foo, a.bar)`
          if token.kind == TokenKind::Comma && min_bp < 10
            advance
            if {TokenKind::RParen, TokenKind::RBracket, TokenKind::RBrace}.includes?(current.kind) || (stop && stop.call)
              unless @arena.node(left).kind == NodeKind::Tuple
                left = @arena.add_node(NodeKind::Tuple, node_span(left), [left], flags: 1_u16)
              end
              next
            end
            # Stop at assignment operators so multi-assign `a, b = 1, 2` parses correctly
            # as Assign(Tuple(a, b), Tuple(1, 2)) instead of Tuple(a, Assign(b, Tuple(1, 2)))
            right = parse_expression(0, -> { assignment_op?(current.kind) }, allow_var_decl, allow_type_apply)
            children = [] of NodeId
            if @arena.node(left).kind == NodeKind::Tuple
              children.concat(@arena.children(left))
            else
              children << left
            end
            if @arena.node(right).kind == NodeKind::Tuple
              children.concat(@arena.children(right))
            else
              children << right
            end
            span = Span.new(node_span(children.first).start, node_span(children.last).finish)
            left = @arena.add_node(NodeKind::Tuple, span, children, flags: 1_u16)
            left = parse_postfix(left, allow_type_apply)
            next
          end
          if token.kind == TokenKind::Question
            if type_expr_like?(left) && adjacent?(left, token) && type_suffix_terminator?(peek1.kind)
              q = advance
              nil_node = @arena.add_node(NodeKind::LiteralNil, q.span)
              span = span_from_nodes(left, nil_node)
              left = @arena.add_binary(TokenKind::Pipe, span, left, nil_node)
              left = parse_postfix(left, allow_type_apply)
              next
            end
            break if min_bp >= 11
            advance
            then_stop = -> { current.kind == TokenKind::Colon || (stop ? stop.call : false) }
            then_expr = parse_expression(0, then_stop, allow_var_decl: false, allow_type_apply: allow_type_apply)
            expect(TokenKind::Colon, "expected ':' in ternary")
            else_expr = parse_expression(min_bp, stop, allow_var_decl, allow_type_apply)
            span = Span.new(node_span(left).start, node_span(else_expr).finish)
            left = @arena.add_node(NodeKind::Ternary, span, [left, then_expr, else_expr])
            left = parse_postfix(left, allow_type_apply)
            next
          end
          if token.kind == TokenKind::KeywordAs || token.kind == TokenKind::KeywordAsQuestion
            @diagnostics << Diagnostic.new(token.span, "unexpected token: \"#{token_text(token)}\"")
            advance
            _ = parse_expression(0, stop, allow_var_decl, allow_type_apply) if expression_follows?
            return @arena.add_node(NodeKind::Error, token.span)
          end
          if token.kind == TokenKind::KeywordRescue
            break if newline_between?(node_span(left).finish, token.span.start)
            advance
            right = parse_expression(0, stop, allow_var_decl, allow_type_apply)
            span = Span.new(node_span(left).start, node_span(right).finish)
            left = @arena.add_node(NodeKind::Rescue, span, [left, right])
            left = parse_postfix(left, allow_type_apply)
            next
          elsif token.kind == TokenKind::KeywordEnsure
            break if newline_between?(node_span(left).finish, token.span.start)
            advance
            right = parse_expression(0, stop, allow_var_decl, allow_type_apply)
            span = Span.new(node_span(left).start, node_span(right).finish)
            left = @arena.add_node(NodeKind::Ensure, span, [left, right])
            left = parse_postfix(left, allow_type_apply)
            next
          end
          bp = infix_binding_power(token.kind)
          break unless bp
          lbp, rbp = bp
          break if lbp < min_bp
          op = advance
          if (op.kind == TokenKind::DotDot || op.kind == TokenKind::DotDotDot) && (expression_stop? || (stop && stop.call))
            right = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
            span = Span.new(node_span(left).start, node_span(right).finish)
            left = build_infix(op.kind, span, left, right, op.span)
            left = parse_postfix(left, allow_type_apply)
            next
          end
          if (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav) &&
             current.kind == TokenKind::Bang
            bang = advance
            sym = @arena.symbols.intern("!")
            right = @arena.add_ident(bang.span, sym)
            span = Span.new(node_span(left).start, bang.span.finish)
            left = @arena.add_binary(op.kind, span, left, right)
            left = parse_postfix(left, allow_type_apply)
            next
          end
          right = if (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav) && soft_identifier_kind?(current.kind)
                    tok = advance
                    sym = @arena.symbols.intern(token_text(tok))
                    member = @arena.add_ident(tok.span, sym)
                    parse_postfix(member, allow_type_apply: false)
                  elsif (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav) &&
                        (current.kind == TokenKind::InstanceVar || current.kind == TokenKind::ClassVar)
                    tok = advance
                    sym = @arena.symbols.intern(token_text(tok))
                    kind = tok.kind == TokenKind::InstanceVar ? NodeKind::InstanceVar : NodeKind::ClassVar
                    member = @arena.add_node(kind, tok.span, payload_index: sym)
                    parse_postfix(member, allow_type_apply: false)
                  elsif (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav) && keyword_token?(current.kind)
                    tok = advance
                    sym = @arena.symbols.intern(token_text(tok))
                    member = @arena.add_ident(tok.span, sym)
                    parse_postfix(member, allow_type_apply: false)
                  elsif (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav || op.kind == TokenKind::DoubleColon) && operator_name_token?(current.kind)
                    tok = advance
                    sym = @arena.symbols.intern(token_text(tok))
                    @arena.add_ident(tok.span, sym)
                  elsif op.kind == TokenKind::DotDot || op.kind == TokenKind::DotDotDot
                    if expression_stop?
                      @arena.add_node(NodeKind::Nop, Span.new(op.span.finish, op.span.finish))
                    else
                      parse_expression(rbp, stop, allow_var_decl, allow_type_apply)
                    end
                  else
                    parse_expression(rbp, stop, allow_var_decl, allow_type_apply)
                  end
          if (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav) && @arena.node(right).kind == NodeKind::Index
            index_node = @arena.node(right)
            index_children = @arena.children(right).to_a
            base = index_children.shift
            member_span = Span.new(node_span(left).start, node_span(base).finish)
            member = build_infix(op.kind, member_span, left, base, op.span)
            span = Span.new(node_span(left).start, index_node.span.finish)
            left = @arena.add_node(NodeKind::Index, span, [member] + index_children, flags: index_node.flags)
            left = parse_postfix(left, allow_type_apply)
            next
          end
          span = Span.new(node_span(left).start, node_span(right).finish)
          if (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav) && responds_to_without_args?(right) && expression_stop?
            @diagnostics << Diagnostic.new(node_span(right), "responds_to? requires an argument")
          end
          left = build_infix(op.kind, span, left, right, op.span)
          left = parse_postfix(left, allow_type_apply)
        end
        left
      end

      private def parse_prefix(stop : Proc(Bool)? = nil, allow_var_decl : Bool = true, allow_type_apply : Bool = true) : NodeId
        token = current
        if token.kind == TokenKind::KeywordUninitialized &&
           peek1.kind != TokenKind::Assign &&
           !@local_assigns.includes?("uninitialized")
          return parse_builtin_like_call(token)
        end
        if allow_var_decl && var_decl_start?(token.kind) && peek1.kind == TokenKind::Colon && !adjacent?(token, peek1)
          return parse_var_decl(stop)
        end
        if macro_control_start?
          return parse_macro_control
        end
        if macro_expr_start?
          return parse_macro_expr
        end
        if macro_var_start?
          return parse_macro_var
        end
        if escaped_macro_literal_start?
          return parse_escaped_macro_literal
        end
        if keyword_token?(token.kind) && @local_assigns.includes?(token_text(token))
          advance
          symbol_id = @arena.symbols.intern(token_text(token))
          return @arena.add_ident(token.span, symbol_id)
        end
        case token.kind
        when TokenKind::Identifier,
             TokenKind::KeywordType,
             TokenKind::KeywordOf,
             TokenKind::KeywordProperty,
             TokenKind::KeywordGetter,
             TokenKind::KeywordSetter,
             TokenKind::KeywordUninitialized,
             TokenKind::KeywordUnion
          advance
          symbol_id = @arena.symbols.intern(token_text(token))
          @arena.add_ident(token.span, symbol_id)
        when TokenKind::InstanceVar
          advance
          symbol_id = @arena.symbols.intern(token_text(token))
          @arena.add_node(NodeKind::InstanceVar, token.span, payload_index: symbol_id)
        when TokenKind::ClassVar
          advance
          symbol_id = @arena.symbols.intern(token_text(token))
          @arena.add_node(NodeKind::ClassVar, token.span, payload_index: symbol_id)
        when TokenKind::GlobalVar
          advance
          parse_global_var(token)
        when TokenKind::Number
          advance
          @arena.add_literal_node(LiteralKind::Number, token.span)
        when TokenKind::String
          advance
          @arena.add_literal_node(LiteralKind::String, token.span)
        when TokenKind::Char
          advance
          @arena.add_literal_node(LiteralKind::Char, token.span)
        when TokenKind::Regex
          advance
          @arena.add_literal_node(LiteralKind::Regex, token.span)
        when TokenKind::KeywordTrue, TokenKind::KeywordFalse
          advance
          flags = token.kind == TokenKind::KeywordTrue ? 1_u16 : 0_u16
          @arena.add_node(NodeKind::LiteralBool, token.span, flags: flags)
        when TokenKind::KeywordNil
          advance
          @arena.add_node(NodeKind::LiteralNil, token.span)
        when TokenKind::KeywordIf
          parse_if
        when TokenKind::KeywordUnless
          parse_unless
        when TokenKind::KeywordCase
          parse_case
        when TokenKind::KeywordBegin
          parse_begin
        when TokenKind::KeywordSuper
          advance
          sym = @arena.symbols.intern("super")
          @arena.add_ident(token.span, sym)
        when TokenKind::KeywordClass
          advance
          sym = @arena.symbols.intern(token_text(token))
          @arena.add_ident(token.span, sym)
        when TokenKind::KeywordIsAQuestion, TokenKind::KeywordRespondsToQuestion, TokenKind::KeywordNilQuestion
          advance
          symbol_id = @arena.symbols.intern(token_text(token))
          @arena.add_ident(token.span, symbol_id)
        when TokenKind::KeywordAs, TokenKind::KeywordAsQuestion
          advance
          sym = @arena.symbols.intern(token_text(token))
          @arena.add_ident(token.span, sym)
        when TokenKind::Arrow
          op = advance
          params_node = nil
          if current.kind == TokenKind::LParen
            params_node = parse_params
          end
          if current.kind == TokenKind::Symbol && span_text(current.span).starts_with?(":")
            @diagnostics << Diagnostic.new(current.span, "a space is mandatory between ':' and return type")
          end
          if current.kind == TokenKind::Colon
            advance
            _ = parse_type
          end
          if current.kind == TokenKind::LBrace || current.kind == TokenKind::KeywordDo
            return parse_lambda_literal(op, params_node)
          end
          target = parse_proc_pointer_target
          span = Span.new(op.span.start, node_span(target).finish)
          @arena.add_unary(op.kind, span, target)
        when TokenKind::KeywordOut
          advance
          sym = @arena.symbols.intern("out")
          @arena.add_ident(token.span, sym)
        when TokenKind::SafeNav
          op = advance
          expr = if current.kind == TokenKind::Bang
                   bang = advance
                   sym = @arena.symbols.intern("!")
                   @arena.add_ident(bang.span, sym)
                 elsif current.kind == TokenKind::LBracket
                   sym = @arena.symbols.intern(".")
                   target = @arena.add_ident(op.span, sym)
                   parse_postfix(target, allow_type_apply: false)
                 elsif keyword_token?(current.kind)
                   tok = advance
                   sym = @arena.symbols.intern(token_text(tok))
                   target = @arena.add_ident(tok.span, sym)
                   parse_postfix(target, allow_type_apply: false)
                 elsif operator_name_token?(current.kind)
                   tok = advance
                   sym = @arena.symbols.intern(token_text(tok))
                   member = @arena.add_ident(tok.span, sym)
                   if command_call_start?(current.kind) && !newline_between?(tok.span.finish, current.span.start)
                     args = parse_command_args
                     span = Span.new(tok.span.start, node_span(args).finish)
                     @arena.add_node(NodeKind::Call, span, [member, args])
                   else
                     member
                   end
                 else
                   parse_expression(prefix_binding_power(TokenKind::Dot), stop, allow_var_decl, allow_type_apply)
                 end
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_unary(TokenKind::SafeNav, span, expr)
        when TokenKind::KeywordSelf
          advance
          symbol_id = @arena.symbols.intern("self")
          @arena.add_ident(token.span, symbol_id)
        when TokenKind::Symbol
          advance
          @arena.add_literal_node(LiteralKind::Symbol, token.span)
        when TokenKind::Dot
          if peek1.kind == TokenKind::LBracket
            dot = advance
            advance
            rb = expect(TokenKind::RBracket, "expected ']' after implicit '.'")
            name = ".[]"
            finish = rb.span.finish
            if current.kind == TokenKind::Assign && adjacent?(rb, current)
              finish = advance.span.finish
              name = ".[]="
            end
            sym = @arena.symbols.intern(name)
            @arena.add_ident(Span.new(dot.span.start, finish), sym)
          elsif implicit_dot_target?(peek1.kind)
            dot = advance
            ident = advance
            name = "." + token_text(ident)
            span = Span.new(dot.span.start, ident.span.finish)
            sym = @arena.symbols.intern(name)
            @arena.add_ident(span, sym)
          else
            if invalid_dot_method_name?(peek1)
              @diagnostics << Diagnostic.new(peek1.span, "expecting any of these tokens: #{CALL_METHOD_NAME_TOKENS} (not '#{diagnostic_token_text(peek1)}')")
            else
              @diagnostics << Diagnostic.new(token.span, "unexpected token in expression")
            end
            advance unless token.eof?
            @arena.add_node(NodeKind::Error, token.span)
          end
        when TokenKind::DotDot, TokenKind::DotDotDot
          op = advance
          nil_node = @arena.add_node(NodeKind::Nop, Span.new(op.span.start, op.span.start))
          if expression_stop? || (stop && stop.call)
            right = @arena.add_node(NodeKind::Nop, Span.new(op.span.finish, op.span.finish))
            span = Span.new(op.span.start, node_span(right).finish)
          else
            right = parse_expression(prefix_binding_power(op.kind), stop, allow_var_decl, allow_type_apply)
            span = Span.new(op.span.start, node_span(right).finish)
          end
          flags = op.kind == TokenKind::DotDotDot ? 1_u16 : 0_u16
          @arena.add_node(NodeKind::Range, span, [nil_node, right], flags: flags)
        when TokenKind::Ampersand
          op = advance
          expr = parse_expression(prefix_binding_power(op.kind), stop, allow_var_decl, allow_type_apply)
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_unary(op.kind, span, expr)
        when TokenKind::KeywordInclude, TokenKind::KeywordExtend
          parse_include_extend(token)
        when TokenKind::KeywordAlignof, TokenKind::KeywordInstanceAlignof, TokenKind::KeywordInstanceSizeof,
             TokenKind::KeywordOffsetof, TokenKind::KeywordPointerof, TokenKind::KeywordSizeof,
             TokenKind::KeywordTypeof, TokenKind::KeywordSelect, TokenKind::KeywordWith
          parse_builtin_like_call(token)
        when TokenKind::KeywordAsm
          parse_asm_expr(token)
        when TokenKind::Percent
          if @macro_def_depth > 0 && peek1.kind == TokenKind::Identifier && adjacent?(token, peek1)
            percent = advance
            ident = advance
            name = "%" + token_text(ident)
            span = Span.new(percent.span.start, ident.span.finish)
            sym = @arena.symbols.intern(name)
            @arena.add_ident(span, sym)
          else
            @diagnostics << Diagnostic.new(token.span, "unexpected token in expression")
            advance unless token.eof?
            @arena.add_node(NodeKind::Error, token.span)
          end
        when TokenKind::Star
          op = advance
          expr = parse_expression(prefix_binding_power(TokenKind::Star), stop, allow_var_decl, allow_type_apply)
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_node(NodeKind::Splat, span, [expr])
        when TokenKind::StarStar
          op = advance
          expr = parse_expression(prefix_binding_power(TokenKind::Star), stop, allow_var_decl, allow_type_apply)
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_node(NodeKind::DoubleSplat, span, [expr])
        when TokenKind::KeywordReturn
          parse_control(NodeKind::Return, stop)
        when TokenKind::KeywordBreak
          parse_control(NodeKind::Break, stop)
        when TokenKind::KeywordNext
          parse_control(NodeKind::Next, stop)
        when TokenKind::KeywordYield
          parse_control(NodeKind::Yield, stop)
        when TokenKind::DoubleColon
          advance
          parse_path
        when TokenKind::LParen
          advance
          exprs = parse_expressions([TokenKind::RParen])
          expect(TokenKind::RParen, "expected ')' to close expression")
          children = @arena.children(exprs)
          if children.size == 1
            children.first
          else
            exprs
          end
        when TokenKind::LBracket
          parse_array
        when TokenKind::LBrace
          parse_brace_literal
        when TokenKind::Plus, TokenKind::Minus, TokenKind::Bang, TokenKind::Tilde,
             TokenKind::AmpersandPlus, TokenKind::AmpersandMinus, TokenKind::AmpersandStar
          op = advance
          expr = parse_expression(prefix_binding_power(op.kind), stop, allow_var_decl, allow_type_apply)
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_unary(op.kind, span, expr)
        else
          if invalid_dot_method_name?(token)
            @diagnostics << Diagnostic.new(token.span, "expecting any of these tokens: #{CALL_METHOD_NAME_TOKENS} (not '#{diagnostic_token_text(token)}')")
          elsif token.kind == TokenKind::Pipe
            @diagnostics << Diagnostic.new(token.span, "unexpected token: \"|\"")
          elsif token.kind == TokenKind::RBrace
            @diagnostics << Diagnostic.new(token.span, "expecting token 'EOF', not '}'")
          elsif token.kind == TokenKind::KeywordDo
            @diagnostics << Diagnostic.new(token.span, "unexpected token: \"do\"")
          elsif token.kind == TokenKind::KeywordWhen || token.kind == TokenKind::KeywordThen
            @diagnostics << Diagnostic.new(token.span, "expecting token 'EOF', not '#{token_text(token)}'")
          else
            @diagnostics << Diagnostic.new(token.span, "unexpected token in expression")
          end
          advance unless token.eof?
          @arena.add_node(NodeKind::Error, token.span)
        end
      end

      private def parse_postfix(left : NodeId, allow_type_apply : Bool = true) : NodeId
        loop do
          if escaped_macro_literal_start? && adjacent?(left, current)
            suffix = parse_escaped_macro_literal
            span = span_from_nodes(left, suffix)
            left = @arena.add_node(NodeKind::Path, span, [left, suffix])
            next
          end
          if macro_expr_start? && adjacent?(left, current)
            suffix = parse_macro_expr
            span = span_from_nodes(left, suffix)
            left = @arena.add_node(NodeKind::Path, span, [left, suffix])
            while {TokenKind::Question, TokenKind::Bang, TokenKind::Assign}.includes?(current.kind) && adjacent?(left, current)
              token = advance
              sym = @arena.symbols.intern(token_text(token))
              suffix_node = @arena.add_ident(token.span, sym)
              span = span_from_nodes(left, suffix_node)
              left = @arena.add_node(NodeKind::Path, span, [left, suffix_node])
            end
            next
          end
          case current.kind
          when TokenKind::LParen
            break unless adjacent?(left, current)
            if allow_type_apply && const_like?(left) && adjacent?(left, current)
              args = parse_type_args_in_expr
              span = Span.new(node_span(left).start, node_span(args).finish)
              left = @arena.add_node(NodeKind::TypeApply, span, [left, args])
            else
              args = cast_call_single_arg?(left) ? parse_cast_args : parse_args
              span = Span.new(node_span(left).start, node_span(args).finish)
              left = @arena.add_node(NodeKind::Call, span, [left, args])
            end
          when TokenKind::LBracket
            break unless adjacent?(left, current)
            start = advance
            indices = [] of NodeId
            if current.kind != TokenKind::RBracket
              loop do
                indices << parse_argument
                if current.kind == TokenKind::Comma
                  advance
                  break if current.kind == TokenKind::RBracket
                  next
                end
                break
              end
            end
            end_token = expect(TokenKind::RBracket, "expected ']' to close index")
            flags = 0_u16
            if current.kind == TokenKind::Question && adjacent?(end_token, current)
              end_token = advance
              flags = 1_u16
            end
            span = Span.new(node_span(left).start, end_token.span.finish)
            left = @arena.add_node(NodeKind::Index, span, [left] + indices, flags: flags)
          when TokenKind::LBrace
            if macro_control_start? || macro_expr_start? || escaped_macro_literal_start?
              break
            end
            if const_like?(left) && !newline_between?(node_span(left).finish, current.span.start)
              literal = parse_brace_literal
              if @arena.node(literal).kind == NodeKind::NamedTuple
                @diagnostics << Diagnostic.new(node_span(literal), "can't use named tuple syntax for Hash-like literal, use '=>'")
              end
              span = Span.new(node_span(left).start, node_span(literal).finish)
              left = @arena.add_node(NodeKind::TypeApply, span, [left, literal])
              next
            end
            break if newline_between?(node_span(left).finish, current.span.start)
            break unless block_callee?(left)
            left = parse_brace_block(left)
          when TokenKind::KeywordDo
            left = parse_block_call(left)
          else
            break
          end
        end
        left
      end

      private def parse_block_call(call : NodeId) : NodeId
        start = advance
        if call_has_block_arg?(call)
          @diagnostics << Diagnostic.new(node_span(call), "can't use captured and non-captured blocks together")
        end
        block_params = parse_block_params
        outer_local_assigns = @local_assigns.dup
        register_param_locals(block_params)
        body = parse_expressions([TokenKind::KeywordRescue, TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])

        rescue_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        else_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        ensure_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))

        if current.kind == TokenKind::KeywordRescue
          rescue_body = parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          while current.kind == TokenKind::KeywordRescue
            parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          end
          rescue_span = span_from_nodes(rescue_body, rescue_body)
          rescue_node = @arena.add_node(NodeKind::Rescue, rescue_span, [rescue_body])
        end

        if current.kind == TokenKind::KeywordElse
          advance
          else_body = parse_expressions([TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          else_span = span_from_nodes(else_body, else_body)
          else_node = @arena.add_node(NodeKind::Rescue, else_span, [else_body])
        end

        if current.kind == TokenKind::KeywordEnsure
          advance
          ensure_body = parse_expressions([TokenKind::KeywordEnd])
          ensure_span = span_from_nodes(ensure_body, ensure_body)
          ensure_node = @arena.add_node(NodeKind::Ensure, ensure_span, [ensure_body])
        end

        block_body = if @arena.node(rescue_node).kind == NodeKind::Nop &&
                        @arena.node(else_node).kind == NodeKind::Nop &&
                        @arena.node(ensure_node).kind == NodeKind::Nop
                       body
                     else
                       span = span_from_nodes(body, ensure_node)
                       @arena.add_node(NodeKind::Begin, span, [body, rescue_node, else_node, ensure_node])
                     end

        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close block")
        span = Span.new(node_span(call).start, end_token.span.finish)
        call_node = @arena.node(call)
        result = if call_node.kind == NodeKind::Binary && (op = @arena.operator_kind(call_node.payload_index)) && (op == TokenKind::Dot || op == TokenKind::DoubleColon)
                   lhs = @arena.children(call)[0]
                   rhs = @arena.children(call)[1]
                   rhs_span = node_span(rhs)
                   block_span = Span.new(rhs_span.start, end_token.span.finish)
                   rhs_with_block = @arena.add_node(NodeKind::CallWithBlock, block_span, [rhs, block_params, block_body])
                   span = Span.new(node_span(lhs).start, end_token.span.finish)
                   @arena.add_node(NodeKind::Binary, span, [lhs, rhs_with_block], payload_index: call_node.payload_index)
                 else
                   @arena.add_node(NodeKind::CallWithBlock, span, [call, block_params, block_body])
                 end
        remove_param_locals(block_params, outer_local_assigns)
        result
      end

      private def parse_block_params : NodeId
        return @arena.add_node(NodeKind::Args, Span.new(current.span.start, current.span.start)) unless current.kind == TokenKind::Pipe
        start = advance
        params = [] of NodeId
        saw_splat = false
        seen_names = {} of String => Bool
        until current.kind == TokenKind::Pipe || current.eof?
          if macro_control_start?
            params << parse_macro_control
            next
          elsif current.kind == TokenKind::Percent && peek1.kind == TokenKind::Identifier && adjacent?(current, peek1)
            params << parse_macro_var
          elsif soft_identifier_kind?(current.kind) || current.kind == TokenKind::KeywordWhen
            ident_token = advance
            name = token_text(ident_token)
            if name != "_" && seen_names.has_key?(name)
              @diagnostics << Diagnostic.new(ident_token.span, "duplicated block parameter name: #{name}")
            elsif name != "_"
              seen_names[name] = true
            end
            sym = @arena.symbols.intern(token_text(ident_token))
            params << @arena.add_ident(ident_token.span, sym)
          elsif keyword_token?(current.kind)
            tok = advance
            @diagnostics << Diagnostic.new(tok.span, "cannot use '#{token_text(tok)}' as a block parameter name")
          elsif current.kind == TokenKind::LParen
            params << parse_block_destructure(seen_names)
          elsif current.kind == TokenKind::Star
            if saw_splat
              @diagnostics << Diagnostic.new(current.span, "splat block parameter already specified")
            end
            saw_splat = true
            star = advance
            name_node = if current.kind == TokenKind::Identifier
                          ident = advance
                          name = token_text(ident)
                          if seen_names.has_key?(name)
                            @diagnostics << Diagnostic.new(ident.span, "duplicated block parameter name: #{name}")
                          else
                            seen_names[name] = true
                          end
                          sym = @arena.symbols.intern(token_text(ident))
                          @arena.add_ident(ident.span, sym)
                        else
                          @arena.add_node(NodeKind::Nop, Span.new(star.span.finish, star.span.finish))
                        end
            span = Span.new(star.span.start, node_span(name_node).finish)
            params << @arena.add_node(NodeKind::Splat, span, [name_node])
          else
            @diagnostics << Diagnostic.new(current.span, "expected block parameter")
            advance
          end
          break unless match(TokenKind::Comma)
        end
        end_pipe = expect(TokenKind::Pipe, "expecting ',' or '|', not #{diagnostic_token_text(current)}")
        span = Span.new(start.span.start, end_pipe.span.finish)
        @arena.add_node(NodeKind::Args, span, params)
      end

      private def parse_block_destructure(seen_names : Hash(String, Bool)) : NodeId
        lparen = advance
        last = lparen
        depth = 1
        prev_param = false
        prev_token_kind = TokenKind::Unknown
        until depth == 0 || current.eof?
          tok = advance
          last = tok
          if keyword_token?(tok.kind) && tok.kind != TokenKind::KeywordType
            @diagnostics << Diagnostic.new(tok.span, "cannot use '#{token_text(tok)}' as a block parameter name")
          end
          if depth == 1
            case tok.kind
            when TokenKind::Identifier, TokenKind::InstanceVar, TokenKind::ClassVar
              name = token_text(tok)
              if name != "_" && seen_names.has_key?(name)
                @diagnostics << Diagnostic.new(tok.span, "duplicated block parameter name: #{name}")
              elsif name != "_"
                seen_names[name] = true
              end
              if prev_param && prev_token_kind != TokenKind::Comma && prev_token_kind != TokenKind::LParen
                @diagnostics << Diagnostic.new(tok.span, "expecting ',' or ')', not #{token_text(tok)}")
              end
              prev_param = true
              prev_token_kind = tok.kind
            when TokenKind::Comma, TokenKind::LParen
              prev_param = false
              prev_token_kind = tok.kind
            when TokenKind::RParen
              prev_param = false
              prev_token_kind = tok.kind
            else
              prev_token_kind = tok.kind
            end
          end
          case tok.kind
          when TokenKind::LParen
            depth += 1
          when TokenKind::RParen
            depth -= 1
          end
        end
        span = Span.new(lparen.span.start, last.span.finish)
        @arena.add_node(NodeKind::Nop, span)
      end

      private def parse_command_args : NodeId
        start_pos = current.span.start
        return @arena.add_node(NodeKind::Args, Span.new(start_pos, start_pos)) if command_args_stop?
        args = [] of NodeId
        loop do
          if command_named_arg_start?
            name = advance
            advance
            value = parse_expression(0, -> { current.kind == TokenKind::Comma || command_args_stop? })
            name_text = token_text(name)
            name_text = name_text[1...-1] if name.kind == TokenKind::String && name_text.size >= 2
            if name_text.empty?
              @diagnostics << Diagnostic.new(name.span, "named argument cannot have an empty name")
            end
            symbol_id = @arena.symbols.intern(name_text)
            span = Span.new(name.span.start, node_span(value).finish)
            args << @arena.add_named_arg(symbol_id, span, value)
          elsif var_decl_start?(current.kind) && peek1.kind == TokenKind::Colon
            args << parse_var_decl(-> { current.kind == TokenKind::Comma || command_args_stop? })
          else
            args << parse_expression(0, -> { current.kind == TokenKind::Comma || command_args_stop? })
          end
          break unless current.kind == TokenKind::Comma
          comma = advance
          if command_args_stop?
            @diagnostics << Diagnostic.new(comma.span, "invalid trailing comma in call")
            break
          end
        end
        span = span_from_nodes(args.first, args.last)
        args_node = @arena.add_node(NodeKind::Args, span, args)
        validate_named_arg_duplicates(args_node)
        validate_call_arg_order(args_node)
        args_node
      end

      private def command_named_arg_start? : Bool
        named_arg_name_token?(current.kind) && peek1.kind == TokenKind::Colon && adjacent?(current, peek1)
      end

      private def command_args_stop? : Bool
        return true if @macro_depth > 0 && (macro_control_end? || macro_expr_end?)
        return false if command_named_arg_start?
        return false if var_decl_start?(current.kind) && peek1.kind == TokenKind::Colon
        expression_stop?
      end

      private def parse_include_extend(token : Token) : NodeId
        advance
        name = token_text(token)
        sym = @arena.symbols.intern(name)
        callee = @arena.add_ident(token.span, sym)
        args = parse_command_args
        if @arena.children(args).empty?
          @diagnostics << Diagnostic.new(token.span, "#{name} expects at least one argument")
        end
        span = Span.new(token.span.start, node_span(args).finish)
        @arena.add_node(NodeKind::Call, span, [callee, args])
      end

      private def parse_builtin_like_call(token : Token) : NodeId
        advance
        name = token_text(token)
        sym = @arena.symbols.intern(name)
        callee = @arena.add_ident(token.span, sym)
        args_children = [] of NodeId
        type_builtin = {
          TokenKind::KeywordAlignof,
          TokenKind::KeywordInstanceAlignof,
          TokenKind::KeywordInstanceSizeof,
          TokenKind::KeywordSizeof,
          TokenKind::KeywordUninitialized,
        }.includes?(token.kind)
        if match(TokenKind::LParen)
          if current.kind != TokenKind::RParen
            arg_index = 0
            loop do
              arg_stop = -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen }
              if type_builtin || (token.kind == TokenKind::KeywordOffsetof && arg_index == 0)
                args_children << parse_type_arg(arg_stop)
              else
                args_children << parse_expression(0, arg_stop)
              end
              arg_index += 1
              break unless match(TokenKind::Comma)
              break if current.kind == TokenKind::RParen
            end
          end
          expect(TokenKind::RParen, "expected ')' after #{name}")
        else
          if type_builtin
            args_children << parse_type_arg(-> { expression_stop? })
          else
            args_children << parse_expression(0, -> { expression_stop? })
          end
        end
        if token.kind == TokenKind::KeywordOffsetof && args_children.size >= 2
          offset = @arena.node(args_children[1])
          valid_offset = offset.kind == NodeKind::InstanceVar || offset.kind == NodeKind::Ident
          if offset.kind == NodeKind::LiteralNumber
            text = span_text(offset.span)
            valid_offset = !text.includes?('.') && !text.includes?('e') && !text.includes?('E')
          end
          unless valid_offset
            message = if offset.kind == NodeKind::LiteralNumber
                        "expecting an integer offset, not '#{span_text(offset.span)}'"
                      elsif offset.kind == NodeKind::LiteralChar
                        text = span_text(offset.span)
                        value = text.size >= 2 ? text.byte_slice(1, text.bytesize - 2) : text
                        "expecting an instance variable or a integer offset, not '#{value}'"
                      else
                        "expecting an instance variable or a integer offset, not '#{span_text(offset.span)}'"
                      end
            @diagnostics << Diagnostic.new(offset.span, message)
          end
        elsif token.kind == TokenKind::KeywordPointerof && (arg = args_children.first?)
          node = @arena.node(arg)
          if node.kind == NodeKind::Ident && @arena.symbols[node.payload_index] == "self"
            @diagnostics << Diagnostic.new(node.span, "can't take address of self")
          end
        end
        args_span = Span.new(token.span.finish, node_span(args_children.last).finish)
        args = @arena.add_node(NodeKind::Args, args_span, args_children)
        span = Span.new(token.span.start, node_span(args).finish)
        @arena.add_node(NodeKind::Call, span, [callee, args])
      end

      private def parse_asm_expr(token : Token) : NodeId
        advance
        name = token_text(token)
        sym = @arena.symbols.intern(name)
        callee = @arena.add_ident(token.span, sym)
        args_children = [] of NodeId
        end_pos = token.span.finish
        if match(TokenKind::LParen)
          depth = 1
          section = 0
          previous = TokenKind::Unknown
          while !current.eof? && depth > 0
            tok = advance
            end_pos = tok.span.finish
            if depth == 1
              case tok.kind
              when TokenKind::DoubleColon
                section += 2
              when TokenKind::Colon
                section += 1
              when TokenKind::String
                text = token_text(tok)
                if text.includes?(%q(#{))
                  message = section >= 4 ? "interpolation not allowed in asm option" : "interpolation not allowed in asm clobber"
                  @diagnostics << Diagnostic.new(tok.span, message)
                end
                args_children << @arena.add_literal_node(LiteralKind::String, tok.span)
              when TokenKind::Number
                args_children << @arena.add_literal_node(LiteralKind::Number, tok.span)
                if section == 1
                  @diagnostics << Diagnostic.new(tok.span, "unexpected token: \"#{token_text(tok)}\"")
                end
              when TokenKind::Identifier, TokenKind::InstanceVar, TokenKind::ClassVar
                symbol = @arena.symbols.intern(token_text(tok))
                args_children << @arena.add_ident(tok.span, symbol)
              end
              if section >= 3 && tok.kind == TokenKind::LParen && previous == TokenKind::String
                @diagnostics << Diagnostic.new(tok.span, "unexpected token: \"(\"")
              end
            elsif depth > 1
              case tok.kind
              when TokenKind::String
                args_children << @arena.add_literal_node(LiteralKind::String, tok.span)
              when TokenKind::Number
                args_children << @arena.add_literal_node(LiteralKind::Number, tok.span)
              when TokenKind::Char
                args_children << @arena.add_literal_node(LiteralKind::Char, tok.span)
              when TokenKind::Identifier, TokenKind::InstanceVar, TokenKind::ClassVar
                symbol = @arena.symbols.intern(token_text(tok))
                args_children << @arena.add_ident(tok.span, symbol)
              end
            end
            depth += 1 if tok.kind == TokenKind::LParen
            depth -= 1 if tok.kind == TokenKind::RParen
            previous = tok.kind unless {TokenKind::Comma, TokenKind::Colon, TokenKind::DoubleColon}.includes?(tok.kind)
          end
          if depth != 0
            @diagnostics << Diagnostic.new(token.span, "expected ')' after asm")
          end
        end
        args_span = if args_children.empty?
                      Span.new(token.span.finish, token.span.finish)
                    else
                      Span.new(token.span.finish, node_span(args_children.last).finish)
                    end
        args = @arena.add_node(NodeKind::Args, args_span, args_children)
        span = Span.new(token.span.start, end_pos)
        @arena.add_node(NodeKind::Call, span, [callee, args])
      end

      private def parse_args : NodeId
        start = advance
        children = [] of NodeId
        if current.kind != TokenKind::RParen
          loop do
            children << parse_argument
            if current.kind == TokenKind::Comma &&
               newline_between?(node_span(children.last).finish, current.span.start)
              @diagnostics << Diagnostic.new(current.span, "expecting token ')', not ','")
            end
            if current.kind == TokenKind::Comma
              advance
              break if current.kind == TokenKind::RParen
              next
            elsif @arena.node(children.last).kind == NodeKind::MacroControl && macro_control_start?
              next
            else
              break
            end
          end
        end
        end_token = expect(TokenKind::RParen, "expecting token ')', not '#{crystal_diagnostic_token_text(current)}'")
        span = Span.new(start.span.start, end_token.span.finish)
        args = @arena.add_node(NodeKind::Args, span, children)
        validate_named_arg_duplicates(args)
        validate_call_arg_order(args)
        args
      end

      private def parse_type_args_in_expr : NodeId
        start = advance
        children = [] of NodeId
        stop = -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen }
        saw_named = false
        saw_positional = false
        if current.kind != TokenKind::RParen
          loop do
            if (current.kind == TokenKind::Identifier || current.kind == TokenKind::String) && peek1.kind == TokenKind::Colon &&
               adjacent?(current, peek1)
              if saw_positional
                @diagnostics << Diagnostic.new(current.span, "unexpected token: \"#{token_text(current)}\"")
              end
              name = advance
              advance
              value = parse_type_arg(stop)
              name_text = token_text(name)
              name_text = name_text[1...-1] if name.kind == TokenKind::String && name_text.size >= 2
              if name_text.empty?
                @diagnostics << Diagnostic.new(name.span, "named argument cannot have an empty name")
              end
              symbol_id = @arena.symbols.intern(name_text)
              span = Span.new(name.span.start, node_span(value).finish)
              children << @arena.add_named_arg(symbol_id, span, value)
              saw_named = true
            else
              if saw_named
                @diagnostics << Diagnostic.new(current.span, "positional argument cannot follow named arguments")
              end
              children << parse_type_arg(stop)
              saw_positional = true
            end
            if current.kind == TokenKind::Comma &&
               newline_between?(node_span(children.last).finish, current.span.start)
              @diagnostics << Diagnostic.new(current.span, "expecting token ')', not ','")
            end
            if current.kind == TokenKind::Comma
              advance
              break if current.kind == TokenKind::RParen
              next
            else
              break
            end
          end
        end
        end_token = expect(TokenKind::RParen, "expecting token ')', not '#{diagnostic_token_text(current)}'")
        span = Span.new(start.span.start, end_token.span.finish)
        args = @arena.add_node(NodeKind::Args, span, children)
        validate_named_arg_duplicates(args)
        args
      end

      private def parse_type_arg(stop : Proc(Bool), allow_tuple : Bool = false) : NodeId
        case current.kind
        when TokenKind::Number
          tok = advance
          @arena.add_literal_node(LiteralKind::Number, tok.span)
        when TokenKind::KeywordTypeof,
             TokenKind::KeywordSizeof, TokenKind::KeywordInstanceSizeof,
             TokenKind::KeywordAlignof, TokenKind::KeywordInstanceAlignof,
             TokenKind::KeywordOffsetof, TokenKind::KeywordPointerof
          parse_expression(0, stop, allow_var_decl: false)
        else
          parse_type(stop, allow_tuple: allow_tuple)
        end
      end

      private def parse_array : NodeId
        start = advance
        children = [] of NodeId
        if current.kind != TokenKind::RBracket
          loop do
            children << parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket })
            if current.kind == TokenKind::Comma &&
               newline_between?(node_span(children.last).finish, current.span.start)
              @diagnostics << Diagnostic.new(current.span, "expecting token ']', not ','")
            end
            break unless match(TokenKind::Comma)
            break if current.kind == TokenKind::RBracket
          end
        end
        end_token = expect(TokenKind::RBracket, "expecting token ']', not '#{crystal_diagnostic_token_text(current)}'")
        flags = 0_u16
        span_end = end_token.span.finish
        if current.kind == TokenKind::KeywordOf
          advance
          type_node = parse_type
          children << type_node
          flags = 1_u16
          span_end = node_span(type_node).finish
        elsif children.empty? && !method_bracket_name_context?(start.span.start)
          @diagnostics << Diagnostic.new(start.span, "for empty arrays use '[] of ElementType'")
        end
        span = Span.new(start.span.start, span_end)
        @arena.add_node(NodeKind::Array, span, children, flags: flags)
      end

      private def parse_brace_literal : NodeId
        start = advance
        entries = [] of NodeId
        mode = :unknown
        named_tuple_keys = nil
        if current.kind != TokenKind::RBrace
          loop do
            entry, entry_mode = parse_brace_entry
            entries << entry
            if mode == :unknown
              mode = entry_mode
            elsif mode != entry_mode && entry_mode != :unknown
              if mode == :hash && entry_mode == :named_tuple
                @diagnostics << Diagnostic.new(node_span(entry), "can't use 'key: value' syntax in a hash literal")
              else
                @diagnostics << Diagnostic.new(node_span(entry), "mixed tuple/hash/named tuple entries")
              end
            end
            if entry_mode == :named_tuple && mode == :named_tuple
              key_id = @arena.node(entry).payload_index
              named_tuple_keys ||= {} of Int32 => Bool
              if named_tuple_keys.has_key?(key_id)
                name = @arena.symbols[key_id]
                position = node_span(entry).start + name.bytesize
                @diagnostics << Diagnostic.new(Span.new(position, position), "duplicated key: #{name}")
              else
                named_tuple_keys[key_id] = true
              end
            end
            if current.kind == TokenKind::Comma &&
               newline_between?(node_span(entries.last).finish, current.span.start)
              @diagnostics << Diagnostic.new(current.span, "expecting token '}', not ','")
            end
            break unless match(TokenKind::Comma)
            break if current.kind == TokenKind::RBrace
          end
        end
        if mode == :named_tuple
          seen = {} of Int32 => Bool
          entries.each do |entry|
            key_id = @arena.node(entry).payload_index
            if seen.has_key?(key_id)
              name = @arena.symbols[key_id]
              position = node_span(entry).start + name.bytesize
              @diagnostics << Diagnostic.new(Span.new(position, position), "duplicated key: #{name}")
            else
              seen[key_id] = true
            end
          end
        end
        if first_entry = entries.first?
          if @arena.node(first_entry).kind == NodeKind::Splat
            if current.kind == TokenKind::HashRocket
              @diagnostics << Diagnostic.new(current.span, "unexpected token: \"=>\"")
            elsif current.kind == TokenKind::Colon && !peek1.eof?
              @diagnostics << Diagnostic.new(peek1.span, "unexpected token: \"#{crystal_diagnostic_token_text(peek1)}\"")
            end
          end
        end
        end_token = expect(TokenKind::RBrace, "expecting token '}', not '#{crystal_diagnostic_token_text(current)}'")
        span = Span.new(start.span.start, end_token.span.finish)
        if current.kind == TokenKind::KeywordOf && (mode == :hash || entries.empty?)
          advance
          key_type = parse_type(-> { current.kind == TokenKind::HashRocket })
          expect(TokenKind::HashRocket, "expected '=>' in typed hash literal")
          value_type = parse_type
          typed_span = Span.new(start.span.start, node_span(value_type).finish)
          return @arena.add_node(NodeKind::Hash, typed_span, entries + [key_type, value_type], flags: 1_u16)
        elsif current.kind == TokenKind::KeywordOf
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"of\"")
        elsif entries.empty?
          @diagnostics << Diagnostic.new(start.span, "for empty hashes use '{} of KeyType => ValueType'")
        end
        case mode
        when :hash
          @arena.add_node(NodeKind::Hash, span, entries)
        when :named_tuple
          @arena.add_node(NodeKind::NamedTuple, span, entries)
        else
          @arena.add_node(NodeKind::Tuple, span, entries)
        end
      end

      private def parse_brace_entry : Tuple(NodeId, Symbol)
        if current.kind == TokenKind::StarStar
          star = advance
          value = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBrace })
          span = Span.new(star.span.start, node_span(value).finish)
          return {@arena.add_node(NodeKind::DoubleSplat, span, [value]), :hash}
        elsif current.kind == TokenKind::Star
          star = advance
          value = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBrace })
          span = Span.new(star.span.start, node_span(value).finish)
          return {@arena.add_node(NodeKind::Splat, span, [value]), :tuple}
        end

        if named_arg_name_token?(current.kind) && peek1.kind == TokenKind::Colon
          name = advance
          if name.kind == TokenKind::String && !adjacent?(name, current)
            @diagnostics << Diagnostic.new(current.span, "space not allowed between named argument name and ':'")
          end
          advance
          value = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBrace })
          name_text = token_text(name)
          name_text = name_text[1...-1] if name.kind == TokenKind::String && name_text.size >= 2
          if name_text.empty?
            @diagnostics << Diagnostic.new(name.span, "named tuple name cannot be empty")
          end
          symbol_id = @arena.symbols.intern(name_text)
          span = Span.new(name.span.start, node_span(value).finish)
          return {@arena.add_named_arg(symbol_id, span, value), :named_tuple}
        end

        key = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBrace || current.kind == TokenKind::HashRocket })
        if current.kind == TokenKind::HashRocket
          op = advance
          value = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBrace })
          span = Span.new(node_span(key).start, node_span(value).finish)
          return {@arena.add_binary(op.kind, span, key, value), :hash}
        end
        if current.kind == TokenKind::Colon
          colon = advance
          @diagnostics << Diagnostic.new(colon.span, "expecting token '=>', not ':'")
          value = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBrace })
          span = Span.new(node_span(key).start, node_span(value).finish)
          return {@arena.add_binary(TokenKind::HashRocket, span, key, value), :hash}
        end

        {key, :tuple}
      end

      private def parse_argument : NodeId
        if named_arg_name_token?(current.kind) && peek1.kind == TokenKind::Colon
          name = advance
          if name.kind == TokenKind::String && !adjacent?(name, current)
            @diagnostics << Diagnostic.new(current.span, "space not allowed between named argument name and ':'")
          end
          advance
          value = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen || current.kind == TokenKind::RBracket })
          name_text = token_text(name)
          name_text = name_text[1...-1] if name.kind == TokenKind::String && name_text.size >= 2
          if name_text.empty?
            @diagnostics << Diagnostic.new(name.span, "named argument cannot have an empty name")
          end
          symbol_id = @arena.symbols.intern(name_text)
          span = Span.new(name.span.start, node_span(value).finish)
          return @arena.add_named_arg(symbol_id, span, value)
        elsif var_decl_start?(current.kind) && peek1.kind == TokenKind::Colon
          return parse_var_decl(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen || current.kind == TokenKind::RBracket })
        end
        parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen || current.kind == TokenKind::RBracket })
      end

      private def parse_lambda_literal(start_arrow : Token, params_node : NodeId?) : NodeId
        if current.kind == TokenKind::LBrace
          lbrace = advance
          if params_node.nil? && current.kind == TokenKind::Pipe
            parameter = token_text(peek1) == "_" ? "param" : "x"
            @diagnostics << Diagnostic.new(current.span, "unexpected token: \"|\", proc literals specify their parameters like this: ->(#{parameter} : Type) { ... }")
          end
          params = params_node || (current.kind == TokenKind::Pipe ? parse_block_params : @arena.add_node(NodeKind::Args, Span.new(lbrace.span.finish, lbrace.span.finish)))
          validate_param_name_duplicates(params, "duplicated proc literal parameter name") if params_node
          validate_proc_literal_params(params) if params_node
          body = parse_expressions([TokenKind::KeywordRescue, TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::RBrace])

          rescue_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
          else_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
          ensure_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))

          if current.kind == TokenKind::KeywordRescue
            rescue_body = parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::RBrace])
            while current.kind == TokenKind::KeywordRescue
              parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::RBrace])
            end
            rescue_span = span_from_nodes(rescue_body, rescue_body)
            rescue_node = @arena.add_node(NodeKind::Rescue, rescue_span, [rescue_body])
          end

          if current.kind == TokenKind::KeywordElse
            advance
            else_body = parse_expressions([TokenKind::KeywordEnsure, TokenKind::RBrace])
            else_span = span_from_nodes(else_body, else_body)
            else_node = @arena.add_node(NodeKind::Rescue, else_span, [else_body])
          end

          if current.kind == TokenKind::KeywordEnsure
            advance
            ensure_body = parse_expressions([TokenKind::RBrace])
            ensure_span = span_from_nodes(ensure_body, ensure_body)
            ensure_node = @arena.add_node(NodeKind::Ensure, ensure_span, [ensure_body])
          end

          block_body = if @arena.node(rescue_node).kind == NodeKind::Nop &&
                          @arena.node(else_node).kind == NodeKind::Nop &&
                          @arena.node(ensure_node).kind == NodeKind::Nop
                         body
                       else
                         span = span_from_nodes(body, ensure_node)
                         @arena.add_node(NodeKind::Begin, span, [body, rescue_node, else_node, ensure_node])
                       end

          end_token = expect(TokenKind::RBrace, "expected '}' to close lambda")
          span = Span.new(start_arrow.span.start, end_token.span.finish)
          return @arena.add_node(NodeKind::Block, span, [params, block_body])
        end

        do_token = expect(TokenKind::KeywordDo, "expected 'do' to start lambda body")
        if params_node.nil? && current.kind == TokenKind::Pipe
          parameter = token_text(peek1) == "_" ? "param" : "x"
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"|\", proc literals specify their parameters like this: ->(#{parameter} : Type) { ... }")
        end
        params = params_node || (current.kind == TokenKind::Pipe ? parse_block_params : @arena.add_node(NodeKind::Args, Span.new(do_token.span.finish, do_token.span.finish)))
        validate_param_name_duplicates(params, "duplicated proc literal parameter name") if params_node
        validate_proc_literal_params(params) if params_node
        body = parse_expressions([TokenKind::KeywordRescue, TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])

        rescue_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        else_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        ensure_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))

        if current.kind == TokenKind::KeywordRescue
          rescue_body = parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          while current.kind == TokenKind::KeywordRescue
            parse_rescue_clause([TokenKind::KeywordElse, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          end
          rescue_span = span_from_nodes(rescue_body, rescue_body)
          rescue_node = @arena.add_node(NodeKind::Rescue, rescue_span, [rescue_body])
        end

        if current.kind == TokenKind::KeywordElse
          advance
          else_body = parse_expressions([TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          else_span = span_from_nodes(else_body, else_body)
          else_node = @arena.add_node(NodeKind::Rescue, else_span, [else_body])
        end

        if current.kind == TokenKind::KeywordEnsure
          advance
          ensure_body = parse_expressions([TokenKind::KeywordEnd])
          ensure_span = span_from_nodes(ensure_body, ensure_body)
          ensure_node = @arena.add_node(NodeKind::Ensure, ensure_span, [ensure_body])
        end

        block_body = if @arena.node(rescue_node).kind == NodeKind::Nop &&
                        @arena.node(else_node).kind == NodeKind::Nop &&
                        @arena.node(ensure_node).kind == NodeKind::Nop
                       body
                     else
                       span = span_from_nodes(body, ensure_node)
                       @arena.add_node(NodeKind::Begin, span, [body, rescue_node, else_node, ensure_node])
                     end

        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close lambda")
        span = Span.new(start_arrow.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Block, span, [params, block_body])
      end

      private def parse_params : NodeId
        start = advance
        children = [] of NodeId
        if current.kind != TokenKind::RParen
          loop do
            children << parse_param
            if current.kind == TokenKind::Comma &&
               newline_between?(node_span(children.last).finish, current.span.start)
              @diagnostics << Diagnostic.new(current.span, "expecting token ')', not ','")
            end
            break unless match(TokenKind::Comma)
            break if current.kind == TokenKind::RParen
          end
        end
        end_token = expect(TokenKind::RParen, "expecting token ')', not '#{diagnostic_token_text(current)}'")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Args, span, children)
      end

      private def validate_def_params(params : NodeId) : Nil
        children = @arena.children(params)
        return if children.empty?

        bare_splat_index = nil
        seen_double_splat = false
        saw_default = false
        positional_phase = true
        seen_internal = {} of String => Bool
        seen_external = {} of String => Bool

        children.each_with_index do |param_id, idx|
          node = @arena.node(param_id)
          case node.kind
          when NodeKind::Param
            param_children = @arena.children(param_id)
            has_default = !param_children.empty? && @arena.node(param_children.last).kind != NodeKind::Nop
            if positional_phase && saw_default && !has_default
              @diagnostics << Diagnostic.new(node.span, "parameter must have a default value")
            end
            saw_default = true if positional_phase && has_default
            if node.payload_index >= 0
              name = @arena.symbols[node.payload_index]
              unless name.empty?
                if seen_internal.has_key?(name)
                  @diagnostics << Diagnostic.new(param_name_span(param_id, name), "duplicated def parameter name: #{name}")
                else
                  seen_internal[name] = true
                end
                if name[0].ascii_uppercase?
                  @diagnostics << Diagnostic.new(node.span, "cannot use '#{name}' as a parameter name")
                end
              end
            end

            if param_children.size == 4
              external = @arena.node(param_children[0])
              if external.kind == NodeKind::Ident
                ext_name = @arena.symbols[external.payload_index]
                if seen_external.has_key?(ext_name)
                  @diagnostics << Diagnostic.new(external.span, "duplicated def parameter external name: #{ext_name}")
                else
                  seen_external[ext_name] = true
                end
              end
            end
          when NodeKind::Splat, NodeKind::DoubleSplat, NodeKind::BlockParam
            positional_phase = false
            if node.payload_index >= 0
              name = @arena.symbols[node.payload_index]
              if seen_internal.has_key?(name)
                @diagnostics << Diagnostic.new(param_name_span(param_id, name), "duplicated def parameter name: #{name}")
              else
                seen_internal[name] = true
              end
            end
          end

          if seen_double_splat && node.kind != NodeKind::BlockParam
            diagnostic_pos = case node.kind
                             when NodeKind::Splat
                               node.span.start + 1
                             when NodeKind::DoubleSplat
                               node.span.start + 2
                             else
                               node.span.finish
                             end
            @diagnostics << Diagnostic.new(Span.new(diagnostic_pos, diagnostic_pos), "only block parameter is allowed after double splat")
            break
          end
          seen_double_splat = true if node.kind == NodeKind::DoubleSplat
          if node.kind == NodeKind::Splat && node.payload_index < 0
            bare_splat_index = idx
          end
        end

        if bare_splat_index && bare_splat_index == children.size - 1
          @diagnostics << Diagnostic.new(@arena.node(children[bare_splat_index]).span, "named parameters must follow bare *")
        end
      end

      private def validate_macro_params(params : NodeId) : Nil
        splat_count = 0
        splat_error = nil
        @arena.children(params).each do |param_id|
          param = @arena.node(param_id)
          if param.kind == NodeKind::Splat
            splat_count += 1
            if splat_count > 1 && splat_error.nil?
              splat_error = param.span
            end
          end
          next unless param.kind == NodeKind::Param
          children = @arena.children(param_id)
          next if children.size < 2
          type_id = children[children.size - 2]
          type_node = @arena.node(type_id)
          next if type_node.kind == NodeKind::Nop
          @diagnostics << Diagnostic.new(node_span(type_id), "unexpected token: \":\"")
        end
        if splat_error
          @diagnostics << Diagnostic.new(splat_error, "unexpected token: \"*\"")
        end
      end

      private def validate_param_name_duplicates(params : NodeId, message_prefix : String) : Nil
        return unless @arena.node(params).kind == NodeKind::Args
        seen = {} of String => Bool
        @arena.children(params).each do |param_id|
          if name = param_like_name(param_id)
            if seen.has_key?(name)
              @diagnostics << Diagnostic.new(param_name_span(param_id, name), "#{message_prefix}: #{name}")
            else
              seen[name] = true
            end
          end
        end
      end

      private def param_name_span(param_id : NodeId, name : String) : Span
        node = @arena.node(param_id)
        if node.kind == NodeKind::Param
          Span.new(node.span.start, node.span.start + name.bytesize)
        else
          Span.new(node.span.finish - name.bytesize, node.span.finish)
        end
      end

      private def validate_proc_literal_params(params : NodeId) : Nil
        return unless @arena.node(params).kind == NodeKind::Args
        @arena.children(params).each do |param_id|
          param = @arena.node(param_id)
          next unless param.kind == NodeKind::Param
          children = @arena.children(param_id)
          if children.size == 4
            @diagnostics << Diagnostic.new(param.span, "expected ',' or ')'")
          end
        end
      end

      private def param_like_name(param_id : NodeId) : String?
        node = @arena.node(param_id)
        case node.kind
        when NodeKind::Param, NodeKind::Splat, NodeKind::DoubleSplat, NodeKind::BlockParam,
             NodeKind::Ident, NodeKind::InstanceVar, NodeKind::ClassVar
          return nil if node.payload_index < 0
          name = @arena.symbols[node.payload_index]
          name.empty? ? nil : name
        else
          nil
        end
      end

      private def register_param_locals(params : NodeId) : Nil
        return unless @arena.node(params).kind == NodeKind::Args
        @arena.children(params).each do |param_id|
          if name = param_like_name(param_id)
            @local_assigns << name unless @local_assigns.includes?(name)
          end
        end
      end

      private def register_assigned_locals(node_id : NodeId) : Nil
        node = @arena.node(node_id)
        case node.kind
        when NodeKind::Ident
          name = @arena.symbols[node.payload_index]
          @local_assigns << name unless @local_assigns.includes?(name)
        when NodeKind::Tuple, NodeKind::Splat
          @arena.children(node_id).each { |child| register_assigned_locals(child) }
        end
      end

      private def register_out_argument(callee_id : NodeId, args_id : NodeId) : Nil
        callee = @arena.node(callee_id)
        return unless callee.kind == NodeKind::Ident
        return unless @arena.symbols[callee.payload_index] == "out"
        @arena.children(args_id).each { |arg| register_assigned_locals(arg) }
      end

      private def remove_param_locals(params : NodeId, existing : Array(String)) : Nil
        return unless @arena.node(params).kind == NodeKind::Args
        @arena.children(params).each do |param_id|
          if name = param_like_name(param_id)
            @local_assigns.delete(name) unless existing.includes?(name)
          end
        end
      end

      private def diagnose_missing_def_parens(kind : NodeKind, name_span : Span) : Nil
        return unless same_line?(name_span, current.span)
        return unless param_start_token?(current.kind) || command_call_start?(current.kind)
        if current.kind == TokenKind::Number
          @diagnostics << Diagnostic.new(current.span, "unexpected token: \"#{token_text(current)}\"")
          return
        end
        suffix = kind == NodeKind::MacroDef ? "macro parameters" : "def parameters"
        message = "unexpected token: \"#{diagnostic_token_text(current)}\" (parentheses are mandatory for #{suffix})"
        @diagnostics << Diagnostic.new(current.span, message)
      end

      private def param_start_token?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier,
             TokenKind::InstanceVar,
             TokenKind::ClassVar,
             TokenKind::Ampersand,
             TokenKind::Star,
             TokenKind::StarStar
          true
        else
          false
        end
      end

      private def parse_param : NodeId
        annotations = [] of NodeId
        while current.kind == TokenKind::Annotation
          annotations << parse_annotation
          skip_separators
        end

        if current.kind == TokenKind::Percent && peek1.kind == TokenKind::Identifier && adjacent?(current, peek1)
          percent = advance
          ident = advance
          name = "%" + token_text(ident)
          span = Span.new(percent.span.start, ident.span.finish)
          symbol_id = @arena.symbols.intern(name)
          name_node = @arena.add_ident(span, symbol_id)
          type_node = @arena.add_node(NodeKind::Nop, Span.new(span.finish, span.finish))
          default_node = @arena.add_node(NodeKind::Nop, Span.new(span.finish, span.finish))
          return @arena.add_node(NodeKind::Param, span, [name_node, type_node, default_node], payload_index: symbol_id)
        end

        if macro_expr_start?
          node = parse_macro_expr
          return annotations.any? ? attach_annotations(annotations, node) : node
        end

        token = current
        if token.kind == TokenKind::String &&
           (peek1.kind == TokenKind::Identifier || peek1.kind == TokenKind::InstanceVar || peek1.kind == TokenKind::ClassVar || keyword_token?(peek1.kind))
          node = parse_named_param(token)
        elsif soft_identifier_kind?(token.kind)
          if @lib_depth > 0 && token.kind == TokenKind::Identifier && token_text(token)[0].ascii_uppercase?
            node = parse_anonymous_lib_param
          else
            node = parse_named_param(token)
          end
        elsif keyword_token?(token.kind)
          node = parse_named_param(token)
        else
          case token.kind
          when TokenKind::StarStar
            node = parse_splat_param(NodeKind::DoubleSplat, token)
          when TokenKind::Star
            node = parse_splat_param(NodeKind::Splat, token)
          when TokenKind::Ampersand
            node = parse_block_param(token)
          when TokenKind::InstanceVar, TokenKind::ClassVar
            node = parse_ivar_param(token)
          when TokenKind::DoubleColon
            node = parse_anonymous_param(token)
          when TokenKind::Identifier, TokenKind::KeywordType
            if @lib_depth > 0 && token.kind == TokenKind::Identifier && token_text(token)[0].ascii_uppercase?
              node = parse_anonymous_lib_param
            else
              node = parse_named_param(token)
            end
          when TokenKind::DotDotDot
            node = parse_variadic_param(token)
          else
            @diagnostics << Diagnostic.new(token.span, "expected parameter name")
            advance unless token.eof?
            node = @arena.add_node(NodeKind::Error, token.span)
          end
        end

        if annotations.any?
          node = attach_annotations(annotations, node)
        end
        node
      end

      private def parse_named_param(token : Token) : NodeId
        external_node = nil
        external_sym = -1
        name_token = token
        # Look for an external name followed by the real name (identifier or ivar).
        if (token.kind == TokenKind::Identifier || token.kind == TokenKind::String || keyword_token?(token.kind)) &&
           (peek1.kind == TokenKind::Identifier ||
           peek1.kind == TokenKind::InstanceVar ||
           peek1.kind == TokenKind::ClassVar ||
           peek1.kind == TokenKind::KeywordType ||
           keyword_token?(peek1.kind))
          after_second = peek2.kind
          if {TokenKind::Colon, TokenKind::Assign, TokenKind::Comma, TokenKind::RParen}.includes?(after_second)
            external_text = token_text(token)
            external_text = external_text[1...-1] if token.kind == TokenKind::String && external_text.size >= 2
            external_sym = @arena.symbols.intern(external_text)
            external_node = @arena.add_ident(token.span, external_sym)
            if external_text.empty?
              @diagnostics << Diagnostic.new(token.span, "external parameter name cannot be empty")
            elsif token.kind == TokenKind::String && token_text(token).includes?(%q(#{))
              @diagnostics << Diagnostic.new(token.span, "interpolation is not allowed in external parameter names")
            end
            advance
            name_token = current
          end
        end
        if keyword_token?(token.kind) && external_node.nil? && @lib_depth == 0 &&
           !soft_identifier_kind?(token.kind)
          @diagnostics << Diagnostic.new(token.span, "cannot use '#{token_text(token)}' as a parameter name")
          advance unless token.eof?
          return @arena.add_node(NodeKind::Error, token.span)
        end

        advance
        if external_node
          external_text = @arena.symbols[external_sym]
          internal_text = token_text(name_token)
          normalized = if internal_text.starts_with?("@@")
                         internal_text[2..]
                       elsif internal_text.starts_with?("@")
                         internal_text[1..]
                       else
                         internal_text
                       end
          if external_text == "_"
            @diagnostics << Diagnostic.new(node_span(external_node), "unexpected token: \"_\"")
          elsif external_text == normalized
            @diagnostics << Diagnostic.new(name_token.span, "when specified, external name must be different than internal name")
          end
        end
        if keyword_token?(name_token.kind) && @lib_depth == 0 && !soft_identifier_kind?(name_token.kind)
          @diagnostics << Diagnostic.new(name_token.span, "cannot use '#{token_text(name_token)}' as a parameter name")
        end
        name_sym = @arena.symbols.intern(token_text(name_token))
        name_node = case name_token.kind
                    when TokenKind::Identifier
                      @arena.add_ident(name_token.span, name_sym)
                    when TokenKind::KeywordType
                      @arena.add_ident(name_token.span, name_sym)
                    when TokenKind::InstanceVar
                      @arena.add_node(NodeKind::InstanceVar, name_token.span, payload_index: name_sym)
                    when TokenKind::ClassVar
                      @arena.add_node(NodeKind::ClassVar, name_token.span, payload_index: name_sym)
                    else
                      @arena.add_ident(name_token.span, name_sym)
                    end
        type_node = @arena.add_node(NodeKind::Nop, Span.new(name_token.span.finish, name_token.span.finish))
        default_node = @arena.add_node(NodeKind::Nop, Span.new(name_token.span.finish, name_token.span.finish))
        if match(TokenKind::Colon)
          colon_token = @tokens.peek(-1)
          if adjacent?(name_token, colon_token)
            @diagnostics << Diagnostic.new(name_token.span, "space required before colon in type restriction")
          elsif adjacent?(colon_token, current)
            @diagnostics << Diagnostic.new(current.span, "space required after colon in type restriction")
          end
          type_node = parse_type(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen })
          validate_param_type_shape(type_node)
        end
        if match(TokenKind::Assign)
          if colon = default_param_type_annotation_ahead
            @diagnostics << Diagnostic.new(colon.span, "the syntax for a parameter with a default value V and type T is `param : T = V`")
          end
          default_node = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen })
        end
        end_node = default_node
        if @arena.node(default_node).kind == NodeKind::Nop
          end_node = type_node if @arena.node(type_node).kind != NodeKind::Nop
        end
        start_node = external_node || name_node
        span = span_from_nodes(start_node, end_node)
        children = [] of NodeId
        children << external_node if external_node
        children << name_node
        children << type_node
        children << default_node
        @arena.add_node(NodeKind::Param, span, children, payload_index: name_sym)
      end

      private def default_param_type_annotation_ahead : Token?
        depth = 0
        saw_question = false
        offset = 0
        loop do
          tok = @tokens.peek(offset)
          case tok.kind
          when TokenKind::Eof
            return nil
          when TokenKind::LParen, TokenKind::LBracket, TokenKind::LBrace
            depth += 1
          when TokenKind::RParen
            return nil if depth == 0
            depth -= 1
          when TokenKind::RBracket, TokenKind::RBrace
            depth -= 1 if depth > 0
          when TokenKind::Comma
            return nil if depth == 0
          when TokenKind::Question
            saw_question = true if depth == 0
          when TokenKind::Colon
            return tok if depth == 0 && !saw_question
          end
          offset += 1
        end
      end

      private def parse_ivar_param(token : Token) : NodeId
        advance
        symbol_id = @arena.symbols.intern(token_text(token))
        name_node = case token.kind
                    when TokenKind::InstanceVar
                      @arena.add_node(NodeKind::InstanceVar, token.span, payload_index: symbol_id)
                    when TokenKind::ClassVar
                      @arena.add_node(NodeKind::ClassVar, token.span, payload_index: symbol_id)
                    else
                      @arena.add_ident(token.span, symbol_id)
                    end
        type_node = @arena.add_node(NodeKind::Nop, Span.new(token.span.finish, token.span.finish))
        default_node = @arena.add_node(NodeKind::Nop, Span.new(token.span.finish, token.span.finish))
        if match(TokenKind::Colon)
          type_node = parse_type(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen })
          validate_param_type_shape(type_node)
        end
        if match(TokenKind::Assign)
          default_node = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen })
        end
        span_end = [node_span(default_node).finish, node_span(type_node).finish, token.span.finish].max
        span = Span.new(token.span.start, span_end)
        @arena.add_node(NodeKind::Param, span, [name_node, type_node, default_node], payload_index: symbol_id)
      end

      private def parse_anonymous_param(token : Token) : NodeId
        start = advance
        name_sym = @arena.symbols.intern("")
        name_node = @arena.add_node(NodeKind::Nop, Span.new(start.span.start, start.span.start))
        type_node = parse_type(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen })
        validate_param_type_shape(type_node)
        default_node = @arena.add_node(NodeKind::Nop, Span.new(node_span(type_node).finish, node_span(type_node).finish))
        span = Span.new(start.span.start, node_span(type_node).finish)
        @arena.add_node(NodeKind::Param, span, [name_node, type_node, default_node], payload_index: name_sym)
      end

      private def parse_anonymous_lib_param : NodeId
        start = current.span.start
        name_sym = @arena.symbols.intern("")
        name_node = @arena.add_node(NodeKind::Nop, Span.new(start, start))
        type_node = parse_type(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen })
        validate_param_type_shape(type_node)
        finish = node_span(type_node).finish
        default_node = @arena.add_node(NodeKind::Nop, Span.new(finish, finish))
        @arena.add_node(NodeKind::Param, Span.new(start, finish), [name_node, type_node, default_node], payload_index: name_sym)
      end

      private def validate_param_type_shape(type_id : NodeId) : Nil
        if invalid_param_type_splat?(type_id, false)
          @diagnostics << Diagnostic.new(node_span(type_id), "invalid type splat")
        end
        if invalid_param_type_tuple?(type_id, false)
          @diagnostics << Diagnostic.new(node_span(type_id), "invalid parameter type restriction")
        end
      end

      private def invalid_param_type_splat?(node_id : NodeId, in_proc_args : Bool) : Bool
        node = @arena.node(node_id)
        if node.kind == NodeKind::ProcType
          args = @arena.children(node_id)[0]?
          ret = @arena.children(node_id)[1]?
          if args
            @arena.children(args).each do |arg|
              return true if invalid_param_type_splat?(arg, true)
            end
          end
          return ret ? invalid_param_type_splat?(ret, false) : false
        end

        if node.kind == NodeKind::Unary
          op = @arena.operator_kind(node.payload_index)
          if op == TokenKind::Star
            child = @arena.children(node_id)[0]?
            if child && node.span.start < node_span(child).start && !in_proc_args
              return true
            end
          end
        end

        @arena.children(node_id).each do |child|
          return true if invalid_param_type_splat?(child, in_proc_args)
        end
        false
      end

      private def invalid_param_type_tuple?(node_id : NodeId, in_proc_args : Bool) : Bool
        node = @arena.node(node_id)
        if node.kind == NodeKind::ProcType
          args = @arena.children(node_id)[0]?
          ret = @arena.children(node_id)[1]?
          if args
            @arena.children(args).each do |arg|
              return true if invalid_param_type_tuple?(arg, true)
            end
          end
          return ret ? invalid_param_type_tuple?(ret, false) : false
        end

        if node.kind == NodeKind::Tuple && !in_proc_args && !brace_tuple_literal?(node_id)
          return true
        end

        @arena.children(node_id).each do |child|
          return true if invalid_param_type_tuple?(child, in_proc_args)
        end
        false
      end

      private def parse_splat_param(kind : NodeKind, star : Token) : NodeId
        advance
        symbol_id = -1
        name_span = star.span
        if current.kind == TokenKind::Identifier
          name_token = advance
          symbol_id = @arena.symbols.intern(token_text(name_token))
          name_span = name_token.span
        end
        type_node = @arena.add_node(NodeKind::Nop, Span.new(name_span.finish, name_span.finish))
        if match(TokenKind::Colon)
          type_node = parse_type(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen })
        end
        span = Span.new(star.span.start, node_span(type_node).finish)
        @arena.add_node(kind, span, [type_node], payload_index: symbol_id)
      end

      private def parse_block_param(amp : Token) : NodeId
        advance
        symbol_id = -1
        name_span = amp.span
        if current.kind == TokenKind::Identifier || current.kind == TokenKind::InstanceVar || current.kind == TokenKind::ClassVar
          name_token = advance
          symbol_id = @arena.symbols.intern(token_text(name_token))
          name_span = name_token.span
        end
        type_node = @arena.add_node(NodeKind::Nop, Span.new(name_span.finish, name_span.finish))
        if match(TokenKind::Colon)
          type_node = parse_type(-> { current.kind == TokenKind::RParen }, allow_tuple: true)
        end
        span = Span.new(amp.span.start, type_node ? node_span(type_node).finish : name_span.finish)
        children = [] of NodeId
        children << type_node unless @arena.node(type_node).kind == NodeKind::Nop
        @arena.add_node(NodeKind::BlockParam, span, children, payload_index: symbol_id)
      end

      private def parse_variadic_param(token : Token) : NodeId
        advance
        span = token.span
        @arena.add_node(NodeKind::Param, span, [] of NodeId, payload_index: -1)
      end

      private def parse_property_like : NodeId
        token = advance
        callee_sym = @arena.symbols.intern(token_text(token))
        callee = @arena.add_ident(token.span, callee_sym)
        if current.kind == TokenKind::LParen
          args = parse_args
          call = @arena.add_node(NodeKind::Call, Span.new(token.span.start, node_span(args).finish), [callee, args])
          return parse_brace_block(call) if current.kind == TokenKind::LBrace
          return parse_block_call(call) if current.kind == TokenKind::KeywordDo
          return call
        end
        args_nodes = [] of NodeId
        loop do
          break if current.eof?
          if macro_expr_start?
            name_node = parse_macro_expr
            type_node = @arena.add_node(NodeKind::Nop, Span.new(node_span(name_node).finish, node_span(name_node).finish))
            default_node = @arena.add_node(NodeKind::Nop, Span.new(node_span(name_node).finish, node_span(name_node).finish))
            if match(TokenKind::Colon)
              type_node = parse_type(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::Assign || current.kind == TokenKind::KeywordEnd || current.kind == TokenKind::Semicolon })
            end
            if match(TokenKind::Assign)
              default_node = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::KeywordEnd || current.kind == TokenKind::Semicolon })
            end
            span_end = [node_span(type_node).finish, node_span(default_node).finish].max
            args_nodes << @arena.add_node(NodeKind::VarDecl, Span.new(node_span(name_node).start, span_end), [name_node, type_node, default_node])
          elsif current.kind == TokenKind::Symbol
            args_nodes << parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::Semicolon || current.kind == TokenKind::KeywordEnd })
          elsif var_decl_start?(current.kind) && peek1.kind == TokenKind::Colon
            args_nodes << parse_var_decl(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::KeywordEnd || current.kind == TokenKind::Semicolon })
          else
            break if terminator?(current.kind, [TokenKind::Semicolon], nil)
            break if current.kind == TokenKind::KeywordEnd
            break unless soft_identifier_kind?(current.kind)
            name_token = advance
            name_sym = @arena.symbols.intern(token_text(name_token))
            name_node = @arena.add_ident(name_token.span, name_sym)
            type_node = @arena.add_node(NodeKind::Nop, Span.new(name_token.span.finish, name_token.span.finish))
            default_node = @arena.add_node(NodeKind::Nop, Span.new(name_token.span.finish, name_token.span.finish))
            if match(TokenKind::Colon)
              type_node = parse_type(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::KeywordEnd || current.kind == TokenKind::Semicolon })
            end
            if match(TokenKind::Assign)
              default_node = parse_expression
            end
            span_end = [node_span(default_node).finish, node_span(type_node).finish, name_token.span.finish].max
            span = Span.new(name_token.span.start, span_end)
            args_nodes << @arena.add_node(NodeKind::VarDecl, span, [name_node, type_node, default_node])
          end
          break unless match(TokenKind::Comma)
        end
        args_span = if args_nodes.empty?
                      Span.new(token.span.finish, token.span.finish)
                    else
                      span_from_nodes(args_nodes.first, args_nodes.last)
                    end
        args = @arena.add_node(NodeKind::Args, args_span, args_nodes)
        span = Span.new(token.span.start, args_span.finish)
        call = @arena.add_node(NodeKind::Call, span, [callee, args])
        if current.kind == TokenKind::KeywordDo
          parse_block_call(call)
        elsif current.kind == TokenKind::LBrace && !macro_control_start? &&
              !newline_between?(node_span(call).finish, current.span.start)
          parse_brace_block(call)
        else
          call
        end
      end

      private def parse_type(stop : Proc(Bool)? = nil, allow_tuple : Bool = false) : NodeId
        left = parse_type_union(stop, allow_tuple)
        if !allow_tuple && current.kind == TokenKind::Comma && proc_type_shorthand_ahead?
          children = [] of NodeId
          children << left
          while match(TokenKind::Comma)
            children << parse_type_union(stop, allow_tuple)
          end
          span = Span.new(node_span(children.first).start, node_span(children.last).finish)
          left = @arena.add_node(NodeKind::Tuple, span, children)
        elsif allow_tuple
          while match(TokenKind::Comma)
            right = parse_type_union(stop, allow_tuple)
            children = [] of NodeId
            if @arena.node(left).kind == NodeKind::Tuple
              children.concat(@arena.children(left))
            else
              children << left
            end
            if @arena.node(right).kind == NodeKind::Tuple
              children.concat(@arena.children(right))
            else
              children << right
            end
            span = Span.new(node_span(children.first).start, node_span(children.last).finish)
            left = @arena.add_node(NodeKind::Tuple, span, children)
          end
        end
        while match(TokenKind::Arrow)
          arrow = @tokens.peek(-1)
          ret = if newline_between?(arrow.span.finish, current.span.start) || current.kind == TokenKind::RParen || current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket || current.kind == TokenKind::RBrace || current.kind == TokenKind::Assign || current.kind == TokenKind::Semicolon || current.kind == TokenKind::Eof
                  @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
                else
                  parse_type(stop, allow_tuple)
                end
          args_children = [] of NodeId
          if @arena.node(left).kind == NodeKind::Tuple
            args_children.concat(@arena.children(left))
          else
            args_children << left
          end
          args_span = if args_children.empty?
                        node_span(left)
                      else
                        Span.new(node_span(args_children.first).start, node_span(args_children.last).finish)
                      end
          args_node = @arena.add_node(NodeKind::Args, args_span, args_children)
          span = Span.new(args_span.start, node_span(ret).finish)
          left = @arena.add_node(NodeKind::ProcType, span, [args_node, ret])
        end
        left
      end

      private def proc_type_shorthand_ahead? : Bool
        mark = @tokens.mark
        paren = 0
        bracket = 0
        brace = 0
        seen_arrow = false

        loop do
          tok = advance
          case tok.kind
          when TokenKind::LParen
            paren += 1
          when TokenKind::RParen
            if paren == 0 && bracket == 0 && brace == 0
              break
            end
            paren -= 1 if paren > 0
          when TokenKind::LBracket
            bracket += 1
          when TokenKind::RBracket
            if paren == 0 && bracket == 0 && brace == 0
              break
            end
            bracket -= 1 if bracket > 0
          when TokenKind::LBrace
            brace += 1
          when TokenKind::RBrace
            if paren == 0 && bracket == 0 && brace == 0
              break
            end
            brace -= 1 if brace > 0
          when TokenKind::Arrow
            if paren == 0 && bracket == 0 && brace == 0
              seen_arrow = true
              break
            end
          when TokenKind::Colon, TokenKind::Assign, TokenKind::Semicolon,
               TokenKind::KeywordEnd, TokenKind::KeywordElse, TokenKind::KeywordElsif,
               TokenKind::KeywordWhen, TokenKind::KeywordRescue, TokenKind::KeywordEnsure,
               TokenKind::KeywordIf, TokenKind::KeywordUnless
            break if paren == 0 && bracket == 0 && brace == 0
          when TokenKind::Eof
            break
          end
        end

        @tokens.rewind(mark)
        seen_arrow
      end

      private def type_expr_like?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        case node.kind
        when NodeKind::TypeApply
          true
        when NodeKind::Ident, NodeKind::Path
          const_like?(node_id)
        else
          false
        end
      end

      private def type_suffix_terminator?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Eof,
             TokenKind::Semicolon,
             TokenKind::KeywordEnd,
             TokenKind::RBrace,
             TokenKind::RParen,
             TokenKind::RBracket,
             TokenKind::Question
          true
        else
          false
        end
      end

      private def parse_brace_block(call : NodeId) : NodeId
        start = advance
        if call_has_block_arg?(call)
          @diagnostics << Diagnostic.new(node_span(call), "can't use captured and non-captured blocks together")
        end
        block_params = parse_block_params
        outer_local_assigns = @local_assigns.dup
        register_param_locals(block_params)
        body = parse_expressions([TokenKind::RBrace])
        end_token = expect(TokenKind::RBrace, "expected '}' to close block")
        span = Span.new(node_span(call).start, end_token.span.finish)
        call_node = @arena.node(call)
        call_with_block = @arena.add_node(NodeKind::CallWithBlock, Span.new(node_span(call).start, end_token.span.finish), [call, block_params, body])
        remove_param_locals(block_params, outer_local_assigns)
        call_with_block
      end

      private def call_has_block_arg?(call_id : NodeId) : Bool
        node = @arena.node(call_id)
        return false unless node.kind == NodeKind::Call
        args_id = @arena.children(call_id)[1]?
        return false unless args_id
        @arena.children(args_id).any? { |arg| block_arg_node?(arg) }
      end

      private def block_arg_node?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        return false unless node.kind == NodeKind::Unary
        op_id = node.payload_index
        return false if op_id < 0
        op = @arena.operator_kind(op_id)
        op == TokenKind::Ampersand
      end

      private def parse_type_union(stop : Proc(Bool)? = nil, allow_tuple : Bool = false) : NodeId
        left = parse_type_primary(stop, allow_tuple)
        while (!stop || !stop.call) && match(TokenKind::Pipe)
          right = parse_type_primary(stop, allow_tuple)
          span = span_from_nodes(left, right)
          left = @arena.add_binary(TokenKind::Pipe, span, left, right)
        end
        left
      end

      private def parse_type_primary(stop : Proc(Bool)? = nil, allow_tuple : Bool = false) : NodeId
        base = parse_type_atom(stop, allow_tuple)
        if current.kind == TokenKind::LParen && node_span(base).finish == current.span.start
          args = parse_type_args(stop, allow_tuple)
          span = Span.new(node_span(base).start, node_span(args).finish)
          base = @arena.add_node(NodeKind::TypeApply, span, [base, args])
        end
        if current.kind == TokenKind::Question && node_span(base).finish <= current.span.start
          question = advance
          nil_node = @arena.add_node(NodeKind::LiteralNil, question.span)
          span = span_from_nodes(base, nil_node)
          base = @arena.add_binary(TokenKind::Pipe, span, base, nil_node)
        end
        while current.kind == TokenKind::Star
          star = advance
          span = Span.new(node_span(base).start, star.span.finish)
          base = @arena.add_unary(TokenKind::Star, span, base)
        end
        while current.kind == TokenKind::LBracket && node_span(base).finish == current.span.start
          lb = advance
          size_node = parse_expression(0, -> { current.kind == TokenKind::RBracket })
          rb = expect(TokenKind::RBracket, "expected ']' in static array type")
          args_span = Span.new(lb.span.start, rb.span.finish)
          args = @arena.add_node(NodeKind::Args, args_span, [size_node])
          span = Span.new(node_span(base).start, args_span.finish)
          base = @arena.add_node(NodeKind::TypeApply, span, [base, args])
        end
        while current.kind == TokenKind::Star
          star = advance
          span = Span.new(node_span(base).start, star.span.finish)
          base = @arena.add_unary(TokenKind::Star, span, base)
        end
        if current.kind == TokenKind::Question && node_span(base).finish <= current.span.start
          question = advance
          nil_node = @arena.add_node(NodeKind::LiteralNil, question.span)
          span = span_from_nodes(base, nil_node)
          base = @arena.add_binary(TokenKind::Pipe, span, base, nil_node)
        end
        while current.kind == TokenKind::StarStar
          tok = advance
          span = Span.new(node_span(base).start, tok.span.finish)
          base = @arena.add_unary(TokenKind::Star, span, base)
          span = Span.new(node_span(base).start, tok.span.finish)
          base = @arena.add_unary(TokenKind::Star, span, base)
        end
        while current.kind == TokenKind::Dot && peek1.kind == TokenKind::KeywordClass
          dot = advance
          kw = advance
          sym = @arena.symbols.intern("class")
          rhs = @arena.add_ident(kw.span, sym)
          span = Span.new(node_span(base).start, kw.span.finish)
          base = @arena.add_node(NodeKind::Path, span, [base, rhs])
        end
        base
      end

      private def parse_type_atom(stop : Proc(Bool)? = nil, allow_tuple : Bool = false) : NodeId
        return parse_macro_expr if macro_expr_start?
        return parse_macro_control if macro_control_start?
        token = current
        case token.kind
        when TokenKind::Star
          star = advance
          inner = parse_type_atom(stop, allow_tuple)
          span = Span.new(star.span.start, node_span(inner).finish)
          @arena.add_unary(TokenKind::Star, span, inner)
        when TokenKind::StarStar
          starstar = advance
          inner = parse_type_atom(stop, allow_tuple)
          span = Span.new(starstar.span.start, node_span(inner).finish)
          inner = @arena.add_unary(TokenKind::Star, span, inner)
          span = Span.new(starstar.span.start, node_span(inner).finish)
          @arena.add_unary(TokenKind::Star, span, inner)
        when TokenKind::DoubleColon
          dbl = advance
          root = @arena.add_ident(Span.new(dbl.span.start, dbl.span.finish), @arena.symbols.intern("::"))
          right = parse_type_path
          span = Span.new(dbl.span.start, node_span(right).finish)
          @arena.add_node(NodeKind::Path, span, [root, right])
        when TokenKind::Identifier, TokenKind::KeywordType
          parse_type_path
        when TokenKind::KeywordSelf
          advance
          symbol_id = @arena.symbols.intern("self")
          @arena.add_ident(token.span, symbol_id)
        when TokenKind::KeywordNil
          advance
          @arena.add_node(NodeKind::LiteralNil, token.span)
        when TokenKind::KeywordTypeof,
             TokenKind::KeywordSizeof, TokenKind::KeywordInstanceSizeof,
             TokenKind::KeywordAlignof, TokenKind::KeywordInstanceAlignof,
             TokenKind::KeywordOffsetof, TokenKind::KeywordPointerof
          parse_expression(0, stop, allow_var_decl: false)
        when TokenKind::LParen
          lparen = advance
          if current.kind == TokenKind::RParen
            rparen = advance
            return @arena.add_node(NodeKind::Tuple, Span.new(lparen.span.start, rparen.span.finish))
          end
          inner = parse_type(-> { current.kind == TokenKind::RParen }, allow_tuple: true)
          rparen = expect(TokenKind::RParen, "expected ')' to close type")
          Span.new(lparen.span.start, rparen.span.finish)
          inner
        when TokenKind::Arrow
          arrow = advance
          newline_after = !current.eof? && newline_between?(arrow.span.finish, current.span.start)
          ret = if newline_after
                  @arena.add_node(NodeKind::Nop, Span.new(arrow.span.finish, arrow.span.finish))
                elsif stop && stop.call
                  @arena.add_node(NodeKind::Nop, Span.new(arrow.span.finish, arrow.span.finish))
                elsif {TokenKind::Comma, TokenKind::RParen, TokenKind::RBracket, TokenKind::RBrace, TokenKind::Semicolon, TokenKind::Eof}.includes?(current.kind)
                  @arena.add_node(NodeKind::Nop, Span.new(arrow.span.finish, arrow.span.finish))
                else
                  parse_type(stop)
                end
          args_node = @arena.add_node(NodeKind::Args, Span.new(arrow.span.start, arrow.span.start))
          span = Span.new(arrow.span.start, node_span(ret).finish)
          @arena.add_node(NodeKind::ProcType, span, [args_node, ret])
        when TokenKind::LBrace
          start = advance
          entries = [] of NodeId
          named = false
          if current.kind != TokenKind::RBrace
            loop do
              if current.kind == TokenKind::Star
                star = advance
                value = parse_type(stop, allow_tuple: true)
                span = Span.new(star.span.start, node_span(value).finish)
                entries << @arena.add_node(NodeKind::Splat, span, [value])
              elsif named_arg_name_token?(current.kind) && peek1.kind == TokenKind::Colon
                key = advance
                advance
                value = parse_type(stop)
                key_text = token_text(key)
                key_text = key_text[1...-1] if key.kind == TokenKind::String && key_text.size >= 2
                if key_text.empty?
                  @diagnostics << Diagnostic.new(key.span, "named tuple name cannot be empty")
                end
                sym = @arena.symbols.intern(key_text)
                span = Span.new(key.span.start, node_span(value).finish)
                entries << @arena.add_named_arg(sym, span, value)
                named = true
              else
                entries << parse_type(stop)
              end
              break unless match(TokenKind::Comma)
              break if current.kind == TokenKind::RBrace
            end
          end
          end_token = expect(TokenKind::RBrace, "expected '}' to close type literal")
          span = Span.new(start.span.start, end_token.span.finish)
          node_kind = named ? NodeKind::NamedTuple : NodeKind::Tuple
          @arena.add_node(node_kind, span, entries)
        else
          @diagnostics << Diagnostic.new(token.span, "expected type name")
          advance unless token.eof?
          @arena.add_node(NodeKind::Error, token.span)
        end
      end

      private def parse_type_path : NodeId
        left, nil_span = parse_type_ident
        while macro_expr_start? && node_span(left).finish == current.span.start
          suffix = parse_macro_expr
          span = span_from_nodes(left, suffix)
          left = @arena.add_node(NodeKind::Path, span, [left, suffix])
        end
        while current.kind == TokenKind::DoubleColon
          break if newline_between?(node_span(left).finish, current.span.start)
          if nil_span
            @diagnostics << Diagnostic.new(nil_span, "nilable suffix not allowed before '::'")
            nil_span = nil
          end
          advance
          right, right_nil_span = parse_type_ident
          span = span_from_nodes(left, right)
          left = @arena.add_node(NodeKind::Path, span, [left, right])
          nil_span = right_nil_span
        end
        if nil_span
          nil_node = @arena.add_node(NodeKind::LiteralNil, nil_span)
          span = span_from_nodes(left, nil_node)
          left = @arena.add_binary(TokenKind::Pipe, span, left, nil_node)
        end
        left
      end

      private def parse_type_ident : Tuple(NodeId, Span?)
        token = current
        if token.kind != TokenKind::Identifier && token.kind != TokenKind::KeywordType
          @diagnostics << Diagnostic.new(token.span, "expected type name")
          advance unless token.eof?
          return {@arena.add_node(NodeKind::Error, token.span), nil}
        end

        text = token_text(token)
        if text.ends_with?("?")
          advance
          base_text = text[0, text.size - 1]
          symbol_id = @arena.symbols.intern(base_text)
          base_span = Span.new(token.span.start, token.span.finish - 1)
          base_node = @arena.add_ident(base_span, symbol_id)
          nil_span = Span.new(token.span.finish - 1, token.span.finish)
          return {base_node, nil_span}
        end

        advance
        symbol_id = @arena.symbols.intern(text)
        {@arena.add_ident(token.span, symbol_id), nil}
      end

      private def parse_type_args(stop : Proc(Bool)? = nil, allow_tuple : Bool = false) : NodeId
        start = advance
        children = [] of NodeId
        if current.kind != TokenKind::RParen
          loop do
            if named_arg_name_token?(current.kind) && peek1.kind == TokenKind::Colon && adjacent?(current, peek1)
              name = advance
              advance
              arg_stop = -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen }
              value = parse_type_arg(arg_stop)
              sym = @arena.symbols.intern(token_text(name))
              span = Span.new(name.span.start, node_span(value).finish)
              children << @arena.add_named_arg(sym, span, value)
            elsif current.kind == TokenKind::Star
              star = advance
              value = parse_type(stop, allow_tuple: false)
              span = Span.new(star.span.start, node_span(value).finish)
              children << @arena.add_node(NodeKind::Splat, span, [value])
            else
              arg_stop = -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen }
              children << parse_type_arg(arg_stop)
            end
            if current.kind == TokenKind::Comma &&
               newline_between?(node_span(children.last).finish, current.span.start)
              @diagnostics << Diagnostic.new(current.span, "expecting token ')', not ','")
            end
            break unless match(TokenKind::Comma)
            break if current.kind == TokenKind::RParen
          end
        end
        end_token = expect(TokenKind::RParen, "expecting token ')', not '#{diagnostic_token_text(current)}'")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Args, span, children)
      end

      private def cast_callee?(node : NodeId) : Bool
        node_info = @arena.node(node)
        case node_info.kind
        when NodeKind::Ident
          name = @arena.symbols[node_info.payload_index]
          name == "as" || name == "as?" || name == "is_a?"
        when NodeKind::Binary
          if (op = @arena.operator_kind(node_info.payload_index)) && {TokenKind::Dot, TokenKind::DoubleColon, TokenKind::SafeNav}.includes?(op)
            rhs = @arena.children(node)[1]?
            rhs ? cast_callee?(rhs) : false
          else
            false
          end
        when NodeKind::Path
          children = @arena.children(node)
          children.size == 2 && cast_callee?(children[1])
        else
          false
        end
      end

      private def cast_call_single_arg?(node : NodeId) : Bool
        return false unless cast_callee?(node)
        return false unless cast_type_start?(peek1.kind)

        depth = 0
        offset = 1
        loop do
          tok = @tokens.peek(offset)
          case tok.kind
          when TokenKind::Eof
            return false
          when TokenKind::LParen, TokenKind::LBracket, TokenKind::LBrace
            depth += 1
          when TokenKind::RParen
            if depth == 0
              return true
            else
              depth -= 1
            end
          when TokenKind::RBracket, TokenKind::RBrace
            depth -= 1 if depth > 0
          when TokenKind::Comma
            return false if depth == 0
          end
          offset += 1
        end
      end

      private def cast_type_start?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier, TokenKind::KeywordSelf, TokenKind::KeywordNil, TokenKind::KeywordType, TokenKind::LBrace, TokenKind::LParen
          true
        else
          false
        end
      end

      private def parse_cast_args : NodeId
        start = advance
        type_node = parse_type(-> { current.kind == TokenKind::RParen })
        end_token = expect(TokenKind::RParen, "expected ')' to close cast")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Args, span, [type_node])
      end

      private def build_infix(kind : TokenKind, span : Span, left : NodeId, right : NodeId, operator_span : Span? = nil) : NodeId
        case kind
        when TokenKind::Assign
          if @macro_def_depth > 0
            return @arena.add_node(NodeKind::Assign, span, [left, right])
          end
          lhs_node = @arena.node(left)
          rhs_node = @arena.node(right)
          op_span = operator_span || span
          if contains_question_index?(left) || invalid_safe_nav_index_setter?(left)
            @diagnostics << Diagnostic.new(op_span, "unexpected token: \"=\"")
            return @arena.add_node(NodeKind::Error, span)
          end

          # reject nested assignments (e.g., `a = 1, b = 2` where outer tries to assign to inner assign)
          if lhs_node.kind == NodeKind::Assign
            @diagnostics << Diagnostic.new(node_span(left), "unexpected token: \"=\"")
            return @arena.add_node(NodeKind::Error, span)
          end
          if lhs_node.kind == NodeKind::Ident
            name = @arena.symbols[lhs_node.payload_index]
            if name == "self"
              @diagnostics << Diagnostic.new(lhs_node.span, "can't change the value of self")
              return @arena.add_node(NodeKind::Error, span)
            end
            if !block_shorthand_member?(lhs_node.span) && !@local_assigns.includes?(name) && rhs_node.kind == NodeKind::Ident &&
               @arena.symbols[rhs_node.payload_index] == name
              @diagnostics << Diagnostic.new(rhs_node.span, "can't use variable name '#{name}' inside assignment to variable '#{name}'")
              return @arena.add_node(NodeKind::Error, span)
            end
          end
          if lhs_node.kind == NodeKind::Call || lhs_node.kind == NodeKind::CallWithBlock
            @diagnostics << Diagnostic.new(op_span, "unexpected token: \"=\"")
            return @arena.add_node(NodeKind::Error, span)
          end
          if {NodeKind::LiteralNumber, NodeKind::LiteralString, NodeKind::LiteralChar,
              NodeKind::LiteralSymbol, NodeKind::LiteralRegex, NodeKind::LiteralNil,
              NodeKind::LiteralBool}.includes?(lhs_node.kind)
            @diagnostics << Diagnostic.new(op_span, "unexpected token: \"=\"")
            return @arena.add_node(NodeKind::Error, span)
          end

          # reject non-assignable LHS (e.g., `1 == 2, a = 4` where LHS becomes Binary)
          # but allow method calls (Binary with Dot/SafeNav) which are valid setter assignments
          if lhs_node.kind == NodeKind::Binary
            op = @arena.operator_kind(lhs_node.payload_index)
            unless op == TokenKind::Dot || op == TokenKind::SafeNav
              @diagnostics << Diagnostic.new(op_span, "unexpected token: \"=\"")
              return @arena.add_node(NodeKind::Error, span)
            end
            rhs = @arena.children(left)[1]?
            if rhs
              rhs_node = @arena.node(rhs)
              if rhs_node.kind == NodeKind::Ident
                name = @arena.symbols[rhs_node.payload_index]
                unless assignable_method_name?(name)
                  @diagnostics << Diagnostic.new(op_span, "unexpected token: \"=\"")
                  return @arena.add_node(NodeKind::Error, span)
                end
              else
                @diagnostics << Diagnostic.new(op_span, "unexpected token: \"=\"")
                return @arena.add_node(NodeKind::Error, span)
              end
            end
          end

          # reject assignments to methods like `b? = 1` or `b! = 1`
          if lhs_node.kind == NodeKind::Ident
            name = @arena.symbols[lhs_node.payload_index]
            if name.ends_with?("?") || name.ends_with?("!")
              @diagnostics << Diagnostic.new(op_span, "unexpected token: \"=\"")
              return @arena.add_node(NodeKind::Error, span)
            end
          end

          # reject assignments to global match data ($0, $1, etc.)
          if lhs_node.kind == NodeKind::Global
            name = @arena.symbols[lhs_node.payload_index]
            if global_match_data_name?(name)
              position = lhs_node.span.finish
              @diagnostics << Diagnostic.new(Span.new(position, position), "global match data cannot be assigned to")
              return @arena.add_node(NodeKind::Error, span)
            end
          end

          # detect invalid targets in multiple-assignment LHS
          if lhs_node.kind == NodeKind::Tuple
            splat_count = 0
            lhs_targets = @arena.children(left)
            lhs_targets.each_with_index do |child_id, child_index|
              child = @arena.node(child_id)
              case child.kind
              when NodeKind::Ident
                name = @arena.symbols[child.payload_index]
                if name == "self"
                  @diagnostics << Diagnostic.new(child.span, "can't change the value of self")
                  return @arena.add_node(NodeKind::Error, span)
                end
                if name =~ /\A[A-Z]/
                  all_constants = lhs_targets.all? do |target_id|
                    target = @arena.node(target_id)
                    target.kind == NodeKind::Ident && @arena.symbols[target.payload_index] =~ /\A[A-Z]/
                  end
                  if all_constants
                    position = Math.min(node_span(lhs_targets.first).finish + 1, @source.size)
                    @diagnostics << Diagnostic.new(Span.new(position, position), "Multiple assignment is not allowed for constants")
                  else
                    @diagnostics << Diagnostic.new(child.span, "can't assign to constant in multiple assignment")
                  end
                  return @arena.add_node(NodeKind::Error, span)
                end
              when NodeKind::Global
                name = @arena.symbols[child.payload_index]
                if global_match_data_name?(name)
                  position = child.span.finish
                  @diagnostics << Diagnostic.new(Span.new(position, position), "global match data cannot be assigned to")
                  return @arena.add_node(NodeKind::Error, span)
                end
              when NodeKind::Binary
                # allow method calls (Binary with Dot/SafeNav) which are valid setter assignments
                op = @arena.operator_kind(child.payload_index)
                unless op == TokenKind::Dot || op == TokenKind::SafeNav
                  add_invalid_multiple_assignment_diagnostic(child_id, child_index, lhs_targets.size, op_span)
                  return @arena.add_node(NodeKind::Error, span)
                end
                rhs = @arena.children(child_id)[1]?
                if rhs
                  rhs_node = @arena.node(rhs)
                  if rhs_node.kind == NodeKind::Ident
                    name = @arena.symbols[rhs_node.payload_index]
                    unless assignable_method_name?(name)
                      add_invalid_multiple_assignment_diagnostic(child_id, child_index, lhs_targets.size, op_span)
                      return @arena.add_node(NodeKind::Error, span)
                    end
                  else
                    add_invalid_multiple_assignment_diagnostic(child_id, child_index, lhs_targets.size, op_span)
                    return @arena.add_node(NodeKind::Error, span)
                  end
                end
              when NodeKind::Call, NodeKind::CallWithBlock
                add_invalid_multiple_assignment_diagnostic(child_id, child_index, lhs_targets.size, op_span)
                return @arena.add_node(NodeKind::Error, span)
              when NodeKind::Assign
                add_invalid_multiple_assignment_diagnostic(child_id, child_index, lhs_targets.size, op_span)
                return @arena.add_node(NodeKind::Error, span)
              when NodeKind::LiteralNumber, NodeKind::LiteralString, NodeKind::LiteralChar,
                   NodeKind::LiteralSymbol, NodeKind::LiteralRegex, NodeKind::LiteralNil,
                   NodeKind::LiteralBool, NodeKind::Array, NodeKind::Hash
                add_invalid_multiple_assignment_diagnostic(child_id, child_index, lhs_targets.size, op_span)
                return @arena.add_node(NodeKind::Error, span)
              when NodeKind::Splat
                splat_count += 1
                if splat_count > 1
                  inner = @arena.children(child_id).first?
                  diagnostic_span = inner ? node_span(inner) : node_span(child_id)
                  @diagnostics << Diagnostic.new(diagnostic_span, "splat assignment already specified")
                  return @arena.add_node(NodeKind::Error, span)
                end
                # check splat contents - must be valid assignment target
                splat_inner_id = @arena.children(child_id).first?
                if splat_inner_id
                  splat_inner = @arena.node(splat_inner_id)
                  case splat_inner.kind
                  when NodeKind::LiteralNumber, NodeKind::LiteralString, NodeKind::LiteralChar,
                       NodeKind::LiteralSymbol, NodeKind::LiteralRegex, NodeKind::LiteralNil,
                       NodeKind::LiteralBool, NodeKind::Array, NodeKind::Hash
                    add_invalid_multiple_assignment_diagnostic(child_id, child_index, lhs_targets.size, op_span)
                    return @arena.add_node(NodeKind::Error, span)
                  else
                    # allow valid splat targets
                  end
                end
              else
                # allow Ident, InstanceVar, ClassVar, Global, Call (for setters), Index
              end
            end
          end

          # detect invalid RHS containing assignments or splats (but allow splats for setter calls)
          if rhs_node.kind == NodeKind::Tuple
            @arena.children(right).each do |child_id|
              child = @arena.node(child_id)
              if child.kind == NodeKind::Assign
                @diagnostics << Diagnostic.new(node_span(child_id), "unexpected token: \"=\"")
                return @arena.add_node(NodeKind::Error, span)
              end
              if child.kind == NodeKind::Splat && !brace_tuple_literal?(right)
                @diagnostics << Diagnostic.new(node_span(child_id), "unexpected token: \"*\"")
                return @arena.add_node(NodeKind::Error, span)
              end
            end
          end

          # detect single splat as RHS (e.g., `a = *1`) but allow for setter calls (foo.bar= *baz)
          if rhs_node.kind == NodeKind::Splat
            # Setter calls (Binary[Dot]) can have splat args
            is_setter_call = lhs_node.kind == NodeKind::Binary &&
                             @arena.operator_kind(lhs_node.payload_index) == TokenKind::Dot
            unless is_setter_call
              @diagnostics << Diagnostic.new(node_span(right), "unexpected token: \"*\"")
              return @arena.add_node(NodeKind::Error, span)
            end
          end

          # single LHS with multiple RHS values requires splat or tuple LHS
          if lhs_node.kind != NodeKind::Tuple && lhs_node.kind != NodeKind::Splat &&
             rhs_node.kind == NodeKind::Tuple && !brace_tuple_literal?(right)
            rhs_count = @arena.children(right).size
            if rhs_count > 1
              @diagnostics << Diagnostic.new(Span.new(span.start, span.start), "Multiple assignment count mismatch")
              return @arena.add_node(NodeKind::Error, span)
            end
          end

          # multi-assign count validation: with splat and >2 non-splat targets, need enough values
          # Crystal appears to only validate when there are more than 2 non-splat targets
          # Example: `a, b, *c, d = 1, 2` needs 3 values (a, b get first 2, d gets last 1)
          # Example: `a, b, *c = 1` is allowed (only 2 non-splat, Crystal allows short assignment)
          if lhs_node.kind == NodeKind::Tuple
            lhs_children = @arena.children(left)
            splat_index = lhs_children.index { |c| @arena.node(c).kind == NodeKind::Splat }
            if splat_index
              non_splat_count = lhs_children.size - 1 # total minus the splat
              # Only validate when there are more than 2 non-splat targets
              if non_splat_count > 2
                rhs_count = if rhs_node.kind == NodeKind::Tuple
                              @arena.children(right).size
                            else
                              1
                            end
                if rhs_count < non_splat_count
                  @diagnostics << Diagnostic.new(Span.new(span.start, span.start), "Multiple assignment count mismatch")
                  return @arena.add_node(NodeKind::Error, span)
                end
              end
            end
          end

          register_assigned_locals(left)
          @arena.add_node(NodeKind::Assign, span, [left, right])
        when TokenKind::PlusEqual, TokenKind::MinusEqual, TokenKind::StarEqual,
             TokenKind::SlashEqual, TokenKind::SlashSlashEqual, TokenKind::PercentEqual,
             TokenKind::PipeEqual, TokenKind::AmpersandEqual, TokenKind::CaretEqual,
             TokenKind::StarStarEqual, TokenKind::ShiftLeftEqual, TokenKind::ShiftRightEqual,
             TokenKind::AmpersandPlusEqual, TokenKind::AmpersandMinusEqual, TokenKind::AmpersandStarEqual,
             TokenKind::AmpersandStarStarEqual, TokenKind::OrOrEqual, TokenKind::AndAndEqual
          if @macro_def_depth > 0
            return @arena.add_binary(kind, span, left, right)
          end
          op_span = operator_span || span
          if contains_question_index?(left)
            @diagnostics << Diagnostic.new(op_span, "unexpected token: \"#{span_text(op_span)}\"")
            return @arena.add_node(NodeKind::Error, span)
          end
          unless valid_simple_assignment_target?(left)
            @diagnostics << Diagnostic.new(op_span, "unexpected token: \"#{span_text(op_span)}\"")
            return @arena.add_node(NodeKind::Error, span)
          end
          if lhs = @arena.node(left)
            if lhs.kind == NodeKind::Ident
              name = @arena.symbols[lhs.payload_index]
              if name == "self"
                @diagnostics << Diagnostic.new(lhs.span, "can't change the value of self")
                return @arena.add_node(NodeKind::Error, span)
              end
              if kind == TokenKind::PlusEqual && !@local_assigns.includes?(name)
                position = op_span.finish
                @diagnostics << Diagnostic.new(Span.new(position, position), "'+=' before definition of '#{name}'")
                return @arena.add_node(NodeKind::Error, span)
              end
            end
          end
          @arena.add_binary(kind, span, left, right)
        when TokenKind::DoubleColon
          @arena.add_node(NodeKind::Path, span, [left, right])
        when TokenKind::DotDot, TokenKind::DotDotDot
          flags = kind == TokenKind::DotDotDot ? 1_u16 : 0_u16
          @arena.add_node(NodeKind::Range, span, [left, right], flags: flags)
        else
          @arena.add_binary(kind, span, left, right)
        end
      end

      private def add_invalid_multiple_assignment_diagnostic(child_id : NodeId, index : Int32, count : Int32, operator_span : Span) : Nil
        if index < count - 1
          position = node_span(child_id).finish
          @diagnostics << Diagnostic.new(Span.new(position, position), "unexpected token: \",\"")
        else
          @diagnostics << Diagnostic.new(operator_span, "unexpected token: \"=\"")
        end
      end

      private def contains_question_index?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        return true if node.kind == NodeKind::Index && (node.flags & 1_u16) == 1_u16
        @arena.children(node_id).any? { |child| contains_question_index?(child) }
      end

      private def block_shorthand_member?(span : Span) : Bool
        span.start >= 2 && @source.bytes[span.start - 2] == '&'.ord.to_u8 && @source.bytes[span.start - 1] == '.'.ord.to_u8
      end

      private def invalid_safe_nav_index_setter?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        return false unless node.kind == NodeKind::Unary
        return false unless @arena.operator_kind(node.payload_index) == TokenKind::SafeNav
        inner_id = @arena.children(node_id)[0]?
        return false unless inner_id
        inner = @arena.node(inner_id)
        return false unless inner.kind == NodeKind::Call
        callee_id = @arena.children(inner_id)[0]?
        return false unless callee_id
        callee = @arena.node(callee_id)
        callee.kind == NodeKind::Index && @arena.children(callee_id).size == 1
      end

      private def infix_binding_power(kind : TokenKind) : Tuple(Int32, Int32)?
        case kind
        when TokenKind::Assign,
             TokenKind::PlusEqual, TokenKind::MinusEqual, TokenKind::StarEqual,
             TokenKind::SlashEqual, TokenKind::SlashSlashEqual, TokenKind::PercentEqual,
             TokenKind::PipeEqual, TokenKind::AmpersandEqual, TokenKind::CaretEqual,
             TokenKind::StarStarEqual, TokenKind::ShiftLeftEqual, TokenKind::ShiftRightEqual,
             TokenKind::AmpersandPlusEqual, TokenKind::AmpersandMinusEqual, TokenKind::AmpersandStarEqual,
             TokenKind::AmpersandStarStarEqual, TokenKind::OrOrEqual, TokenKind::AndAndEqual
          {10, 9}
        when TokenKind::OrOr
          {15, 16}
        when TokenKind::AndAnd
          {17, 18}
        when TokenKind::EqualEqual, TokenKind::BangEqual, TokenKind::TripleEqual,
             TokenKind::Match, TokenKind::NotMatch,
             TokenKind::Less, TokenKind::LessEqual, TokenKind::Greater, TokenKind::GreaterEqual,
             TokenKind::Spaceship
          {40, 41}
        when TokenKind::Pipe
          {42, 43}
        when TokenKind::Caret
          {43, 44}
        when TokenKind::Ampersand
          {44, 45}
        when TokenKind::ShiftLeft, TokenKind::ShiftRight
          {45, 46}
        when TokenKind::DotDot, TokenKind::DotDotDot
          {47, 48}
        when TokenKind::Plus, TokenKind::Minus, TokenKind::AmpersandPlus, TokenKind::AmpersandMinus
          {50, 51}
        when TokenKind::Star, TokenKind::Slash, TokenKind::SlashSlash, TokenKind::Percent, TokenKind::AmpersandStar
          {60, 61}
        when TokenKind::StarStar, TokenKind::AmpersandStarStar
          {70, 69}
        when TokenKind::Dot, TokenKind::DoubleColon, TokenKind::SafeNav
          {80, 81}
        else
          nil
        end
      end

      private def prefix_binding_power(kind : TokenKind) : Int32
        case kind
        when TokenKind::Plus, TokenKind::Minus, TokenKind::Bang, TokenKind::Tilde, TokenKind::Star, TokenKind::Ampersand,
             TokenKind::AmpersandPlus, TokenKind::AmpersandMinus, TokenKind::AmpersandStar
          70
        when TokenKind::DotDot, TokenKind::DotDotDot
          48
        else
          0
        end
      end

      private def operator_name_token?(kind : TokenKind) : Bool
        {
          TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash, TokenKind::SlashSlash,
          TokenKind::Percent, TokenKind::Caret, TokenKind::Ampersand, TokenKind::Pipe, TokenKind::Bang,
          TokenKind::Tilde, TokenKind::StarStar, TokenKind::ShiftLeft, TokenKind::ShiftRight,
          TokenKind::EqualEqual, TokenKind::BangEqual, TokenKind::Less, TokenKind::LessEqual,
          TokenKind::Greater, TokenKind::GreaterEqual, TokenKind::Match, TokenKind::NotMatch,
          TokenKind::Spaceship, TokenKind::TripleEqual, TokenKind::HashRocket,
          TokenKind::AmpersandPlus, TokenKind::AmpersandMinus,
          TokenKind::AmpersandStar, TokenKind::AmpersandStarStar,
        }.includes?(kind)
      end

      private def pseudo_method_name?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        return false if contains_macro_expr?(node_id)
        case node.kind
        when NodeKind::Ident
          name = @arena.symbols.entries[node.payload_index]
          pseudo_method_name?(name)
        when NodeKind::Path
          children = @arena.children(node_id)
          return false if children.empty?
          pseudo_method_name?(children.last)
        else
          false
        end
      end

      private def contains_macro_expr?(node_id : NodeId) : Bool
        return true if @arena.node(node_id).kind == NodeKind::MacroExpr
        @arena.children(node_id).any? { |child| contains_macro_expr?(child) }
      end

      private def pseudo_method_name?(name : String) : Bool
        case name
        when "!", "is_a?", "as", "as?", "responds_to?", "nil?"
          true
        else
          false
        end
      end

      private def responds_to_without_args?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        return false unless node.kind == NodeKind::Ident
        name = @arena.symbols.entries[node.payload_index]
        name == "responds_to?"
      end

      private def skip_balanced_parens
        depth = 0
        while !current.eof?
          token = advance
          if token.kind == TokenKind::LParen
            depth += 1
          elsif token.kind == TokenKind::RParen
            depth -= 1
            break if depth <= 0
          end
        end
      end

      private def synchronize(terminators : Array(TokenKind))
        advance unless current.eof?
        while !current.eof?
          break if terminator?(current.kind, terminators, nil)
          break if sync_stop_token?(current.kind)
          advance
        end
      end

      private def sync_stop_token?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Semicolon,
             TokenKind::RParen, TokenKind::RBracket, TokenKind::RBrace,
             TokenKind::KeywordEnd, TokenKind::KeywordElse, TokenKind::KeywordElsif,
             TokenKind::KeywordWhen, TokenKind::KeywordRescue, TokenKind::KeywordEnsure,
             TokenKind::KeywordThen, TokenKind::KeywordDo
          true
        else
          false
        end
      end

      private def expression_follows? : Bool
        case current.kind
        when TokenKind::Eof,
             TokenKind::Semicolon,
             TokenKind::Comma,
             TokenKind::KeywordEnd,
             TokenKind::KeywordElse,
             TokenKind::KeywordElsif,
             TokenKind::KeywordWhen,
             TokenKind::KeywordIn,
             TokenKind::KeywordIf,
             TokenKind::KeywordUnless,
             TokenKind::KeywordRescue,
             TokenKind::KeywordEnsure,
             TokenKind::Colon,
             TokenKind::RBrace,
             TokenKind::RParen,
             TokenKind::RBracket
          false
        else
          true
        end
      end

      private def expression_stop? : Bool
        case current.kind
        when TokenKind::Eof,
             TokenKind::Semicolon,
             TokenKind::KeywordEnd,
             TokenKind::KeywordElse,
             TokenKind::KeywordElsif,
             TokenKind::KeywordWhen,
             TokenKind::KeywordDo,
             TokenKind::KeywordIf,
             TokenKind::KeywordUnless,
             TokenKind::KeywordRescue,
             TokenKind::KeywordEnsure,
             TokenKind::RBrace,
             TokenKind::RParen,
             TokenKind::RBracket
          true
        else
          false
        end
      end

      private def assignment_op?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Assign,
             TokenKind::PlusEqual, TokenKind::MinusEqual, TokenKind::StarEqual,
             TokenKind::SlashEqual, TokenKind::SlashSlashEqual, TokenKind::PercentEqual,
             TokenKind::PipeEqual, TokenKind::AmpersandEqual, TokenKind::CaretEqual,
             TokenKind::StarStarEqual, TokenKind::ShiftLeftEqual, TokenKind::ShiftRightEqual,
             TokenKind::AmpersandPlusEqual, TokenKind::AmpersandMinusEqual, TokenKind::AmpersandStarEqual,
             TokenKind::AmpersandStarStarEqual, TokenKind::OrOrEqual, TokenKind::AndAndEqual
          true
        else
          false
        end
      end

      # Check for splats used outside of valid contexts (args, tuple, array, hash, or assignment lhs).
      private def validate_no_standalone_splat(node_id : NodeId, allow_splat : Bool = false) : Nil
        node = @arena.node(node_id)
        case node.kind
        when NodeKind::Splat, NodeKind::DoubleSplat
          unless allow_splat
            eof = @source.size
            @diagnostics << Diagnostic.new(Span.new(eof, eof), "unexpected token: EOF")
          end
          return
        when NodeKind::CallWithBlock
          children = @arena.children(node_id)
          children.each_with_index do |child, idx|
            child_allow = allow_splat || idx == 2
            validate_no_standalone_splat(child, child_allow)
          end
          return
        when NodeKind::Assign
          children = @arena.children(node_id)
          if children.size >= 2
            lhs = children[0]
            rhs = children[1]
            allow_rhs_splat = false
            rhs_node = @arena.node(rhs)
            if rhs_node.kind == NodeKind::Splat
              lhs_node = @arena.node(lhs)
              if lhs_node.kind == NodeKind::Binary &&
                 @arena.operator_kind(lhs_node.payload_index) == TokenKind::Dot
                rhs_name = @arena.children(lhs)[1]?
                if rhs_name && @arena.node(rhs_name).kind == NodeKind::Ident
                  name = @arena.symbols[@arena.node(rhs_name).payload_index]
                  allow_rhs_splat = assignable_method_name?(name)
                end
              end
            end
            validate_no_standalone_splat(lhs, true)
            validate_no_standalone_splat(rhs, allow_rhs_splat)
          end
          return
        when NodeKind::Args, NodeKind::Tuple, NodeKind::Array, NodeKind::Hash, NodeKind::NamedTuple, NodeKind::Index,
             NodeKind::MacroExpr, NodeKind::MacroControl, NodeKind::Block
          allow_splat = true
        when NodeKind::Return, NodeKind::Break, NodeKind::Next, NodeKind::Yield
          allow_splat = true
        end

        @arena.children(node_id).each do |child|
          validate_no_standalone_splat(child, allow_splat)
        end
      end

      private def validate_enum_body(body_id : NodeId) : Nil
        @arena.children(body_id).each do |child|
          kind = @arena.node(child).kind
          case kind
          when NodeKind::Tuple
            @diagnostics << Diagnostic.new(node_span(child), "expecting ';', 'end' or newline after enum member")
          when NodeKind::Call
            callee, args = @arena.children(child)
            unless newline_between?(node_span(callee).finish, node_span(args).start)
              @diagnostics << Diagnostic.new(node_span(child), "expecting ';', 'end' or newline after enum member")
            end
          end
        end
      end

      private def validate_comma_tuple(node_id : NodeId, parent_kind : NodeKind? = nil, in_macro : Bool = false, in_block : Bool = false) : Nil
        node = @arena.node(node_id)
        in_macro ||= node.kind == NodeKind::MacroDef
        if node.kind == NodeKind::Tuple && (node.flags & 1_u16) == 1_u16
          allowed = in_macro
          unless allowed
            allowed = {NodeKind::Assign, NodeKind::Return, NodeKind::Break, NodeKind::Next, NodeKind::Yield}.includes?(parent_kind)
          end
          unless allowed
            @diagnostics << Diagnostic.new(node.span, "unexpected token: \",\"")
          end
        end

        @arena.children(node_id).each_with_index do |child, idx|
          child_in_block = in_block
          if node.kind == NodeKind::CallWithBlock && idx == 2
            child_in_block = true
          elsif node.kind == NodeKind::Block
            child_in_block = true
          end
          validate_comma_tuple(child, node.kind, in_macro, child_in_block)
        end
      end

      private def validate_named_tuple_duplicates(node_id : NodeId) : Nil
        node = @arena.node(node_id)
        if node.kind == NodeKind::NamedTuple
          seen = {} of Int32 => Bool
          @arena.children(node_id).each do |entry|
            key_id = @arena.node(entry).payload_index
            if seen.has_key?(key_id)
              name = @arena.symbols[key_id]
              position = node_span(entry).start + name.bytesize
              @diagnostics << Diagnostic.new(Span.new(position, position), "duplicated key: #{name}")
            else
              seen[key_id] = true
            end
          end
        end
        @arena.children(node_id).each do |child|
          validate_named_tuple_duplicates(child)
        end
      end

      private def validate_named_arg_duplicates(args_id : NodeId) : Nil
        return unless @arena.node(args_id).kind == NodeKind::Args
        seen = {} of Int32 => Bool
        @arena.children(args_id).each do |arg_id|
          arg = @arena.node(arg_id)
          next unless arg.kind == NodeKind::NamedArg
          key = arg.payload_index
          if seen.has_key?(key)
            @diagnostics << Diagnostic.new(arg.span, "duplicated named argument: #{@arena.symbols[key]}")
          else
            seen[key] = true
          end
        end
      end

      private def validate_call_arg_order(args_id : NodeId) : Nil
        saw_double_splat = false
        @arena.children(args_id).each do |arg_id|
          arg = @arena.node(arg_id)
          if saw_double_splat && !block_arg_node?(arg_id)
            if arg.kind == NodeKind::Splat
              target = @arena.children(arg_id).first?
              span = target ? node_span(target) : arg.span
              @diagnostics << Diagnostic.new(span, "splat not allowed after double splat")
            elsif call_named?(arg_id, "out")
              callee = @arena.children(arg_id).first?
              position = callee ? node_span(callee).finish : arg.span.start
              span = Span.new(position, position)
              @diagnostics << Diagnostic.new(span, "out argument not allowed after double splat")
            else
              @diagnostics << Diagnostic.new(arg.span, "argument not allowed after double splat")
            end
          end
          saw_double_splat = true if arg.kind == NodeKind::DoubleSplat
        end
      end

      private def validate_dynamic_constant_assignments(node_id : NodeId, in_def : Bool, parent_exprs : Bool, allow_const : Bool) : Nil
        node = @arena.node(node_id)
        if node.kind == NodeKind::Assign
          lhs = @arena.children(node_id)[0]?
          if lhs
            lhs_node = @arena.node(lhs)
            if lhs_node.kind == NodeKind::Ident
              name = @arena.symbols[lhs_node.payload_index]
              if !name.empty? && name[0].ascii_uppercase?
                rhs = @arena.children(node_id)[1]?
                position = rhs ? Math.max(node_span(rhs).start - 1, 0) : lhs_node.span.start
                diagnostic_span = Span.new(position, position)
                if in_def
                  @diagnostics << Diagnostic.new(diagnostic_span, "dynamic constant assignment. Constants can only be declared at the top level or inside other types.")
                elsif !allow_const
                  @diagnostics << Diagnostic.new(diagnostic_span, "dynamic constant assignment. Constants can only be declared at the top level or inside other types.")
                end
              end
            end
          end
        end

        child_in_def = in_def || node.kind == NodeKind::Def
        child_parent_exprs = node.kind == NodeKind::Expressions
        children = @arena.children(node_id)
        children.each_with_index do |child, idx|
          child_allow_const = false
          case node.kind
          when NodeKind::File
            child_allow_const = true
          when NodeKind::Expressions
            child_allow_const = allow_const
          when NodeKind::Annotation
            child_allow_const = allow_const && idx == 1
          when NodeKind::CallWithBlock
            child_allow_const = idx == 2 && record_block?(node_id)
          when NodeKind::Call
            child_allow_const = idx == 1 && call_named?(node_id, "type")
          when NodeKind::Args
            child_allow_const = allow_const
          when NodeKind::Class, NodeKind::Struct, NodeKind::Module, NodeKind::Enum, NodeKind::Lib
            child_allow_const = idx == 2
          end
          validate_dynamic_constant_assignments(child, child_in_def, child_parent_exprs, child_allow_const)
        end
      end

      private def call_named?(node_id : NodeId, name : String) : Bool
        children = @arena.children(node_id)
        return false if children.empty?
        callee = @arena.node(children[0])
        callee.kind == NodeKind::Ident && @arena.symbols[callee.payload_index] == name
      end

      private def validate_void_value_expressions(node_id : NodeId, parent_kind : NodeKind? = nil, parent_id : NodeId? = nil) : Nil
        node = @arena.node(node_id)
        if {NodeKind::Break, NodeKind::Return, NodeKind::Next}.includes?(node.kind)
          if parent_kind != NodeKind::Expressions && parent_kind != NodeKind::Rescue && parent_kind != NodeKind::Ensure
            short_circuit = false
            if parent_kind == NodeKind::Binary && parent_id
              parent = @arena.node(parent_id)
              op = @arena.operator_kind(parent.payload_index)
              short_circuit = op == TokenKind::OrOr || op == TokenKind::AndAnd
            end
            add_void_value_diagnostic(node) unless short_circuit
          else
            children = @arena.children(node_id)
            if children.size > 0
              child = children[0]
              child_node = @arena.node(child)
              error_is_void = child_node.kind == NodeKind::Error && span_text(child_node.span) != "do"
              if void_control_adjacent?(node, child_node) || void_control_operator_arg?(child) || error_is_void
                add_void_value_diagnostic(node, child_node)
              end
            end
          end
        end
        @arena.children(node_id).each do |child|
          validate_void_value_expressions(child, node.kind, node_id)
        end
      end

      private def add_void_value_diagnostic(node : Node, child : Node? = nil) : Nil
        span = if child && child.kind == NodeKind::Error && span_text(child.span) == "?"
                 child.span
               else
                 Span.new(node.span.start, node.span.start)
               end
        @diagnostics.unshift(Diagnostic.new(span, "void value expression"))
      end

      private def validate_case_when_clauses(node_id : NodeId) : Nil
        node = @arena.node(node_id)
        if node.kind == NodeKind::Case
          subject_id = @arena.children(node_id)[0]?
          subject = subject_id ? @arena.node(subject_id) : nil
          if subject_id && contains_node_kind?(subject_id, NodeKind::Splat)
            @diagnostics << Diagnostic.new(node_span(subject_id), "splat is not allowed in case subject")
          end
          whens_node = @arena.children(node_id)[1]?
          if whens_node
            seen = {} of String => Bool
            @arena.children(whens_node).each do |when_id|
              conds_node = @arena.children(when_id)[0]?
              next unless conds_node
              @arena.children(conds_node).each do |cond_id|
                cond = @arena.node(cond_id)
                if void_id = first_void_control_node(cond_id)
                  void_node = @arena.node(void_id)
                  @diagnostics.unshift(Diagnostic.new(Span.new(void_node.span.start, void_node.span.start), "void value expression"))
                end
                if subject && subject.kind == NodeKind::Nop && span_text(cond.span).starts_with?(".")
                  @diagnostics << Diagnostic.new(cond.span, "implicit object condition requires a case subject")
                end
                if subject && subject.kind == NodeKind::Tuple && cond.kind == NodeKind::Tuple &&
                   @arena.children(subject_id.not_nil!).size != @arena.children(cond_id).size
                  given = @arena.children(cond_id).size
                  expected = @arena.children(subject_id.not_nil!).size
                  @diagnostics << Diagnostic.new(cond.span, "wrong number of tuple elements (given #{given}, expected #{expected})")
                end
                if subject && subject.kind == NodeKind::Tuple && contains_node_kind?(cond_id, NodeKind::Splat)
                  @diagnostics << Diagnostic.new(cond.span, "splat is not allowed in case condition")
                end
                if case_when_underscore?(cond_id)
                  @diagnostics << Diagnostic.new(node_span(cond_id), "'when _' is not supported, use 'else' block instead")
                end

                if key = case_when_duplicate_key(cond_id)
                  if seen.has_key?(key)
                    @diagnostics << Diagnostic.new(node_span(cond_id), "duplicate when #{key} in case")
                  else
                    seen[key] = true
                  end
                end
              end
            end
          end
        end

        @arena.children(node_id).each do |child|
          validate_case_when_clauses(child)
        end
      end

      private def contains_node_kind?(node_id : NodeId, kind : NodeKind) : Bool
        return true if @arena.node(node_id).kind == kind
        @arena.children(node_id).any? { |child| contains_node_kind?(child, kind) }
      end

      private def first_void_control_node(node_id : NodeId) : NodeId?
        node = @arena.node(node_id)
        return node_id if {NodeKind::Break, NodeKind::Return, NodeKind::Next}.includes?(node.kind)
        @arena.children(node_id).each do |child|
          if found = first_void_control_node(child)
            return found
          end
        end
        nil
      end

      private def case_when_underscore?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        node.kind == NodeKind::Ident && @arena.symbols[node.payload_index] == "_"
      end

      private def case_when_duplicate_key(node_id : NodeId) : String?
        node = @arena.node(node_id)
        case node.kind
        when NodeKind::LiteralNil,
             NodeKind::LiteralBool,
             NodeKind::LiteralNumber,
             NodeKind::LiteralChar,
             NodeKind::LiteralString,
             NodeKind::LiteralSymbol,
             NodeKind::LiteralRegex,
             NodeKind::Tuple,
             NodeKind::Array,
             NodeKind::Range,
             NodeKind::Path
          span_text(node.span)
        when NodeKind::Ident
          text = @arena.symbols[node.payload_index]
          if !text.empty? && text[0].ascii_uppercase?
            text
          else
            nil
          end
        else
          nil
        end
      end

      private def void_control_adjacent?(node : Node, child : Node) : Bool
        keyword_len = case node.kind
                      when NodeKind::Break
                        5
                      when NodeKind::Return
                        6
                      else
                        4
                      end
        node.span.start + keyword_len == child.span.start
      end

      private def void_control_operator_arg?(child_id : NodeId) : Bool
        child = @arena.node(child_id)
        case child.kind
        when NodeKind::Unary
          operand = @arena.children(child_id)[0]?
          return false unless operand
          op_kind = @arena.operator_kind(child.payload_index)
          op_len = case op_kind
                   when TokenKind::AmpersandPlus, TokenKind::AmpersandMinus, TokenKind::AmpersandStar
                     2
                   else
                     1
                   end
          op_end = child.span.start + op_len
          whitespace_between?(op_end, @arena.node(operand).span.start)
        when NodeKind::Splat, NodeKind::DoubleSplat
          operand = @arena.children(child_id)[0]?
          return false unless operand
          op_len = child.kind == NodeKind::DoubleSplat ? 2 : 1
          op_end = child.span.start + op_len
          whitespace_between?(op_end, @arena.node(operand).span.start)
        when NodeKind::Range
          left = @arena.children(child_id)[0]?
          left ? @arena.node(left).kind == NodeKind::Nop : false
        else
          false
        end
      end

      private def command_call_start?(kind : TokenKind) : Bool
        return true if soft_identifier_kind?(kind)
        case kind
        when TokenKind::Identifier, TokenKind::InstanceVar, TokenKind::ClassVar, TokenKind::GlobalVar,
             TokenKind::Number, TokenKind::String, TokenKind::Char, TokenKind::Regex, TokenKind::Symbol,
             TokenKind::KeywordTrue, TokenKind::KeywordFalse, TokenKind::KeywordNil, TokenKind::KeywordSelf,
             TokenKind::LParen, TokenKind::LBracket, TokenKind::LBrace, TokenKind::SafeNav, TokenKind::Arrow,
             TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::StarStar, TokenKind::Bang, TokenKind::Tilde,
             TokenKind::KeywordOut, TokenKind::KeywordBegin, TokenKind::KeywordYield,
             TokenKind::KeywordAlignof, TokenKind::KeywordInstanceAlignof,
             TokenKind::KeywordInstanceSizeof, TokenKind::KeywordOffsetof,
             TokenKind::KeywordPointerof, TokenKind::KeywordSizeof,
             TokenKind::KeywordTypeof, TokenKind::KeywordUninitialized,
             TokenKind::KeywordAs, TokenKind::KeywordAsQuestion
          true
        else
          false
        end
      end

      private def command_call_start_here? : Bool
        if {TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::StarStar}.includes?(current.kind) && !adjacent?(current, peek1)
          return false
        end
        command_call_start?(current.kind)
      end

      private def expression_start_token?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier,
             TokenKind::InstanceVar,
             TokenKind::ClassVar,
             TokenKind::GlobalVar,
             TokenKind::Number,
             TokenKind::String,
             TokenKind::Char,
             TokenKind::Symbol,
             TokenKind::KeywordTrue,
             TokenKind::KeywordFalse,
             TokenKind::KeywordNil,
             TokenKind::KeywordSelf
          true
        else
          false
        end
      end

      private def command_callee?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
        case node.kind
        when NodeKind::Ident, NodeKind::InstanceVar, NodeKind::ClassVar, NodeKind::Global,
             NodeKind::Path, NodeKind::Call, NodeKind::CallWithBlock, NodeKind::MacroVar
          true
        when NodeKind::Binary
          op = @arena.operator_kind(node.payload_index)
          op == TokenKind::Dot || op == TokenKind::SafeNav
        else
          false
        end
      end

      private def record_block?(node_id : NodeId) : Bool
        call = @arena.children(node_id)[0]?
        return false unless call && @arena.node(call).kind == NodeKind::Call
        callee = @arena.children(call)[0]?
        return false unless callee
        callee_node = @arena.node(callee)
        callee_node.kind == NodeKind::Ident && @arena.symbols[callee_node.payload_index] == "record"
      end

      private def block_callee?(node_id : NodeId) : Bool
        case @arena.node(node_id).kind
        when NodeKind::Ident, NodeKind::InstanceVar, NodeKind::ClassVar, NodeKind::Global,
             NodeKind::Path, NodeKind::Call, NodeKind::CallWithBlock, NodeKind::Binary, NodeKind::MacroVar
          true
        else
          false
        end
      end

      private def skip_separators
        while current.kind == TokenKind::Semicolon
          advance
        end
      end

      private def terminator?(kind : TokenKind, terminators : Array(TokenKind), stop : Proc(Bool)?) : Bool
        return true if stop && stop.call
        terminators.includes?(kind)
      end

      private def current : Token
        @tokens.peek
      end

      private def peek1 : Token
        @tokens.peek(1)
      end

      private def peek2 : Token
        @tokens.peek(2)
      end

      private def advance : Token
        @tokens.next
      end

      private def match(kind : TokenKind) : Bool
        return false unless current.kind == kind
        advance
        true
      end

      private def expect(kind : TokenKind, message : String) : Token
        token = current
        if token.kind == kind
          advance
          return token
        end
        @diagnostics << Diagnostic.new(token.span, message)
        advance unless token.eof?
        token
      end

      private def parse_require : NodeId
        start = advance
        if @def_depth > 0
          @diagnostics << Diagnostic.new(start.span, "can't require inside def")
        elsif @type_depth > 0
          @diagnostics << Diagnostic.new(start.span, "can't require inside type declarations")
        end
        if current.kind == TokenKind::String
          str = advance
          span = Span.new(start.span.start, str.span.finish)
          return @arena.add_node(NodeKind::Require, span, [@arena.add_literal_node(LiteralKind::String, str.span)])
        else
          @diagnostics << Diagnostic.new(current.span, "expected string literal after require")
          return @arena.add_node(NodeKind::Error, current.span)
        end
      end

      private def span_from(start : Span, finish : Span) : Span
        Span.new(start.start, finish.finish)
      end

      private def span_from_nodes(start_id : NodeId, end_id : NodeId) : Span
        span_from(@arena.node(start_id).span, @arena.node(end_id).span)
      end

      private def node_span(node_id : NodeId) : Span
        @arena.node(node_id).span
      end

      private def token_text(token : Token) : String
        String.new(@source.bytes[token.span.start, token.span.length])
      end

      private def diagnostic_token_text(token : Token) : String
        token.eof? ? "EOF" : token_text(token)
      end

      private def crystal_diagnostic_token_text(token : Token) : String
        text = diagnostic_token_text(token)
        case token.kind
        when TokenKind::String, TokenKind::Regex
          text.starts_with?("%w") || text.starts_with?("%W") ? "STRING_ARRAY_START" : "DELIMITER_START"
        when TokenKind::Char
          text.size >= 2 ? text.byte_slice(1, text.bytesize - 2) : text
        else
          text
        end
      end

      private def diagnose_declaration_inside_def : Nil
        return unless @def_depth > 0
        message = case current.kind
                  when TokenKind::KeywordDef        then "can't define def inside def"
                  when TokenKind::KeywordMacro      then "can't define macro inside def"
                  when TokenKind::KeywordClass      then "can't define class inside def"
                  when TokenKind::KeywordStruct     then "can't define struct inside def"
                  when TokenKind::KeywordModule     then "can't define module inside def"
                  when TokenKind::KeywordEnum       then "can't define enum inside def"
                  when TokenKind::KeywordLib        then "can't define lib inside def"
                  when TokenKind::KeywordFun        then "can't define fun inside def"
                  when TokenKind::KeywordAlias      then "can't define alias inside def"
                  when TokenKind::KeywordAnnotation then "can't define annotation inside def"
                  when TokenKind::KeywordAbstract   then "can't use abstract inside def"
                  when TokenKind::KeywordInclude    then "can't include inside def"
                  when TokenKind::KeywordExtend     then "can't extend inside def"
                  end
        @diagnostics << Diagnostic.new(current.span, message) if message
      end

      private def invalid_dot_method_name?(token : Token) : Bool
        return false unless token.span.start > 0 && @source.bytes[token.span.start - 1] == '.'.ord.to_u8
        assignment_op?(token.kind) || token.kind == TokenKind::AndAnd || token.kind == TokenKind::OrOr
      end

      private def span_text(span : Span) : String
        String.new(@source.bytes[span.start, span.length])
      end

      private def method_bracket_name_context?(lbracket_pos : Int32) : Bool
        return false if lbracket_pos <= 0
        @source.bytes[lbracket_pos - 1] == '.'.ord.to_u8
      end

      private def newline_between?(start_pos : Int32, end_pos : Int32) : Bool
        bytes = @source.bytes
        if start_pos > 0
          previous = bytes[start_pos - 1]
          return true if previous == 0x0a_u8 || previous == 0x0d_u8
        end
        i = start_pos
        while i < end_pos
          byte = bytes[i]
          return true if byte == 0x0a_u8 || byte == 0x0d_u8
          i += 1
        end
        false
      end

      private def whitespace_between?(start_pos : Int32, end_pos : Int32) : Bool
        i = start_pos
        bytes = @source.bytes
        while i < end_pos && i < bytes.size
          byte = bytes[i]
          return true if byte == 0x20_u8 || byte == 0x09_u8 || byte == 0x0d_u8 || byte == 0x0a_u8
          i += 1
        end
        false
      end

      private def adjacent?(left : Token, right : Token) : Bool
        left.span.finish == right.span.start
      end

      private def macro_expr_start? : Bool
        return false if escaped_macro_start?(current)
        current.kind == TokenKind::LBrace &&
          peek1.kind == TokenKind::LBrace &&
          adjacent?(current, peek1)
      end

      private def macro_control_start? : Bool
        return false if escaped_macro_start?(current)
        current.kind == TokenKind::LBrace &&
          peek1.kind == TokenKind::Percent &&
          adjacent?(current, peek1)
      end

      private def escaped_macro_start?(token : Token) : Bool
        return false if token.span.start == 0
        @source.bytes[token.span.start - 1] == '\\'.ord.to_u8
      end

      private def escaped_macro_literal_start? : Bool
        current.kind == TokenKind::LBrace &&
          {TokenKind::Percent, TokenKind::LBrace}.includes?(peek1.kind) &&
          escaped_macro_start?(current)
      end

      private def parse_escaped_macro_literal : NodeId
        start = advance # '{'
        delimiter = current.kind
        advance if {TokenKind::Percent, TokenKind::LBrace}.includes?(delimiter)
        depth = 1
        finish = start.span.finish
        while !current.eof?
          if delimiter == TokenKind::LBrace && current.kind == TokenKind::LBrace && peek1.kind == TokenKind::LBrace
            depth += 1
            advance
            finish = advance.span.finish
          elsif ((delimiter == TokenKind::Percent && current.kind == TokenKind::Percent) ||
                (delimiter == TokenKind::LBrace && current.kind == TokenKind::RBrace)) &&
                peek1.kind == TokenKind::RBrace
            advance
            finish = advance.span.finish
            depth -= 1
            break if depth == 0
          else
            finish = advance.span.finish
          end
        end
        span = Span.new(start.span.start, finish)
        @arena.add_node(NodeKind::Nop, span)
      end

      private def macro_expr_end? : Bool
        current.kind == TokenKind::RBrace &&
          peek1.kind == TokenKind::RBrace &&
          adjacent?(current, peek1)
      end

      private def macro_control_end? : Bool
        current.kind == TokenKind::Percent &&
          peek1.kind == TokenKind::RBrace &&
          adjacent?(current, peek1)
      end

      private def macro_var_start? : Bool
        @macro_depth > 0 &&
          current.kind == TokenKind::Percent &&
          peek1.kind == TokenKind::Identifier &&
          adjacent?(current, peek1)
      end

      private def parse_macro_expr : NodeId
        if @macro_expr_depth > 0
          @diagnostics << Diagnostic.new(current.span, "can't nest macro expressions")
        end
        start = advance
        advance if current.kind == TokenKind::LBrace
        @macro_depth += 1
        @macro_expr_depth += 1
        body = parse_expressions([TokenKind::Eof], -> { macro_expr_end? })
        @macro_expr_depth -= 1
        @macro_depth -= 1
        end_token = if macro_expr_end?
                      advance
                      advance
                    else
                      @diagnostics << Diagnostic.new(current.span, "expected '}}' to close macro expression")
                      current
                    end
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::MacroExpr, span, [body])
      end

      private def parse_macro_control : NodeId
        if @macro_expr_depth > 0
          @diagnostics << Diagnostic.new(current.span, "can't nest macro expressions")
        end
        tag_kind, header, tag_span = parse_macro_tag
        case tag_kind
        when TokenKind::KeywordIf, TokenKind::KeywordUnless
          if hidden_end = hidden_macro_end_between?(tag_span.finish, current.span.start)
            empty = @arena.add_node(NodeKind::Expressions, Span.new(tag_span.finish, tag_span.finish))
            span = Span.new(tag_span.start, hidden_end.finish)
            return @arena.add_node(NodeKind::MacroControl, span, [header, empty, empty], payload_index: tag_kind.to_i32)
          end
          then_body = parse_macro_body([TokenKind::KeywordElse, TokenKind::KeywordElsif, TokenKind::KeywordEnd])
          else_body = parse_macro_if_tail(tag_kind == TokenKind::KeywordUnless)
          end_span = consume_macro_end("expected '{% end %}' to close macro if")
          span = Span.new(tag_span.start, end_span.finish)
          @arena.add_node(NodeKind::MacroControl, span, [header, then_body, else_body], payload_index: tag_kind.to_i32)
        when TokenKind::KeywordFor
          body = parse_macro_body([TokenKind::KeywordEnd])
          end_span = consume_macro_end("expected '{% end %}' to close macro for")
          span = Span.new(tag_span.start, end_span.finish)
          @arena.add_node(NodeKind::MacroControl, span, [header, body], payload_index: tag_kind.to_i32)
        when TokenKind::KeywordBegin
          body = parse_macro_body([TokenKind::KeywordEnd])
          end_span = consume_macro_end("expected '{% end %}' to close macro begin")
          span = Span.new(tag_span.start, end_span.finish)
          @arena.add_node(NodeKind::MacroControl, span, [header, body], payload_index: tag_kind.to_i32)
        when TokenKind::KeywordVerbatim
          end_span = parse_macro_verbatim_body
          span = Span.new(tag_span.start, end_span.finish)
          @arena.add_node(NodeKind::MacroControl, span, [header], payload_index: tag_kind.to_i32)
        when TokenKind::KeywordEnd, TokenKind::KeywordElse, TokenKind::KeywordElsif
          @diagnostics << Diagnostic.new(tag_span, "unexpected macro control tag")
          @arena.add_node(NodeKind::MacroControl, tag_span, [header], payload_index: tag_kind.to_i32)
        else
          @arena.add_node(NodeKind::MacroControl, tag_span, [header], payload_index: tag_kind.to_i32)
        end
      end

      private def parse_macro_body(end_kinds : Array(TokenKind)) : NodeId
        @macro_depth += 1
        children = [] of NodeId
        while !current.eof? && !macro_control_boundary?(end_kinds, tag_only: true)
          if macro_control_start?
            children << parse_macro_control
          elsif macro_expr_start?
            children << parse_macro_expr
          elsif macro_var_start?
            children << parse_macro_var
          elsif escaped_macro_literal_start?
            children << parse_escaped_macro_literal
          else
            start_pos = current.span.start
            last = current
            while !current.eof? &&
                  !macro_control_start? &&
                  !macro_expr_start? &&
                  !macro_var_start? &&
                  !escaped_macro_literal_start? &&
                  !macro_control_boundary?(end_kinds, tag_only: true)
              last = advance
            end
            span = Span.new(start_pos, last.span.finish)
            children << @arena.add_node(NodeKind::Nop, span)
          end
        end
        span = if children.empty?
                 Span.new(current.span.start, current.span.start)
               else
                 span_from_nodes(children.first, children.last)
               end
        @macro_depth -= 1
        @arena.add_node(NodeKind::Expressions, span, children)
      end

      private def hidden_macro_end_between?(start_pos : Int32, end_pos : Int32) : Span?
        return nil if end_pos <= start_pos
        segment = @source.text.byte_slice(start_pos, end_pos - start_pos)
        return nil unless offset = segment.index("{% end %}")
        absolute_start = start_pos + offset
        Span.new(absolute_start, absolute_start + 9)
      end

      private def parse_macro_var : NodeId
        percent = advance
        ident = advance
        symbol_id = @arena.symbols.intern(token_text(ident))
        span = Span.new(percent.span.start, ident.span.finish)
        @arena.add_node(NodeKind::MacroVar, span, payload_index: symbol_id)
      end

      private def parse_macro_tag : Tuple(TokenKind, NodeId, Span)
        start = advance
        advance if current.kind == TokenKind::Percent
        tag_token = current
        tag_kind = tag_token.kind
        single_block = @macro_def_depth > 0 && macro_single_block?
        consume_tag = true
        control_kinds = {
          TokenKind::KeywordIf, TokenKind::KeywordUnless, TokenKind::KeywordFor,
          TokenKind::KeywordBegin, TokenKind::KeywordVerbatim,
          TokenKind::KeywordEnd, TokenKind::KeywordElse, TokenKind::KeywordElsif,
        }
        if single_block && {TokenKind::KeywordIf, TokenKind::KeywordUnless, TokenKind::KeywordFor}.includes?(tag_kind)
          tag_kind = TokenKind::Unknown
          consume_tag = false
        elsif !control_kinds.includes?(tag_kind)
          tag_kind = TokenKind::Unknown
          consume_tag = false
        elsif {TokenKind::KeywordIf, TokenKind::KeywordUnless, TokenKind::KeywordFor, TokenKind::KeywordBegin}.includes?(tag_kind) &&
              macro_tag_contains_end?
          # Inline `{% if ... end %}` style macro expressions should be parsed as a generic tag.
          tag_kind = TokenKind::Unknown
          consume_tag = false
        end
        advance if consume_tag && !tag_token.eof?
        @macro_depth += 1
        header = if tag_kind == TokenKind::KeywordFor
                   parse_macro_for_header
                 elsif tag_kind == TokenKind::KeywordVerbatim
                   advance if current.kind == TokenKind::KeywordDo
                   @arena.add_node(NodeKind::Nop, Span.new(tag_token.span.finish, tag_token.span.finish))
                 else
                   parse_expressions([TokenKind::Eof], -> { macro_control_end? })
                 end
        @macro_depth -= 1
        end_token = if macro_control_end?
                      advance
                      advance
                    else
                      @diagnostics << Diagnostic.new(current.span, "expected '%}' to close macro control")
                      current
                    end
        span = Span.new(start.span.start, end_token.span.finish)
        {tag_kind, header, span}
      end

      private def macro_tag_contains_end? : Bool
        offset = 0
        loop do
          tok = @tokens.peek(offset)
          return false if tok.kind == TokenKind::Eof
          if tok.kind == TokenKind::Percent
            nxt = @tokens.peek(offset + 1)
            return false if nxt.kind == TokenKind::RBrace && adjacent?(tok, nxt)
          end
          return true if tok.kind == TokenKind::KeywordEnd
          offset += 1
        end
      end

      private def parse_macro_verbatim_body : Span
        depth = 1
        loop do
          if macro_control_start?
            kind = peek2.kind
            if {TokenKind::KeywordIf, TokenKind::KeywordUnless, TokenKind::KeywordFor,
                TokenKind::KeywordBegin, TokenKind::KeywordVerbatim}.includes?(kind)
              depth += 1
            elsif kind == TokenKind::KeywordEnd
              depth -= 1
            end
            _, _, span = parse_macro_tag
            return span if depth == 0
            next
          end
          break if current.eof?
          advance
        end
        current.span
      end

      private def parse_macro_for_header : NodeId
        targets = parse_macro_for_targets

        if match(TokenKind::KeywordIn)
          if macro_control_end?
            @diagnostics << Diagnostic.new(current.span, "expected expression after 'in'")
            iter = @arena.add_node(NodeKind::Error, current.span)
          else
            iter = parse_expression(0, -> { macro_control_end? })
          end
        else
          @diagnostics << Diagnostic.new(current.span, "expected 'in' in macro for")
          iter = @arena.add_node(NodeKind::Error, current.span)
        end

        span = span_from_nodes(targets, iter)
        @arena.add_node(NodeKind::MacroForHeader, span, [targets, iter])
      end

      private def parse_macro_for_targets : NodeId
        children = [] of NodeId
        stop_at_in = -> { current.kind == TokenKind::Comma || current.kind == TokenKind::KeywordIn || macro_control_end? }

        if stop_at_in.call
          @diagnostics << Diagnostic.new(current.span, "expected for loop variable")
          children << @arena.add_node(NodeKind::Error, current.span)
        else
          loop do
            children << parse_expression(0, stop_at_in)
            break unless match(TokenKind::Comma)
            break if stop_at_in.call
          end
        end

        span = if children.empty?
                 Span.new(current.span.start, current.span.start)
               else
                 span_from_nodes(children.first, children.last)
               end
        @arena.add_node(NodeKind::Args, span, children)
      end

      private def macro_control_boundary?(kinds : Array(TokenKind), tag_only : Bool = false) : Bool
        unless tag_only
          return true if kinds.includes?(current.kind) && {TokenKind::KeywordElse, TokenKind::KeywordElsif, TokenKind::KeywordEnd}.includes?(current.kind)
        end
        return false if escaped_macro_start?(current)
        return false unless current.kind == TokenKind::LBrace && peek1.kind == TokenKind::Percent
        kind = peek2.kind
        kinds.empty? || kinds.includes?(kind)
      end

      private def macro_single_block? : Bool
        i = 0
        loop do
          tok = @tokens.peek(i)
          return true if tok.kind == TokenKind::Eof
          if tok.kind == TokenKind::LBrace && tok.span.finish == @tokens.peek(i + 1).span.start && @tokens.peek(i + 1).kind == TokenKind::Percent
            return false
          end
          i += 1
        end
      end

      private def consume_macro_end(message : String) : Span
        if macro_control_boundary?([TokenKind::KeywordEnd])
          _, _, span = parse_macro_tag
          return span
        end
        if @macro_def_depth > 0 && current.kind == TokenKind::KeywordEnd
          return Span.new(current.span.start, current.span.start)
        end
        @diagnostics << Diagnostic.new(current.span, message)
        current.span
      end

      private def parse_macro_if_tail(disallow_elsif : Bool = false) : NodeId
        return @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start)) unless macro_control_start?

        if macro_control_boundary?([TokenKind::KeywordElse])
          parse_macro_tag
          body = parse_macro_body([TokenKind::KeywordEnd])
          return body
        end

        if macro_control_boundary?([TokenKind::KeywordElsif])
          if disallow_elsif
            @diagnostics << Diagnostic.new(current.span, "'elsif' is not allowed after macro 'unless'")
          end
          tag_kind, header, tag_span = parse_macro_tag
          then_body = parse_macro_body([TokenKind::KeywordElse, TokenKind::KeywordElsif, TokenKind::KeywordEnd])
          else_body = parse_macro_if_tail(disallow_elsif)
          end_node = if @arena.node(else_body).kind == NodeKind::Nop
                       then_body
                     else
                       else_body
                     end
          span = span_from_nodes(header, end_node)
          return @arena.add_node(NodeKind::MacroControl, span, [header, then_body, else_body], payload_index: tag_kind.to_i32)
        end

        @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
      end
    end
  end
end
