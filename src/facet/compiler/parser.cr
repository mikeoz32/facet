module Facet
  module Compiler
    class Parser
      getter diagnostics : Array(Diagnostic)

      def initialize(@source : Source)
        @lexer = Lexer.new(@source)
        @tokens = TokenStream.new(@lexer)
        @arena = AstArena.new
        @diagnostics = [] of Diagnostic
        @macro_depth = 0
        @macro_def_depth = 0
        @lib_depth = 0
      end

      def parse_file : AstFile
        exprs = parse_expressions([TokenKind::Eof])

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
        if macro_control_start?
          return parse_macro_control
        end
        if current.kind == TokenKind::Annotation
          annotations = [] of NodeId
          while current.kind == TokenKind::Annotation
            annotations << parse_annotation
            skip_separators
          end
          if current.kind == TokenKind::Semicolon || current.kind == TokenKind::KeywordEnd
            nop = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
            return attach_annotations(annotations, nop)
          end
          target = parse_statement(terminators, expr_stop)
          return attach_annotations(annotations, target)
        end
        if var_decl_start?(current.kind) && (
             peek1.kind == TokenKind::Colon ||
             (peek1.kind == TokenKind::Comma && var_decl_with_comma_ahead?) ||
             (peek1.kind == TokenKind::Assign && var_decl_with_assign_ahead?)
           )
          return parse_var_decl
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
                 parse_control(NodeKind::Return)
               when TokenKind::KeywordBreak
                 parse_control(NodeKind::Break)
               when TokenKind::KeywordNext
                 parse_control(NodeKind::Next)
               when TokenKind::KeywordYield
                 parse_control(NodeKind::Yield)
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
                 parse_type_block(NodeKind::Struct, "expected 'end' to close union")
               when TokenKind::KeywordLib
                 parse_type_block(NodeKind::Lib, "expected 'end' to close lib")
               when TokenKind::KeywordAnnotation
                 parse_annotation_def
        when TokenKind::KeywordPrivate, TokenKind::KeywordProtected
          parse_visibility
        when TokenKind::KeywordFun
          parse_fun
        when TokenKind::KeywordAlias
                 parse_alias
               when TokenKind::KeywordType
                 parse_type_def
               when TokenKind::KeywordFor
                 parse_for
               when TokenKind::KeywordCase
                 parse_case
               when TokenKind::KeywordProperty, TokenKind::KeywordGetter, TokenKind::KeywordSetter
                  parse_property_like
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

      private def parse_annotation : NodeId
        at = advance
        expect(TokenKind::LBracket, "expected '[' after annotation")
        arg_node = if current.kind == TokenKind::RBracket
                     @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
                   else
                     values = [] of NodeId
                     values << parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket })
                     while match(TokenKind::Comma)
                       break if current.kind == TokenKind::RBracket
                       values << parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket })
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
        body = parse_expressions([TokenKind::KeywordEnd])
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close annotation")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::AnnotationDef, span, [name_node, body])
      end

      private def var_decl_start?(kind : TokenKind) : Bool
        soft_identifier_kind?(kind) || kind == TokenKind::InstanceVar || kind == TokenKind::ClassVar || kind == TokenKind::GlobalVar
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

      private def parse_var_decl : NodeId
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
          value_node = parse_expression(0, -> { current.kind == TokenKind::Colon || current.kind == TokenKind::Semicolon || current.kind == TokenKind::KeywordEnd })
        end
        colon = expect(TokenKind::Colon, "expected ':' in declaration")
        type_node = parse_type(-> { current.kind == TokenKind::Assign || current.kind == TokenKind::Semicolon || current.kind == TokenKind::KeywordEnd }, allow_tuple: true)
        value_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if !value_before_type && match(TokenKind::Assign)
          value_node = parse_expression
        elsif value_before_type
          # value already parsed
        end
        decls = names.map do |lhs|
          span = Span.new(node_span(lhs).start, [node_span(value_node).finish, node_span(type_node).finish].max)
          @arena.add_node(NodeKind::VarDecl, span, [lhs, type_node, value_node])
        end
        if decls.size == 1
          decls.first
        else
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
          sym = @arena.symbols.intern(token_text(ident))
          @arena.add_node(NodeKind::Global, ident.span, payload_index: sym)
        else
          tok = current
          @diagnostics << Diagnostic.new(tok.span, "expected variable name")
          advance unless tok.eof?
          @arena.add_node(NodeKind::Error, tok.span)
        end
      end

      private def soft_identifier_kind?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier, TokenKind::KeywordType
          true
        else
          false
        end
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
             TokenKind::ShiftLeft, TokenKind::ShiftRight
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
          cond = parse_expression
          then_body = wrap_expressions(node)
          else_body = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
          span = Span.new(node_span(node).start, node_span(cond).finish)
          return @arena.add_node(
            kw.kind == TokenKind::KeywordIf ? NodeKind::If : NodeKind::Unless,
            span,
            [cond, then_body, else_body]
          )
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
        body = parse_expressions([TokenKind::KeywordEnd])
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close while")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::While, span, [cond, body])
      end

      private def parse_until : NodeId
        start = advance
        cond = parse_expression
        body = parse_expressions([TokenKind::KeywordEnd])
        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close until")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Until, span, [cond, body])
      end

      private def parse_begin : NodeId
        start = advance
        body = parse_expressions([TokenKind::KeywordRescue, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
        rescue_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        ensure_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))

        if current.kind == TokenKind::KeywordRescue
          advance
          rescue_body = parse_expressions([TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          if current.kind == TokenKind::KeywordRescue
            while current.kind == TokenKind::KeywordRescue
              advance
              _ = parse_expression if expression_follows?
              parse_expressions([TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
            end
          end
          rescue_span = span_from_nodes(rescue_body, rescue_body)
          rescue_node = @arena.add_node(NodeKind::Rescue, rescue_span, [rescue_body])
        end

        if current.kind == TokenKind::KeywordEnsure
          advance
          ensure_body = parse_expressions([TokenKind::KeywordEnd])
          ensure_span = span_from_nodes(ensure_body, ensure_body)
          ensure_node = @arena.add_node(NodeKind::Ensure, ensure_span, [ensure_body])
        end

        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close begin")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Begin, span, [body, rescue_node, ensure_node])
      end

      private def parse_visibility : NodeId
        vis_token = advance
        case current.kind
        when TokenKind::KeywordDef
          parse_def(NodeKind::Def, TokenKind::KeywordEnd, "expected 'end' to close def")
        when TokenKind::KeywordMacro
          parse_def(NodeKind::MacroDef, TokenKind::KeywordEnd, "expected 'end' to close macro")
        else
          @arena.add_node(NodeKind::Nop, vis_token.span)
        end
      end

      private def parse_case : NodeId
        start = advance
        subject = if current.kind == TokenKind::KeywordWhen || current.kind == TokenKind::KeywordEnd
                    @arena.add_node(NodeKind::Nop, Span.new(start.span.finish, start.span.finish))
                  else
                    parse_expression
                  end
        skip_separators

        whens = [] of NodeId
        while current.kind == TokenKind::KeywordWhen || current.kind == TokenKind::KeywordIn
          whens << parse_when
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
          whens << parse_when
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
        while match(TokenKind::Comma)
          conds << parse_expression(0, -> { when_condition_stop? })
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
        when TokenKind::KeywordThen, TokenKind::KeywordWhen, TokenKind::KeywordElse, TokenKind::KeywordEnd
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
          if current.kind == TokenKind::Identifier
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

      private def parse_control(kind : NodeKind) : NodeId
        start = advance
        children = [] of NodeId
        if expression_follows?
          children << parse_expression
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

        params = @arena.add_node(NodeKind::Args, Span.new(name_span.finish, name_span.finish))
        if current.kind == TokenKind::LParen
          params = parse_params
        end
        return_type = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if match(TokenKind::Colon)
          return_type = parse_type
        end
        if pseudo_method_name?(name_node)
          @diagnostics << Diagnostic.new(name_span, "pseudo-method cannot be redefined")
        end
        if current.kind == TokenKind::Identifier && token_text(current) == "forall"
          advance
          loop do
            if current.kind == TokenKind::Identifier
              advance
            else
              @diagnostics << Diagnostic.new(current.span, "expected type variable after forall")
              advance unless current.eof?
            end
            break unless match(TokenKind::Comma)
          end
        end
        @macro_def_depth += 1 if kind == NodeKind::MacroDef
        body = parse_expressions([TokenKind::KeywordRescue, TokenKind::KeywordEnsure, end_kind])

        rescue_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        ensure_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        handlers = false

        if current.kind == TokenKind::KeywordRescue
          handlers = true
          advance
          rescue_body = parse_expressions([TokenKind::KeywordEnsure, end_kind])
          while current.kind == TokenKind::KeywordRescue
            advance
            _ = parse_expression if expression_follows?
            parse_expressions([TokenKind::KeywordEnsure, end_kind])
          end
          rescue_span = span_from_nodes(rescue_body, rescue_body)
          rescue_node = @arena.add_node(NodeKind::Rescue, rescue_span, [rescue_body])
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
                        elsif @arena.node(rescue_node).kind != NodeKind::Nop
                          rescue_node
                        else
                          body
                        end
                 span = Span.new(node_span(body).start, node_span(tail).finish)
                 @arena.add_node(NodeKind::Begin, span, [body, rescue_node, ensure_node])
               else
                 body
               end

        @macro_def_depth -= 1 if kind == NodeKind::MacroDef
        end_token = expect(end_kind, end_message)
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(kind, span, [name_node, params, return_type, body])
      end

      private def parse_abstract_def : NodeId
        start = advance
        name_node, name_span = parse_def_name

        params = @arena.add_node(NodeKind::Args, Span.new(name_span.finish, name_span.finish))
        if current.kind == TokenKind::LParen
          params = parse_params
        end
        return_type = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if match(TokenKind::Colon)
          return_type = parse_type
        end
        if pseudo_method_name?(name_node)
          @diagnostics << Diagnostic.new(name_span, "pseudo-method cannot be redefined")
        end
        if current.kind == TokenKind::Identifier && token_text(current) == "forall"
          advance
          loop do
            if current.kind == TokenKind::Identifier
              advance
            else
              @diagnostics << Diagnostic.new(current.span, "expected type variable after forall")
              advance unless current.eof?
            end
            break unless match(TokenKind::Comma)
          end
        end
        body = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        end_span = if @arena.node(return_type).kind == NodeKind::Nop
                     node_span(params).finish
                   else
                     node_span(return_type).finish
                   end
        span = Span.new(start.span.start, end_span)
        @arena.add_node(NodeKind::Def, span, [name_node, params, return_type, body])
      end

      private def parse_fun : NodeId
        start = advance
        name_node, name_span = parse_def_name
        params = if current.kind == TokenKind::LParen
                   parse_params
                 else
                   @arena.add_node(NodeKind::Args, Span.new(name_span.finish, name_span.finish))
                 end
        return_type = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if match(TokenKind::Colon)
          return_type = parse_type
        end
        external = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if match(TokenKind::Assign)
          if current.kind == TokenKind::String
            str_tok = advance
            sym = @arena.symbols.intern(token_text(str_tok))
            external = @arena.add_ident(str_tok.span, sym)
          else
            external = parse_identifier_or_error
          end
          if match(TokenKind::Colon)
            return_type = parse_type
          end
        end
        body = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        end_span = node_span(return_type)
        end_span = node_span(external) if @arena.node(external).kind != NodeKind::Nop
        end_span = node_span(params) if @arena.node(return_type).kind == NodeKind::Nop && @arena.node(external).kind == NodeKind::Nop
        if @lib_depth == 0 && current.kind == TokenKind::KeywordEnd
          body = parse_expressions([TokenKind::KeywordEnd])
          end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close fun")
          end_span = end_token.span
        end
        span = Span.new(start.span.start, end_span.finish)
        @arena.add_node(NodeKind::Fun, span, [name_node, params, return_type, external, body])
      end

      private def parse_alias : NodeId
        start = advance
        name = parse_identifier_or_error
        expect(TokenKind::Assign, "expected '=' in alias")
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
        superclass = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        if match(TokenKind::Less)
          superclass = parse_type
        end
        @lib_depth += 1 if kind == NodeKind::Lib
        body = parse_expressions([TokenKind::KeywordEnd])
        @lib_depth -= 1 if kind == NodeKind::Lib
        end_token = expect(TokenKind::KeywordEnd, end_message)
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(kind, span, [name_node, superclass, body])
      end

      private def parse_path : NodeId
        left = parse_identifier_or_error
        while current.kind == TokenKind::DoubleColon
          op = advance
          right = parse_identifier_or_error
          span = span_from_nodes(left, right)
          left = @arena.add_node(NodeKind::Path, span, [left, right])
        end
        left
      end

      private def parse_identifier_or_error : NodeId
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
        if def_receiver_start?(current.kind) && (peek1.kind == TokenKind::Dot || peek1.kind == TokenKind::DoubleColon)
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
          if op_name = operator_method_name(token.kind)
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
        when TokenKind::Plus           then "+"
        when TokenKind::Minus          then "-"
        when TokenKind::Star           then "*"
        when TokenKind::Slash          then "/"
        when TokenKind::SlashSlash     then "//"
        when TokenKind::Percent        then "%"
        when TokenKind::Caret          then "^"
        when TokenKind::Ampersand      then "&"
        when TokenKind::Pipe           then "|"
        when TokenKind::Bang           then "!"
        when TokenKind::Tilde          then "~"
        when TokenKind::Backtick       then "`"
        when TokenKind::StarStar       then "**"
        when TokenKind::EqualEqual     then "=="
        when TokenKind::BangEqual      then "!="
        when TokenKind::Less           then "<"
        when TokenKind::LessEqual      then "<="
        when TokenKind::Greater        then ">"
        when TokenKind::GreaterEqual   then ">="
        when TokenKind::Spaceship      then "<=>"
        when TokenKind::TripleEqual    then "==="
        when TokenKind::Match          then "=~"
        when TokenKind::NotMatch       then "!~"
        when TokenKind::ShiftLeft      then "<<"
        when TokenKind::ShiftRight     then ">>"
        when TokenKind::AmpersandPlus  then "&+"
        when TokenKind::AmpersandMinus then "&-"
        when TokenKind::AmpersandStar  then "&*"
        when TokenKind::AmpersandStarStar then "&**"
        else
          nil
        end
      end

      private def parse_expression(min_bp : Int32 = 0, stop : Proc(Bool)? = nil) : NodeId
        left = parse_prefix(stop)
        left = parse_postfix(left)
        loop do
          break if stop && stop.call
          token = current
          break if macro_control_start? || macro_expr_start?
          if command_callee?(left) && command_call_start?(token.kind)
            args = parse_command_args
            span = Span.new(node_span(left).start, node_span(args).finish)
            left = @arena.add_node(NodeKind::Call, span, [left, args])
            left = parse_postfix(left)
            next
          end
          if token.kind == TokenKind::Dot && peek1.kind == TokenKind::LBracket && peek2.kind == TokenKind::RBracket
            dot = advance
            lb = advance
            rb = expect(TokenKind::RBracket, "expected ']' in operator call")
            name_span = Span.new(lb.span.start, rb.span.finish)
            name = "[]"
            if current.kind == TokenKind::Assign && current.span.start == name_span.finish
              assign = advance
              name = "[]="
              name_span = Span.new(name_span.start, assign.span.finish)
            end
            sym = @arena.symbols.intern(name)
            rhs = @arena.add_ident(name_span, sym)
            span = Span.new(node_span(left).start, name_span.finish)
            left = @arena.add_binary(dot.kind, span, left, rhs)
            left = parse_postfix(left)
            next
          end
          if stop.nil? && token.kind == TokenKind::Comma
            advance
            right = parse_expression(0, stop)
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
            left = parse_postfix(left)
            next
          end
          if token.kind == TokenKind::Question
            advance
            then_expr = parse_expression(0, stop)
            expect(TokenKind::Colon, "expected ':' in ternary")
            else_expr = parse_expression(min_bp, stop)
            span = Span.new(node_span(left).start, node_span(else_expr).finish)
            left = @arena.add_node(NodeKind::Ternary, span, [left, then_expr, else_expr])
            left = parse_postfix(left)
            next
          end
          if token.kind == TokenKind::KeywordRescue
            advance
            right = parse_expression(0, stop)
            span = Span.new(node_span(left).start, node_span(right).finish)
            left = @arena.add_node(NodeKind::Rescue, span, [left, right])
            left = parse_postfix(left)
            next
          elsif token.kind == TokenKind::KeywordEnsure
            advance
            right = parse_expression(0, stop)
            span = Span.new(node_span(left).start, node_span(right).finish)
            left = @arena.add_node(NodeKind::Ensure, span, [left, right])
            left = parse_postfix(left)
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
            left = build_infix(op.kind, span, left, right)
            left = parse_postfix(left)
            next
          end
          if (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav) &&
             current.kind == TokenKind::Bang
            bang = advance
            sym = @arena.symbols.intern("!")
            right = @arena.add_ident(bang.span, sym)
            span = Span.new(node_span(left).start, bang.span.finish)
            left = @arena.add_binary(op.kind, span, left, right)
            left = parse_postfix(left)
            next
          end
          right = if (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav || op.kind == TokenKind::DoubleColon) && operator_name_token?(current.kind)
                    tok = advance
                    sym = @arena.symbols.intern(token_text(tok))
                    @arena.add_ident(tok.span, sym)
                  elsif op.kind == TokenKind::DotDot || op.kind == TokenKind::DotDotDot
                    if expression_stop?
                      @arena.add_node(NodeKind::Nop, Span.new(op.span.finish, op.span.finish))
                    else
                      parse_expression(rbp, stop)
                    end
                  else
                    parse_expression(rbp, stop)
                  end
          span = Span.new(node_span(left).start, node_span(right).finish)
          if (op.kind == TokenKind::Dot || op.kind == TokenKind::SafeNav) && responds_to_without_args?(right) && expression_stop?
            @diagnostics << Diagnostic.new(node_span(right), "responds_to? requires an argument")
          end
          left = build_infix(op.kind, span, left, right)
          left = parse_postfix(left)
        end
        left
      end

      private def parse_prefix(stop : Proc(Bool)? = nil) : NodeId
        token = current
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
        case token.kind
        when TokenKind::Identifier
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
          symbol_id = @arena.symbols.intern(token_text(token))
          @arena.add_node(NodeKind::Global, token.span, payload_index: symbol_id)
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
          if current.kind == TokenKind::LBrace
            return parse_lambda_block(op)
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
          expr = parse_expression(prefix_binding_power(TokenKind::Dot), stop)
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
          if implicit_dot_target?(peek1.kind)
            dot = advance
            ident = advance
            name = "." + token_text(ident)
            span = Span.new(dot.span.start, ident.span.finish)
            sym = @arena.symbols.intern(name)
            @arena.add_ident(span, sym)
          else
            @diagnostics << Diagnostic.new(token.span, "unexpected token in expression")
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
            right = parse_expression(prefix_binding_power(op.kind), stop)
            span = Span.new(op.span.start, node_span(right).finish)
          end
          flags = op.kind == TokenKind::DotDotDot ? 1_u16 : 0_u16
          @arena.add_node(NodeKind::Range, span, [nil_node, right], flags: flags)
        when TokenKind::Ampersand
          op = advance
          expr = parse_expression(prefix_binding_power(op.kind), stop)
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_unary(op.kind, span, expr)
        when TokenKind::KeywordInclude, TokenKind::KeywordExtend
          parse_include_extend(token)
        when TokenKind::KeywordAlignof, TokenKind::KeywordInstanceAlignof, TokenKind::KeywordInstanceSizeof,
             TokenKind::KeywordOffsetof, TokenKind::KeywordPointerof, TokenKind::KeywordSizeof,
             TokenKind::KeywordTypeof, TokenKind::KeywordUninitialized, TokenKind::KeywordSelect, TokenKind::KeywordWith
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
          expr = parse_expression(prefix_binding_power(TokenKind::Star), stop)
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_node(NodeKind::Splat, span, [expr])
        when TokenKind::StarStar
          op = advance
          expr = parse_expression(prefix_binding_power(TokenKind::Star), stop)
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_node(NodeKind::DoubleSplat, span, [expr])
        when TokenKind::KeywordReturn
          parse_control(NodeKind::Return)
        when TokenKind::KeywordBreak
          parse_control(NodeKind::Break)
        when TokenKind::KeywordNext
          parse_control(NodeKind::Next)
        when TokenKind::KeywordYield
          parse_control(NodeKind::Yield)
        when TokenKind::DoubleColon
          advance
          parse_path
        when TokenKind::LParen
          advance
          exprs = parse_expressions([TokenKind::RParen], stop, stop)
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
          expr = parse_expression(prefix_binding_power(op.kind), stop)
          span = Span.new(op.span.start, node_span(expr).finish)
          @arena.add_unary(op.kind, span, expr)
        else
          @diagnostics << Diagnostic.new(token.span, "unexpected token in expression")
          advance unless token.eof?
          @arena.add_node(NodeKind::Error, token.span)
        end
      end

      private def parse_postfix(left : NodeId) : NodeId
        loop do
          case current.kind
          when TokenKind::LParen
            args = cast_call_single_arg?(left) ? parse_cast_args : parse_args
            span = Span.new(node_span(left).start, node_span(args).finish)
            left = @arena.add_node(NodeKind::Call, span, [left, args])
          when TokenKind::LBracket
            start = advance
            indices = [] of NodeId
            if current.kind != TokenKind::RBracket
              loop do
                indices << parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket })
                break unless match(TokenKind::Comma)
                break if current.kind == TokenKind::RBracket
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
            if macro_control_start? || macro_expr_start?
              break
            end
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
        block_params = parse_block_params
        body = parse_expressions([TokenKind::KeywordRescue, TokenKind::KeywordEnsure, TokenKind::KeywordEnd])

        rescue_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))
        ensure_node = @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start))

        if current.kind == TokenKind::KeywordRescue
          advance
          rescue_body = parse_expressions([TokenKind::KeywordEnsure, TokenKind::KeywordEnd])
          rescue_span = span_from_nodes(rescue_body, rescue_body)
          rescue_node = @arena.add_node(NodeKind::Rescue, rescue_span, [rescue_body])
        end

        if current.kind == TokenKind::KeywordEnsure
          advance
          ensure_body = parse_expressions([TokenKind::KeywordEnd])
          ensure_span = span_from_nodes(ensure_body, ensure_body)
          ensure_node = @arena.add_node(NodeKind::Ensure, ensure_span, [ensure_body])
        end

        block_body = if @arena.node(rescue_node).kind == NodeKind::Nop && @arena.node(ensure_node).kind == NodeKind::Nop
                       body
                     else
                       span = span_from_nodes(body, ensure_node)
                       @arena.add_node(NodeKind::Begin, span, [body, rescue_node, ensure_node])
                     end

        end_token = expect(TokenKind::KeywordEnd, "expected 'end' to close block")
        span = Span.new(node_span(call).start, end_token.span.finish)
        call_node = @arena.node(call)
        if call_node.kind == NodeKind::Binary && (op = @arena.operator_kind(call_node.payload_index)) && (op == TokenKind::Dot || op == TokenKind::DoubleColon)
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
      end

      private def parse_block_params : NodeId
        return @arena.add_node(NodeKind::Args, Span.new(current.span.start, current.span.start)) unless current.kind == TokenKind::Pipe
        start = advance
        params = [] of NodeId
        until current.kind == TokenKind::Pipe || current.eof?
          if current.kind == TokenKind::Identifier
            ident_token = advance
            sym = @arena.symbols.intern(token_text(ident_token))
            params << @arena.add_ident(ident_token.span, sym)
          elsif current.kind == TokenKind::LParen
            params << parse_block_destructure
          elsif current.kind == TokenKind::Star
            star = advance
            name_node = if current.kind == TokenKind::Identifier
                          ident = advance
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
        end_pipe = expect(TokenKind::Pipe, "expected '|' to close block parameters")
        span = Span.new(start.span.start, end_pipe.span.finish)
        @arena.add_node(NodeKind::Args, span, params)
      end

      private def parse_block_destructure : NodeId
        lparen = advance
        last = lparen
        depth = 1
        until depth == 0 || current.eof?
          tok = advance
          last = tok
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
        return @arena.add_node(NodeKind::Args, Span.new(start_pos, start_pos)) if expression_stop?
        args = [] of NodeId
        loop do
          if (current.kind == TokenKind::Identifier || current.kind == TokenKind::String || current.kind == TokenKind::KeywordEnd) && peek1.kind == TokenKind::Colon
            name = advance
            advance
            value = parse_expression(0, -> { current.kind == TokenKind::Comma || expression_stop? })
            name_text = token_text(name)
            name_text = name_text[1...-1] if name.kind == TokenKind::String && name_text.size >= 2
            symbol_id = @arena.symbols.intern(name_text)
            span = Span.new(name.span.start, node_span(value).finish)
            args << @arena.add_named_arg(symbol_id, span, value)
          elsif var_decl_start?(current.kind) && peek1.kind == TokenKind::Colon
            args << parse_var_decl
          else
            args << parse_expression(0, -> { current.kind == TokenKind::Comma || expression_stop? })
          end
          break unless match(TokenKind::Comma)
          break if expression_stop?
        end
        span = span_from_nodes(args.first, args.last)
        @arena.add_node(NodeKind::Args, span, args)
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
        if match(TokenKind::LParen)
          args_children << parse_expression
          expect(TokenKind::RParen, "expected ')' after #{name}")
        else
          args_children << parse_expression(0, -> { expression_stop? })
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
        if match(TokenKind::LParen)
          depth = 1
          while !current.eof? && depth > 0
            tok = advance
            depth += 1 if tok.kind == TokenKind::LParen
            depth -= 1 if tok.kind == TokenKind::RParen
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
        span = Span.new(token.span.start, args_span.finish)
        @arena.add_node(NodeKind::Call, span, [callee, args])
      end

      private def parse_args : NodeId
        start = advance
        children = [] of NodeId
        if current.kind != TokenKind::RParen
          loop do
            children << parse_argument
            if current.kind == TokenKind::Comma
              comma = advance
              if current.kind == TokenKind::RParen
                @diagnostics << Diagnostic.new(comma.span, "invalid trailing comma in call")
                break
              end
              next
            else
              break
            end
          end
        end
        end_token = expect(TokenKind::RParen, "expected ')' to close arguments")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Args, span, children)
      end

      private def parse_array : NodeId
        start = advance
        children = [] of NodeId
        if current.kind != TokenKind::RBracket
          loop do
            children << parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket })
            break unless match(TokenKind::Comma)
            break if current.kind == TokenKind::RBracket
          end
        end
        end_token = expect(TokenKind::RBracket, "expected ']' to close array")
        flags = 0_u16
        span_end = end_token.span.finish
        if current.kind == TokenKind::KeywordOf
          advance
          type_node = parse_type
          children << type_node
          flags = 1_u16
          span_end = node_span(type_node).finish
        end
        span = Span.new(start.span.start, span_end)
        @arena.add_node(NodeKind::Array, span, children, flags: flags)
      end

      private def parse_brace_literal : NodeId
        start = advance
        entries = [] of NodeId
        mode = :unknown
        if current.kind != TokenKind::RBrace
          loop do
            entry, entry_mode = parse_brace_entry
            entries << entry
            if mode == :unknown
              mode = entry_mode
            elsif mode != entry_mode && entry_mode != :unknown
              @diagnostics << Diagnostic.new(node_span(entry), "mixed tuple/hash/named tuple entries")
            end
            break unless match(TokenKind::Comma)
            break if current.kind == TokenKind::RBrace
          end
        end
        end_token = expect(TokenKind::RBrace, "expected '}' to close literal")
        span = Span.new(start.span.start, end_token.span.finish)
        if entries.empty? && current.kind == TokenKind::KeywordOf
          advance
          key_type = parse_type
          expect(TokenKind::HashRocket, "expected '=>' in typed hash literal")
          value_type = parse_type
          typed_span = Span.new(start.span.start, node_span(value_type).finish)
          return @arena.add_node(NodeKind::Hash, typed_span, [key_type, value_type], flags: 1_u16)
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

        if (current.kind == TokenKind::Identifier || current.kind == TokenKind::String) && peek1.kind == TokenKind::Colon
          name = advance
          advance
          value = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RBrace })
          name_text = token_text(name)
          name_text = name_text[1...-1] if name.kind == TokenKind::String && name_text.size >= 2
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

        {key, :tuple}
      end

      private def parse_argument : NodeId
        if ((current.kind == TokenKind::Identifier || current.kind == TokenKind::String || current.kind == TokenKind::KeywordEnd) && peek1.kind == TokenKind::Colon)
          name = advance
          advance
          value = parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen || current.kind == TokenKind::RBracket })
          name_text = token_text(name)
          name_text = name_text[1...-1] if name.kind == TokenKind::String && name_text.size >= 2
          symbol_id = @arena.symbols.intern(name_text)
          span = Span.new(name.span.start, node_span(value).finish)
          return @arena.add_named_arg(symbol_id, span, value)
        end
        parse_expression(0, -> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen || current.kind == TokenKind::RBracket })
      end

      private def parse_lambda_block(start_arrow : Token) : NodeId
        lbrace = advance
        params = if current.kind == TokenKind::Pipe
                   parse_block_params
                 else
                   @arena.add_node(NodeKind::Args, Span.new(lbrace.span.finish, lbrace.span.finish))
                 end
        body = parse_expressions([TokenKind::RBrace])
        end_token = expect(TokenKind::RBrace, "expected '}' to close lambda")
        span = Span.new(start_arrow.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Block, span, [params, body])
      end

      private def parse_params : NodeId
        start = advance
        children = [] of NodeId
        if current.kind != TokenKind::RParen
          loop do
            children << parse_param
            break unless match(TokenKind::Comma)
            break if current.kind == TokenKind::RParen
          end
        end
        end_token = expect(TokenKind::RParen, "expected ')' to close parameters")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Args, span, children)
      end

      private def parse_param : NodeId
        annotations = [] of NodeId
        while current.kind == TokenKind::Annotation
          annotations << parse_annotation
          skip_separators
        end

        token = current
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
        when TokenKind::Identifier, TokenKind::KeywordOut, TokenKind::KeywordType
          node = parse_named_param(token)
        when TokenKind::DotDotDot
          node = parse_variadic_param(token)
        else
          @diagnostics << Diagnostic.new(token.span, "expected parameter name")
          advance unless token.eof?
          node = @arena.add_node(NodeKind::Error, token.span)
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
        if token.kind == TokenKind::Identifier && {TokenKind::Identifier, TokenKind::InstanceVar, TokenKind::ClassVar, TokenKind::KeywordType}.includes?(peek1.kind)
          after_second = peek2.kind
          if {TokenKind::Colon, TokenKind::Assign, TokenKind::Comma, TokenKind::RParen}.includes?(after_second)
            external_sym = @arena.symbols.intern(token_text(token))
            external_node = @arena.add_ident(token.span, external_sym)
            advance
            name_token = current
          end
        end

        advance
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
          type_node = parse_type(-> { current.kind == TokenKind::Comma || current.kind == TokenKind::RParen })
        end
        if match(TokenKind::Assign)
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
        default_node = @arena.add_node(NodeKind::Nop, Span.new(node_span(type_node).finish, node_span(type_node).finish))
        span = Span.new(start.span.start, node_span(type_node).finish)
        @arena.add_node(NodeKind::Param, span, [name_node, type_node, default_node], payload_index: name_sym)
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
        args_nodes = [] of NodeId
        loop do
          break if current.eof?
          break if terminator?(current.kind, [TokenKind::Semicolon], nil)
          break if current.kind == TokenKind::KeywordEnd
          if var_decl_start?(current.kind) && peek1.kind == TokenKind::Colon
            args_nodes << parse_var_decl
          elsif soft_identifier_kind?(current.kind)
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
          else
            break
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
        @arena.add_node(NodeKind::Call, span, [callee, args])
      end

      private def parse_type(stop : Proc(Bool)? = nil, allow_tuple : Bool = false) : NodeId
        left = parse_type_union(stop, allow_tuple)
        if allow_tuple
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
          ret = if current.kind == TokenKind::RParen || current.kind == TokenKind::Comma || current.kind == TokenKind::RBracket || current.kind == TokenKind::RBrace || current.kind == TokenKind::Eof
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
          args_span = node_span(args_children.first)
          args_span = Span.new(args_span.start, node_span(args_children.last).finish)
          args_node = @arena.add_node(NodeKind::Args, args_span, args_children)
          span = Span.new(args_span.start, node_span(ret).finish)
          left = @arena.add_node(NodeKind::ProcType, span, [args_node, ret])
        end
        left
      end

      private def parse_brace_block(call : NodeId) : NodeId
        start = advance
        block_params = parse_block_params
        body = parse_expressions([TokenKind::RBrace])
        end_token = expect(TokenKind::RBrace, "expected '}' to close block")
        span = Span.new(node_span(call).start, end_token.span.finish)
        call_node = @arena.node(call)
        call_with_block = @arena.add_node(NodeKind::CallWithBlock, Span.new(node_span(call).start, end_token.span.finish), [call, block_params, body])
        call_with_block
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
        while current.kind == TokenKind::LBracket
          lb = advance
          size_node = parse_expression(0, -> { current.kind == TokenKind::RBracket })
          rb = expect(TokenKind::RBracket, "expected ']' in static array type")
          args_span = Span.new(lb.span.start, rb.span.finish)
          args = @arena.add_node(NodeKind::Args, args_span, [size_node])
          span = Span.new(node_span(base).start, args_span.finish)
          base = @arena.add_node(NodeKind::TypeApply, span, [base, args])
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
        token = current
        case token.kind
        when TokenKind::Star
          star = advance
          inner = parse_type_atom(stop, allow_tuple)
          span = Span.new(star.span.start, node_span(inner).finish)
          @arena.add_unary(TokenKind::Star, span, inner)
        when TokenKind::DoubleColon
          dbl = advance
          root = @arena.add_ident(Span.new(dbl.span.start, dbl.span.finish), @arena.symbols.intern("::"))
          right = parse_type_path
          span = Span.new(dbl.span.start, node_span(right).finish)
          @arena.add_node(NodeKind::Path, span, [root, right])
        when TokenKind::Identifier
          parse_type_path
        when TokenKind::KeywordSelf
          advance
          symbol_id = @arena.symbols.intern("self")
          @arena.add_ident(token.span, symbol_id)
        when TokenKind::KeywordNil
          advance
          @arena.add_node(NodeKind::LiteralNil, token.span)
        when TokenKind::LParen
          lparen = advance
          inner = parse_type(-> { current.kind == TokenKind::RParen }, allow_tuple: true)
          rparen = expect(TokenKind::RParen, "expected ')' to close type")
          Span.new(lparen.span.start, rparen.span.finish)
          inner
        when TokenKind::Arrow
          arrow = advance
          ret = if stop && stop.call
                  @arena.add_node(NodeKind::Nop, Span.new(arrow.span.finish, arrow.span.finish))
                elsif {TokenKind::Comma, TokenKind::RParen, TokenKind::RBracket, TokenKind::RBrace, TokenKind::Eof}.includes?(current.kind)
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
              if current.kind == TokenKind::Identifier && peek1.kind == TokenKind::Colon
                key = advance
                advance
                value = parse_type(stop)
                sym = @arena.symbols.intern(token_text(key))
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
        while current.kind == TokenKind::DoubleColon
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
        if token.kind != TokenKind::Identifier
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
            children << parse_type(stop, allow_tuple: allow_tuple)
            break unless match(TokenKind::Comma)
            break if current.kind == TokenKind::RParen
          end
        end
        end_token = expect(TokenKind::RParen, "expected ')' to close type arguments")
        span = Span.new(start.span.start, end_token.span.finish)
        @arena.add_node(NodeKind::Args, span, children)
      end

      private def cast_callee?(node : NodeId) : Bool
        node_info = @arena.node(node)
        case node_info.kind
        when NodeKind::Ident
          name = @arena.symbols[node_info.payload_index]
          name == "as" || name == "as?"
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

      private def build_infix(kind : TokenKind, span : Span, left : NodeId, right : NodeId) : NodeId
        case kind
        when TokenKind::Assign
          @arena.add_node(NodeKind::Assign, span, [left, right])
        when TokenKind::DoubleColon
          @arena.add_node(NodeKind::Path, span, [left, right])
        when TokenKind::DotDot, TokenKind::DotDotDot
          flags = kind == TokenKind::DotDotDot ? 1_u16 : 0_u16
          @arena.add_node(NodeKind::Range, span, [left, right], flags: flags)
        else
          @arena.add_binary(kind, span, left, right)
        end
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
          TokenKind::Spaceship, TokenKind::TripleEqual, TokenKind::HashRocket
        }.includes?(kind)
      end

      private def pseudo_method_name?(node_id : NodeId) : Bool
        node = @arena.node(node_id)
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
             TokenKind::KeywordIf,
             TokenKind::KeywordUnless,
             TokenKind::KeywordRescue,
             TokenKind::KeywordEnsure,
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

      private def command_call_start?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier, TokenKind::InstanceVar, TokenKind::ClassVar, TokenKind::GlobalVar,
             TokenKind::Number, TokenKind::String, TokenKind::Char, TokenKind::Regex, TokenKind::Symbol,
             TokenKind::KeywordTrue, TokenKind::KeywordFalse, TokenKind::KeywordNil, TokenKind::KeywordSelf,
             TokenKind::LParen, TokenKind::LBracket, TokenKind::LBrace, TokenKind::SafeNav, TokenKind::Arrow
          true
        else
          false
        end
      end

      private def command_callee?(node_id : NodeId) : Bool
        case @arena.node(node_id).kind
        when NodeKind::Ident, NodeKind::InstanceVar, NodeKind::ClassVar, NodeKind::Global,
             NodeKind::Path, NodeKind::Call, NodeKind::CallWithBlock, NodeKind::MacroVar
          true
        else
          false
        end
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
          peek1.kind == TokenKind::Percent &&
          escaped_macro_start?(current)
      end

      private def parse_escaped_macro_literal : NodeId
        start = advance # '{'
        advance if current.kind == TokenKind::Percent
        while !current.eof?
          if current.kind == TokenKind::Percent && peek1.kind == TokenKind::RBrace
            advance
            advance
            break
          end
          advance
        end
        span = Span.new(start.span.start, current.span.finish)
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
        start = advance
        advance if current.kind == TokenKind::LBrace
        @macro_depth += 1
        body = parse_expressions([TokenKind::Eof], -> { macro_expr_end? })
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
        tag_kind, header, tag_span = parse_macro_tag
        case tag_kind
        when TokenKind::KeywordIf, TokenKind::KeywordUnless
          then_body = parse_macro_body([TokenKind::KeywordElse, TokenKind::KeywordElsif, TokenKind::KeywordEnd])
          else_body = parse_macro_if_tail
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
        while !current.eof? && !macro_control_boundary?(end_kinds)
          children << parse_statement([TokenKind::Eof])
        end
        span = if children.empty?
                 Span.new(current.span.start, current.span.start)
               else
                 span_from_nodes(children.first, children.last)
               end
        @macro_depth -= 1
        @arena.add_node(NodeKind::Expressions, span, children)
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
        if single_block && {TokenKind::KeywordIf, TokenKind::KeywordUnless, TokenKind::KeywordFor}.includes?(tag_kind)
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

      private def parse_macro_verbatim_body : Span
        loop do
          if macro_control_start? && peek2.kind == TokenKind::KeywordEnd
            _, _, span = parse_macro_tag
            return span
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
        stop_at_in = -> { current.kind == TokenKind::KeywordIn || macro_control_end? }

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

      private def macro_control_boundary?(kinds : Array(TokenKind)) : Bool
        return true if kinds.includes?(current.kind) && {TokenKind::KeywordElse, TokenKind::KeywordElsif, TokenKind::KeywordEnd}.includes?(current.kind)
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

      private def parse_macro_if_tail : NodeId
        return @arena.add_node(NodeKind::Nop, Span.new(current.span.start, current.span.start)) unless macro_control_start?

        if macro_control_boundary?([TokenKind::KeywordElse])
          parse_macro_tag
          body = parse_macro_body([TokenKind::KeywordEnd])
          return body
        end

        if macro_control_boundary?([TokenKind::KeywordElsif])
          tag_kind, header, tag_span = parse_macro_tag
          then_body = parse_macro_body([TokenKind::KeywordElse, TokenKind::KeywordElsif, TokenKind::KeywordEnd])
          else_body = parse_macro_if_tail
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
