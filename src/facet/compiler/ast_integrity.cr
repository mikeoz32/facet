require "set"

module Facet
  module Compiler
    module AstIntegrity
      SYMBOL_PAYLOAD_KINDS = {
        NodeKind::Ident,
        NodeKind::InstanceVar,
        NodeKind::ClassVar,
        NodeKind::Global,
        NodeKind::NamedArg,
        NodeKind::Param,
        NodeKind::Splat,
        NodeKind::DoubleSplat,
        NodeKind::BlockParam,
        NodeKind::MacroVar,
      }

      LITERAL_PAYLOAD_KINDS = {
        NodeKind::LiteralNumber,
        NodeKind::LiteralString,
        NodeKind::LiteralChar,
        NodeKind::LiteralRegex,
        NodeKind::LiteralSymbol,
      }

      SIGNIFICANT_TOKEN_KINDS = {
        TokenKind::Identifier,
        TokenKind::InstanceVar,
        TokenKind::ClassVar,
        TokenKind::GlobalVar,
        TokenKind::Annotation,
        TokenKind::Symbol,
        TokenKind::Number,
        TokenKind::String,
        TokenKind::Regex,
        TokenKind::Char,
        TokenKind::KeywordNil,
        TokenKind::KeywordTrue,
        TokenKind::KeywordFalse,
        TokenKind::KeywordSelf,
        TokenKind::KeywordSuper,
      }

      def self.contract_violations(ast : AstFile) : Array(String)
        violations = [] of String
        node_count = ast.arena.nodes.size
        unless valid_node_id?(ast.root, node_count)
          violations << "root node id #{ast.root} is outside arena size #{node_count}"
          return violations
        end
        root = ast.node(ast.root)
        violations << "root node #{ast.root} is #{root.kind}; expected File" unless root.kind == NodeKind::File
        unless root.span.start == 0 && root.span.finish == ast.source.size
          violations << "root span #{root.span} does not cover source size #{ast.source.size}"
        end

        active = Set(NodeId).new
        done = Set(NodeId).new
        stack = [{ast.root, nil.as(NodeId?), false}]
        until stack.empty?
          node_id, parent_id, exiting = stack.pop
          if exiting
            active.delete(node_id)
            done << node_id
            next
          end
          unless valid_node_id?(node_id, node_count)
            violations << "child node id #{node_id} is outside arena size #{node_count}"
            next
          end
          if active.includes?(node_id)
            violations << "node #{node_id} forms a cycle in the reachable AST"
            next
          end

          node = ast.node(node_id)
          validate_span(ast, node_id, node, parent_id, violations)
          next if done.includes?(node_id)

          active << node_id
          stack << {node_id, parent_id, true}
          unless valid_edge_slice?(ast, node)
            violations << "#{node.kind}##{node_id} has invalid child edge slice #{node.first_child}+#{node.child_count}"
            next
          end

          children = ast.children(node_id)
          violations << "Error##{node_id} is reachable from the AST root" if node.kind == NodeKind::Error
          validate_arity(node_id, node, children.size, violations)
          validate_payload(ast, node_id, node, violations)
          validate_flags(node_id, node, violations)
          validate_child_roles(ast, node_id, node, children, violations)
          children.reverse_each { |child| stack << {child, node_id, false} }
        end

        violations
      end

      def self.missing_semantic_tokens(ast : AstFile, tokens : Array(Token)) : Array(Token)
        reachable = reachable_node_ids(ast)
        tokens.reject(&.eof?).select do |token|
          SIGNIFICANT_TOKEN_KINDS.includes?(token.kind) && !semantic_token_owned?(ast, token, reachable)
        end
      end

      def self.semantic_token_owned?(ast : AstFile, token : Token) : Bool
        semantic_token_owned?(ast, token, reachable_node_ids(ast))
      end

      def self.reachable_node_ids(ast : AstFile) : Array(NodeId)
        reachable = [] of NodeId
        seen = Set(NodeId).new
        stack = [ast.root]
        until stack.empty?
          node_id = stack.pop
          next if seen.includes?(node_id)
          seen << node_id
          reachable << node_id
          ast.children(node_id).each { |child| stack << child }
        end
        reachable
      end

      private def self.valid_node_id?(node_id : NodeId, node_count : Int32) : Bool
        node_id >= 0 && node_id < node_count
      end

      private def self.valid_edge_slice?(ast : AstFile, node : Node) : Bool
        node.first_child >= 0 && node.child_count >= 0 &&
          node.first_child.to_i64 + node.child_count.to_i64 <= ast.arena.edges.size
      end

      private def self.validate_span(
        ast : AstFile,
        node_id : NodeId,
        node : Node,
        parent_id : NodeId?,
        violations : Array(String),
      ) : Nil
        source_size = ast.source.size
        if node.span.start < 0 || node.span.finish < node.span.start || node.span.finish > source_size
          violations << "#{node.kind}##{node_id} has invalid span #{node.span} for source size #{source_size}"
          return
        end
        return unless parent_id
        return if node.kind == NodeKind::Nop && node.span.length == 0

        parent = ast.node(parent_id)
        unless parent.span.start <= node.span.start && parent.span.finish >= node.span.finish
          violations << "#{node.kind}##{node_id} span #{node.span} escapes #{parent.kind}##{parent_id} span #{parent.span}"
        end
      end

      private def self.validate_arity(node_id : NodeId, node : Node, size : Int32, violations : Array(String)) : Nil
        expected = case node.kind
                   when NodeKind::File, NodeKind::NamedArg, NodeKind::Unary, NodeKind::MacroExpr, NodeKind::Require
                     "1" unless size == 1
                   when NodeKind::AnnotationDef, NodeKind::Call, NodeKind::Assign, NodeKind::Binary,
                        NodeKind::While, NodeKind::Until, NodeKind::When, NodeKind::Alias, NodeKind::TypeDef,
                        NodeKind::Range, NodeKind::Path, NodeKind::TypeApply, NodeKind::ProcType,
                        NodeKind::MacroForHeader, NodeKind::AsmOperand
                     "2" unless size == 2
                   when NodeKind::If, NodeKind::Unless, NodeKind::Case, NodeKind::For, NodeKind::Block,
                        NodeKind::CallWithBlock, NodeKind::Ternary, NodeKind::Class, NodeKind::Module,
                        NodeKind::Struct, NodeKind::Enum, NodeKind::Lib
                     "3" unless size == 3
                   when NodeKind::Begin
                     "4" unless size == 4
                   when NodeKind::Asm
                     "4" unless size == 4
                   when NodeKind::Def, NodeKind::MacroDef, NodeKind::Fun
                     "5" unless size == 5
                   when NodeKind::VarDecl
                     "3 or 4" unless size == 3 || size == 4
                   when NodeKind::Annotation
                     "2" unless size == 2
                   when NodeKind::Param
                     "0, 3, or 4" unless size == 0 || size == 3 || size == 4
                   when NodeKind::Splat, NodeKind::DoubleSplat
                     "1" unless size == 1
                   when NodeKind::BlockParam, NodeKind::Break, NodeKind::Next, NodeKind::Return
                     "0 or 1" unless size == 0 || size == 1
                   when NodeKind::Yield
                     nil
                   when NodeKind::Rescue, NodeKind::Ensure
                     "1 or 2" unless size == 1 || size == 2
                   when NodeKind::MacroControl
                     "1, 2, or 3" unless size.in?(1, 2, 3)
                   when NodeKind::MacroVar
                     nil
                   when NodeKind::StringInterpolation
                     nil
                   when NodeKind::Ident, NodeKind::Const, NodeKind::InstanceVar, NodeKind::ClassVar, NodeKind::Global,
                        NodeKind::LiteralNumber, NodeKind::LiteralString, NodeKind::LiteralChar,
                        NodeKind::LiteralSymbol, NodeKind::LiteralBool, NodeKind::LiteralNil,
                        NodeKind::MacroLiteral, NodeKind::Nop, NodeKind::Error
                     "0" unless size == 0
                   when NodeKind::LiteralRegex
                     "0 or 1" unless size == 0 || size == 1
                   else
                     nil
                   end
        violations << "#{node.kind}##{node_id} has #{size} children; expected #{expected}" if expected
      end

      private def self.validate_payload(ast : AstFile, node_id : NodeId, node : Node, violations : Array(String)) : Nil
        if SYMBOL_PAYLOAD_KINDS.includes?(node.kind)
          optional = {NodeKind::Param, NodeKind::Splat, NodeKind::DoubleSplat, NodeKind::BlockParam}.includes?(node.kind)
          unless (optional && node.payload_index == -1) || node.payload_index.in?(0...ast.arena.symbols.entries.size)
            violations << "#{node.kind}##{node_id} has invalid symbol payload #{node.payload_index}"
          end
        elsif LITERAL_PAYLOAD_KINDS.includes?(node.kind)
          unless node.payload_index.in?(0...ast.arena.literals.size)
            violations << "#{node.kind}##{node_id} has invalid literal payload #{node.payload_index}"
          else
            expected = case node.kind
                       when NodeKind::LiteralNumber then LiteralKind::Number
                       when NodeKind::LiteralString then LiteralKind::String
                       when NodeKind::LiteralChar   then LiteralKind::Char
                       when NodeKind::LiteralRegex  then LiteralKind::Regex
                       when NodeKind::LiteralSymbol then LiteralKind::Symbol
                       end
            actual = ast.arena.literal(node.payload_index).kind
            if expected && actual != expected
              violations << "#{node.kind}##{node_id} has #{actual} literal payload; expected #{expected}"
            end
            if content_span = ast.arena.literal(node.payload_index).content_span
              if content_span.start < 0 || content_span.finish < content_span.start ||
                 content_span.finish > ast.source.size
                violations << "#{node.kind}##{node_id} has invalid literal content span #{content_span}"
              elsif content_span.start < node.span.start || content_span.finish > node.span.finish
                violations << "#{node.kind}##{node_id} literal content span #{content_span} escapes node span #{node.span}"
              end
            end
          end
        elsif {NodeKind::Unary, NodeKind::Binary}.includes?(node.kind)
          unless node.payload_index.in?(0...ast.arena.operators.size)
            violations << "#{node.kind}##{node_id} has invalid operator payload #{node.payload_index}"
          end
        elsif node.kind == NodeKind::MacroControl
          unless node.payload_index.in?(0...TokenKind.values.size)
            violations << "MacroControl##{node_id} has invalid control tag payload #{node.payload_index}"
          end
        elsif node.payload_index != -1
          violations << "#{node.kind}##{node_id} unexpectedly carries payload #{node.payload_index}"
        end
      end

      private def self.validate_flags(node_id : NodeId, node : Node, violations : Array(String)) : Nil
        storage_mask = case node.kind
                       when NodeKind::LiteralBool, NodeKind::Array, NodeKind::Hash, NodeKind::Tuple,
                            NodeKind::Range, NodeKind::Index
                         0x0001_u16
                       when NodeKind::Asm
                         0x000f_u16
                       when NodeKind::Yield
                         0x0001_u16
                       when NodeKind::CallWithBlock
                         0x0001_u16
                       else
                         0_u16
                       end
        semantic_mask = case node.kind
                        when NodeKind::Def
                          SemanticFlag::Abstract.value | SemanticFlag::Private.value | SemanticFlag::Protected.value
                        when NodeKind::MacroDef
                          SemanticFlag::Private.value | SemanticFlag::Protected.value
                        when NodeKind::Class, NodeKind::Module
                          SemanticFlag::Abstract.value | SemanticFlag::Private.value | SemanticFlag::Protected.value
                        when NodeKind::Struct
                          SemanticFlag::Abstract.value | SemanticFlag::Private.value | SemanticFlag::Protected.value | SemanticFlag::Union.value
                        when NodeKind::Enum, NodeKind::Lib, NodeKind::Fun, NodeKind::Alias, NodeKind::TypeDef,
                             NodeKind::AnnotationDef, NodeKind::Call, NodeKind::Assign, NodeKind::CallWithBlock
                          SemanticFlag::Private.value | SemanticFlag::Protected.value
                        when NodeKind::Case
                          SemanticFlag::Select.value | SemanticFlag::Exhaustive.value
                        when NodeKind::MacroLiteral
                          SemanticFlag::Escaped.value
                        when NodeKind::Rescue
                          SemanticFlag::RescueClause.value
                        else
                          0_u16
                        end
        unknown = node.flags & ~(storage_mask | semantic_mask)
        violations << "#{node.kind}##{node_id} has unsupported flags 0x#{unknown.to_s(16)}" unless unknown == 0
      end

      private def self.validate_child_roles(
        ast : AstFile,
        node_id : NodeId,
        node : Node,
        children : Slice(NodeId),
        violations : Array(String),
      ) : Nil
        return unless children.all? { |child| valid_node_id?(child, ast.arena.nodes.size) }

        case node.kind
        when NodeKind::File
          expect_child_kind(ast, node_id, children, 0, {NodeKind::Expressions}, violations)
        when NodeKind::Call
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Args}, violations)
        when NodeKind::Block
          expect_child_kind(ast, node_id, children, 0, {NodeKind::Args}, violations)
          expect_child_kind(ast, node_id, children, 2, {NodeKind::Expressions, NodeKind::Begin}, violations)
        when NodeKind::If, NodeKind::Unless
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Expressions}, violations)
          expect_child_kind(ast, node_id, children, 2, {NodeKind::Expressions, NodeKind::If, NodeKind::Unless, NodeKind::Nop}, violations)
        when NodeKind::While, NodeKind::Until
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Expressions}, violations)
        when NodeKind::Case
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Expressions}, violations)
          expect_child_kind(ast, node_id, children, 2, {NodeKind::Expressions, NodeKind::Nop}, violations)
        when NodeKind::When
          expect_child_kind(ast, node_id, children, 0, {NodeKind::Expressions}, violations)
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Expressions}, violations)
        when NodeKind::For
          expect_child_kind(ast, node_id, children, 0, {NodeKind::Args}, violations)
          expect_child_kind(ast, node_id, children, 2, {NodeKind::Expressions}, violations)
        when NodeKind::Def, NodeKind::MacroDef
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Args}, violations)
          expect_child_kind(ast, node_id, children, 3, {NodeKind::Expressions, NodeKind::Begin, NodeKind::Nop}, violations)
          expect_child_kind(ast, node_id, children, 4, {NodeKind::Args, NodeKind::Nop}, violations)
        when NodeKind::Class, NodeKind::Module, NodeKind::Struct, NodeKind::Enum, NodeKind::Lib
          expect_child_kind(ast, node_id, children, 2, {NodeKind::Expressions}, violations)
        when NodeKind::Fun
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Args}, violations)
          expect_child_kind(ast, node_id, children, 4, {NodeKind::Expressions, NodeKind::Nop}, violations)
        when NodeKind::Begin
          expect_child_kind(ast, node_id, children, 0, {NodeKind::Expressions}, violations)
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Expressions, NodeKind::Nop}, violations)
          expect_child_kind(ast, node_id, children, 2, {NodeKind::Expressions, NodeKind::Nop}, violations)
          expect_child_kind(ast, node_id, children, 3, {NodeKind::Ensure, NodeKind::Nop}, violations)
        when NodeKind::Rescue
          if node.semantic_flag?(SemanticFlag::RescueClause)
            expect_child_kind(ast, node_id, children, 1, {NodeKind::Expressions}, violations)
          elsif children.size != 2
            violations << "Rescue##{node_id} without RescueClause flag must be an inline two-child expression"
          end
        when NodeKind::Ensure
          expect_child_kind(ast, node_id, children, 0, {NodeKind::Expressions}, violations) if children.size == 1
        when NodeKind::CallWithBlock
          expect_child_kind(ast, node_id, children, 1, {NodeKind::Args}, violations)
          expect_child_kind(ast, node_id, children, 2, {NodeKind::Expressions, NodeKind::Begin}, violations)
        when NodeKind::MacroForHeader
          expect_child_kind(ast, node_id, children, 0, {NodeKind::Args}, violations)
        end
      end

      private def self.expect_child_kind(
        ast : AstFile,
        node_id : NodeId,
        children : Slice(NodeId),
        index : Int32,
        allowed,
        violations : Array(String),
      ) : Nil
        return if index >= children.size
        actual = ast.node(children[index]).kind
        unless allowed.includes?(actual)
          violations << "#{ast.node(node_id).kind}##{node_id} child #{index} is #{actual}; expected #{allowed.join(" or ")}"
        end
      end

      private def self.semantic_token_owned?(ast : AstFile, token : Token, reachable : Array(NodeId)) : Bool
        text = String.new(ast.source.bytes[token.span.start, token.span.length])
        return true if token.kind == TokenKind::String && macro_fragments_own_token?(ast, token, reachable)
        reachable.any? do |node_id|
          node = ast.node(node_id)
          next false unless node.span.start <= token.span.start && node.span.finish >= token.span.finish

          case node.kind
          when NodeKind::Ident,
               NodeKind::InstanceVar,
               NodeKind::ClassVar,
               NodeKind::Global
            node.payload_index >= 0 && semantic_name_owned?(node, ast.arena.symbols[node.payload_index], token, text)
          when NodeKind::LiteralNumber,
               NodeKind::LiteralString,
               NodeKind::LiteralChar,
               NodeKind::LiteralRegex,
               NodeKind::LiteralSymbol,
               NodeKind::LiteralBool,
               NodeKind::LiteralNil
            literal_token_owned?(ast, node, token, text)
          when NodeKind::StringInterpolation
            if token.kind == TokenKind::String && node.span.start <= token.span.start && node.span.finish >= token.span.finish
              node.span.start == token.span.start ||
                ast.source.text.byte_slice(node.span.start, token.span.start - node.span.start).includes?("\\\n") ||
                ast.source.text.byte_slice(node.span.start, token.span.start - node.span.start).includes?("\\\r\n")
            else
              false
            end
          when NodeKind::Array
            token.kind == TokenKind::String && node.span.start == token.span.start &&
              (node.flags & 0x0001_u16) != 0
          when NodeKind::Index
            global_match_token_owned?(ast, node_id, node, token, text)
          when NodeKind::Asm
            asm_option_token_owned?(node, token, text)
          when NodeKind::MacroLiteral
            true
          when NodeKind::MacroControl
            token.kind == TokenKind::Identifier && text == "verbatim" &&
              node.payload_index == TokenKind::KeywordVerbatim.to_i32
          when NodeKind::NamedArg
            if token.kind == TokenKind::Symbol && token.span.start > node.span.start &&
               ast.source.bytes[token.span.start] == ':'.ord.to_u8
              true
            else
              node.payload_index >= 0 && payload_name_owned?(node, ast.arena.symbols[node.payload_index], token, text)
            end
          when NodeKind::Splat,
               NodeKind::DoubleSplat,
               NodeKind::BlockParam,
               NodeKind::MacroVar
            node.payload_index >= 0 && payload_name_owned?(node, ast.arena.symbols[node.payload_index], token, text)
          when NodeKind::Annotation
            node.span.start == token.span.start
          when NodeKind::Args
            text == "forall" && node.span.start == token.span.start
          when NodeKind::Binary
            nilable_type_token_owned?(ast, node_id, node, token, text)
          else
            false
          end
        end
      end

      private def self.macro_fragments_own_token?(ast : AstFile, token : Token, reachable : Array(NodeId)) : Bool
        spans = reachable.compact_map do |node_id|
          node = ast.node(node_id)
          next unless {NodeKind::MacroLiteral, NodeKind::MacroVar, NodeKind::MacroExpr}.includes?(node.kind)
          next unless node.span.finish > token.span.start && node.span.start < token.span.finish
          node.span
        end
        return false if spans.empty?
        spans.sort_by!(&.start)
        cursor = token.span.start
        spans.each do |span|
          next if span.finish <= cursor
          return false if span.start > cursor
          cursor = Math.max(cursor, span.finish)
          return true if cursor >= token.span.finish
        end
        false
      end

      private def self.literal_token_owned?(ast : AstFile, node : Node, token : Token, text : String) : Bool
        if token.kind == TokenKind::Identifier && node.span == token.span
          return true if node.kind == NodeKind::LiteralNumber && {"__LINE__", "__END_LINE__"}.includes?(text)
          return true if node.kind == NodeKind::LiteralString && {"__FILE__", "__DIR__"}.includes?(text)
        end
        expected = case node.kind
                   when NodeKind::LiteralNumber then TokenKind::Number
                   when NodeKind::LiteralString then TokenKind::String
                   when NodeKind::LiteralChar   then TokenKind::Char
                   when NodeKind::LiteralRegex  then TokenKind::Regex
                   when NodeKind::LiteralSymbol then TokenKind::Symbol
                   when NodeKind::LiteralBool   then token.kind
                   when NodeKind::LiteralNil    then token.kind
                   else                              return false
                   end
        return false unless token.kind == expected
        return false unless node.span.start <= token.span.start && node.span.finish >= token.span.finish
        if node.kind == NodeKind::LiteralString
          return true if node.span.start == token.span.start
          separator = ast.source.text.byte_slice(node.span.start, token.span.start - node.span.start)
          return separator.includes?("\\\n") || separator.includes?("\\\r\n")
        end
        return node.span.start == token.span.start if node.kind != NodeKind::LiteralNumber
        node.span.finish == token.span.finish && token.span.start - node.span.start <= 1
      end

      private def self.global_match_token_owned?(
        ast : AstFile,
        node_id : NodeId,
        node : Node,
        token : Token,
        text : String,
      ) : Bool
        return false unless token.kind == TokenKind::GlobalVar && node.span == token.span
        return false unless text.size > 1 && text.starts_with?('$')
        suffix = text.ends_with?('?') ? text[1...-1] : text[1..]
        return false if suffix.empty? || !suffix.each_char.all? { |char| char >= '0' && char <= '9' }
        children = ast.children(node_id)
        return false unless children.size == 2
        global = ast.node(children[0])
        number = ast.node(children[1])
        global.kind == NodeKind::Global && global.payload_index >= 0 &&
          ast.arena.symbols[global.payload_index] == "$~" && number.kind == NodeKind::LiteralNumber
      end

      private def self.asm_option_token_owned?(node : Node, token : Token, text : String) : Bool
        return false unless token.kind == TokenKind::String && node.span.start <= token.span.start && node.span.finish >= token.span.finish
        option = text.size >= 2 && text.starts_with?('"') && text.ends_with?('"') ? text[1...-1] : text
        {"volatile", "alignstack", "intel", "unwind"}.includes?(option)
      end

      private def self.semantic_name_owned?(node : Node, symbol : String, token : Token, text : String) : Bool
        return false unless semantic_name_matches?(symbol, text)
        prefix = symbol.ends_with?(text) && {'.', '%'}.includes?(symbol[0]) ? 1 : 0
        token.span.start == node.span.start + prefix
      end

      private def self.payload_name_owned?(node : Node, symbol : String, token : Token, text : String) : Bool
        return false unless semantic_name_matches?(symbol, text)
        prefix = case node.kind
                 when NodeKind::Splat, NodeKind::BlockParam, NodeKind::MacroVar
                   1
                 when NodeKind::DoubleSplat
                   2
                 else
                   0
                 end
        token.span.start == node.span.start + prefix
      end

      private def self.semantic_name_matches?(symbol : String, token_text : String) : Bool
        return true if symbol == token_text
        if token_text.size >= 2 && token_text.starts_with?('"') && token_text.ends_with?('"')
          return true if symbol == token_text[1...-1]
        end
        return true if symbol.ends_with?(token_text) && {'.', '%'}.includes?(symbol[0])
        symbol.starts_with?(token_text) && symbol[token_text.size..].in?({"=", "?", "!", "?=", "!="})
      end

      private def self.nilable_type_token_owned?(
        ast : AstFile,
        node_id : NodeId,
        node : Node,
        token : Token,
        text : String,
      ) : Bool
        return false unless text.ends_with?('?')
        return false unless ast.arena.operator_kind(node.payload_index) == TokenKind::Pipe
        left = ast.arena.children(node_id)[0]?
        return false unless left
        base_text = text.byte_slice(0, text.bytesize - 1)
        stack = [left]
        until stack.empty?
          candidate_id = stack.pop
          candidate = ast.node(candidate_id)
          if candidate.kind == NodeKind::Ident && candidate.payload_index >= 0 &&
             candidate.span.start == token.span.start && candidate.span.finish == token.span.finish - 1 &&
             ast.arena.symbols[candidate.payload_index] == base_text
            return true
          end
          ast.children(candidate_id).each { |child| stack << child }
        end
        false
      end
    end
  end
end
