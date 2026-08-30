module Facet
  module Compiler
    module AstIntegrity
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

      def self.missing_semantic_tokens(ast : AstFile, tokens : Array(Token)) : Array(Token)
        tokens.reject(&.eof?).select do |token|
          SIGNIFICANT_TOKEN_KINDS.includes?(token.kind) && !semantic_token_owned?(ast, token)
        end
      end

      def self.semantic_token_owned?(ast : AstFile, token : Token) : Bool
        text = String.new(ast.source.bytes[token.span.start, token.span.length])
        ast.arena.nodes.each_with_index.any? do |node, node_id|
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
            node.span == token.span
          when NodeKind::MacroLiteral
            true
          when NodeKind::NamedArg,
               NodeKind::Splat,
               NodeKind::DoubleSplat,
               NodeKind::BlockParam,
               NodeKind::MacroVar
            node.payload_index >= 0 && payload_name_owned?(node, ast.arena.symbols[node.payload_index], token, text)
          when NodeKind::Annotation
            node.span.start == token.span.start
          when NodeKind::Args
            text == "forall" && node.span.start == token.span.start
          when NodeKind::Binary
            nilable_type_token_owned?(ast, node_id.to_i32, node, token, text)
          else
            false
          end
        end
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
        left_node = ast.node(left)
        left_node.span.start == token.span.start && left_node.span.finish == token.span.finish - 1
      end
    end
  end
end
