module Facet
  module Compiler
    class MacroExpander
      getter diagnostics : Array(Diagnostic)

      def initialize(@max_passes : Int32 = 8)
        @diagnostics = [] of Diagnostic
      end

      def expand(ast : AstFile) : AstFile
        current_ast = ast
        passes = 0

        loop do
          macros = [] of NodeId
          collect_macros(current_ast.root, current_ast, macros)
          break if macros.empty?

          if passes >= @max_passes
            @diagnostics << Diagnostic.new(Span.new(0, current_ast.source.size), "macro expansion exceeded max passes (#{@max_passes})")
            break
          end

          expanded_text = expand_text(current_ast, macros)
          site = ExpansionSite.new(current_ast.source, Span.new(0, current_ast.source.size))
          new_source = Source.new(expanded_text, current_ast.source.filename, SourceKind::Virtual, site)
          parser = Parser.new(new_source)
          next_ast = parser.parse_file
          parser.diagnostics.each { |d| @diagnostics << d }

          current_ast = next_ast
          passes += 1
        end

        current_ast
      end

      private def expand_text(ast : AstFile, macros : Array(NodeId)) : String
        macros.sort_by! { |id| ast.node(id).span.start }

        src = ast.source
        bytes = src.bytes
        builder = String::Builder.new
        last = 0

        macros.each do |id|
          span = ast.node(id).span
          builder.write bytes[last, span.start - last]
          expansion = expand_macro(id, ast) || ""
          builder << expansion
          last = span.finish
        end

        builder.write bytes[last, src.size - last]
        builder.to_s
      end

      private def collect_macros(node_id : NodeId, ast : AstFile, acc : Array(NodeId))
        node = ast.node(node_id)
        if node.kind == NodeKind::MacroExpr || node.kind == NodeKind::MacroControl
          acc << node_id
          return
        end
        ast.children(node_id).each do |child|
          collect_macros(child, ast, acc)
        end
      end

      private def expand_macro(node_id : NodeId, ast : AstFile) : String?
        node = ast.node(node_id)
        case node.kind
        when NodeKind::MacroExpr
          expand_macro_expr(node_id, ast)
        when NodeKind::MacroControl
          expand_macro_control(node_id, ast)
        else
          nil
        end
      end

      private def expand_macro_expr(node_id : NodeId, ast : AstFile) : String?
        body_id = ast.children(node_id)[0]
        exprs = ast.children(body_id)
        return "" if exprs.empty?
        eval_to_string(exprs.first, ast)
      end

      private def expand_macro_control(node_id : NodeId, ast : AstFile) : String?
        tag = ast.macro_control_tag(node_id)
        children = ast.children(node_id)
        case tag
        when TokenKind::KeywordIf, TokenKind::KeywordUnless
          header = children[0]
          then_body = children[1]
          else_body = children.size > 2 ? children[2] : nil
          cond = eval_truthy(header, ast)
          cond = !cond if tag == TokenKind::KeywordUnless
          body_span = cond ? ast.node(then_body).span : (else_body ? ast.node(else_body).span : nil)
          body_span ? slice_text(ast.source, body_span) : ""
        when TokenKind::KeywordFor
          header = children[0]
          body = children[1]?
          return "" unless body
          count = eval_for_count(header, ast)
          text = slice_text(ast.source, ast.node(body).span)
          String.build do |io|
            count.times { io << text }
          end
        else
          @diagnostics << Diagnostic.new(ast.node(node_id).span, "unsupported macro control tag #{tag}")
          nil
        end
      end

      private def eval_truthy(node_id : NodeId, ast : AstFile) : Bool
        node = ast.node(node_id)
        case node.kind
        when NodeKind::Expressions
          children = ast.children(node_id)
          return false if children.empty?
          return eval_truthy(children.first, ast)
        when NodeKind::LiteralBool
          node.flags == 1
        when NodeKind::LiteralNil
          false
        when NodeKind::LiteralNumber
          text = ast.node_string(node_id)
          text != "0"
        else
          true
        end
      end

      private def eval_for_count(header_id : NodeId, ast : AstFile) : Int32
        node = ast.node(header_id)
        return 0 unless node.kind == NodeKind::MacroForHeader
        parts = ast.macro_for_header_parts(header_id)
        return 0 unless parts
        _, iter_id = parts
        iter_node = ast.node(iter_id)
        case iter_node.kind
        when NodeKind::Array, NodeKind::Tuple
          ast.children(iter_id).size
        else
          0
        end
      end

      private def eval_to_string(node_id : NodeId, ast : AstFile) : String?
        node = ast.node(node_id)
        case node.kind
        when NodeKind::LiteralString
          text = ast.node_string(node_id)
          return text[1, text.size - 2] if text.size >= 2 && text.starts_with?("\"") && text.ends_with?("\"")
          text
        when NodeKind::LiteralNumber, NodeKind::LiteralChar, NodeKind::LiteralRegex
          ast.node_string(node_id)
        when NodeKind::LiteralBool
          node.flags == 1 ? "true" : "false"
        when NodeKind::LiteralNil
          "nil"
        else
          @diagnostics << Diagnostic.new(node.span, "unsupported macro expression")
          nil
        end
      end

      private def slice_text(source : Source, span : Span) : String
        String.new(source.bytes[span.start, span.length])
      end
    end
  end
end
