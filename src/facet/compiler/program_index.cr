module Facet
  module Compiler
    struct DeclRef
      getter source : Source
      getter node_id : NodeId

      def initialize(@source : Source, @node_id : NodeId)
      end
    end

    class ProgramIndex
      getter macros : Hash(SymbolId, Array(DeclRef))

      def initialize
        @macros = Hash(SymbolId, Array(DeclRef)).new { |h, k| h[k] = [] of DeclRef }
      end

      def add_macro(symbol_id : SymbolId, ref : DeclRef)
        @macros[symbol_id] << ref
      end

      def macros_for(symbol_id : SymbolId) : Array(DeclRef)
        @macros[symbol_id]?
      end
    end

    module Indexer
      extend self

      def index_macros(ast : AstFile, index : ProgramIndex = ProgramIndex.new) : ProgramIndex
        walk(ast.root, ast, index)
        index
      end

      private def walk(node_id : NodeId, ast : AstFile, index : ProgramIndex)
        node = ast.node(node_id)
        if node.kind == NodeKind::MacroDef
          name_id = ast.children(node_id)[0]
          name_node = ast.node(name_id)
          symbol_id = name_node.payload_index
          index.add_macro(symbol_id, DeclRef.new(ast.source, node_id))
        end

        ast.children(node_id).each do |child|
          walk(child, ast, index)
        end
      end
    end
  end
end
