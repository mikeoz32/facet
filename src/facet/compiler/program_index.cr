module Facet
  module Compiler
    struct DeclRef
      getter ast : AstFile
      getter node_id : NodeId

      def initialize(@ast : AstFile, @node_id : NodeId)
      end
    end

    class ProgramIndex
      getter macros : Hash(String, Array(DeclRef))
      getter fingerprint : UInt64

      def initialize
        @macros = Hash(String, Array(DeclRef)).new { |h, k| h[k] = [] of DeclRef }
        @fingerprint = 0_u64
      end

      def add_macro(name : String, ref : DeclRef)
        @macros[name] << ref
        @fingerprint = @fingerprint ^ ref.node_id.to_u64 ^ ref.ast.source.text.hash.to_u64
      end

      def macros_for(name : String) : Array(DeclRef)?
        @macros[name]?
      end

      def merge!(other : ProgramIndex)
        other.macros.each do |name, refs|
          @macros[name].concat(refs)
        end
        @fingerprint ^= other.fingerprint
        self
      end
    end

    module Indexer
      extend self

      def index_macros(ast : AstFile, index : ProgramIndex = ProgramIndex.new) : ProgramIndex
        walk(ast.root, ast, index)
        index
      end

      def index_macros(asts : Array(AstFile), index : ProgramIndex = ProgramIndex.new) : ProgramIndex
        asts.each do |ast|
          index_macros(ast, index)
        end
        index
      end

      private def walk(node_id : NodeId, ast : AstFile, index : ProgramIndex)
        node = ast.node(node_id)
        if node.kind == NodeKind::MacroDef
          name_id = ast.children(node_id)[0]
          name_node = ast.node(name_id)
          symbol_id = name_node.payload_index
          name = ast.arena.symbols[symbol_id]
          index.add_macro(name, DeclRef.new(ast, node_id))
        end

        ast.children(node_id).each do |child|
          walk(child, ast, index)
        end
      end
    end
  end
end
