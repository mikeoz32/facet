module Facet
  module Compiler
    struct DeclRef
      getter ast : AstFile
      getter node_id : NodeId
      getter scope : String

      def initialize(@ast : AstFile, @node_id : NodeId, @scope : String = "")
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
        @fingerprint = @fingerprint ^ ref.node_id.to_u64 ^ ref.ast.source.text.hash.to_u64 ^ ref.scope.hash.to_u64
      end

      def macros_for(name : String, scope : String? = nil) : Array(DeclRef)?
        refs = @macros[name]?
        return nil unless refs
        return refs unless scope

        lexical_scopes(scope).each do |candidate|
          matches = refs.select { |ref| ref.scope == candidate }
          return matches unless matches.empty?
        end
        nil
      end

      def merge!(other : ProgramIndex)
        other.macros.each do |name, refs|
          @macros[name].concat(refs)
        end
        @fingerprint ^= other.fingerprint
        self
      end

      private def lexical_scopes(scope : String) : Array(String)
        parts = scope.split("::")
        scopes = [] of String
        until parts.empty?
          scopes << parts.join("::")
          parts.pop
        end
        scopes << ""
        scopes
      end
    end

    module Indexer
      extend self

      def index_macros(ast : AstFile, index : ProgramIndex = ProgramIndex.new) : ProgramIndex
        tree = SyntaxTree.new(ast)
        walk(tree.root, index, "")
        index
      end

      def index_macros(asts : Array(AstFile), index : ProgramIndex = ProgramIndex.new) : ProgramIndex
        asts.each do |ast|
          index_macros(ast, index)
        end
        index
      end

      private def walk(node : SyntaxNode, index : ProgramIndex, scope : String)
        if node.kind == NodeKind::MacroDef
          if name = node.name
            index.add_macro(name, DeclRef.new(node.tree.ast, node.id, scope))
          end
          return
        end

        child_scope = if type_declaration?(node.kind)
                        qualify_scope(scope, node.name)
                      else
                        scope
                      end
        node.children.each do |child|
          walk(child, index, child_scope)
        end
      end

      private def type_declaration?(kind : NodeKind) : Bool
        {NodeKind::Class, NodeKind::Module, NodeKind::Struct, NodeKind::Enum, NodeKind::Lib}.includes?(kind)
      end

      private def qualify_scope(scope : String, name : String?) : String
        return scope unless name
        normalized = name.lchop("::")
        return normalized if name.starts_with?("::") || normalized.includes?("::") || scope.empty?
        "#{scope}::#{normalized}"
      end
    end
  end
end
