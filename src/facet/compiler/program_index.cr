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
      getter types : Hash(String, Array(DeclRef))
      getter methods : Hash(String, Array(DeclRef))
      getter instance_vars : Hash(String, Array(DeclRef))
      getter constants : Hash(String, Array(DeclRef))
      getter superclasses : Hash(String, String)
      getter fingerprint : UInt64

      def initialize
        @macros = Hash(String, Array(DeclRef)).new { |h, k| h[k] = [] of DeclRef }
        @types = Hash(String, Array(DeclRef)).new { |h, k| h[k] = [] of DeclRef }
        @methods = Hash(String, Array(DeclRef)).new { |h, k| h[k] = [] of DeclRef }
        @instance_vars = Hash(String, Array(DeclRef)).new { |h, k| h[k] = [] of DeclRef }
        @constants = Hash(String, Array(DeclRef)).new { |h, k| h[k] = [] of DeclRef }
        @superclasses = {} of String => String
        @fingerprint = 0_u64
      end

      def add_macro(name : String, ref : DeclRef)
        @macros[name] << ref
        mix_fingerprint(ref, "macro")
      end

      def add_type(name : String, ref : DeclRef)
        @types[name] << ref
        mix_fingerprint(ref, "type")
      end

      def add_method(scope : String, ref : DeclRef)
        return if scope.empty?
        @methods[scope] << ref
        mix_fingerprint(ref, "method")
      end

      def add_instance_var(scope : String, ref : DeclRef)
        return if scope.empty?
        @instance_vars[scope] << ref
        mix_fingerprint(ref, "instance-var")
      end

      def add_constant(scope : String, ref : DeclRef)
        return if scope.empty?
        @constants[scope] << ref
        mix_fingerprint(ref, "constant")
      end

      def add_superclass(scope : String, name : String, ref : DeclRef)
        return if scope.empty?
        @superclasses[scope] = name
        mix_fingerprint(ref, "superclass")
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

      def type_for(name : String, scope : String? = nil) : DeclRef?
        normalized = normalize_type_name(name)
        return nil if normalized.empty?
        return @types[normalized]?.try(&.first?) if name.starts_with?("::") || normalized.includes?("::")

        if scope
          lexical_scopes(scope).each do |candidate|
            qualified = candidate.empty? ? normalized : "#{candidate}::#{normalized}"
            if refs = @types[qualified]?
              return refs.first?
            end
          end
        end
        @types[normalized]?.try(&.first?)
      end

      def methods_for(type_name : String) : Array(DeclRef)
        @methods[normalize_type_name(type_name)]? || [] of DeclRef
      end

      def instance_vars_for(type_name : String) : Array(DeclRef)
        @instance_vars[normalize_type_name(type_name)]? || [] of DeclRef
      end

      def constants_for(type_name : String) : Array(DeclRef)
        @constants[normalize_type_name(type_name)]? || [] of DeclRef
      end

      def superclass_for(type_name : String) : String?
        @superclasses[normalize_type_name(type_name)]?
      end

      def merge!(other : ProgramIndex)
        other.macros.each do |name, refs|
          @macros[name].concat(refs)
        end
        other.types.each do |name, refs|
          @types[name].concat(refs)
        end
        other.methods.each do |scope, refs|
          @methods[scope].concat(refs)
        end
        other.instance_vars.each do |scope, refs|
          @instance_vars[scope].concat(refs)
        end
        other.constants.each do |scope, refs|
          @constants[scope].concat(refs)
        end
        other.superclasses.each do |scope, name|
          @superclasses[scope] = name
        end
        @fingerprint ^= other.fingerprint
        self
      end

      private def mix_fingerprint(ref : DeclRef, category : String) : Nil
        @fingerprint ^= ref.node_id.to_u64
        @fingerprint ^= ref.ast.source.text.hash.to_u64
        @fingerprint ^= ref.scope.hash.to_u64
        @fingerprint ^= category.hash.to_u64
      end

      private def normalize_type_name(name : String) : String
        normalized = name.strip.lchop("::")
        normalized = normalized.split('(', 2).first
        normalized = normalized.rchop('?')
        normalized
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

        child_scope = scope
        if type_declaration?(node.kind)
          child_scope = qualify_scope(scope, node.name)
          index.add_type(child_scope, DeclRef.new(node.tree.ast, node.id, child_scope)) unless child_scope.empty?
          if superclass = node.superclass
            index.add_superclass(child_scope, superclass.text, DeclRef.new(node.tree.ast, superclass.id, child_scope))
          end
          index_type_constants(node, index, child_scope)
        elsif {NodeKind::Def, NodeKind::Fun}.includes?(node.kind)
          index.add_method(scope, DeclRef.new(node.tree.ast, node.id, scope))
        elsif node.kind == NodeKind::InstanceVar
          index.add_instance_var(scope, DeclRef.new(node.tree.ast, node.id, scope))
        end
        node.children.each do |child|
          walk(child, index, child_scope)
        end
      end

      private def index_type_constants(node : SyntaxNode, index : ProgramIndex, scope : String) : Nil
        body = node.body
        return unless body
        statements = body.kind == NodeKind::Expressions ? body.children : [body]
        statements.each do |statement|
          target = if statement.kind == NodeKind::Assign
                     statement.target
                   elsif node.kind == NodeKind::Enum && {NodeKind::Ident, NodeKind::Const}.includes?(statement.kind)
                     statement
                   end
          next unless target
          name = target.symbol_name
          next unless name && name[0]?.try(&.uppercase?)
          index.add_constant(scope, DeclRef.new(node.tree.ast, target.id, scope))
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
