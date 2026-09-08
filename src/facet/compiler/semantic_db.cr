require "set"

module Facet
  module Compiler
    class SemanticStats
      property file_index_executions : Int32 = 0
      property file_index_cache_hits : Int32 = 0
      property analysis_executions : Int32 = 0
      property analysis_cache_hits : Int32 = 0
      property body_inference_executions : Int32 = 0
      property require_graph_milliseconds : Float64 = 0.0
      property snapshot_milliseconds : Float64 = 0.0
    end

    record IndexedTypeDecl,
      key : String,
      kind : SemanticDefinitionKind,
      name : String,
      qualified_name : String,
      node_id : NodeId,
      span : Span,
      superclass : String?,
      type_parameters : Array(String),
      generated : Bool

    record IndexedMethodDecl,
      key : String,
      name : String,
      owner : String,
      node_id : NodeId,
      span : Span,
      class_method : Bool,
      min_arity : Int32,
      max_arity : Int32?,
      parameter_types : Array(String?),
      return_type : String?,
      generated : Bool

    record IndexedInclude,
      owner : String,
      target : String

    class SemanticFileIndex
      getter revision : UInt64
      getter types : Array(IndexedTypeDecl)
      getter methods : Array(IndexedMethodDecl)
      getter includes : Array(IndexedInclude)
      getter macros : Set(String)

      def initialize(
        @revision : UInt64,
        @types : Array(IndexedTypeDecl),
        @methods : Array(IndexedMethodDecl),
        @includes : Array(IndexedInclude),
        @macros : Set(String),
      )
      end
    end

    class SemanticSnapshot
      getter mode : SemanticMode
      getter graph : RequireGraph
      getter types : TypeStore
      getter definitions : Hash(DefId, SemanticDefinition)
      getter diagnostics : Array(SemanticDiagnostic)
      getter completeness_reasons : Set(SemanticCompletenessReason)
      getter node_types : Hash(NodeRef, TypeId)
      getter bindings : Hash(NodeRef, DefId)

      @trees : Hash(FileId, SyntaxTree)
      @revisions : Hash(FileId, UInt64)
      @methods_by_owner : Hash(String, Array(DefId))
      @type_definitions : Hash(String, DefId)
      @superclasses : Hash(String, String)
      @includes : Hash(String, Array(String))

      def initialize(
        @mode : SemanticMode,
        @graph : RequireGraph,
        @types : TypeStore,
        @definitions : Hash(DefId, SemanticDefinition),
        @diagnostics : Array(SemanticDiagnostic),
        @completeness_reasons : Set(SemanticCompletenessReason),
        @node_types : Hash(NodeRef, TypeId),
        @bindings : Hash(NodeRef, DefId),
        @trees : Hash(FileId, SyntaxTree),
        @revisions : Hash(FileId, UInt64),
        @methods_by_owner : Hash(String, Array(DefId)),
        @type_definitions : Hash(String, DefId),
        @superclasses : Hash(String, String),
        @includes : Hash(String, Array(String)),
      )
      end

      def complete? : Bool
        @completeness_reasons.empty?
      end

      def success? : Bool
        return true if @mode == SemanticMode::Tolerant
        @diagnostics.none? { |diagnostic| diagnostic.severity == SemanticDiagnosticSeverity::Error }
      end

      def type_of(node : NodeRef) : TypeId?
        return nil unless current?(node)
        @node_types[node]?
      end

      def definition_of(node : NodeRef) : SemanticDefinition?
        return nil unless current?(node)
        @bindings[node]?.try { |id| @definitions[id]? }
      end

      def methods(type_name : String, name : String? = nil, class_method : Bool? = nil) : Array(SemanticDefinition)
        ids = @methods_by_owner[type_name.lchop("::")]? || [] of DefId
        ids.compact_map do |id|
          definition = @definitions[id]?
          next unless definition
          next if name && definition.name != name
          next unless class_method.nil? || definition.class_method == class_method
          definition
        end
      end

      def method_candidates(type_id : TypeId, name : String) : Array(SemanticDefinition)
        type = @types[type_id]
        members = case type.kind
                  when SemanticTypeKind::Nominal
                    [{type.name, false}]
                  when SemanticTypeKind::Metaclass
                    instance = type.arguments.first?.try { |id| @types[id] }
                    [{instance.try(&.name), true}]
                  when SemanticTypeKind::Union
                    return type.arguments.flat_map { |member| method_candidates(member, name) }.uniq(&.id)
                  else
                    return [] of SemanticDefinition
                  end
        results = [] of SemanticDefinition
        members.each do |member_name, class_method|
          next unless member_name
          visited = Set(String).new
          queue = [member_name]
          until queue.empty?
            owner = queue.shift
            next if visited.includes?(owner)
            visited << owner
            results.concat(methods(owner, name, class_method))
            if superclass = @superclasses[owner]?
              queue << resolve_relative_type(superclass, owner)
            end
            (@includes[owner]? || [] of String).each do |included|
              queue << resolve_relative_type(included, owner)
            end
          end
        end
        results.uniq(&.id)
      end

      def diagnostics_for(file_id : FileId) : Array(SemanticDiagnostic)
        @diagnostics.select { |diagnostic| diagnostic.file_id == file_id }
      end

      def node(ref : NodeRef) : SyntaxNode?
        return nil unless current?(ref)
        @trees[ref.file_id]?.try { |tree| tree.node(ref.node_id) }
      end

      private def current?(ref : NodeRef) : Bool
        tree = @trees[ref.file_id]?
        return false unless tree
        ref.node_id >= 0 && tree.ast.arena.nodes.size > ref.node_id && @revisions[ref.file_id]? == ref.revision
      end

      private def resolve_relative_type(name : String, owner : String) : String
        normalized = name.strip.lchop("::")
        return normalized if name.starts_with?("::") || normalized.includes?("::")
        parts = owner.split("::")
        parts.pop
        until parts.empty?
          candidate = "#{parts.join("::")}::#{normalized}"
          return candidate if @type_definitions.has_key?(candidate)
          parts.pop
        end
        normalized
      end
    end

    private record SemanticCacheEntry,
      workspace_revision : UInt64,
      dependencies : Array(UInt64),
      snapshot : SemanticSnapshot

    # Incremental semantic queries over Facet's native AST. The first slice is
    # intentionally conservative: an unknown fact suppresses diagnostics rather
    # than being guessed from global workspace declarations.
    class SemanticDb
      getter queries : QueryDb
      getter resolver : RequireResolver
      getter stats : SemanticStats
      getter types : TypeStore

      def initialize(
        @queries : QueryDb,
        @resolver : RequireResolver = RegisteredSourceResolver.new,
        @macro_context : MacroExpansionContext = MacroExpansionContext.new,
      )
        @types = TypeStore.new
        @stats = SemanticStats.new
        @file_indexes = {} of FileId => SemanticFileIndex
        @analysis_cache = {} of String => SemanticCacheEntry
        @definition_ids = {} of String => DefId
        @next_definition_id = 0
      end

      def analyze(
        entries : Enumerable(FileId),
        mode : SemanticMode = SemanticMode::Tolerant,
        expanded_entries : Hash(FileId, SyntaxTree) = {} of FileId => SyntaxTree,
        expand_entries : Bool = true,
      ) : SemanticSnapshot
        entry_ids = entries.to_a.uniq.sort
        expanded_key = expanded_entries.keys.sort.map do |file_id|
          "#{file_id}:#{expanded_entries[file_id].ast.source.hash}"
        end.join(',')
        cache_key = "#{mode.value}:#{expand_entries}:#{entry_ids.join(',')}:#{expanded_key}"
        if cached = @analysis_cache[cache_key]?
          if cached.workspace_revision == @queries.manager.workspace_revision
            @stats.analysis_cache_hits += 1
            return cached.snapshot
          end
        end

        graph_started = Time.instant
        graph = RequireGraph.build(@queries, entry_ids, @resolver)
        @stats.require_graph_milliseconds += (Time.instant - graph_started).total_milliseconds
        if cached = @analysis_cache[cache_key]?
          if cached.dependencies == graph.dependencies
            @stats.analysis_cache_hits += 1
            @analysis_cache[cache_key] = SemanticCacheEntry.new(
              @queries.manager.workspace_revision,
              graph.dependencies,
              cached.snapshot
            )
            return cached.snapshot
          end
        end

        snapshot_started = Time.instant
        snapshot = build_snapshot(graph, mode, expanded_entries, expand_entries)
        @stats.snapshot_milliseconds += (Time.instant - snapshot_started).total_milliseconds
        @stats.analysis_executions += 1
        @analysis_cache[cache_key] = SemanticCacheEntry.new(
          @queries.manager.workspace_revision,
          graph.dependencies,
          snapshot
        )
        snapshot
      end

      private def build_snapshot(
        graph : RequireGraph,
        mode : SemanticMode,
        expanded_entries : Hash(FileId, SyntaxTree),
        expand_entries : Bool,
      ) : SemanticSnapshot
        trees = {} of FileId => SyntaxTree
        revisions = {} of FileId => UInt64
        indexes = {} of FileId => SemanticFileIndex
        reasons = Set(SemanticCompletenessReason).new
        diagnostics = graph.diagnostics.dup
        reasons << SemanticCompletenessReason::MissingRequire if graph.diagnostics.any?

        graph.reachable.each do |file_id|
          tree = @queries.syntax(file_id)
          trees[file_id] = tree
          revisions[file_id] = @queries.manager.revision(file_id)
          if tree.ast.diagnostics.any?
            reasons << SemanticCompletenessReason::ParseRecovery
          end
          indexes[file_id] = file_index(file_id, tree)
          if graph.entries.includes?(file_id)
            if expanded = expanded_entries[file_id]?
              merge_expanded_tree(file_id, indexes[file_id], expanded, reasons)
            elsif expand_entries
              merge_expanded_declarations(file_id, indexes[file_id], reasons)
            end
          end
        end

        definitions = {} of DefId => SemanticDefinition
        type_definitions = {} of String => DefId
        methods_by_owner = Hash(String, Array(DefId)).new { |hash, key| hash[key] = [] of DefId }
        superclasses = {} of String => String
        type_parameters = {} of String => Array(String)
        includes = Hash(String, Array(String)).new { |hash, key| hash[key] = [] of String }
        macro_names = Set(String).new

        object_type = @types.named("Object")
        object_id = definition_id("builtin:type:Object")
        definitions[object_id] = SemanticDefinition.new(
          object_id,
          SemanticDefinitionKind::Class,
          "Object",
          "Object",
          Span.new(0, 0),
          object_type
        )
        type_definitions["Object"] = object_id

        indexes.each do |file_id, index|
          revision = @queries.manager.revision(file_id)
          index.types.each do |declaration|
            type_id = @types.named(declaration.qualified_name)
            id = definition_id("#{file_id}:#{declaration.key}")
            definition = SemanticDefinition.new(
              id,
              declaration.kind,
              declaration.name,
              declaration.qualified_name,
              declaration.span,
              type_id,
              declaration.generated ? nil : NodeRef.new(file_id, declaration.node_id, revision),
              generated: declaration.generated
            )
            definitions[id] = definition
            type_definitions[declaration.qualified_name] ||= id
            type_parameters[declaration.qualified_name] = declaration.type_parameters
            superclasses[declaration.qualified_name] = declaration.superclass.not_nil! if declaration.superclass
          end
          index.includes.each { |edge| includes[edge.owner] << edge.target }
          macro_names.concat(index.macros)
        end

        indexes.each do |file_id, index|
          revision = @queries.manager.revision(file_id)
          index.methods.each do |declaration|
            parameter_types = declaration.parameter_types.map do |name|
              name ? resolve_type_text(name, declaration.owner, type_definitions) : @types.unknown
            end
            return_type = declaration.return_type.try do |name|
              resolve_type_text(name, declaration.owner, type_definitions)
            end || @types.unknown
            id = definition_id("#{file_id}:#{declaration.key}")
            definition = SemanticDefinition.new(
              id,
              SemanticDefinitionKind::Method,
              declaration.name,
              declaration.owner.empty? ? declaration.name : "#{declaration.owner}##{declaration.name}",
              declaration.span,
              @types.unknown,
              declaration.generated ? nil : NodeRef.new(file_id, declaration.node_id, revision),
              declaration.owner,
              declaration.class_method,
              declaration.min_arity,
              declaration.max_arity,
              parameter_types,
              return_type,
              declaration.generated
            )
            definitions[id] = definition
            methods_by_owner[declaration.owner] << id
          end
        end

        node_types = {} of NodeRef => TypeId
        bindings = {} of NodeRef => DefId

        analyzer = BodyAnalyzer.new(
          @types,
          trees,
          revisions,
          definitions,
          methods_by_owner,
          type_definitions,
          type_parameters,
          macro_names,
          superclasses,
          includes,
          node_types,
          bindings,
          diagnostics,
          reasons,
          true
        )
        graph.entries.each do |file_id|
          analyzer.analyze_file(file_id) if trees[file_id].ast.diagnostics.empty?
        end
        @stats.body_inference_executions += analyzer.body_executions

        SemanticSnapshot.new(
          mode,
          graph,
          @types,
          definitions,
          diagnostics,
          reasons,
          node_types,
          bindings,
          trees,
          revisions,
          methods_by_owner,
          type_definitions,
          superclasses,
          includes
        )
      end

      private def file_index(file_id : FileId, tree : SyntaxTree) : SemanticFileIndex
        revision = @queries.manager.revision(file_id)
        if cached = @file_indexes[file_id]?
          if cached.revision == revision
            @stats.file_index_cache_hits += 1
            return cached
          end
        end
        index = FileSemanticIndexer.new(tree, revision, false).index
        @file_indexes[file_id] = index
        @stats.file_index_executions += 1
        index
      end

      private def merge_expanded_declarations(
        file_id : FileId,
        index : SemanticFileIndex,
        reasons : Set(SemanticCompletenessReason),
      ) : Nil
        expanded = @queries.expand(file_id, @macro_context)
        return if expanded.arena.same?(@queries.parse(file_id).arena)
        if expanded.diagnostics.any?
          reasons << SemanticCompletenessReason::MacroExpansion
          return
        end
        merge_expanded_tree(file_id, index, SyntaxTree.new(expanded), reasons)
      rescue
        reasons << SemanticCompletenessReason::MacroExpansion
      end

      private def merge_expanded_tree(
        file_id : FileId,
        index : SemanticFileIndex,
        expanded : SyntaxTree,
        reasons : Set(SemanticCompletenessReason),
      ) : Nil
        if expanded.ast.diagnostics.any?
          reasons << SemanticCompletenessReason::MacroExpansion
          return
        end
        generated = FileSemanticIndexer.new(expanded, @queries.manager.revision(file_id), true).index
        type_keys = index.types.map(&.key).to_set
        method_keys = index.methods.map(&.key).to_set
        generated.types.each { |declaration| index.types << declaration unless type_keys.includes?(declaration.key) }
        generated.methods.each { |declaration| index.methods << declaration unless method_keys.includes?(declaration.key) }
        generated.includes.each { |edge| index.includes << edge unless index.includes.includes?(edge) }
        index.macros.concat(generated.macros)
      rescue
        reasons << SemanticCompletenessReason::MacroExpansion
      end

      private def definition_id(key : String) : DefId
        if id = @definition_ids[key]?
          return id
        end
        id = @next_definition_id
        @next_definition_id += 1
        @definition_ids[key] = id
        id
      end

      private def resolve_type_text(text : String, scope : String, definitions : Hash(String, DefId)) : TypeId
        TypeTextResolver.new(@types, definitions).resolve(text, scope)
      end
    end

    private class TypeTextResolver
      def initialize(@types : TypeStore, @definitions : Hash(String, DefId))
      end

      def resolve(source : String, scope : String) : TypeId
        text = source.strip
        return @types.unknown if text.empty?
        if text.ends_with?('?')
          return @types.union([resolve(text.rchop('?'), scope), @types.named("Nil")])
        end
        union_parts = split_top_level(text, '|')
        return @types.union(union_parts.map { |part| resolve(part, scope) }) if union_parts.size > 1
        if open = text.index('(')
          if text.ends_with?(')')
            name = text.byte_slice(0, open)
            inside = text.byte_slice(open + 1, text.bytesize - open - 2)
            args = split_top_level(inside, ',').map { |part| resolve(part, scope) }
            return @types.named(resolve_name(name, scope), args)
          end
        end
        name = resolve_name(text, scope)
        return @types.type_parameter(name) if !@definitions.has_key?(name) && name.size == 1 && name[0].uppercase?
        @types.named(name)
      end

      private def resolve_name(name : String, scope : String) : String
        normalized = name.strip.lchop("::")
        return normalized if name.starts_with?("::") || normalized.includes?("::")
        parts = scope.split("::")
        until parts.empty?
          candidate = "#{parts.join("::")}::#{normalized}"
          return candidate if @definitions.has_key?(candidate)
          parts.pop
        end
        normalized
      end

      private def split_top_level(text : String, separator : Char) : Array(String)
        values = [] of String
        depth = 0
        start = 0
        text.each_char_with_index do |char, index|
          case char
          when '(', '[', '{' then depth += 1
          when ')', ']', '}' then depth -= 1
          when separator
            if depth == 0
              values << text.byte_slice(start, index - start).strip
              start = index + 1
            end
          end
        end
        values << text.byte_slice(start, text.bytesize - start).strip
        values
      end
    end

    private class FileSemanticIndexer
      def initialize(@tree : SyntaxTree, @revision : UInt64, @generated : Bool)
        @types = [] of IndexedTypeDecl
        @methods = [] of IndexedMethodDecl
        @includes = [] of IndexedInclude
        @macros = Set(String).new
        @overload_ordinals = Hash(String, Int32).new(0)
      end

      def index : SemanticFileIndex
        walk(@tree.root, "")
        SemanticFileIndex.new(@revision, @types, @methods, @includes, @macros)
      end

      private def walk(node : SyntaxNode, scope : String) : Nil
        if type_kind = definition_kind(node.kind)
          name = node.name
          return unless name
          qualified = qualify(scope, name)
          superclass = node.superclass.try(&.text)
          generic_parameters = node.name_node.try do |name_node|
            if name_node.kind == NodeKind::TypeApply
              name_node.child(1).try(&.children).try(&.compact_map(&.symbol_name)) || [] of String
            else
              [] of String
            end
          end || [] of String
          @types << IndexedTypeDecl.new(
            "type:#{qualified}:#{type_kind.value}",
            type_kind,
            name.lchop("::").split("::").last,
            qualified,
            node.id,
            node.name_span || node.span,
            superclass,
            generic_parameters,
            @generated
          )
          node.body.try { |body| walk(body, qualified) }
          return
        end

        if node.kind == NodeKind::Def
          index_method(node, scope)
          return
        end
        if node.kind == NodeKind::MacroDef
          @macros << name if name = node.name
          return
        end

        if !scope.empty? && node.call_name == "include"
          node.arguments.each do |argument|
            @includes << IndexedInclude.new(scope, argument.text)
          end
        end
        node.children.each { |child| walk(child, scope) }
      end

      private def index_method(node : SyntaxNode, scope : String) : Nil
        raw_name = node.name || return
        scope = "Object" if scope.empty?
        class_method = raw_name.starts_with?("self.")
        name = class_method ? raw_name.lchop("self.") : raw_name
        parameters = node.parameters
        min_arity = parameters.count do |parameter|
          !{NodeKind::Splat, NodeKind::DoubleSplat, NodeKind::BlockParam}.includes?(parameter.kind) && parameter.value.nil?
        end
        has_splat = parameters.any? { |parameter| {NodeKind::Splat, NodeKind::DoubleSplat}.includes?(parameter.kind) }
        max_arity = has_splat ? nil : parameters.count { |parameter| parameter.kind != NodeKind::BlockParam }
        parameter_types = parameters.reject { |parameter| parameter.kind == NodeKind::BlockParam }.map do |parameter|
          parameter.declared_type.try(&.text)
        end
        signature = "#{scope}:#{class_method}:#{name}:#{parameter_types.join(',')}:#{min_arity}:#{max_arity}"
        ordinal = @overload_ordinals[signature]
        @overload_ordinals[signature] = ordinal + 1
        @methods << IndexedMethodDecl.new(
          "method:#{signature}:#{ordinal}",
          name,
          scope,
          node.id,
          node.name_span || node.span,
          class_method,
          min_arity,
          max_arity,
          parameter_types,
          node.return_type.try(&.text),
          @generated
        )
      end

      private def definition_kind(kind : NodeKind) : SemanticDefinitionKind?
        case kind
        when NodeKind::Class  then SemanticDefinitionKind::Class
        when NodeKind::Module then SemanticDefinitionKind::Module
        when NodeKind::Struct then SemanticDefinitionKind::Struct
        when NodeKind::Enum   then SemanticDefinitionKind::Enum
        when NodeKind::Lib    then SemanticDefinitionKind::Lib
        else                       nil
        end
      end

      private def qualify(scope : String, name : String) : String
        normalized = name.lchop("::")
        return normalized if name.starts_with?("::") || normalized.includes?("::") || scope.empty?
        "#{scope}::#{normalized}"
      end
    end
  end
end
