module Facet
  module Compiler
    struct QueryResult(T)
      getter value : T
      getter version : UInt64
      getter deps : Array(UInt64)

      def initialize(@value : T, @version : UInt64, @deps : Array(UInt64))
      end
    end

    struct ExpandCacheEntry
      getter value : AstFile
      getter version : UInt64
      getter deps : Array(UInt64)
      getter macro_names : Set(String)
      getter required_files : Set(FileId)

      def initialize(
        @value : AstFile,
        @version : UInt64,
        @deps : Array(UInt64),
        @macro_names : Set(String),
        @required_files : Set(FileId),
      )
      end
    end

    # Observable counters make cache behaviour testable and are useful for LSP
    # telemetry without coupling consumers to QueryDb internals.
    class QueryStats
      property parse_executions : Int32 = 0
      property parse_cache_hits : Int32 = 0
      property index_executions : Int32 = 0
      property index_cache_hits : Int32 = 0
      property syntax_executions : Int32 = 0
      property syntax_cache_hits : Int32 = 0
      property expand_executions : Int32 = 0
      property expand_cache_hits : Int32 = 0
      property global_index_rebuilds : Int32 = 0
      property global_index_cache_hits : Int32 = 0
    end

    # File-grained incremental frontend database. Source revisions are the only
    # invalidation input; callers may update SourceManager directly or use the
    # convenience mutation methods below. Parse and index queries are recomputed
    # only for changed files, while expansion queries depend only on the macros
    # and required files they actually touched.
    class QueryDb
      getter manager : SourceManager
      getter stats : QueryStats

      def initialize(@manager : SourceManager)
        @parse_cache = {} of FileId => QueryResult(AstFile)
        @syntax_cache = {} of FileId => QueryResult(SyntaxTree)
        @index_cache = {} of FileId => QueryResult(ProgramIndex)
        @expand_cache = {} of FileId => ExpandCacheEntry
        @macro_providers = {} of String => Set(FileId)
        @provider_macros = {} of FileId => Set(String)
        @macro_generations = {} of String => UInt64
        @pending_expansion_file_ids = Set(FileId).new
        @generation = 0_u64
        @global_index_cache = nil.as(QueryResult(ProgramIndex)?)
        @stats = QueryStats.new
      end

      def parse(file_id : FileId) : AstFile
        revision = @manager.revision(file_id)
        if cached = @parse_cache[file_id]?
          if cached.version == revision
            @stats.parse_cache_hits += 1
            return cached.value
          end
        end

        source = @manager.source(file_id)
        parser = Parser.new(source)
        ast = parser.parse_file
        @stats.parse_executions += 1
        @parse_cache[file_id] = QueryResult.new(ast, revision, [revision])
        ast
      end

      def index(file_id : FileId) : ProgramIndex
        revision = @manager.revision(file_id)
        if cached = @index_cache[file_id]?
          if cached.version == revision
            @stats.index_cache_hits += 1
            return cached.value
          end
        end

        old_names = @provider_macros[file_id]?.try(&.dup) || Set(String).new
        detach_provider(file_id, old_names)

        ast = parse(file_id)
        idx = Indexer.index_macros(ast)
        new_names = idx.macros.keys.to_set
        @provider_macros[file_id] = new_names
        new_names.each do |name|
          @macro_providers[name] ||= Set(FileId).new
          @macro_providers[name] << file_id
        end

        changed_names = old_names | new_names
        changed_names.each { |name| bump_macro_generation(name) }
        invalidate_expansions_by_macro(changed_names)
        @stats.index_executions += 1
        @index_cache[file_id] = QueryResult.new(idx, revision, [revision])
        @global_index_cache = nil
        idx
      end

      def syntax(file_id : FileId) : SyntaxTree
        revision = @manager.revision(file_id)
        if cached = @syntax_cache[file_id]?
          if cached.version == revision
            @stats.syntax_cache_hits += 1
            return cached.value
          end
        end

        tree = SyntaxTree.new(parse(file_id))
        @stats.syntax_executions += 1
        @syntax_cache[file_id] = QueryResult.new(tree, revision, [revision])
        tree
      end

      def expand(file_id : FileId) : AstFile
        parse_ast = parse(file_id)
        idx = build_global_index

        if cached = @expand_cache[file_id]?
          current_deps = expansion_dependencies(file_id, cached.required_files, cached.macro_names)
          if cached.deps == current_deps
            @stats.expand_cache_hits += 1
            return cached.value
          end
        end

        footprint = MacroFootprint.new
        footprint.require_file(file_id)
        expander = MacroExpander.new(idx)
        expanded = expander.expand(parse_ast, idx, footprint)
        footprint = expander.last_footprint || footprint
        required_files = footprint.required_files.to_set
        macro_names = footprint.macro_names.to_set
        deps = expansion_dependencies(file_id, required_files, macro_names)
        version = deps.max? || 0_u64

        @stats.expand_executions += 1
        @expand_cache[file_id] = ExpandCacheEntry.new(
          expanded,
          version,
          deps,
          macro_names,
          required_files
        )
        @pending_expansion_file_ids.delete(file_id)
        expanded
      end

      def pending_expansion_file_ids : Array(FileId)
        @pending_expansion_file_ids.to_a
      end

      # Mutating through QueryDb eagerly drops stale entries. Revision checks
      # still make direct SourceManager updates safe.
      def update(file_id : FileId, text : String) : Bool
        changed = @manager.update(file_id, text)
        invalidate(file_id) if changed
        changed
      end

      def apply_edit(file_id : FileId, span : Span, replacement : String) : Bool
        changed = @manager.apply_edit(file_id, span, replacement)
        invalidate(file_id) if changed
        changed
      end

      def upsert(text : String, filename : String, kind : SourceKind = SourceKind::Real) : {FileId, Bool}
        existing = @manager.file_id(filename)
        file_id, changed = @manager.upsert(text, filename, kind)
        invalidate(file_id) if existing && changed
        @global_index_cache = nil if changed
        {file_id, changed}
      end

      def invalidate(file_id : FileId)
        @pending_expansion_file_ids << file_id
        @parse_cache.delete(file_id)
        @syntax_cache.delete(file_id)
        @index_cache.delete(file_id)
        if names = @provider_macros.delete(file_id)
          detach_provider(file_id, names)
          names.each { |name| bump_macro_generation(name) }
          invalidate_expansions_by_macro(names)
        end
        @expand_cache.delete(file_id)
        @global_index_cache = nil
      end

      private def build_global_index : ProgramIndex
        revision = @manager.workspace_revision
        if cached = @global_index_cache
          if cached.version == revision
            @stats.global_index_cache_hits += 1
            return cached.value
          end
        end

        idx = ProgramIndex.new
        @manager.size.times do |file_id|
          idx.merge!(index(file_id))
        end
        @stats.global_index_rebuilds += 1
        @global_index_cache = QueryResult.new(idx, revision, [revision])
        idx
      end

      private def expansion_dependencies(
        file_id : FileId,
        required_files : Set(FileId),
        macro_names : Set(String),
      ) : Array(UInt64)
        deps = [@manager.revision(file_id)]
        required_files.to_a.sort.each do |required_file|
          next if required_file == file_id
          deps << @manager.revision(required_file)
        end
        macro_names.to_a.sort.each do |name|
          deps << (@macro_generations[name]? || 0_u64)
        end
        deps
      end

      private def detach_provider(file_id : FileId, names : Set(String)) : Nil
        names.each do |name|
          if providers = @macro_providers[name]?
            providers.delete(file_id)
            @macro_providers.delete(name) if providers.empty?
          end
        end
      end

      private def bump_macro_generation(name : String) : Nil
        @generation &+= 1_u64
        @macro_generations[name] = @generation
      end

      private def invalidate_expansions_by_macro(names : Set(String)) : Nil
        return if names.empty?
        to_delete = [] of FileId
        @expand_cache.each do |file_id, entry|
          to_delete << file_id unless (entry.macro_names & names).empty?
        end
        to_delete.each do |file_id|
          @expand_cache.delete(file_id)
          @pending_expansion_file_ids << file_id
        end
      end
    end
  end
end
