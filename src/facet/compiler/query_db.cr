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

      def initialize(@value : AstFile, @version : UInt64, @deps : Array(UInt64), @macro_names : Set(String))
      end
    end

    class QueryDb
      getter manager : SourceManager

      def initialize(@manager : SourceManager)
        @parse_cache = {} of FileId => QueryResult(AstFile)
        @index_cache = {} of FileId => QueryResult(ProgramIndex)
        @expand_cache = {} of FileId => ExpandCacheEntry
        @macro_providers = {} of String => Set(FileId)
        @provider_macros = {} of FileId => Set(String)
      end

      def parse(file_id : FileId) : AstFile
        src_fp = @manager.fingerprint(file_id)
        if cached = @parse_cache[file_id]?
          return cached.value if cached.version == src_fp
        end
        source = @manager.source(file_id)
        parser = Parser.new(source)
        ast = parser.parse_file
        parser.diagnostics.each { |d| ast.diagnostics << d }
        @parse_cache[file_id] = QueryResult.new(ast, src_fp, [src_fp])
        ast
      end

      def index(file_id : FileId) : ProgramIndex
        src_fp = @manager.fingerprint(file_id)
        if cached = @index_cache[file_id]?
          return cached.value if cached.deps.all? { |dep| dep == src_fp }
        end
        if old_names = @provider_macros[file_id]?
          old_names.each do |name|
            if providers = @macro_providers[name]?
              providers.delete(file_id)
              @macro_providers.delete(name) if providers.empty?
            end
          end
        end
        ast = parse(file_id)
        idx = Indexer.index_macros(ast)
        names = idx.macros.keys.to_set
        @provider_macros[file_id] = names
        names.each do |name|
          @macro_providers[name] ||= Set(FileId).new
          @macro_providers[name] << file_id
        end
        deps = [src_fp]
        @index_cache[file_id] = QueryResult.new(idx, src_fp, deps)
        idx
      end

      def expand(file_id : FileId) : AstFile
        src_fp = @manager.fingerprint(file_id)
        parse_ast = parse(file_id)
        idx, idx_fp = build_global_index
        dep_fps = [src_fp, idx_fp]
        if cached = @expand_cache[file_id]?
          current_macro_versions = cached.macro_names.map { |name| macro_provider_version(name) }.compact
          current_deps = dep_fps + current_macro_versions
          return cached.value if cached.deps == current_deps
        end

        footprint = MacroFootprint.new
        footprint.require_file(file_id)
        expander = MacroExpander.new(idx)
        expanded = expander.expand(parse_ast, idx, footprint)
        footprint = expander.last_footprint || footprint
        footprint.required_files.each do |fid|
          dep_fps << @manager.fingerprint(fid)
        end
        macro_dep_versions = footprint.macro_names.map { |name| macro_provider_version(name) }.compact
        dep_versions = dep_fps + macro_dep_versions
        version = dep_versions.max? || 0_u64
        @expand_cache[file_id] = ExpandCacheEntry.new(expanded, version, dep_versions, footprint.macro_names)
        expanded
      end

      private def macro_provider_version(name : String) : UInt64?
        providers = @macro_providers[name]?
        return nil unless providers
        providers.map { |fid| @manager.fingerprint(fid) }.max?
      end

      private def build_global_index : {ProgramIndex, UInt64}
        idx = ProgramIndex.new
        fp = 0_u64
        @manager.sources.size.times do |fid|
          file_idx = index(fid)
          idx.merge!(file_idx)
          fp ^= file_idx.fingerprint
        end
        {idx, fp}
      end

      def invalidate(file_id : FileId)
        @parse_cache.delete(file_id)
        @index_cache.delete(file_id)
        if names = @provider_macros.delete(file_id)
          names.each do |name|
            if providers = @macro_providers[name]?
              providers.delete(file_id)
              @macro_providers.delete(name) if providers.empty?
            end
          end
          invalidate_expansions_by_macro(names)
        end
        @expand_cache.delete(file_id)
      end

      private def invalidate_expansions_by_macro(names : Set(String))
        return if names.empty?
        to_delete = [] of FileId
        @expand_cache.each do |fid, entry|
          to_delete << fid unless (entry.macro_names & names).empty?
        end
        to_delete.each { |fid| @expand_cache.delete(fid) }
      end
    end
  end
end
