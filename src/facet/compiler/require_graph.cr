require "set"

module Facet
  module Compiler
    record RequireEdge,
      from : FileId,
      to : FileId,
      path : String,
      span : Span

    struct RequireResolution
      getter files : Array(FileId)
      getter searched : Array(String)

      def initialize(
        @files : Array(FileId) = [] of FileId,
        @searched : Array(String) = [] of String,
      )
      end
    end

    abstract class RequireResolver
      abstract def resolve(manager : SourceManager, from : FileId, path : String) : RequireResolution

      def prelude(manager : SourceManager) : Array(FileId)
        [] of FileId
      end

      def fingerprint(manager : SourceManager) : UInt64
        0_u64
      end
    end

    # Resolves Crystal requires against sources already registered in the
    # SourceManager. This keeps the semantic core deterministic and lets both a
    # compiler driver and an editor decide how files enter the database.
    class RegisteredSourceResolver < RequireResolver
      getter roots : Array(String)
      getter prelude_name : String?

      def initialize(roots : Enumerable(String) = [] of String, @prelude_name : String? = "prelude")
        @roots = roots.map { |root| normalize(root) }.uniq
        @indexed_manager = nil.as(SourceManager?)
        @indexed_size = -1
        @directory_files = Hash(String, Array(FileId)).new { |hash, key| hash[key] = [] of FileId }
      end

      def resolve(manager : SourceManager, from : FileId, path : String) : RequireResolution
        from_name = manager.source(from).filename
        bases = [] of String
        if relative_require?(path)
          bases << File.dirname(from_name || ".")
        else
          @roots.each { |root| bases << root }
        end

        searched = [] of String
        bases.each do |base|
          candidate = normalize(File.join(base, path))
          if candidate.ends_with?(".cr") && !glob?(candidate)
            searched << candidate
            if file_id = manager.file_id(candidate)
              return RequireResolution.new([file_id], searched)
            end
            next
          end

          unless glob?(candidate)
            exact = "#{candidate}.cr"
            searched << exact
            if file_id = manager.file_id(exact)
              return RequireResolution.new([file_id], searched)
            end
            searched << File.join(candidate, "**", "*.cr")
            matches = files_below(manager, candidate)
            unless matches.empty?
              return RequireResolution.new(matches, searched)
            end
            next
          end

          searched << candidate
          matches = matching_files(manager, candidate)
          unless matches.empty?
            return RequireResolution.new(matches, searched)
          end
        end
        RequireResolution.new([] of FileId, searched.uniq)
      end

      def prelude(manager : SourceManager) : Array(FileId)
        name = @prelude_name
        return [] of FileId unless name
        @roots.each do |root|
          patterns_for(normalize(File.join(root, name))).each do |candidate|
            if file_id = manager.file_id(candidate)
              return [file_id]
            end
          end
        end
        [] of FileId
      end

      def fingerprint(manager : SourceManager) : UInt64
        value = 0_u64
        @roots.each { |root| value ^= root.hash.to_u64 }
        value ^= (@prelude_name || "").hash.to_u64
        value
      end

      private def patterns_for(candidate : String) : Array(String)
        return [candidate] if candidate.ends_with?(".cr") || glob?(candidate)
        ["#{candidate}.cr", File.join(candidate, "**", "*.cr")]
      end

      private def matching_files(manager : SourceManager, pattern : String) : Array(FileId)
        matches = [] of FileId
        manager.sources.each_with_index do |source, file_id|
          filename = source.filename
          next unless filename
          matches << file_id.to_i32 if path_match?(pattern, normalize(filename))
        end
        matches.uniq.sort
      end

      private def files_below(manager : SourceManager, directory : String) : Array(FileId)
        ensure_directory_index(manager)
        @directory_files[directory]?.try(&.dup) || [] of FileId
      end

      private def ensure_directory_index(manager : SourceManager) : Nil
        return if @indexed_manager.try(&.same?(manager)) && @indexed_size == manager.size

        @directory_files.clear
        manager.sources.each_with_index do |source, file_id|
          filename = source.filename
          next unless filename && filename.ends_with?(".cr")
          directory = File.dirname(normalize(filename))
          loop do
            @directory_files[directory] << file_id.to_i32
            parent = File.dirname(directory)
            break if parent == directory || directory == "."
            directory = parent
          end
        end
        @directory_files.each_value(&.sort!)
        @indexed_manager = manager
        @indexed_size = manager.size
      end

      private def path_match?(pattern : String, filename : String) : Bool
        if glob?(pattern)
          File.match?(pattern, filename)
        else
          pattern == filename
        end
      end

      private def relative_require?(path : String) : Bool
        path.starts_with?("./") || path.starts_with?("../")
      end

      private def glob?(path : String) : Bool
        path.includes?('*') || path.includes?('?') || path.includes?('[')
      end

      private def normalize(path : String) : String
        Path.new(path).normalize.to_s
      end
    end

    class RequireGraph
      getter entries : Array(FileId)
      getter reachable : Set(FileId)
      getter edges : Array(RequireEdge)
      getter diagnostics : Array(SemanticDiagnostic)
      getter dependencies : Array(UInt64)

      def initialize(
        @entries : Array(FileId),
        @reachable : Set(FileId),
        @edges : Array(RequireEdge),
        @diagnostics : Array(SemanticDiagnostic),
        @dependencies : Array(UInt64),
      )
      end

      def self.build(
        queries : QueryDb,
        entries : Enumerable(FileId),
        resolver : RequireResolver,
      ) : RequireGraph
        manager = queries.manager
        seeds = entries.to_a.uniq
        seeds.concat(resolver.prelude(manager))
        seeds.uniq!
        reachable = Set(FileId).new
        edges = [] of RequireEdge
        diagnostics = [] of SemanticDiagnostic
        queue = seeds.dup

        until queue.empty?
          file_id = queue.shift
          next if reachable.includes?(file_id)
          next unless file_id.in?(0...manager.size)
          reachable << file_id
          tree = queries.syntax(file_id)
          tree.nodes(NodeKind::Require).each do |require_node|
            literal = require_node.child(0)
            next unless literal
            path = tree.ast.decoded_literal_string(literal.id)
            resolution = resolver.resolve(manager, file_id, path)
            if resolution.files.empty?
              diagnostics << SemanticDiagnostic.new(
                "facet.missing_require",
                file_id,
                literal.span,
                "can't find file '#{path}'"
              )
              next
            end
            resolution.files.each do |target|
              edges << RequireEdge.new(file_id, target, path, literal.span)
              queue << target unless reachable.includes?(target)
            end
          end
        end

        dependencies = reachable.to_a.sort.map { |file_id| manager.revision(file_id) }
        dependencies << resolver.fingerprint(manager)
        RequireGraph.new(entries.to_a.uniq, reachable, edges, diagnostics, dependencies)
      end
    end
  end
end
