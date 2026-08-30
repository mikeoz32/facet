module Facet
  module Compiler
    alias FileId = Int32

    class SourceManager
      getter sources : Array(Source)
      getter versions : Array(Int32)
      getter hashes : Array(UInt64)
      getter revisions : Array(UInt64)
      getter workspace_revision : UInt64

      def initialize
        @sources = [] of Source
        @versions = [] of Int32
        @hashes = [] of UInt64
        @revisions = [] of UInt64
        @files_by_name = {} of String => FileId
        @workspace_revision = 0_u64
      end

      def add(text : String, filename : String? = nil, kind : SourceKind = SourceKind::Real) : FileId
        src = Source.new(text, filename, kind)
        fid = @sources.size.to_i32
        advance_revision
        @sources << src
        @versions << 0
        @hashes << hash_text(text)
        @revisions << @workspace_revision
        @files_by_name[filename] = fid if filename
        fid
      end

      # Adds a named source once and updates the existing file on later calls.
      # Returns the stable FileId and whether its contents changed.
      def upsert(text : String, filename : String, kind : SourceKind = SourceKind::Real) : {FileId, Bool}
        if file_id = @files_by_name[filename]?
          return {file_id, update(file_id, text)}
        end
        {add(text, filename, kind), true}
      end

      # Updates a source and advances its revision only when bytes actually
      # changed. QueryDb observes revisions, so callers do not need to manually
      # invalidate caches after this operation.
      def update(file_id : FileId, text : String) : Bool
        src = @sources[file_id]
        return false if src.text == text

        advance_revision
        @sources[file_id] = Source.new(text, src.filename, src.kind, src.expanded_from)
        @versions[file_id] += 1
        @hashes[file_id] = hash_text(text)
        @revisions[file_id] = @workspace_revision
        true
      end

      # Applies one byte-oriented source edit. Spans deliberately use byte
      # offsets, matching Lexer, Parser, and AstFile.
      def apply_edit(file_id : FileId, span : Span, replacement : String) : Bool
        text = @sources[file_id].text
        unless span.start >= 0 && span.finish >= span.start && span.finish <= text.bytesize
          raise IndexError.new("edit span #{span} is outside source size #{text.bytesize}")
        end

        updated = String.build(text.bytesize - span.length + replacement.bytesize) do |io|
          io.write(text.to_slice[0, span.start]) if span.start > 0
          io << replacement
          if span.finish < text.bytesize
            io.write(text.to_slice[span.finish, text.bytesize - span.finish])
          end
        end
        update(file_id, updated)
      end

      def source(file_id : FileId) : Source
        @sources[file_id]
      end

      def version(file_id : FileId) : Int32
        @versions[file_id]
      end

      def revision(file_id : FileId) : UInt64
        @revisions[file_id]
      end

      def fingerprint(file_id : FileId) : UInt64
        revision(file_id)
      end

      def file_id(filename : String) : FileId?
        @files_by_name[filename]?
      end

      def size : Int32
        @sources.size
      end

      private def hash_text(text : String) : UInt64
        text.hash.to_u64
      end

      private def advance_revision : Nil
        @workspace_revision &+= 1_u64
      end
    end
  end
end
