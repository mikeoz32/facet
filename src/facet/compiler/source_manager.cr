module Facet
  module Compiler
    alias FileId = Int32

    class SourceManager
      getter sources : Array(Source)
      getter versions : Array(Int32)
      getter hashes : Array(UInt64)

      def initialize
        @sources = [] of Source
        @versions = [] of Int32
        @hashes = [] of UInt64
      end

      def add(text : String, filename : String? = nil, kind : SourceKind = SourceKind::Real) : FileId
        src = Source.new(text, filename, kind)
        fid = @sources.size.to_i32
        @sources << src
        @versions << 0
        @hashes << hash_text(text)
        fid
      end

      def update(file_id : FileId, text : String)
        src = @sources[file_id]
        @sources[file_id] = Source.new(text, src.filename, src.kind, src.expanded_from)
        @versions[file_id] += 1
        @hashes[file_id] = hash_text(text)
      end

      def source(file_id : FileId) : Source
        @sources[file_id]
      end

      def version(file_id : FileId) : Int32
        @versions[file_id]
      end

      def fingerprint(file_id : FileId) : UInt64
        @hashes[file_id] ^ @versions[file_id].to_u64
      end

      private def hash_text(text : String) : UInt64
        text.hash.to_u64
      end
    end
  end
end
