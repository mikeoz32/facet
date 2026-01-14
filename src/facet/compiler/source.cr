module Facet
  module Compiler
    enum SourceKind
      Real
      Virtual
    end

    class ExpansionSite
      getter source : Source
      getter span : Span

      def initialize(@source : Source, @span : Span)
      end
    end

    struct Source
      getter text : String
      getter bytes : Bytes
      getter filename : String?
      getter kind : SourceKind
      getter expanded_from : ExpansionSite?

      def initialize(
        @text : String,
        @filename : String? = nil,
        @kind : SourceKind = SourceKind::Real,
        @expanded_from : ExpansionSite? = nil
      )
        @bytes = @text.to_slice
      end

      def size : Int32
        @bytes.size
      end

      # Walk expansion chain to the original source, returning root source,
      # the mapped span at the expansion site, and the chain of sites visited.
      def map_to_origin(span : Span) : {Source, Span, Array(ExpansionSite)}
        chain = [] of ExpansionSite
        current_source = self
        current_span = span
        while site = current_source.expanded_from
          chain << site
          current_source = site.source
          current_span = site.span
        end
        {current_source, current_span, chain}
      end
    end
  end
end
