module Facet
  module Compiler
    alias HygieneMark = Int32

    class Hygiene
      getter next_mark : HygieneMark

      def initialize
        @next_mark = 1
      end

      def gensym(base : String, symbols : SymbolTable) : SymbolId
        mark = @next_mark
        @next_mark += 1
        symbols.intern("__#{base}_#{mark}")
      end
    end
  end
end
