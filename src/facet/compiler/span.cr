module Facet
  module Compiler
    struct Span
      getter start : Int32
      getter finish : Int32

      def initialize(@start : Int32, @finish : Int32)
      end

      def length : Int32
        @finish - @start
      end
    end
  end
end
