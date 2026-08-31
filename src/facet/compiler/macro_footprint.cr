module Facet
  module Compiler
    class MacroFootprint
      getter macro_names : Set(String)
      getter required_sources : Set(Source)
      getter required_files : Set(Int32)
      getter? type_introspection : Bool

      def initialize
        @macro_names = Set(String).new
        @required_sources = Set(Source).new
        @required_files = Set(Int32).new
        @type_introspection = false
      end

      def macro_use(name : String)
        @macro_names << name
      end

      def merge_requires(source : Source)
        @required_sources << source
      end

      def require_file(file_id : Int32)
        @required_files << file_id
      end

      def type_introspection
        @type_introspection = true
      end

      def merge_macro_uses
        # placeholder for future aggregation
      end
    end
  end
end
