module Facet
  module Compiler
    struct Diagnostic
      enum Severity
        Error
        Warning
      end

      getter span : Span
      getter message : String
      getter severity : Severity

      def initialize(@span : Span, @message : String, @severity : Severity = Severity::Error)
      end
    end
  end
end
