module UpstreamSupport
  def parse_ok(code : String)
    source = Facet::Compiler::Source.new(code, "upstream_port")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file
    if parser.diagnostics.any?
      first = parser.diagnostics.first
      raise "diagnostic: #{first.message} @ #{first.span.start}"
    end
    ast
  end

  # Small DSL helpers to speed up porting upstream specs.
  macro it_parses(code_literal, description = nil)
    it({{description || "parses " + code_literal.stringify}}) do
      parse_ok({{code_literal}})
    end
  end

  macro it_diagnoses(code_literal, message)
    it("diagnoses snippet") do
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new({{code_literal}}, "diag"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
      parser.diagnostics.first.message.should contain({{message}})
    end
  end
end
