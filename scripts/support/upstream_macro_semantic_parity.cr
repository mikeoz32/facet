require "../../src/facet"
require "./facet_ast_normalizer"
require "json"

record UpstreamSemanticMacroHeader,
  kind : String,
  crystal_version : String,
  crystal_revision : String,
  suites : Array(String),
  semantic_example_count : Int32,
  event_count : Int32,
  call_count : Int32,
  inline_count : Int32,
  success_count : Int32,
  error_count : Int32 do
  include JSON::Serializable
end

record UpstreamSemanticMacroCase,
  kind : String,
  invocation : String,
  definition : String,
  macro_name : String?,
  scope : String,
  expected : String?,
  expected_error_type : String?,
  expected_error_message : String? do
  include JSON::Serializable
end

record UpstreamSemanticMacroResult,
  actual : String?,
  diagnostics : Array(String),
  skipped_file : Bool = false do
  def matches?(fixture_case : UpstreamSemanticMacroCase) : Bool
    if fixture_case.expected_error_type
      return skipped_file if fixture_case.expected_error_type == "Crystal::SkipMacroException"
      if expected_error = fixture_case.expected_error_message
        diagnostics == [expected_error]
      else
        false
      end
    elsif expected = fixture_case.expected
      diagnostics.empty? && UpstreamSemanticMacroParity.equivalent_output?(actual, expected)
    else
      false
    end
  end
end

module UpstreamSemanticMacroParity
  extend self

  def load(path : String) : {UpstreamSemanticMacroHeader, Array(UpstreamSemanticMacroCase)}
    lines = File.read_lines(path)
    header = UpstreamSemanticMacroHeader.from_json(lines.shift)
    cases = lines.map { |line| UpstreamSemanticMacroCase.from_json(line) }
    {header, cases}
  end

  def expand(fixture_case : UpstreamSemanticMacroCase, index : Int32) : UpstreamSemanticMacroResult
    if fixture_case.kind == "call"
      expand_call(fixture_case, index)
    else
      expand_inline(fixture_case, index)
    end
  rescue ex
    UpstreamSemanticMacroResult.new(nil, ["#{ex.class}: #{ex.message}"])
  end

  def equivalent_output?(actual : String?, expected : String) : Bool
    return false unless actual
    return true if actual.strip == expected.strip

    actual_parser = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new(actual, "facet-semantic-macro-actual.cr", Facet::Compiler::SourceKind::Virtual)
    )
    actual_ast = actual_parser.parse_file
    expected_parser = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new(expected, "facet-semantic-macro-expected.cr", Facet::Compiler::SourceKind::Virtual)
    )
    expected_ast = expected_parser.parse_file
    return false unless actual_parser.diagnostics.empty? && expected_parser.diagnostics.empty?
    FacetAstNormalizer.normalize(actual_ast) == FacetAstNormalizer.normalize(expected_ast)
  end

  private def expand_call(fixture_case : UpstreamSemanticMacroCase, index : Int32) : UpstreamSemanticMacroResult
    definition_parser = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new(
        fixture_case.definition,
        "semantic-macro-definition-#{index}.cr",
        Facet::Compiler::SourceKind::Virtual
      )
    )
    definition = definition_parser.parse_file
    call_parser = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new(
        selected_call_source(fixture_case),
        "semantic-macro-call-#{index}.cr",
        Facet::Compiler::SourceKind::Virtual
      )
    )
    call = call_parser.parse_file
    diagnostics = (definition_parser.diagnostics + call_parser.diagnostics).map(&.message)
    return UpstreamSemanticMacroResult.new(nil, diagnostics) unless diagnostics.empty?

    program_index = Facet::Compiler::Indexer.index_macros([definition])
    context = Facet::Compiler::MacroExpansionContext.new(resolve_type_arguments: true)
    expander = Facet::Compiler::MacroExpander.new(program_index, context: context)
    expanded = expander.expand(call, program_index)
    diagnostics.concat(expander.diagnostics.map(&.message))
    UpstreamSemanticMacroResult.new(expanded.source.text, diagnostics, expander.skipped_file)
  end

  private def expand_inline(fixture_case : UpstreamSemanticMacroCase, index : Int32) : UpstreamSemanticMacroResult
    expander = Facet::Compiler::MacroExpander.new
    scope = fixture_case.scope.rchop("+")
    type = Facet::Compiler::MacroTypeValue.new(scope, Facet::Compiler::MacroTypeKind::Class)
    actual = expander.expand_template(
      fixture_case.invocation,
      {
        "@type"   => type.as(Facet::Compiler::MacroValue),
        "@caller" => nil.as(Facet::Compiler::MacroValue),
      },
      "semantic-macro-inline-#{index}.cr"
    )
    UpstreamSemanticMacroResult.new(actual, expander.diagnostics.map(&.message), expander.skipped_file)
  end

  private def selected_call_source(fixture_case : UpstreamSemanticMacroCase) : String
    macro_name = fixture_case.macro_name
    return fixture_case.invocation unless macro_name
    marker = ".#{macro_name}"
    marker_index = fixture_case.invocation.rindex(marker)
    return fixture_case.invocation unless marker_index
    receiver = fixture_case.invocation.byte_slice(0, marker_index).strip
    return fixture_case.invocation unless receiver.matches?(/\A::?[A-Z]/) || receiver.matches?(/\A[A-Z]/)
    fixture_case.invocation.byte_slice(marker_index + 1..)
  end
end
