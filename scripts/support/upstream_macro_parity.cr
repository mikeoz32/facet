require "json"
require "../../src/facet"

record UpstreamMacroFixtureHeader,
  kind : String,
  crystal_version : String,
  crystal_revision : String,
  suites : Array(String),
  semantic_suites : Array(String),
  example_count : Int32,
  semantic_example_count : Int32,
  assertion_counts : Hash(String, Int32),
  assertion_count : Int32,
  case_count : Int32,
  excluded_count : Int32,
  exclusion_counts : Hash(String, Int32) do
  include JSON::Serializable
end

record UpstreamMacroFixtureCase,
  suite : String,
  source_file : String,
  line : Int32,
  body : String,
  expected : String do
  include JSON::Serializable
end

record UpstreamMacroParityResult,
  actual : String?,
  diagnostics : Array(String),
  output_diagnostics : Array(String) do
  def matches?(fixture_case : UpstreamMacroFixtureCase) : Bool
    diagnostics.empty? && actual == fixture_case.expected
  end
end

module UpstreamMacroParity
  extend self

  def load(path : String) : {UpstreamMacroFixtureHeader, Array(UpstreamMacroFixtureCase)}
    lines = File.read_lines(path)
    header = UpstreamMacroFixtureHeader.from_json(lines.shift)
    cases = lines.map { |line| UpstreamMacroFixtureCase.from_json(line) }
    {header, cases}
  end

  def expand(fixture_case : UpstreamMacroFixtureCase, index : Int32) : UpstreamMacroParityResult
    macro_name = "__facet_upstream_macro_#{index}"
    definition_source = Facet::Compiler::Source.new(
      "macro #{macro_name};#{fixture_case.body};end",
      "#{fixture_case.source_file}:#{fixture_case.line}"
    )
    definition_parser = Facet::Compiler::Parser.new(definition_source)
    definition = definition_parser.parse_file

    call_source = Facet::Compiler::Source.new(macro_name, "upstream_macro_case_#{index}.cr")
    call_parser = Facet::Compiler::Parser.new(call_source)
    call = call_parser.parse_file
    diagnostics = (definition_parser.diagnostics + call_parser.diagnostics).map(&.message)
    return UpstreamMacroParityResult.new(nil, diagnostics, [] of String) unless diagnostics.empty?

    program_index = Facet::Compiler::Indexer.index_macros([definition])
    expander = Facet::Compiler::MacroExpander.new(program_index)
    expanded = expander.expand(call, program_index)
    diagnostics.concat(expander.diagnostics.map(&.message))
    output_diagnostics = expanded.diagnostics.map(&.message)
    output_diagnostics.each { |message| diagnostics.delete(message) }
    UpstreamMacroParityResult.new(expanded.source.text.chomp(';'), diagnostics, output_diagnostics)
  rescue ex : Exception
    UpstreamMacroParityResult.new(nil, ["#{ex.class}: #{ex.message}"], [] of String)
  end
end
