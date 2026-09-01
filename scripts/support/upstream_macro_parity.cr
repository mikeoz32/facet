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

  def matches?(fixture_case : UpstreamRuntimeMacroFixtureCase) : Bool
    diagnostics.empty? && actual == fixture_case.expected
  end
end

record UpstreamRuntimeMacroArgument,
  name : String,
  source : String,
  kind : String,
  filename : String?,
  line_number : Int32?,
  column_number : Int32?,
  end_filename : String?,
  end_line_number : Int32?,
  end_column_number : Int32?,
  doc : String? do
  include JSON::Serializable
end

record UpstreamRuntimeMacroFixtureHeader,
  kind : String,
  crystal_version : String,
  crystal_revision : String,
  case_count : Int32,
  direct_case_count : Int32,
  contextual_case_count : Int32,
  argument_case_count : Int32,
  metadata_argument_count : Int32 do
  include JSON::Serializable
end

record UpstreamRuntimeMacroFixtureCase,
  source_file : String,
  line : Int32,
  body : String,
  expected : String,
  flags : JSON::Any,
  contextual_program : Bool,
  arguments : Array(UpstreamRuntimeMacroArgument) do
  include JSON::Serializable
end

module UpstreamMacroParity
  extend self

  def load(path : String) : {UpstreamMacroFixtureHeader, Array(UpstreamMacroFixtureCase)}
    lines = File.read_lines(path)
    header = UpstreamMacroFixtureHeader.from_json(lines.shift)
    cases = lines.map { |line| UpstreamMacroFixtureCase.from_json(line) }
    {header, cases}
  end

  def load_runtime(path : String) : {UpstreamRuntimeMacroFixtureHeader, Array(UpstreamRuntimeMacroFixtureCase)}
    lines = File.read_lines(path)
    header = UpstreamRuntimeMacroFixtureHeader.from_json(lines.shift)
    cases = lines.map { |line| UpstreamRuntimeMacroFixtureCase.from_json(line) }
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

  def expand(fixture_case : UpstreamRuntimeMacroFixtureCase, index : Int32) : UpstreamMacroParityResult
    if fixture_case.arguments.any? { |argument| captured_metadata(argument) }
      expander = Facet::Compiler::MacroExpander.new
      arguments = {} of String => Facet::Compiler::MacroValue
      fixture_case.arguments.each do |argument|
        metadata = captured_metadata(argument) || Facet::Compiler::MacroNodeMetadata.new
        arguments[argument.name] = Facet::Compiler::MacroSyntaxValue.captured(
          argument.source,
          argument.kind,
          metadata
        )
      end
      actual = expander.expand_template(
        fixture_case.body,
        arguments,
        "#{fixture_case.source_file}:#{fixture_case.line}"
      ).chomp(';')
      return UpstreamMacroParityResult.new(actual, expander.diagnostics.map(&.message), [] of String)
    end

    macro_name = "__facet_upstream_runtime_macro_#{index}"
    parameters = fixture_case.arguments.map(&.name).join(", ")
    definition_source = Facet::Compiler::Source.new(
      "macro #{macro_name}(#{parameters});#{fixture_case.body};end",
      "#{fixture_case.source_file}:#{fixture_case.line}"
    )
    definition_parser = Facet::Compiler::Parser.new(definition_source)
    definition = definition_parser.parse_file

    arguments = fixture_case.arguments.map(&.source).join(", ")
    call_source = Facet::Compiler::Source.new(
      "#{macro_name}(#{arguments})",
      "upstream_runtime_macro_case_#{index}.cr"
    )
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

  private def captured_metadata(argument : UpstreamRuntimeMacroArgument) : Facet::Compiler::MacroNodeMetadata?
    location = captured_location(argument.filename, argument.line_number, argument.column_number)
    end_location = captured_location(argument.end_filename, argument.end_line_number, argument.end_column_number)
    return nil unless location || end_location || argument.doc
    Facet::Compiler::MacroNodeMetadata.new(location, end_location, argument.doc)
  end

  private def captured_location(filename : String?, line : Int32?, column : Int32?) : Facet::Compiler::MacroSourceLocation?
    return nil unless filename && line && column
    Facet::Compiler::MacroSourceLocation.new(filename, line, column)
  end
end
