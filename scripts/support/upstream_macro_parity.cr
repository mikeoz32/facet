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
  output_diagnostics : Array(String),
  side_effect_output : String = "" do
  def matches?(fixture_case : UpstreamMacroFixtureCase) : Bool
    diagnostics.empty? && actual == fixture_case.expected
  end

  def matches?(fixture_case : UpstreamRuntimeMacroFixtureCase) : Bool
    output_matches = side_effect_output == (fixture_case.side_effect_output || "")
    if expected_error = fixture_case.expected_error_message
      diagnostics == [expected_error] && output_matches
    else
      diagnostics.empty? && actual == fixture_case.expected && output_matches
    end
  end
end

class UpstreamRuntimeMacroNode
  include JSON::Serializable

  getter source : String
  getter kind : String
  getter fields : Hash(String, UpstreamRuntimeMacroNode)
  getter collections : Hash(String, Array(UpstreamRuntimeMacroNode))
  getter booleans : Hash(String, Bool)
  getter nil_fields : Array(String)

  def initialize(
    @source : String,
    @kind : String,
    @fields = {} of String => UpstreamRuntimeMacroNode,
    @collections = {} of String => Array(UpstreamRuntimeMacroNode),
    @booleans = {} of String => Bool,
    @nil_fields = [] of String,
  )
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
  doc : String?,
  name_source : String?,
  name_kind : String?,
  name_without_generic_args_source : String?,
  structure : UpstreamRuntimeMacroNode? do
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
  environment_case_count : Int32,
  flag_case_count : Int32,
  command_case_count : Int32,
  error_case_count : Int32,
  side_effect_case_count : Int32,
  metadata_argument_count : Int32,
  structured_type_node_argument_count : Int32,
  structured_name_argument_count : Int32,
  structured_call_argument_count : Int32,
  structured_control_flow_argument_count : Int32,
  structured_declaration_argument_count : Int32,
  structured_type_declaration_argument_count : Int32,
  structured_asm_argument_count : Int32,
  structured_type_syntax_argument_count : Int32,
  structured_expression_argument_count : Int32,
  structured_collection_argument_count : Int32,
  structured_misc_argument_count : Int32,
  structured_block_control_argument_count : Int32,
  structured_value_argument_count : Int32 do
  include JSON::Serializable
end

record UpstreamRuntimeMacroFixtureCase,
  source_file : String,
  line : Int32,
  body : String,
  expected : String,
  flags : JSON::Any,
  environment : Hash(String, String?),
  commands : Hash(String, String),
  expected_error_type : String?,
  expected_error_message : String?,
  side_effect_output : String?,
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
    context = Facet::Compiler::MacroExpansionContext.new(
      fixture_case.environment,
      runtime_flags(fixture_case.flags),
      fixture_case.commands
    )
    if fixture_case.arguments.any? { |argument| captured_argument_required?(fixture_case.body, argument) }
      expander = Facet::Compiler::MacroExpander.new(context: context)
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
      return UpstreamMacroParityResult.new(
        actual,
        expander.diagnostics.map(&.message),
        [] of String,
        expander.side_effect_output
      )
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
    expander = Facet::Compiler::MacroExpander.new(program_index, context: context)
    expanded = expander.expand(call, program_index)
    diagnostics.concat(expander.diagnostics.map(&.message))
    output_diagnostics = expanded.diagnostics.map(&.message)
    output_diagnostics.each { |message| diagnostics.delete(message) }
    UpstreamMacroParityResult.new(expanded.source.text.chomp(';'), diagnostics, output_diagnostics, expander.side_effect_output)
  rescue ex : Exception
    UpstreamMacroParityResult.new(nil, ["#{ex.class}: #{ex.message}"], [] of String)
  end

  private def runtime_flags(flags : JSON::Any) : Array(String)
    case raw = flags.raw
    when String
      [raw]
    when Array(JSON::Any)
      raw.compact_map(&.as_s?)
    else
      [] of String
    end
  end

  private def captured_metadata(argument : UpstreamRuntimeMacroArgument) : Facet::Compiler::MacroNodeMetadata?
    location = captured_location(argument.filename, argument.line_number, argument.column_number)
    end_location = captured_location(argument.end_filename, argument.end_line_number, argument.end_column_number)
    fields = {} of String => Facet::Compiler::MacroCapturedField
    if source = argument.name_source
      fields["name"] = Facet::Compiler::MacroCapturedField.new(source, argument.name_kind || "identifier")
    end
    if source = argument.name_without_generic_args_source
      fields["name_without_generic_args"] = Facet::Compiler::MacroCapturedField.new(source, argument.name_kind || "identifier")
    end
    structure = argument.structure.try { |node| captured_structure(node) }
    return nil unless location || end_location || argument.doc || !fields.empty? || structure
    Facet::Compiler::MacroNodeMetadata.new(location, end_location, argument.doc, fields, structure)
  end

  private def captured_argument_required?(body : String, argument : UpstreamRuntimeMacroArgument) : Bool
    return true if argument.kind == "Crystal::MacroId"
    return true if argument.filename || argument.end_filename || argument.doc
    return true if argument.name_source && root_member_requested?(body, argument.name, "name")
    return true if root_member_requested?(body, argument.name, "is_a?")
    if structure = argument.structure
      return true if structure.kind == "Crystal::Annotation"
      return true if structure.kind == "Crystal::TypeNode"
      if {"Crystal::ProcNotation", "Crystal::Metaclass", "Crystal::Generic", "Crystal::Union"}.includes?(structure.kind)
        return true if root_member_requested?(body, argument.name, "resolve") ||
                       root_member_requested?(body, argument.name, "resolve?")
      end
      if structure.kind == "Crystal::RangeLiteral"
        return true if root_member_requested?(body, argument.name, "map") ||
                       root_member_requested?(body, argument.name, "to_a")
      end
      members = structure.fields.keys + structure.collections.keys + structure.booleans.keys + structure.nil_fields
      return true if members.any? { |member| root_member_requested?(body, argument.name, member) }
    end
    false
  end

  private def root_member_requested?(body : String, argument_name : String, member : String) : Bool
    needle = "#{argument_name}.#{member}"
    offset = 0
    while index = body.index(needle, offset)
      next_byte = body.byte_at?(index + needle.bytesize)
      return true unless next_byte && ((next_byte >= 'a'.ord && next_byte <= 'z'.ord) ||
                         (next_byte >= 'A'.ord && next_byte <= 'Z'.ord) ||
                         (next_byte >= '0'.ord && next_byte <= '9'.ord) || next_byte == '_'.ord)
      offset = index + needle.bytesize
    end
    false
  end

  private def captured_structure(node : UpstreamRuntimeMacroNode) : Facet::Compiler::MacroCapturedNode
    fields = node.fields.transform_values { |field| captured_structure(field) }
    collections = node.collections.transform_values do |items|
      items.map { |item| captured_structure(item) }
    end
    Facet::Compiler::MacroCapturedNode.new(node.source, node.kind, fields, collections, node.booleans, node.nil_fields)
  end

  private def captured_location(filename : String?, line : Int32?, column : Int32?) : Facet::Compiler::MacroSourceLocation?
    return nil unless filename && line && column
    Facet::Compiler::MacroSourceLocation.new(filename, line, column)
  end
end
