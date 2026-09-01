require "json"

# Normalizes the runtime capture produced by `upstream_macro_runtime_capture.patch`
# into a stable, checkout-independent fixture. Runtime capture is necessary for
# argument-bearing contracts because the official specs construct Crystal AST
# nodes in ordinary Crystal code before calling `assert_macro`.

class CapturedMacroNode
  include JSON::Serializable

  getter source : String
  getter kind : String
  getter fields : Hash(String, CapturedMacroNode)
  getter collections : Hash(String, Array(CapturedMacroNode))
  getter booleans : Hash(String, Bool)
  getter nil_fields : Array(String)

  def initialize(
    @source : String,
    @kind : String,
    @fields = {} of String => CapturedMacroNode,
    @collections = {} of String => Array(CapturedMacroNode),
    @booleans = {} of String => Bool,
    @nil_fields = [] of String,
  )
  end
end

record CapturedMacroArgument,
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
  structure : CapturedMacroNode? do
  include JSON::Serializable
end

record CapturedMacroCase,
  source_file : String,
  line : Int32,
  body : String,
  expected : String,
  flags : JSON::Any,
  contextual_program : Bool,
  arguments : Array(CapturedMacroArgument) do
  include JSON::Serializable
end

record RuntimeMacroFixtureHeader,
  kind : String,
  crystal_version : String,
  crystal_revision : String,
  case_count : Int32,
  direct_case_count : Int32,
  contextual_case_count : Int32,
  argument_case_count : Int32,
  metadata_argument_count : Int32,
  structured_name_argument_count : Int32,
  structured_call_argument_count : Int32,
  structured_control_flow_argument_count : Int32 do
  include JSON::Serializable
end

capture_path = ARGV[0]? || abort "usage: crystal run scripts/normalize_upstream_macro_capture.cr -- CAPTURE.jsonl CRYSTAL_CHECKOUT OUTPUT.jsonl"
checkout = ARGV[1]? || abort "usage: crystal run scripts/normalize_upstream_macro_capture.cr -- CAPTURE.jsonl CRYSTAL_CHECKOUT OUTPUT.jsonl"
output_path = ARGV[2]? || abort "usage: crystal run scripts/normalize_upstream_macro_capture.cr -- CAPTURE.jsonl CRYSTAL_CHECKOUT OUTPUT.jsonl"

cases = File.read_lines(capture_path).reject(&.blank?).map do |line|
  captured = CapturedMacroCase.from_json(line)
  relative_source = Path[captured.source_file].relative_to(checkout).to_s
  captured.copy_with(source_file: relative_source)
end.to_a

header = RuntimeMacroFixtureHeader.new(
  kind: "facet-upstream-macro-runtime-fixture",
  crystal_version: "1.21.0",
  crystal_revision: "57cf7da5094db6c5d3c058c6d054a757b5ced19e",
  case_count: cases.size,
  direct_case_count: cases.count { |fixture_case| !fixture_case.contextual_program },
  contextual_case_count: cases.count(&.contextual_program),
  argument_case_count: cases.count { |fixture_case| !fixture_case.arguments.empty? },
  metadata_argument_count: cases.sum do |fixture_case|
    fixture_case.arguments.count do |argument|
      !argument.filename.nil? || !argument.end_filename.nil? || !argument.doc.nil?
    end
  end,
  structured_name_argument_count: cases.sum do |fixture_case|
    fixture_case.arguments.count { |argument| !argument.name_source.nil? }
  end,
  structured_call_argument_count: cases.sum do |fixture_case|
    fixture_case.arguments.count do |argument|
      argument.structure.try do |node|
        {"Crystal::Call", "Crystal::IsA", "Crystal::RespondsTo", "Crystal::TypeOf"}.includes?(node.kind)
      end || false
    end
  end,
  structured_control_flow_argument_count: cases.sum do |fixture_case|
    fixture_case.arguments.count do |argument|
      argument.structure.try do |node|
        {"Crystal::Case", "Crystal::Select", "Crystal::ExceptionHandler", "Crystal::Rescue"}.includes?(node.kind)
      end || false
    end
  end,
)

File.open(output_path, "w") do |io|
  io.puts header.to_json
  cases.each { |fixture_case| io.puts fixture_case.to_json }
end

puts "cases=#{header.case_count} direct=#{header.direct_case_count} contextual=#{header.contextual_case_count} arguments=#{header.argument_case_count} output=#{output_path}"
