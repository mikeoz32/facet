require "json"

record RawSemanticMacroEvent,
  kind : String,
  invocation : String,
  definition : String,
  macro_name : String?,
  scope : String,
  free_vars : Hash(String, JSON::Any),
  instance_vars : Array(String),
  resolved_paths : Hash(String, JSON::Any),
  scope_class_methods : Array(JSON::Any),
  path_errors : Hash(String, String),
  expanded : String?,
  error_type : String?,
  error_message : String?,
  invocation_file : String?,
  invocation_line : Int32?,
  definition_file : String?,
  definition_line : Int32? do
  include JSON::Serializable
end

record SemanticMacroFixtureHeader,
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

record SemanticMacroFixtureCase,
  kind : String,
  invocation : String,
  definition : String,
  macro_name : String?,
  scope : String,
  free_vars : Hash(String, JSON::Any),
  instance_vars : Array(String),
  resolved_paths : Hash(String, JSON::Any),
  scope_class_methods : Array(JSON::Any),
  path_errors : Hash(String, String),
  expected : String?,
  expected_error_type : String?,
  expected_error_message : String? do
  include JSON::Serializable
end

capture_path = ARGV[0]? || abort "usage: crystal run scripts/normalize_upstream_macro_semantic_capture.cr -- CAPTURE.jsonl CRYSTAL_CHECKOUT OUTPUT.jsonl"
checkout = ARGV[1]? || abort "usage: crystal run scripts/normalize_upstream_macro_semantic_capture.cr -- CAPTURE.jsonl CRYSTAL_CHECKOUT OUTPUT.jsonl"
output_path = ARGV[2]? || abort "usage: crystal run scripts/normalize_upstream_macro_semantic_capture.cr -- CAPTURE.jsonl CRYSTAL_CHECKOUT OUTPUT.jsonl"

revision = "57cf7da5094db6c5d3c058c6d054a757b5ced19e"
actual_revision = `git -C #{Process.quote(checkout)} rev-parse HEAD`.strip
abort "expected Crystal revision #{revision}, got #{actual_revision}" unless actual_revision == revision

primitive_path = File.expand_path("src/primitives.cr", checkout)
events = File.read_lines(capture_path).map { |line| RawSemanticMacroEvent.from_json(line) }
events.reject! { |event| event.invocation_file == primitive_path }

cases = events.map do |event|
  SemanticMacroFixtureCase.new(
    kind: event.kind,
    invocation: event.invocation,
    definition: event.definition,
    macro_name: event.macro_name,
    scope: event.scope,
    free_vars: event.free_vars,
    instance_vars: event.instance_vars,
    resolved_paths: event.resolved_paths,
    scope_class_methods: event.scope_class_methods,
    path_errors: event.path_errors,
    expected: event.expanded,
    expected_error_type: event.error_type,
    expected_error_message: event.error_message,
  )
end

header = SemanticMacroFixtureHeader.new(
  kind: "facet-upstream-macro-semantic-events",
  crystal_version: "1.21.0",
  crystal_revision: revision,
  suites: [
    "spec/compiler/semantic/macro_spec.cr",
    "spec/compiler/semantic/macro_overload_spec.cr",
  ],
  semantic_example_count: 133,
  event_count: cases.size,
  call_count: cases.count { |event| event.kind == "call" },
  inline_count: cases.count { |event| event.kind == "inline" },
  success_count: cases.count { |event| !event.expected.nil? },
  error_count: cases.count { |event| !event.expected_error_type.nil? },
)

File.open(output_path, "w") do |io|
  io.puts header.to_json
  cases.each { |event| io.puts event.to_json }
end

puts "crystal_version=#{header.crystal_version} examples=#{header.semantic_example_count} events=#{header.event_count} calls=#{header.call_count} inline=#{header.inline_count} successes=#{header.success_count} errors=#{header.error_count} output=#{output_path}"
