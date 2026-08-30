require "base64"
require "compiler/crystal/syntax"
require "json"
require "set"

# Materializes a stable, self-contained parser corpus from the inputs captured
# by scripts/upstream_input_trace.patch. The generated fixture lets Facet run
# the upstream Crystal parser inputs without a Crystal source checkout.

record FixtureHeader,
  kind : String,
  crystal_version : String,
  suite : String,
  input_count : Int32 do
  include JSON::Serializable
end

record FixtureCase,
  source : String,
  accepted : Bool,
  ast : String?,
  error : String?,
  line : Int32?,
  column : Int32? do
  include JSON::Serializable
end

trace_path = ARGV[0]? || abort "usage: crystal run scripts/generate_upstream_parser_fixture.cr -- TRACE.b64 OUTPUT.jsonl [SUITE]"
output_path = ARGV[1]? || abort "usage: crystal run scripts/generate_upstream_parser_fixture.cr -- TRACE.b64 OUTPUT.jsonl [SUITE]"
suite = ARGV[2]? || "spec/compiler/parser"

unless Crystal::VERSION == "1.21.0"
  abort "fixture generation requires Crystal 1.21.0, got #{Crystal::VERSION}"
end

seen = Set(String).new
inputs = File.read_lines(trace_path).compact_map do |line|
  source = Base64.decode_string(line)
  next if seen.includes?(source)
  seen << source
  source
end

File.open(output_path, "w") do |io|
  io.puts FixtureHeader.new(
    kind: "facet-upstream-parser-fixture",
    crystal_version: Crystal::VERSION,
    suite: suite,
    input_count: inputs.size,
  ).to_json

  inputs.each do |source|
    fixture_case = begin
      ast = Crystal::Parser.new(source).parse
      FixtureCase.new(
        source: source,
        accepted: true,
        ast: ast.inspect,
        error: nil,
        line: nil,
        column: nil,
      )
    rescue ex : Crystal::SyntaxException
      FixtureCase.new(
        source: source,
        accepted: false,
        ast: nil,
        error: ex.message,
        line: ex.line_number,
        column: ex.column_number,
      )
    end
    io.puts fixture_case.to_json
  end
end

puts "crystal_version=#{Crystal::VERSION} inputs=#{inputs.size} output=#{output_path}"
