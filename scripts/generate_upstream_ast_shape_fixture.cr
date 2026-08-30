require "json"
require "compiler/crystal/syntax"
require "./support/upstream_ast_normalizer"

record ParserFixtureCase,
  source : String,
  accepted : Bool,
  ast : String?,
  error : String?,
  line : Int32?,
  column : Int32? do
  include JSON::Serializable
end

record AstShapeFixtureHeader,
  kind : String,
  crystal_version : String,
  accepted_input_count : Int32 do
  include JSON::Serializable
end

record AstShapeFixtureCase,
  input_index : Int32,
  shape : String do
  include JSON::Serializable
end

parser_fixture = ARGV[0]? || File.expand_path("../spec/fixtures/crystal_1_21_parser.jsonl", __DIR__)
output_path = ARGV[1]? || File.expand_path("../spec/fixtures/crystal_1_21_ast_shape.jsonl", __DIR__)

lines = File.read_lines(parser_fixture)
lines.shift
cases = lines.map { |line| ParserFixtureCase.from_json(line) }
accepted = cases.count(&.accepted)

File.open(output_path, "w") do |io|
  AstShapeFixtureHeader.new(
    "facet-upstream-semantic-ast-fixture",
    Crystal::VERSION,
    accepted,
  ).to_json(io)
  io << '\n'

  cases.each_with_index do |fixture_case, index|
    next unless fixture_case.accepted

    upstream = Crystal::Parser.parse(fixture_case.source)
    unless upstream.inspect == fixture_case.ast
      abort "upstream AST drift at parser input #{index}"
    end
    shape = UpstreamAstNormalizer.normalize(upstream).render
    AstShapeFixtureCase.new(index, shape).to_json(io)
    io << '\n'
  end
end

puts "accepted=#{accepted} output=#{output_path}"
