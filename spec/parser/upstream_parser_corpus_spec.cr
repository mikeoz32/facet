require "json"
require "../spec_helper"
require "./upstream_support"
require "../../scripts/support/facet_ast_normalizer"

record UpstreamParserFixtureHeader,
  kind : String,
  crystal_version : String,
  suite : String,
  input_count : Int32 do
  include JSON::Serializable
end

record UpstreamParserFixtureCase,
  source : String,
  accepted : Bool,
  ast : String?,
  error : String?,
  line : Int32?,
  column : Int32? do
  include JSON::Serializable
end

record UpstreamAstShapeFixtureHeader,
  kind : String,
  crystal_version : String,
  accepted_input_count : Int32 do
  include JSON::Serializable
end

record UpstreamAstShapeFixtureCase,
  input_index : Int32,
  shape : String do
  include JSON::Serializable
end

fixture_path = File.expand_path("../fixtures/crystal_1_21_parser.jsonl", __DIR__)
fixture_lines = File.read_lines(fixture_path)
fixture_header = UpstreamParserFixtureHeader.from_json(fixture_lines.shift)
fixture_cases = fixture_lines.map { |line| UpstreamParserFixtureCase.from_json(line) }

shape_fixture_path = File.expand_path("../fixtures/crystal_1_21_ast_shape.jsonl", __DIR__)
shape_fixture_lines = File.read_lines(shape_fixture_path)
shape_fixture_header = UpstreamAstShapeFixtureHeader.from_json(shape_fixture_lines.shift)
shape_by_input = {} of Int32 => String
shape_fixture_lines.each do |line|
  fixture = UpstreamAstShapeFixtureCase.from_json(line)
  shape_by_input[fixture.input_index] = fixture.shape
end

describe "Crystal 1.21 parser corpus" do
  it "loads the complete captured parser suite corpus" do
    fixture_header.kind.should eq("facet-upstream-parser-fixture")
    fixture_header.crystal_version.should eq("1.21.0")
    fixture_header.suite.should eq("spec/compiler/parser")
    fixture_header.input_count.should eq(4_378)
    fixture_cases.size.should eq(fixture_header.input_count)
    shape_fixture_header.kind.should eq("facet-upstream-semantic-ast-fixture")
    shape_fixture_header.crystal_version.should eq("1.21.0")
    shape_fixture_header.accepted_input_count.should eq(3_437)
    shape_by_input.size.should eq(shape_fixture_header.accepted_input_count)
  end

  it "maintains the first-diagnostic parity baseline" do
    rejected = 0
    exact_messages = 0
    exact_locations = 0
    exact_both = 0

    fixture_cases.each_with_index do |fixture_case, index|
      next if fixture_case.accepted
      rejected += 1

      source = Facet::Compiler::Source.new(fixture_case.source, "upstream_diagnostic_#{index}")
      parser = Facet::Compiler::Parser.new(source)
      parser.parse_file
      diagnostic = parser.diagnostics.first

      lexer = Facet::Compiler::Lexer.new(source)
      lexer.tokenize_all
      line, column = lexer.line_and_column(diagnostic.span.start)
      message_match = diagnostic.message == fixture_case.error
      location_match = line == fixture_case.line && column == fixture_case.column

      exact_messages += 1 if message_match
      exact_locations += 1 if location_match
      exact_both += 1 if message_match && location_match
    end

    rejected.should eq(941)
    exact_messages.should eq(941)
    exact_locations.should eq(941)
    exact_both.should eq(941)
  end

  fixture_cases.each_with_index do |fixture_case, index|
    expectation = fixture_case.accepted ? "accepts" : "rejects"
    preview = fixture_case.source.lines.first?.to_s.strip
    preview = preview[0, Math.min(preview.size, 60)]

    it "#{expectation} upstream parser input #{index}: #{preview.dump}" do
      source = Facet::Compiler::Source.new(fixture_case.source, "upstream_parser_#{index}")
      parser = Facet::Compiler::Parser.new(source)
      ast = parser.parse_file

      if fixture_case.accepted
        fixture_case.ast.should_not be_nil
        fixture_case.error.should be_nil
        if parser.diagnostics.any?
          first = parser.diagnostics.first
          fail "unexpected diagnostic: #{first.message} @ #{first.span.start}"
        end
        UpstreamSupport.validate_ast_integrity(ast)
        expected_shape = shape_by_input[index]? || fail "missing semantic AST oracle for input #{index}"
        FacetAstNormalizer.normalize(ast).render.should eq(expected_shape)
      else
        fixture_case.ast.should be_nil
        fixture_case.error.should_not be_nil
        fixture_case.line.should_not be_nil
        fixture_case.column.should_not be_nil
        parser.diagnostics.should_not be_empty
        UpstreamSupport.validate_diagnostics(parser.diagnostics, source)
      end
    end
  end
end
