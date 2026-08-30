require "json"
require "../src/facet"

alias F = Facet::Compiler

record FixtureCase,
  source : String,
  accepted : Bool,
  ast : String?,
  error : String?,
  line : Int32?,
  column : Int32? do
  include JSON::Serializable
end

record Mismatch,
  index : Int32,
  source : String,
  expected_message : String?,
  actual_message : String,
  expected_location : Tuple(Int32?, Int32?),
  actual_location : Tuple(Int32, Int32)

fixture_path = ARGV[0]? || File.expand_path("../spec/fixtures/crystal_1_21_parser.jsonl", __DIR__)
lines = File.read_lines(fixture_path)
lines.shift
cases = lines.map { |line| FixtureCase.from_json(line) }

rejected = 0
message_matches = 0
location_matches = 0
full_matches = 0
mismatches = [] of Mismatch

cases.each_with_index do |fixture_case, index|
  next if fixture_case.accepted
  rejected += 1

  source = F::Source.new(fixture_case.source, "diagnostic_parity_#{index}")
  parser = F::Parser.new(source)
  parser.parse_file
  diagnostic = parser.diagnostics.first?
  unless diagnostic
    mismatches << Mismatch.new(
      index: index,
      source: fixture_case.source,
      expected_message: fixture_case.error,
      actual_message: "<missing diagnostic>",
      expected_location: {fixture_case.line, fixture_case.column},
      actual_location: {0, 0},
    )
    next
  end

  lexer = F::Lexer.new(source)
  actual_location = lexer.line_and_column(diagnostic.span.start)
  message_match = fixture_case.error == diagnostic.message
  location_match = fixture_case.line == actual_location[0] && fixture_case.column == actual_location[1]
  message_matches += 1 if message_match
  location_matches += 1 if location_match
  full_matches += 1 if message_match && location_match

  unless message_match && location_match
    mismatches << Mismatch.new(
      index: index,
      source: fixture_case.source,
      expected_message: fixture_case.error,
      actual_message: diagnostic.message,
      expected_location: {fixture_case.line, fixture_case.column},
      actual_location: actual_location,
    )
  end
end

puts "rejected=#{rejected} exact_messages=#{message_matches} exact_locations=#{location_matches} exact_message_and_location=#{full_matches} mismatches=#{mismatches.size}"
mismatches.first(50).each do |mismatch|
  puts "\n##{mismatch.index}: #{mismatch.source.dump}"
  puts "expected #{mismatch.expected_location}: #{mismatch.expected_message}"
  puts "actual   #{mismatch.actual_location}: #{mismatch.actual_message}"
end

strict = ENV["STRICT_PARSER_DIAGNOSTICS"]? == "1"
exit(strict && !mismatches.empty? ? 1 : 0)
