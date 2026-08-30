require "json"
require "compiler/crystal/syntax"
require "./support/facet_ast_normalizer"
require "./support/upstream_ast_normalizer"

record AstShapeFixtureCase,
  source : String,
  accepted : Bool,
  ast : String?,
  error : String?,
  line : Int32?,
  column : Int32? do
  include JSON::Serializable
end

fixture_path = ARGV[0]? || File.expand_path("../spec/fixtures/crystal_1_21_parser.jsonl", __DIR__)
lines = File.read_lines(fixture_path)
lines.shift
cases = lines.map { |line| AstShapeFixtureCase.from_json(line) }

matched = 0
mismatched = 0
oracle_drift = 0
unsupported = Hash(String, Int32).new(0)
unsupported_samples = Hash(String, String).new
differences = Hash(String, Int32).new(0)
difference_samples = Hash(String, String).new
facet_diagnostics = Hash(String, Int32).new(0)
facet_diagnostic_samples = Hash(String, String).new
samples = [] of String

cases.each_with_index do |fixture_case, index|
  next unless fixture_case.accepted

  upstream = Crystal::Parser.parse(fixture_case.source)
  if upstream.inspect != fixture_case.ast
    oracle_drift += 1
    samples << "oracle drift ##{index}: #{fixture_case.source.inspect}" if samples.size < 20
    next
  end

  source = Facet::Compiler::Source.new(fixture_case.source, "ast_shape_#{index}")
  parser = Facet::Compiler::Parser.new(source)
  begin
    facet = parser.parse_file
  rescue error
    STDERR.puts "Facet parser crash ##{index}: #{fixture_case.source.inspect}"
    raise error
  end
  unless parser.diagnostics.empty?
    message = parser.diagnostics.first.message
    facet_diagnostics[message] += 1
    facet_diagnostic_samples[message] ||= fixture_case.source
    samples << "Facet diagnostic ##{index}: #{message}" if samples.size < 20
    mismatched += 1
    next
  end

  begin
    expected = UpstreamAstNormalizer.normalize(upstream)
    actual = FacetAstNormalizer.normalize(facet)
    if expected == actual
      matched += 1
    else
      mismatched += 1
      difference = expected.first_difference(actual) || "unknown difference"
      category = difference.gsub(/\[\d+\]/, "[]")
      differences[category] += 1
      difference_samples[category] ||= fixture_case.source
      if samples.size < 20
        samples << String.build do |io|
          io << "shape mismatch ##{index}: " << fixture_case.source.inspect << '\n'
          io << "  upstream: " << expected.render << '\n'
          io << "  facet:    " << actual.render
        end
      end
    end
  rescue error : UnsupportedSemanticAst
    unsupported[error.node_kind] += 1
    unsupported_samples[error.node_kind] ||= fixture_case.source
  end
end

comparable = matched + mismatched
puts "accepted=#{cases.count(&.accepted)} comparable=#{comparable} matched=#{matched} " \
     "mismatched=#{mismatched} unsupported=#{unsupported.values.sum} oracle_drift=#{oracle_drift}"

unless unsupported.empty?
  puts "\nUnsupported first-hit nodes:"
  unsupported.to_a.sort_by { |name, count| {-count, name} }.each do |name, count|
    puts "  #{count.to_s.rjust(5)}  #{name}  #{unsupported_samples[name].inspect}"
  end
end

unless facet_diagnostics.empty?
  puts "\nUnexpected Facet diagnostics on accepted inputs:"
  facet_diagnostics.to_a.sort_by { |message, count| {-count, message} }.each do |message, count|
    puts "  #{count.to_s.rjust(5)}  #{message}  #{facet_diagnostic_samples[message].inspect}"
  end
end

unless differences.empty?
  puts "\nFirst differences:"
  differences.to_a.sort_by { |difference, count| {-count, difference} }.first(30).each do |difference, count|
    puts "  #{count.to_s.rjust(5)}  #{difference}  #{difference_samples[difference].inspect}"
  end
end

unless samples.empty?
  puts "\nSamples:"
  samples.each { |sample| puts sample }
end

exit 1 unless oracle_drift == 0 && unsupported.empty? && mismatched == 0
