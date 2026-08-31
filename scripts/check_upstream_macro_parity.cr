require "./support/upstream_macro_parity"

fixture_path = ARGV[0]? || File.expand_path("../spec/fixtures/crystal_1_21_macro.jsonl", __DIR__)
baseline_path = ARGV[1]?
header, cases = UpstreamMacroParity.load(fixture_path)

matched = [] of Int32
mismatches = [] of {Int32, UpstreamMacroFixtureCase, UpstreamMacroParityResult}
cases.each_with_index do |fixture_case, index|
  result = UpstreamMacroParity.expand(fixture_case, index)
  if result.matches?(fixture_case)
    matched << index
  else
    mismatches << {index, fixture_case, result}
  end
end

if baseline_path
  File.open(baseline_path, "w") do |io|
    matched.each { |index| io.puts index }
  end
end

puts "crystal_version=#{header.crystal_version} examples=#{header.example_count} semantic_examples=#{header.semantic_example_count} assertions=#{header.assertion_count} eligible=#{cases.size} excluded=#{header.excluded_count} exact=#{matched.size} mismatches=#{mismatches.size} parity=#{(matched.size * 100.0 / cases.size).round(2)}%"
mismatches.first(20).each do |index, fixture_case, result|
  actual = result.actual || "<no expansion>"
  diagnostic = result.diagnostics.first?
  output_diagnostic = result.output_diagnostics.first?
  puts "MISMATCH #{index} #{fixture_case.source_file}:#{fixture_case.line} expected=#{fixture_case.expected.dump} actual=#{actual.dump}#{diagnostic ? " diagnostic=#{diagnostic.dump}" : ""}#{output_diagnostic ? " output_diagnostic=#{output_diagnostic.dump}" : ""}"
end
