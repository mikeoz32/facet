require "./support/upstream_macro_semantic_parity"

fixture_path = ARGV[0]? || File.expand_path("../spec/fixtures/crystal_1_21_macro_semantic_events.jsonl", __DIR__)
baseline_path = ARGV[1]?
header, cases = UpstreamSemanticMacroParity.load(fixture_path)

matched = [] of Int32
mismatches = [] of {Int32, UpstreamSemanticMacroCase, UpstreamSemanticMacroResult}
cases.each_with_index do |fixture_case, index|
  result = UpstreamSemanticMacroParity.expand(fixture_case, index)
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

puts "crystal_version=#{header.crystal_version} examples=#{header.semantic_example_count} events=#{header.event_count} calls=#{header.call_count} inline=#{header.inline_count} exact=#{matched.size} mismatches=#{mismatches.size} parity=#{(matched.size * 100.0 / cases.size).round(2)}%"
display_limit = ENV["FACET_MACRO_MISMATCH_LIMIT"]?.try(&.to_i?) || 30
mismatches.first(display_limit).each do |index, fixture_case, result|
  expected = fixture_case.expected || fixture_case.expected_error_message || "<missing oracle>"
  actual = result.actual || "<no expansion>"
  puts "MISMATCH #{index} #{fixture_case.kind} scope=#{fixture_case.scope} invocation=#{fixture_case.invocation.lines.first?.to_s.dump} expected=#{expected.strip.dump} actual=#{actual.strip.dump} diagnostics=#{result.diagnostics.inspect}"
end
