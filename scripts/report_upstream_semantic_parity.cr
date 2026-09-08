require "./support/upstream_semantic_parity"

fixture_path = ARGV[0]? || File.expand_path("../spec/fixtures/crystal_1_21_semantic_contracts.jsonl", __DIR__)
limit = (ARGV[1]? || "30").to_i
_, cases = UpstreamSemanticParity.load(fixture_path)
counts = Hash(String, Int32).new(0)
shown = 0

cases.each_with_index do |fixture, index|
  result = UpstreamSemanticParity.evaluate(fixture)
  reason = UpstreamSemanticParity.classification(fixture, result)
  counts[reason] += 1
  next if reason == "supported" || shown >= limit
  shown += 1
  puts "#{index} #{reason} #{fixture.id} expected=#{fixture.expected_type || fixture.expected_code || fixture.expected_error} actual=#{result.actual_type || result.actual_code}"
end

puts counts.keys.sort.map { |key| "#{key}=#{counts[key]}" }.join(' ')
