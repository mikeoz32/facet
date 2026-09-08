require "./support/upstream_semantic_parity"

fixture_path = ARGV[0]? || File.expand_path("../spec/fixtures/crystal_1_21_semantic_contracts.jsonl", __DIR__)
baseline_path = ARGV[1]? || File.expand_path("../spec/fixtures/crystal_1_21_semantic_supported.txt", __DIR__)
deferred_path = baseline_path.sub("_supported.txt", "_deferred.tsv")
write = ARGV.includes?("--write")

header, cases = UpstreamSemanticParity.load(fixture_path)
results = cases.map { |fixture| UpstreamSemanticParity.evaluate(fixture) }
matching = cases.each_with_index.compact_map do |fixture, index|
  results[index].matches?(fixture) ? index : nil
end.to_a

if write
  File.write(baseline_path, matching.join('\n') + (matching.empty? ? "" : "\n"))
  deferred = cases.each_with_index.compact_map do |fixture, index|
    result = results[index]
    next if result.matches?(fixture)
    "#{index}\t#{UpstreamSemanticParity.classification(fixture, result)}\t#{fixture.id}"
  end.to_a
  File.write(deferred_path, deferred.join('\n') + (deferred.empty? ? "" : "\n"))
end

supported = File.exists?(baseline_path) ? File.read_lines(baseline_path).reject(&.blank?).map(&.to_i) : [] of Int32
regressions = supported.reject { |index| matching.includes?(index) }
puts "crystal=#{header.crystal_version} captured=#{cases.size} matching=#{matching.size} supported=#{supported.size} deferred=#{cases.size - supported.size} regressions=#{regressions.size}"
unless regressions.empty?
  regressions.first(20).each { |index| puts "regression #{index}: #{cases[index].id}" }
  exit 1
end
