require "./support/upstream_macro_semantic_parity"

fixture_path = ARGV[0]? || File.expand_path(
  "../spec/fixtures/crystal_1_21_macro_semantic_full_events.jsonl",
  __DIR__
)
header, cases = UpstreamSemanticMacroParity.load(fixture_path)

exact = 0
equivalent_with_diagnostics = 0
output_mismatch_without_diagnostics = 0
diagnostic_counts = Hash(String, Int32).new(0)
macro_counts = Hash(String, Int32).new(0)
kind_counts = Hash(String, Int32).new(0)
first_indices = Hash(String, Array(Int32)).new { |hash, key| hash[key] = [] of Int32 }
macro_first_indices = Hash(String, Array(Int32)).new { |hash, key| hash[key] = [] of Int32 }

cases.each_with_index do |fixture_case, index|
  result = UpstreamSemanticMacroParity.expand(fixture_case, index)
  if result.matches?(fixture_case)
    exact += 1
    next
  end

  macro_name = fixture_case.macro_name || "<inline>"
  macro_counts[macro_name] += 1
  macro_first_indices[macro_name] << index if macro_first_indices[macro_name].size < 8
  kind_counts[fixture_case.kind] += 1
  if expected = fixture_case.expected
    if UpstreamSemanticMacroParity.equivalent_output?(result.actual, expected)
      equivalent_with_diagnostics += 1 unless result.diagnostics.empty?
    elsif result.diagnostics.empty?
      output_mismatch_without_diagnostics += 1
    end
  end
  summaries = result.diagnostics.empty? ? ["<none>"] : result.diagnostics.map(&.lines.first.to_s).uniq
  summaries.each do |summary|
    summary = summary[0, Math.min(summary.size, 160)]
    diagnostic_counts[summary] += 1
    first_indices[summary] << index if first_indices[summary].size < 8
  end
end

mismatches = cases.size - exact
puts "events=#{header.event_count} exact=#{exact} mismatches=#{mismatches} equivalent_with_diagnostics=#{equivalent_with_diagnostics} output_mismatch_without_diagnostics=#{output_mismatch_without_diagnostics}"
puts "mismatch kinds: #{kind_counts}"
puts "top diagnostics:"
diagnostic_counts.to_a.sort_by { |entry| -entry[1] }.first(30).each do |diagnostic, count|
  puts "#{count}\t#{first_indices[diagnostic].join(',')}\t#{diagnostic}"
end
puts "top macro names:"
macro_counts.to_a.sort_by { |entry| -entry[1] }.first(30).each do |macro_name, count|
  puts "#{count}\t#{macro_first_indices[macro_name].join(',')}\t#{macro_name}"
end
