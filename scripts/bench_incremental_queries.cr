require "../src/facet"

file_count = (ARGV[0]? || "1000").to_i
manager = Facet::Compiler::SourceManager.new
file_ids = Array(Facet::Compiler::FileId).new(file_count)
file_count.times do |index|
  file_ids << manager.add(
    "class Type#{index}\n  def value; #{index}; end\nend\n",
    "type_#{index}.cr"
  )
end
queries = Facet::Compiler::QueryDb.new(manager)

cold = Time.measure { file_ids.each { |file_id| queries.syntax(file_id) } }
warm = Time.measure { file_ids.each { |file_id| queries.syntax(file_id) } }

edited = file_ids[file_count // 2]
queries.update(edited, "class Edited\n  def value; 1; end\nend\n")
single_edit = Time.measure { file_ids.each { |file_id| queries.syntax(file_id) } }

provider = manager.add("macro answer; 42; end", "provider.cr")
use = manager.add("{{ answer }}", "use.cr")
unrelated = manager.add("value = 1", "unrelated.cr")
expansion_cold = Time.measure { queries.expand(use) }
queries.update(unrelated, "value = 2")
expansion_unrelated_edit = Time.measure { queries.expand(use) }

puts "files=#{file_count}"
puts "syntax_cold_ms=#{cold.total_milliseconds.round(3)}"
puts "syntax_warm_ms=#{warm.total_milliseconds.round(3)}"
puts "syntax_after_one_edit_ms=#{single_edit.total_milliseconds.round(3)}"
puts "expansion_cold_ms=#{expansion_cold.total_milliseconds.round(3)}"
puts "expansion_after_unrelated_edit_ms=#{expansion_unrelated_edit.total_milliseconds.round(3)}"
puts "parse_executions=#{queries.stats.parse_executions} parse_hits=#{queries.stats.parse_cache_hits}"
puts "syntax_executions=#{queries.stats.syntax_executions} syntax_hits=#{queries.stats.syntax_cache_hits}"
puts "expand_executions=#{queries.stats.expand_executions} expand_hits=#{queries.stats.expand_cache_hits}"

# Keep the provider live in optimized builds and make the expected footprint
# explicit in benchmark output.
puts "macro_provider_file=#{provider}"
