require "compiler/crystal/syntax"
require "../src/facet"

alias F = Facet::Compiler

# Crystal heredocs require `<<-`; only single-quoted labels are supported.
headers = [
  {"<<-TEXT", "TEXT"},
  {"<<-'TEXT'", "TEXT"},
  {"<<-\"TEXT\"", "TEXT"},
  {"<<~TEXT", "TEXT"},
  {"<<TEXT", "TEXT"},
  {"<<'TEXT'", "TEXT"},
  {"<<\"TEXT\"", "TEXT"},
  {"<<+TEXT", "TEXT"},
]
indents = ["", "  "]
sources = headers.flat_map do |header, label|
  indents.map { |indent| "#{header}\n#{indent}body\n#{indent}#{label}\n" }
end
mismatches = [] of Tuple(String, Bool, Bool, String)

sources.each do |code|
  upstream = begin
    Crystal::Parser.parse(code)
    true
  rescue Crystal::SyntaxException
    false
  end
  parser = F::Parser.new(F::Source.new(code))
  parser.parse_file
  facet = parser.diagnostics.empty?
  if upstream != facet
    detail = parser.diagnostics.first?.try(&.message) || "accepted"
    mismatches << {code, upstream, facet, detail}
  end
end

puts "cases=#{sources.size} mismatches=#{mismatches.size}"
mismatches.each { |code, upstream, facet, detail| puts "#{code.inspect} upstream=#{upstream} facet=#{facet} #{detail}" }
exit 1 unless mismatches.empty?
