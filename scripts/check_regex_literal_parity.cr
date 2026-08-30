require "compiler/crystal/syntax"
require "../src/facet"

alias F = Facet::Compiler

# Generated acceptance matrix for slash and percent regex option suffixes.
options = [""] + ('a'..'z').map(&.to_s) + ('A'..'Z').map(&.to_s) + ["i", "m", "x", "imx", "imximx", "ixm", "zix"]
sources = options.flat_map { |option| ["/body/#{option}", "%r(body)#{option}"] }
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
