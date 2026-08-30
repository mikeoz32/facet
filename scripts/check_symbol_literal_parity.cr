require "compiler/crystal/syntax"
require "../src/facet"

alias F = Facet::Compiler

# Generated acceptance matrix for unquoted operator/identifier symbols and
# quoted symbol spellings, including invalid numeric and punctuation bodies.
bodies = [
  "foo", "foo?", "foo!", "foo=", "Foo", "_foo", "他", "if", "self",
  "+", "-", "*", "**", "/", "//", "%", "&", "|", "^", "~", "!",
  "==", "===", "!=", "=~", "!~", "<", "<=", ">", ">=", "<=>", "<<", ">>",
  "[]", "[]=", "[]?", "&+", "&-", "&*", "&**",
  "", "1", "01", "foo bar", "@foo", "@@foo", "$foo", ".", ",", ":", "::",
]
quoted = [
  %q(:"foo"), %q(:"foo bar"), %q(:"\n"), %q(:"\x41"), %q(:"\u0041"),
  %q(:""), %q(:"unterminated),
]
sources = bodies.map { |body| ":#{body}" } + quoted
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
