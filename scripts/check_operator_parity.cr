require "compiler/crystal/syntax"
require "../src/facet"

alias F = Facet::Compiler

# Generated acceptance matrix for Crystal operator tokens in infix, prefix,
# postfix, and parenthesized positions. This catches context-sensitive false
# positives such as bare `&`, implicit-dot syntax outside `case`, and compound
# assignments to locals that have not been defined yet.
operators = [
  "+", "-", "*", "/", "//", "%", "**", "&", "|", "^", "~", "!",
  "<<", ">>", "&+", "&-", "&*", "&**",
  "==", "===", "!=", "=~", "!~", "<", "<=", ">", ">=", "<=>",
  "&&", "||", "..", "...", "=>", "=", "+=", "-=", "*=", "/=", "//=", "%=",
  "|=", "&=", "^=", "**=", "<<=", ">>=", "&&=", "||=", "&+=", "&-=", "&*=", "&**=",
  ".", "?.", "::", "?", ":",
]
forms = operators.flat_map do |operator|
  ["left #{operator} right", "#{operator} value", "value#{operator}", "(left #{operator} right)"]
end
mismatches = [] of Tuple(String, Bool, Bool, String)

forms.each do |code|
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

puts "cases=#{forms.size} mismatches=#{mismatches.size}"
mismatches.first(100).each { |code, upstream, facet, detail| puts "#{code.inspect} upstream=#{upstream} facet=#{facet} #{detail}" }
exit 1 unless mismatches.empty?
