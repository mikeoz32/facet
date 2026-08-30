require "compiler/crystal/syntax"
require "../src/facet"

alias F = Facet::Compiler

# Directly compares Facet's decoded source-backed payloads with the values
# produced by Crystal::Parser, then checks the complete char-escape acceptance
# matrix. Keep these cases focused on syntax with deterministic literal values;
# interpolated literals are structural containers rather than static values.
cases = [
  %q("plain"),
  %q("\n\r\t\b\f\v\e\a"),
  %q("\0\1\101\377"),
  %q("\x00\x41\x7f"),
  %q("\u0041\u{42 43 1F600}"),
  %q("\"\\\#\%\(\)\[\]\{\}\<\>\|"),
  %q("\h"),
  "%q(raw\\n)",
  "%Q(escaped\\n)",
  "%(escaped\\n)",
  "%q{nested {body}}",
  %q('a'),
  %q('\n'),
  %q('\u{1F600}'),
  %q(:symbol),
  %q(:"symbol\n"),
  %q(/plain/),
  %q(/\s\/\n/),
  %q(%r{\s\/\n}),
  "<<-TEXT\n  one\\n\n    two\n  TEXT\n",
  "<<-'TEXT'\n  one\\n\n  TEXT\n",
]

private def upstream_value(node : Crystal::ASTNode) : String?
  case node
  when Crystal::StringLiteral
    node.value
  when Crystal::CharLiteral
    node.value.to_s
  when Crystal::SymbolLiteral
    node.value
  when Crystal::RegexLiteral
    value = node.value
    value.is_a?(Crystal::StringLiteral) ? value.value : nil
  else
    nil
  end
end

mismatches = [] of Tuple(String, String, String)
cases.each do |code|
  upstream = upstream_value(Crystal::Parser.parse(code))
  next unless upstream

  parser = F::Parser.new(F::Source.new(code))
  ast = parser.parse_file
  abort "Facet rejected #{code.inspect}: #{parser.diagnostics.map(&.message)}" unless parser.diagnostics.empty?
  expressions = ast.children(ast.root)[0]
  node_id = ast.children(expressions)[0]
  actual = ast.decoded_literal_string(node_id)
  mismatches << {code, upstream, actual} unless upstream == actual
end

acceptance_mismatches = [] of Tuple(String, Bool, Bool, String)
escape_tails = ["a", "b", "e", "f", "n", "r", "t", "v", "0", "1", "7", "8", "q", "x41", "u0041", "u{41}", "'", "\"", "\\", "#", "%", "(", ")", "[", "]", "{", "}", "<", ">", "|"]
escape_tails.each do |tail|
  code = "'\\#{tail}'"
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
    acceptance_mismatches << {code, upstream, facet, detail}
  end
end

puts "value_cases=#{cases.size} value_mismatches=#{mismatches.size} " \
     "acceptance_cases=#{escape_tails.size} acceptance_mismatches=#{acceptance_mismatches.size}"
mismatches.each do |code, expected, actual|
  puts "#{code.inspect}\n  Crystal: #{expected.inspect}\n  Facet:   #{actual.inspect}"
end
acceptance_mismatches.each do |code, upstream, facet, detail|
  puts "#{code.inspect} upstream=#{upstream} facet=#{facet} #{detail}"
end
exit 1 unless mismatches.empty? && acceptance_mismatches.empty?
