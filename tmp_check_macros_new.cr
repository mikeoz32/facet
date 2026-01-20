require "./src/facet/compiler"
include Facet::Compiler
cases = [
  "macro foo; end",
  "macro [];end",
  %(macro foo; 1 + 2; end),
  %(macro foo(x); 1 + 2; end),
  %(macro foo(x)\n 1 + 2; end),
  "macro foo; 1 + 2 {{foo}} 3 + 4; end",
  "macro foo; 1 + 2 {{ foo }} 3 + 4; end",
  "macro foo;bar{% for x in y %}body{% end %}baz;end",
  "macro foo;bar{% for x, y in z %}body{% end %}baz;end",
  "macro foo;bar{% if x %}body{% end %}baz;end",
  "macro foo;bar{% if x %}body{% else %}body2{%end%}baz;end",
  "macro foo;bar{% if x %}body{% elsif y %}body2{%end%}baz;end",
  "macro foo;bar{% if x %}body{% elsif y %}body2{% else %}body3{%end%}baz;end",
  "macro foo;bar{% unless x %}body{% end %}baz;end",
  "macro foo;bar{% for x in y %}\\  \n   body{% end %}baz;end",
  "macro foo;bar{% for x in y %}\\  \n   body{% end %}\\   baz;end",
  "macro foo; 1 + 2 {{foo}}\\ 3 + 4; end",
  "macro foo(\na = 0\n)\nend",
  "macro foo;{% verbatim do %}1{% foo %}2{% end %};end",
  "macro foo\n{%\nif 1\n2\nelse\n3\nend\n%}end",
  "macro foo\neenum\nend",
  "macro foo\n'\\''\nend",
  "macro foo\n'\\\\'\nend",
  %(macro foo\n"\\'"\nend),
  %(macro foo\n"\\\\"\nend),
  "macro foo;bar(end: 1);end",
]
cases.each do |code|
  parser = Parser.new(Source.new(code, "m"))
  parser.parse_file
  if diag = parser.diagnostics.first?
    puts "FAIL: #{code.inspect} -> #{diag.message} @ #{diag.span.start}"
    break
  end
end
