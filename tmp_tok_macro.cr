require "./src/facet/compiler"
include Facet::Compiler
src = Source.new("macro foo\n{%\nif 1\n2\nelse\n3\nend\n%}end")
lex = Lexer.new(src)
while true
  tok = lex.next_token
  text = src.text[tok.span.start, tok.span.length]?
  puts "#{tok.kind}: #{text.inspect}"
  break if tok.kind == TokenKind::Eof
end
