require "./src/facet/compiler"
include Facet::Compiler
src = Source.new("foo.[]=(1)", "tmp")
lex = Lexer.new(src)
loop do
  tok = lex.next_token
  text = src.text[tok.span.start, tok.span.length]?
  puts "#{tok.kind}: '#{text}'"
  break if tok.kind == TokenKind::Eof
end
