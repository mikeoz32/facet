require "./src/facet"
source = Facet::Compiler::Source.new("begin 1 end / 2", "tmp")
lexer = Facet::Compiler::Lexer.new(source)
loop do
  tok = lexer.next_token
  puts "#{tok.kind} @ #{tok.span.start}-#{tok.span.finish}"
  break if tok.kind == Facet::Compiler::TokenKind::Eof
end
