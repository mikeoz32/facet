require "./src/facet"
src = Facet::Compiler::Source.new("foo ->bar=", "tmp")
lex = Facet::Compiler::Lexer.new(src)
loop do
  tok = lex.next_token
  puts "#{tok.kind}: #{tok.span.start}-#{tok.span.finish}"
  break if tok.kind == Facet::Compiler::TokenKind::Eof
end
parser = Facet::Compiler::Parser.new(src)
parser.parse_file
puts "diagnostics: #{parser.diagnostics.map(&.message)}"
