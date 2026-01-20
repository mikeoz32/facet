require "./src/facet/compiler/source"
require "./src/facet/compiler/lexer"
require "./src/facet/compiler/diagnostic"
file = ARGV[0]
src = Facet::Compiler::Source.new(File.read(file), file)
lex = Facet::Compiler::Lexer.new(src)
lex.tokenize_all
lex.diagnostics.first(5).each_with_index do |d,i|
  puts "#{i}: #{d.message} @#{d.span.start}"
end
