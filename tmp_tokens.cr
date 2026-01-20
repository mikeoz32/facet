require "./src/facet"
code = <<-CODE
lib LibC
  type Void = Void*
  struct Stat
    st_size : SizeT
  end
  fun puts(str : UInt8*) : Int32
end
CODE
lex = Facet::Compiler::Lexer.new(Facet::Compiler::Source.new(code, "tmp"))
loop do
  tok = lex.next_token
  puts "#{tok.kind} #{tok.span.start}-#{tok.span.finish}"
  break if tok.kind == Facet::Compiler::TokenKind::Eof
end
