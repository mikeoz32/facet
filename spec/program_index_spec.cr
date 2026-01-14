require "./spec_helper"

describe Facet::Compiler::Indexer do
  it "indexes macro defs by name" do
    source = Facet::Compiler::Source.new("macro foo; end\nmacro foo; end\nmacro bar; end")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    index = Facet::Compiler::Indexer.index_macros(ast)
    foo_id = ast.arena.symbols.intern("foo")
    bar_id = ast.arena.symbols.intern("bar")

    index.macros[foo_id].size.should eq(2)
    index.macros[bar_id].size.should eq(1)
  end

  it "can append into existing index" do
    src1 = Facet::Compiler::Source.new("macro foo; end")
    ast1 = Facet::Compiler::Parser.new(src1).parse_file

    src2 = Facet::Compiler::Source.new("macro foo; end")
    ast2 = Facet::Compiler::Parser.new(src2).parse_file

    index = Facet::Compiler::Indexer.index_macros(ast1)
    Facet::Compiler::Indexer.index_macros(ast2, index)

    foo_id = ast1.arena.symbols.intern("foo")
    index.macros[foo_id].size.should eq(2)
  end
end
