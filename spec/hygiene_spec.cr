require "./spec_helper"

describe Facet::Compiler::Hygiene do
  it "gensyms unique symbols" do
    symbols = Facet::Compiler::SymbolTable.new
    hygiene = Facet::Compiler::Hygiene.new

    a = hygiene.gensym("tmp", symbols)
    b = hygiene.gensym("tmp", symbols)
    a.should_not eq(b)
    symbols[a].should_not eq(symbols[b])
  end
end
