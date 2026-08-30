require "./spec_helper"

describe Facet::Compiler::Indexer do
  it "indexes macro defs by name" do
    source = Facet::Compiler::Source.new("macro foo; end\nmacro foo; end\nmacro bar; end")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    index = Facet::Compiler::Indexer.index_macros(ast)
    foo = "foo"
    bar = "bar"

    index.macros[foo].size.should eq(2)
    index.macros[bar].size.should eq(1)
  end

  it "can append into existing index" do
    src1 = Facet::Compiler::Source.new("macro foo; end")
    ast1 = Facet::Compiler::Parser.new(src1).parse_file

    src2 = Facet::Compiler::Source.new("macro foo; end")
    ast2 = Facet::Compiler::Parser.new(src2).parse_file

    index = Facet::Compiler::Indexer.index_macros(ast1)
    Facet::Compiler::Indexer.index_macros(ast2, index)

    index.macros["foo"].size.should eq(2)
  end

  it "merges indexes across files" do
    src1 = Facet::Compiler::Source.new("macro foo; end")
    ast1 = Facet::Compiler::Parser.new(src1).parse_file

    src2 = Facet::Compiler::Source.new("macro bar; end")
    ast2 = Facet::Compiler::Parser.new(src2).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast1, ast2])
    foo = "foo"
    bar = "bar"

    index.macros[foo].size.should eq(1)
    index.macros[bar].size.should eq(1)
  end

  it "indexes macros by lexical type scope" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro root_macro; end
      class Outer
        macro scoped; end
        module Nested
          macro deep; end
        end
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)

    index.macros_for("root_macro", "Outer").not_nil!.map(&.scope).should eq([""])
    index.macros_for("scoped", "Outer::Nested").not_nil!.map(&.scope).should eq(["Outer"])
    index.macros_for("deep", "Outer::Nested").not_nil!.map(&.scope).should eq(["Outer::Nested"])
    index.macros_for("scoped", "Other").should be_nil
  end

  it "does not index macro definitions emitted by another macro template" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro outer
        macro generated
        end
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)

    index.macros.has_key?("outer").should be_true
    index.macros.has_key?("generated").should be_false
  end
end
