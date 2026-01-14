require "./spec_helper"

describe Facet::Compiler::MacroExpander do
  it "expands macro expressions with literal strings" do
    src = Facet::Compiler::Source.new("puts {{ \"hi\" }}")
    ast = Facet::Compiler::Parser.new(src).parse_file

    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.kind.should eq(Facet::Compiler::SourceKind::Virtual)
    expanded.source.expanded_from.should_not be_nil
    expanded.source.text.should eq("puts hi")
    expander.diagnostics.should be_empty
  end

  it "expands macro control if/else" do
    src = Facet::Compiler::Source.new("{% if true %}1{% else %}2{% end %}")
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq("1")
  end

  it "repeats macro for body for array literals" do
    src = Facet::Compiler::Source.new("{% for x in [1,2,3] %}x{% end %}")
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq("xxx")
  end

  it "expands across passes until macros are gone" do
    src = Facet::Compiler::Source.new("{{ \"{{ 1 }}\" }}")
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq("1")
    expander.diagnostics.should be_empty
  end

  it "emits diagnostic when expansion exceeds max passes" do
    src = Facet::Compiler::Source.new("{{ \"{{ 1 }}\" }}")
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new(1)
    expanded = expander.expand(ast)

    expanded.source.text.should eq("{{ 1 }}")
    expander.diagnostics.any? { |d| d.message.includes?("max passes") }.should be_true
  end
end
