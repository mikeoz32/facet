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
    expander = Facet::Compiler::MacroExpander.new(nil, 1)
    expanded = expander.expand(ast)

    expanded.source.text.should eq("{{ 1 }}")
    expander.diagnostics.any? { |d| d.message.includes?("max passes") }.should be_true
  end

  it "expands macro defs across files" do
    src_def = Facet::Compiler::Source.new("macro foo\n1\nend")
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file

    src_use = Facet::Compiler::Source.new("{{ foo }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand_all([ast_use], index)

    expanded.first.source.text.should eq("1")
  end

  it "binds macro params with defaults and named/double splats" do
    src_def = Facet::Compiler::Source.new(<<-CR)
      macro foo(a = 1 + 2, *rest, **opts)
        A={{a}} REST={{rest}} OPTS={{opts}}
      end
    CR
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file

    src_use = Facet::Compiler::Source.new("{{ foo(10, 20, bar: 30) }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand_all([ast_use], index)

    expanded.first.source.text.should contain("A=10")
    expanded.first.source.text.should contain("REST=20")
    expanded.first.source.text.should contain("OPTS=bar=30")
  end

  it "expands ranges and array/hash macro values" do
    src_def = Facet::Compiler::Source.new(<<-CR)
      macro foo(a = 1..3, b = [1,2], c = {"x" => 1})
        A={{a}} B={{b}} C={{c}}
      end
    CR
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file

    src_use = Facet::Compiler::Source.new("{{ foo }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand_all([ast_use], index)

    expanded.first.source.text.should contain("A=1,2,3")
    expanded.first.source.text.should contain("B=1,2")
    expanded.first.source.text.should contain("C=x=1")
  end

  it "caches macro def expansions by args and body" do
    src_def = Facet::Compiler::Source.new("macro foo(x)\n{{x}}\nend")
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file

    src_use = Facet::Compiler::Source.new("{{ foo(1) }} {{ foo(1) }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expander.expand_all([ast_use], index)
    expander.expand_all([ast_use], index)

    expander.cache_hits.should be > 0
  end

  it "provides gensym for unique identifiers" do
    src_def = Facet::Compiler::Source.new(<<-CR)
      macro foo
        {{ gensym("x") }} {{ gensym("x") }} {{ gensym }}
      end
    CR
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file
    src_use = Facet::Compiler::Source.new("{{ foo }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand_all([ast_use], index)
    tokens = expanded.first.source.text.split
    tokens.size.should eq(3)
    tokens.uniq.size.should eq(3)
  end
end
