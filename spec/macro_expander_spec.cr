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

  it "uses decoded string values in macro expressions" do
    src = Facet::Compiler::Source.new(%q(value = {{ "a\x62\u0063" }}))
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq("value = abc")
    expander.diagnostics.should be_empty
  end

  it "expands macro control if/else" do
    src = Facet::Compiler::Source.new("{% if true %}1{% else %}2{% end %}")
    ast = Facet::Compiler::Parser.new(src).parse_file
    ast.diagnostics.should be_empty
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

  it "binds macro for values and array indices while expanding the body" do
    src = Facet::Compiler::Source.new("{% for x, i in [10,20] %}[{{i}}, {{x}}]\n{% end %}")
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq("[0, 10]\n[1, 20]\n")
    expander.diagnostics.should be_empty
  end

  it "binds macro for hash keys and values" do
    src = Facet::Compiler::Source.new(%q({% for key, value in {"a" => 1, "b" => 2} %}{{key}}={{value}};{% end %}))
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq("a=1;b=2;")
    expander.diagnostics.should be_empty
  end

  it "iterates tuple, named tuple, and range macro values" do
    src = Facet::Compiler::Source.new(<<-CR)
      {% for value in {1, 2} %}puts {{value}}
      {% end %}{% for key, value in {one: 3, two: 4} %}{{key}} = {{value}}
      {% end %}{% for value in 5...7 %}puts {{value}}
      {% end %}
    CR
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    lines = expanded.source.text.lines.map(&.strip).reject(&.empty?)
    lines.should eq(["puts 1", "puts 2", "one = 3", "two = 4", "puts 5", "puts 6"])
    expander.diagnostics.should be_empty
  end

  it "uses Crystal truthiness for zero and empty strings" do
    src = Facet::Compiler::Source.new(%q({% if 0 %}zero{% else %}bad{% end %}{% if "" %}empty{% else %}bad{% end %}))
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq("zeroempty")
    expander.diagnostics.should be_empty
  end

  it "evaluates macro assignments and preserves mutations across loop iterations" do
    src = Facet::Compiler::Source.new(<<-CR)
      {% total = 0 %}
      {% for value in [1, 2, 3] %}
        {% total += value %}
      {% end %}
      {% enabled = false %}
      {% enabled ||= total == 6 %}
      answer = {{total}}
      enabled = {{enabled}}
    CR
    ast = Facet::Compiler::Parser.new(src).parse_file
    ast.diagnostics.should be_empty
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should contain("answer = 6")
    expanded.source.text.should contain("enabled = true")
    expander.diagnostics.should be_empty
  end

  it "evaluates common macro collection and string methods" do
    src = Facet::Compiler::Source.new(<<-CR)
      {% words = ["one", "two"] %}
      {% mapping = {one: 1, two: 2} %}
      {% if words.size == 2 && words.first.upcase == "ONE" && "foobar".starts_with?("foo") %}
        picked = {{words[1]}}
        joined = {{words.join("-")}}
        mapped = {{mapping["two"]}}
      {% else %}
        bad
      {% end %}
    CR
    ast = Facet::Compiler::Parser.new(src).parse_file
    ast.diagnostics.should be_empty
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should contain("picked = two")
    expanded.source.text.should contain("joined = one-two")
    expanded.source.text.should contain("mapped = 2")
    expanded.source.text.should_not contain("bad")
    expander.diagnostics.should be_empty
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

    # Crystal's MacroLiteral for this body is exactly "1\n". Preserve that
    # trailing newline instead of normalizing source owned by the macro AST.
    expanded.first.source.text.should eq("1\n")
  end

  it "does not expand macro definition templates before they are invoked" do
    src = Facet::Compiler::Source.new(<<-CR)
      macro echo(x)
        {{x}}
      end

      {{ echo(1) }}
    CR
    ast = Facet::Compiler::Parser.new(src).parse_file
    index = Facet::Compiler::Indexer.index_macros([ast])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.scan("{{x}}").size.should eq(1)
    expanded.source.text.rstrip.should end_with("1")
    expander.diagnostics.should be_empty
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

  it "preserves false and nil macro arguments" do
    src_def = Facet::Compiler::Source.new(<<-CR)
      macro echo(a, b)
        {{a}}/{{b}}
      end
    CR
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file
    src_use = Facet::Compiler::Source.new("{{ echo(false, nil) }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand_all([ast_use], index)

    expanded.first.source.text.should contain("false/nil")
    expander.diagnostics.should be_empty
  end

  it "expands nested controls and macro calls with the active parameter environment" do
    src_def = Facet::Compiler::Source.new(<<-CR)
      macro inner(x)
        {{x}}
      end

      macro choose(x)
        {% if x %}{{inner(x)}}{% else %}bad{% end %}
      end
    CR
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file
    src_use = Facet::Compiler::Source.new("{{ choose(0) }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand_all([ast_use], index)

    expanded.first.source.text.should contain("0")
    expanded.first.source.text.should_not contain("bad")
    expander.diagnostics.should be_empty
  end

  it "gives repeated macro variables stable hygienic names per expansion" do
    src_def = Facet::Compiler::Source.new(<<-CR)
      macro locals
        %value = 1
        puts %value
      end
    CR
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file
    src_use = Facet::Compiler::Source.new("{{ locals }}; {{ locals }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand_all([ast_use], index)
    names = expanded.first.source.text.scan(/__value_\d+/).map(&.[0])

    names.size.should eq(4)
    names[0].should eq(names[1])
    names[2].should eq(names[3])
    names[0].should_not eq(names[2])
    expander.diagnostics.should be_empty
  end

  it "keys hygienic macro variables by evaluated arguments" do
    src_def = Facet::Compiler::Source.new(<<-CR)
      macro locals
        %entry{1} = 10
        puts %entry{1}
        %entry{2} = 20
        puts %entry{2}
      end
    CR
    ast_def = Facet::Compiler::Parser.new(src_def).parse_file
    src_use = Facet::Compiler::Source.new("{{ locals }}")
    ast_use = Facet::Compiler::Parser.new(src_use).parse_file

    index = Facet::Compiler::Indexer.index_macros([ast_def])
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand_all([ast_use], index)
    names = expanded.first.source.text.scan(/__entry_\d+/).map(&.[0])

    names.size.should eq(4)
    names[0].should eq(names[1])
    names[2].should eq(names[3])
    names[0].should_not eq(names[2])
    expander.diagnostics.should be_empty
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
