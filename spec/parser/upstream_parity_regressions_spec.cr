require "../spec_helper"

private def parity_parse(source_text : String)
  source = Facet::Compiler::Source.new(source_text, "upstream_parity_regression")
  parser = Facet::Compiler::Parser.new(source)
  ast = parser.parse_file
  {parser, ast}
end

describe "Crystal 1.21 upstream parser parity regressions" do
  valid_sources = [
    "def foo(\"bar baz\" qux)\nend",
    "macro foo\n\\{%@type %}\nend",
    "type(Foo = Void)",
    "case 1\nwhen .[](2)\n  3\nwhen .[]=(4)\n  5\nend",
    "1.&*",
    "1.&**",
    "def foo(var : () -> Double); end",
    %q(asm("nop" :: : :)),
    %q(asm("nop" ::: :)),
    "class Foo\n  def bar\n    print as Foo\n  end\nend",
    "{% begin %}%-{% end %}",
    "x : Foo ->; 1",
    %q("a\]b"),
    %q(:"a\[b"),
    "lib Foo\n  fun c(Void*) : Char[2]*\nend",
    "macro foo\n  %ǲ{1} = 2\nend",
    "macro foo\n  %ǲ = 1\nend",
    "record Change, type : ModuleType, def : Def",
    "def foo\n  LibC.get(out first)\n  first += 1\nend",
    "def foo\n  out = Pointer(Int32).null\n  out.value = 1\nend",
    "def foo\n  begin\n    yield\n  rescue ex : Error\n    ex\n  else\n    1\n  end\nend",
    "if a && (!b || (\n  c && d\n)) && e\n  1\nend",
    "macro outer\n  private def foo\n    1\n  end\nend",
    "macro outer\n  fun foo : Int32\n    1\n  end\nend",
    "macro outer\n  macro inner\n    1\n  end\nend",
    "macro outer\n  lib LibC\n    fun foo : Int32\n  end\nend",
  ]

  valid_sources.each do |source_text|
    it "accepts #{source_text.dump}" do
      parser, ast = parity_parse(source_text)
      parser.diagnostics.should be_empty
      Facet::Compiler::AstIntegrity.contract_violations(ast).should be_empty
      tokens = Facet::Compiler::Lexer.new(ast.source).tokenize_all
      Facet::Compiler::AstIntegrity.missing_semantic_tokens(ast, tokens).should be_empty
    end
  end

  invalid_sources = [
    %q(def foo("" y); y; end),
    %q(macro foo("" y); end),
    %q(def foo("bar #{1} qux" y); y; end),
    "fun foo(Int32); end",
    "fun Foo : Int64\nend",
    "{% unless 1 %} 2 {% elsif 3 %} 3 {% end %}",
    "$foo",
    "offsetof(X, 1.0)",
    "offsetof(X, 'c')",
    "case {1, 2}; when {3}; 4; end",
    "select\nwhen 1\n2\nend",
    "->::foo.foo",
    "->::@foo.foo",
    "->::@@foo.foo",
    "foo **bar, 1",
    "foo(**bar, 1)",
    "foo **bar, *x",
    "foo(**bar, *x)",
    "foo **bar, out x",
    "foo(**bar, out x)",
    "case {*1}\nwhen {2}; 3; end",
    "case {1}\nwhen {*2}; 3; end",
    %q(asm("nop" ::: "#{foo}")),
    %q(asm("nop" :::: "#{volatile}")),
    %q(asm("" ::: ""(var))),
    %q(asm("" : 1)),
    "a = a",
    "def foo(x = 1, y); end",
    "case when .foo? then 1; end",
    "foo {1, 2}",
    "pointerof(self)",
    "def foo 1; end",
    "if 1\n  foo 1,\nend",
    "foo 1,",
    "def foo:String\nend",
    "def foo :String\nend",
    "def foo():String\nend",
    "def foo() :String\nend",
    "foo :: Foo",
    "@foo :: Foo",
    "@@foo :: Foo",
    "$foo :: Foo",
    "foo[0]? = 1",
    "foo[0]? += 1",
    "foo.[0]? = 1",
    "foo.[0]? += 1",
    "foo &.[0]? = 1",
    "foo &.[]?=(1)",
    "foo &.[]? = 1",
    "foo &.[]?(0)=(1)",
    "foo &.[]?(0) = 1",
    "foo &.[](0)=(1)",
    "foo &.[](0) = 1",
    %q(%r[a\u{41}b]),
    %q(/a\u{41}b/),
    %q(%r{a\u{41}b}),
    %q(%r|a\u{41}b|),
    %q(%r{a[b\]c}),
    %q(%r|a[b\]c|),
    %q(%W[a#{*b}]),
    %q(%W[#{a}#{*b}]),
    %q(%W[#{*a}b]),
    %q(%W[#{*a}#{b}]),
  ]

  invalid_sources.each do |source_text|
    it "rejects #{source_text.dump}" do
      parser, _ = parity_parse(source_text)
      parser.diagnostics.should_not be_empty
    end
  end

  it "retains forall variables and asm operands in the AST" do
    [
      "def foo(x : X, y : Y) forall X, Y\nend",
      %q(asm("nop" : "a"(0) : "b"(1))),
    ].each do |source_text|
      parser, ast = parity_parse(source_text)
      parser.diagnostics.should be_empty
      tokens = Facet::Compiler::Lexer.new(ast.source).tokenize_all
      significant = tokens.select do |token|
        {Facet::Compiler::TokenKind::Identifier, Facet::Compiler::TokenKind::Number}.includes?(token.kind)
      end
      significant.each do |token|
        ast.arena.nodes.any? do |node|
          node.kind != Facet::Compiler::NodeKind::File &&
            node.kind != Facet::Compiler::NodeKind::Expressions &&
            node.span.start <= token.span.start && node.span.finish >= token.span.finish
        end.should be_true
      end
    end
  end
end
