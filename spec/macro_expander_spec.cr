require "./spec_helper"

describe Facet::Compiler::MacroExpander do
  it "expands macro expressions with literal strings" do
    src = Facet::Compiler::Source.new("puts {{ \"hi\" }}")
    ast = Facet::Compiler::Parser.new(src).parse_file

    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.kind.should eq(Facet::Compiler::SourceKind::Virtual)
    expanded.source.expanded_from.should_not be_nil
    expanded.source.text.should eq(%(puts "hi"))
    expander.diagnostics.should be_empty
  end

  it "uses decoded string values in macro expressions" do
    src = Facet::Compiler::Source.new(%q(value = {{ "a\x62\u0063" }}))
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq(%(value = "abc"))
    expander.diagnostics.should be_empty
  end

  it "preserves literal syntax and exposes basic AST-node predicates" do
    source = Facet::Compiler::Source.new(<<-CR)
      string = {{"value"}}
      symbol = {{:value}}
      identifier = {{:value.id}}
      {% if :value.is_a?(SymbolLiteral) && "value".is_a?(StringLiteral) %}
        predicates = true
      {% end %}
      {% if :value.responds_to?(:id) && !1.nil? %}
        methods = true
      {% end %}
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should contain(%(string = "value"))
    expanded.source.text.should contain("symbol = :value")
    expanded.source.text.should contain("identifier = value")
    expanded.source.text.should contain("predicates = true")
    expanded.source.text.should contain("methods = true")
    expanded.diagnostics.should be_empty
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
    src = Facet::Compiler::Source.new(<<-CR)
      {
      {% for key, value in {"a" => 1, "b" => 2} %}
        {{key}} => {{value}},
      {% end %}
      }
    CR
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should contain(%("a" => 1))
    expanded.source.text.should contain(%("b" => 2))
    expander.diagnostics.should be_empty
  end

  it "iterates tuple, named tuple, and range macro values" do
    src = Facet::Compiler::Source.new(<<-CR)
      {% for value in {1, 2} %}puts {{value}}
      {% end %}named = {
      {% for key, value in {one: 3, two: 4} %}{{key}} => {{value}},
      {% end %}}
      {% for value in 5...7 %}puts {{value}}
      {% end %}
    CR
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should contain("puts 1")
    expanded.source.text.should contain("puts 2")
    expanded.source.text.should contain(%("one" => 3))
    expanded.source.text.should contain(%("two" => 4))
    expanded.source.text.should contain("puts 5")
    expanded.source.text.should contain("puts 6")
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

    expanded.source.text.should contain(%(picked = "two"))
    expanded.source.text.should contain(%(joined = "one-two"))
    expanded.source.text.should contain("mapped = 2")
    expanded.source.text.should_not contain("bad")
    expander.diagnostics.should be_empty
  end

  it "expands across passes until macros are gone" do
    src = Facet::Compiler::Source.new(%({{ "{{ 1 }}".id }}))
    ast = Facet::Compiler::Parser.new(src).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should eq("1")
    expander.diagnostics.should be_empty
  end

  it "emits diagnostic when expansion exceeds max passes" do
    src = Facet::Compiler::Source.new(%({{ "{{ 1 }}".id }}))
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

  it "expands ordinary receiverless macro calls" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro make_getter(name)
        def {{name.id}}
          @{{name.id}}
        end
      end

      class Box
        make_getter :value
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.should contain("def value")
    expanded.source.text.should contain("@value")
    expanded.source.text.should_not contain("make_getter :value")
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "does not treat calls with receivers as macro invocations" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro render(value)
        {{value}}
      end

      object.render(1)
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expanded = Facet::Compiler::MacroExpander.new(index).expand(ast, index)

    expanded.source.text.should contain("object.render(1)")
  end

  it "resolves ordinary macro calls through lexical type scopes" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro pick
        root_value
      end

      class Outer
        macro pick
          scoped_value
        end

        pick()
      end

      class Other
        pick()
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expanded = Facet::Compiler::MacroExpander.new(index).expand(ast, index)

    expanded.source.text.scan("scoped_value").size.should eq(2)
    expanded.source.text.scan("root_value").size.should eq(2)
  end

  it "selects an ordinary macro overload by arity" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro choose(value)
        one_argument
      end

      macro choose(value, other)
        two_arguments
      end

      choose(1, 2)
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expanded = Facet::Compiler::MacroExpander.new(index).expand(ast, index)

    expanded.source.text.scan("one_argument").size.should eq(1)
    expanded.source.text.scan("two_arguments").size.should eq(2)
  end

  it "expands bare zero-argument macro calls" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro answer
        42
      end

      value = answer
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expanded = Facet::Compiler::MacroExpander.new(index).expand(ast, index)

    expanded.source.text.should match(/value =\s+42/)
  end

  it "does not expand bare identifiers shadowed by locals or parameters" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro value
        expanded_value
      end

      def assigned
        value = 1
        value
      end

      def parameter(value)
        value
      end

      top_level = value
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expanded = Facet::Compiler::MacroExpander.new(index).expand(ast, index)

    expanded.source.text.should contain("value = 1\n    value")
    expanded.source.text.should contain("def parameter(value)\n    value")
    expanded.source.text.should match(/top_level =\s+expanded_value/)
  end

  it "does not let assignments in another lexical type shadow a bare macro" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro answer
        42
      end

      class Other
        answer = 1
      end

      top_level = answer
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expanded = Facet::Compiler::MacroExpander.new(index).expand(ast, index)

    expanded.source.text.should match(/top_level =\s+42/)
  end

  it "lowers standard accessor macro families without the stdlib index" do
    source = Facet::Compiler::Source.new(<<-CR)
      class Settings
        getter name
        property age : Int32
        getter? ready
        class_property version : String
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    expanded = Facet::Compiler::MacroExpander.new.expand(ast)

    expanded.source.text.should contain("def name")
    expanded.source.text.should contain("def age : Int32")
    expanded.source.text.should contain("def age=(value : Int32)")
    expanded.source.text.should contain("def ready?")
    expanded.source.text.should contain("def self.version : String")
    expanded.source.text.should contain("def self.version=(value : String)")
    expanded.diagnostics.should be_empty
  end

  it "lowers record and its generated getter calls across passes" do
    source = Facet::Compiler::Source.new("record Point, x : Int32, y = 1")
    ast = Facet::Compiler::Parser.new(source).parse_file
    expanded = Facet::Compiler::MacroExpander.new.expand(ast)

    expanded.source.text.should contain("struct Point")
    expanded.source.text.should contain("def x : Int32")
    expanded.source.text.should contain("def y")
    expanded.source.text.should contain("def initialize(@x : Int32, @y = 1)")
    expanded.diagnostics.should be_empty
  end

  it "lowers accessor macros with blocks as one invocation" do
    source = Facet::Compiler::Source.new(<<-CR)
      class Lazy
        getter value do
          1
        end
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    expanded = Facet::Compiler::MacroExpander.new.expand(ast)

    expanded.source.text.should contain("def value")
    expanded.source.text.should_not contain("getter value")
    expanded.diagnostics.should be_empty
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

  it "preserves unsupported macro arguments as opaque AST source" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro define(name, type, value)
        def {{name.id}} : {{type}}
          {{value}}
        end
      end

      define fetch, Array(String), build_value(1, nested: true)
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.should contain("def fetch : Array(String)")
    expanded.source.text.should contain("build_value(1, nested: true)")
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "preserves opaque named and default macro arguments" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro define(name, type = Array(String))
        def {{name.id}} : {{type}}
        end
      end

      define(name: fetch, type: Hash(String, Int32))
      define cached
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.should contain("def fetch : Hash(String, Int32)")
    expanded.source.text.should contain("def cached : Array(String)")
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "supports stringify and symbolize on macro values" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro describe(name)
        LABEL = {{name.stringify}}
        SYMBOL = {{name.symbolize}}
      end

      describe value
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.should contain(%(LABEL = "value"))
    expanded.source.text.should contain("SYMBOL = :value")
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "binds macro blocks for yield and block body expansion" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro wrap(&block)
        def yielded
          {{yield}}
        end

        def explicit
          {{block.body}}
        end
      end

      wrap do
        source_call(1)
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.scan("source_call(1)").size.should eq(2)
    expanded.source.text.should_not contain("wrap do")
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "exposes macro block parameters and caches distinct block bodies separately" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro capture(&block)
        puts {{block.args.join(",")}}
        {{yield}}
      end

      capture do |left, right|
        first_call(left, right)
      end

      capture do |value|
        second_call(value)
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.should contain(%(puts "left,right"))
    expanded.source.text.should contain("first_call(left, right)")
    expanded.source.text.should contain(%(puts "value"))
    expanded.source.text.should contain("second_call(value)")
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "evaluates collection macro blocks with lexical parameters" do
    source = Facet::Compiler::Source.new(<<-CR)
      {% words = ["one", "two", "three"] %}
      mapped = {{words.select { |word| word.size > 3 }.map { |word| word.upcase }.join(",")}}
      any = {{words.any? { |word| word.starts_with?("t") }}}
      all = {{words.all? { |word| !word.empty? }}}
      indexed = [{{words.map_with_index { |word, index| index }.join(",").id}}]
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should contain(%(mapped = "THREE"))
    expanded.source.text.should contain("any = true")
    expanded.source.text.should contain("all = true")
    expanded.source.text.should contain("indexed = [0,1,2]")
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "preserves outer macro assignments across collection block iterations" do
    source = Facet::Compiler::Source.new(<<-CR)
      {% total = 0 %}
      {% [1, 2, 3].each { |value| total += value } %}
      total = {{total}}
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    expander = Facet::Compiler::MacroExpander.new
    expanded = expander.expand(ast)

    expanded.source.text.should contain("total = 6")
    expanded.diagnostics.should be_empty
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

  it "exposes the lexical type, methods, and instance variables" do
    source = Facet::Compiler::Source.new(<<-CR)
      class Widget
        @name : String

        def initialize(@count : Int32)
        end

        def render(value : String) : Bool
          true
        end

        macro describe
          type_name = {{@type.name.stringify}}
          method_names = {{@type.methods.map { |method| method.name }.join(",")}}
          ivar_names = {{@type.instance_vars.map { |ivar| ivar.name }.join(",")}}
          first_arg = {{@type.methods.last.args.first.name.stringify}}
          first_arg_type = {{@type.methods.last.args.first.type.resolve.name.stringify}}
        end

        describe
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.should contain(%(type_name = "Widget"))
    expanded.source.text.should contain(%(method_names = "initialize,render"))
    expanded.source.text.should contain(%(ivar_names = "name,count"))
    expanded.source.text.should contain(%(first_arg = "value"))
    expanded.source.text.should contain(%(first_arg_type = "String"))
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "resolves macro type arguments through the program index" do
    source = Facet::Compiler::Source.new(<<-CR)
      struct Payload
        def encode(io : IO) : Nil
        end
      end

      macro describe(type)
        resolved_name = {{type.resolve.name.stringify}}
        resolved_kind = {{type.resolve.struct?}}
        method_name = {{type.resolve.methods.first.name.stringify}}
        return_type = {{type.resolve.methods.first.return_type.resolve.name.stringify}}
      end

      describe Payload
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.should contain(%(resolved_name = "Payload"))
    expanded.source.text.should contain("resolved_kind = true")
    expanded.source.text.should contain(%(method_name = "encode"))
    expanded.source.text.should contain(%(return_type = "Nil"))
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "exposes constants and explicit inheritance to type-aware macros" do
    source = Facet::Compiler::Source.new(<<-CR)
      class Parent
      end

      class Child < Parent
        Zebra = 1
        Alpha = 2

        macro describe
          constant_names = {{@type.constants.map { |constant| constant.stringify }.sort.join(",")}}
          parent_name = {{@type.superclass.name.stringify}}
          ancestor_name = {{@type.ancestors.first.name.stringify}}
          subtype = {{@type < Parent}}
        end

        describe
      end


      enum Shade
        Red
        Blue

        macro describe_members
          members = {{@type.constants.map { |constant| constant.stringify }.join(",")}}
        end

        describe_members
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expander = Facet::Compiler::MacroExpander.new(index)
    expanded = expander.expand(ast, index)

    expanded.source.text.should contain(%(constant_names = "Alpha,Zebra"))
    expanded.source.text.should contain(%(parent_name = "Parent"))
    expanded.source.text.should contain(%(ancestor_name = "Parent"))
    expanded.source.text.should contain("subtype = true")
    expanded.source.text.should contain(%(members = "Red,Blue"))
    expanded.diagnostics.should be_empty
    expander.diagnostics.should be_empty
  end

  it "keeps lexical type-aware expansion cache entries scope-specific" do
    source = Facet::Compiler::Source.new(<<-CR)
      macro type_name
        {{@type.name.stringify}}
      end

      class First
        NAME = type_name
      end

      class Second
        NAME = type_name
      end
    CR
    ast = Facet::Compiler::Parser.new(source).parse_file
    index = Facet::Compiler::Indexer.index_macros(ast)
    expanded = Facet::Compiler::MacroExpander.new(index).expand(ast, index)

    expanded.source.text.should match(/class First\s+NAME =\s+"First"/)
    expanded.source.text.should match(/class Second\s+NAME =\s+"Second"/)
  end
end
