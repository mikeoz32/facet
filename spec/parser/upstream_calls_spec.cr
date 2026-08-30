require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (calls and blocks)" do
  it_parses "Number.expand_div [Int32, Int64], BigInt"
  it_parses "ivar_ptr type, name, value"
  it_parses "default_value_index.try(&.< splat_index)"
  it_parses "pointerof(func).as({Void*, Void*}*)"
  it_parses "start_attribute *args, **nargs"
  it_parses "$~ = regex.match self, pos, options: options"
  it "keeps consecutive parenthesized calls as separate expressions" do
    code = 2_000.times.map { |index| "put(data, #{index})" }.join('\n')
    ast = parse_ok(code)
    expressions = ast.children(ast.root)[0]

    ast.children(expressions).size.should eq(2_000)
  end

  it "parses chained calls with blocks and params" do
    ast = parse_ok("foo(1).bar do |x, y| x end")
    exprs = ast.children(ast.root)[0]
    bin = ast.children(exprs)[0]
    rhs = ast.children(bin)[1]
    ast.node(rhs).kind.should eq(Facet::Compiler::NodeKind::CallWithBlock)
  end

  it_parses "include Foo, Bar"
  it_parses "include Foo"
  it_parses "include Foo\nif true; end"
  it_parses "extend Foo, Bar"
  it_parses "extend Foo"
  it_parses "extend Foo\nif true; end"
  it_parses "extend self"
  it_parses "sizeof(Int32)\nalignof Int32"
  it_parses "Time.measure { yield }"
  it_parses "require \"./foo\""
  it_parses "foo x do\nend"
  it_parses "foo x, y do\nend"
  it_parses "foo(bar do\nend)"
  it_parses "foo(bar { })"
  it_parses "(bar do\nend)"
  it_parses "(foo bar do\nend)"
  it_parses "(baz; bar do\nend)"
  it_parses "(bar {})"
  it_parses "(a;\nb)"
  it_parses "1.x; foo do\nend"
  it_parses "x = 1; foo.bar x do\nend"
  it_parses "foo do\n//\nend"
  it_parses "foo x do\n//\nend"
  it_parses "foo(x) do\n//\nend"
  it_parses "foo"
  it_parses "foo()"
  it_parses "foo(1)"
  it_parses "foo 1"
  it_parses "foo 1\n"
  it_parses "foo 1;"
  it_parses "foo 1, 2"
  it_parses "foo (1 + 2), 3"
  it_parses "foo(\n1)"
  it_parses "::foo"
  it_parses "foo + 1"
  it_parses "foo +1"
  it_parses "foo +1.0"
  it_parses "foo +1_i64"
  it_parses "foo -1.0, -2.0"
  it_parses "foo(1 + 2)"
  it_parses "foo do; 1; end"
  it_parses "foo do |a|; 1; end"
  it_parses "1.foo do; 1; end"
  it_parses "a b() {}"
  it_parses "foo !false"
  it_parses "!a && b"
  it_parses "foo.bar.baz"
  it_parses "f.x Foo.new"
  it_parses "f.x = Foo.new"
  it_parses "f.x = - 1"
  it_parses "foo = 1; foo +1"
  it_parses "foo = 1; foo -1"
  it_parses "foo = 1; foo(+1)"
  it_parses "foo = 1; foo(-1)"
  it_parses "foo = 1; b = 2; foo -b"
  it_parses "foo = 1; b = 2; foo +b"
  it_parses "def foo(x)\n x\nend; foo = 1; b = 2; foo -b"
  it_parses "def foo(x)\n x\nend; foo = 1; b = 2; foo +b"
  it_parses "foo = 1; foo a: 1"
  it_parses "foo = 1; foo {}"
  it_parses "foo = 1; foo &x"
  it_parses "foo bar, out baz"
  it_parses "foo(&bar)"
  it_parses "foo &bar"
  it_parses "foo(&block)"
  it_parses "foo &block"
  it_parses "a.foo &block"
  it_parses "a.foo(&block)"
  it_parses "foo(&.bar)"
  it_parses "foo &.bar"
  it_parses "foo(&.block)"
  it_parses "foo &.block"
  it_parses "foo &.block(1)"
  it_parses "foo &.block[]"
  it_parses "foo &.+(2)"
  it_parses "foo &.bar.baz"
  it_parses "foo(&.bar.baz)"
  it_parses "foo &./(1)"
  it_parses "foo &.%(1)"
  it_parses "foo &.block[] = 1"
  it_parses "foo &.block[0] = 1"
  it_parses "foo &.block=(0)"
  it_parses "foo &.[]"
  it_parses "foo &.[0]"
  it_parses "foo &.[] = 1"
  it_parses "foo &.[0] = 1"
  it_parses "foo &.block = 0"
  it_parses "foo(&.!)"
  it_parses "foo &.block[0]"
  it_parses "foo(&.block[0])"
  it_parses "foo &.@bar"
  it_parses "foo &.@bar.baz"
  it_parses "foo(&.@bar.baz)"
  it_parses "foo &.@bar[baz]"
  it_parses "foo &.@bar.@baz"
  it_parses "foo(&.as(T))"
  it_parses "foo &.as(T)"
  it_parses "foo(&.as(T).bar)"
  it_parses "foo &.as(T).bar"
  it_parses "foo &.each {\n}"
  it_parses "foo &.each do\nend"
  it_parses "foo { a = 1 }; a"
  it_parses "x = 2; foo do bar x end"
  it_parses "call @foo.bar"
  it_parses "call \"foo\""
  it_parses "A.new(\"x\", B.new(\"y\"))"
  it_parses "var.@foo"
  it_parses "var.@foo.@bar"
  it_parses "puts ::foo"
  it_parses "foo [1]"
  it_parses "foo.bar [1]"
  it_parses "foo.bar(1).baz"
  it_parses "foo\n.bar"
  it_parses "foo\n   .bar"
  it_parses "foo\n\n  .bar"
  it_parses "foo\n  #comment\n  .bar"
  it_parses "foo(\n1\n)"
  it_parses "a = 1\nfoo - a"
  it_parses "a = 1\nfoo -a"
  it_parses "foo out x; x"
  it_parses "foo(out x); x"
  it_parses "foo out @x; @x"
  it_parses "foo(out @x); @x"
  it_parses "foo out _"
  it_parses "foo z: out x; x"
  it_parses "foo *bar"
  it_parses "foo(*bar)"
  it_parses "foo x, *bar"
  it_parses "foo(x, *bar, *baz, y)"
  it_parses "foo.[0]"
  it_parses "foo.[0] = 1"

  it_parses "foo(**bar)"
  it_parses "foo 1, **bar"
  it_parses "foo(1, **bar)"
  it_parses "foo 1, **bar, &block"
  it_parses "foo(1, **bar, &block)"
  it_parses "foo.bar=(*baz)"
  it_parses "foo.bar = (1).abs"
  it_parses "foo[*baz]"
  it_parses "foo[*baz] = 1"

  it_parses "foo(a: 1, b: 2)"
  it_parses "foo(1, a: 1, b: 2)"
  it_parses "foo a: 1, b: 2"
  it_parses "foo 1, a: 1, b: 2"
  it_parses "foo 1, a: 1, b: 2\n1"
  it_parses "x.foo a: 1, b: 2 "
  it_parses "foo(a: 1\n)"
  it_parses "foo(\na: 1,\n)"
  assert_syntax_error "foo(\"\": 1)", "named argument cannot have an empty name"

  it_parses %(foo("foo bar": 1, "baz": 2))
  it_parses %(foo "foo bar": 1, "baz": 2)
  it_parses %(foo(Foo: 1, Bar: 2))

  it_parses "x.foo(a: 1, b: 2)"
  it_parses "x.foo a: 1, b: 2"

  it_parses "x[a: 1, b: 2]"
  it_parses "x[a: 1, b: 2,]"
  it_parses "x[{1}]"
  it_parses "x[+ 1]"

  it_parses "foo(a: 1, &block)"
  it_parses "foo a: 1, &block"
  it_parses "foo a: b(1) do\nend"
  it_parses "foo(*{1})"
  it_parses "foo *{1}"
  it_parses "foo(Bar) { 1 }"
  it_parses "foo Bar { 1 }"
  it_parses "foo(Bar { 1 })"
  it_parses "Foo.bar x.y do\nend"
  it_parses "Foo.foo(count: 3).bar { }"
  it_parses "x = 1; foo x do\nend"
  it_parses "x = 1; foo x { }"
  it_parses "x = 1; foo x {\n}"
  it_parses "foo begin\nbar do\nend\nend"
  it_parses "foo 1.bar do\nend"
  it_parses "return 1.bar do\nend"
  it_parses "foo bar.baz(1) do\nend"
  it_parses "foo(\n  &.block\n)"
  it_parses "foo result : Int32; result"
  it_parses "foo(x: result : Int32); result"
  it_parses <<-CRYSTAL
    foo(
      begin
        result : Int32 = 1
        result
      end
    )
  CRYSTAL
  it_parses <<-CRYSTAL
    foo(x:
      begin
        result : Int32 = 1
        result
      end
    )
  CRYSTAL
  it_parses "foo(\n        begin\n          result : Int32 = 1\n          result\n        end\n      )"
  it_parses "foo(x:\n        begin\n          result : Int32 = 1\n          result\n        end\n      )"

  it_parses "foo.bar= *baz"

  it_parses "foo[bar { 1 }]"
  it_parses "foo.[bar { 1 }]"
  it_parses "foo.[](bar { 1 })"
  it_parses "foo[bar do; 1; end]"
  it_parses "foo.[bar do; 1; end]"
  it_parses "foo.[](bar do; 1; end)"
  it_parses "foo[\n1\n]"
  it_parses "foo[\nfoo[\n1\n]\n]"
  it_parses "call(foo : A, end : B)"
  it_parses "call foo : A, end : B"

  it_parses "a b c d e do; end"
  it_parses "a b c d e {}"
  it_parses "a b c d e do 1 end do 2 end { 3 } do 4 end"
  it_parses "a b c d e { 1 } { 2 } do 3 end { 4 }"
  it_parses "a b c d e 1, 2 do; end"
  it_parses "a b c d e 1, 2 {}"
  it_parses "a 1, (2), b do end"
  it_parses "a 1, (2), b {}"

  %w(bar? bar!).each do |name|
    it_parses "foo(#{name})"
    it_parses "foo #{name}"
  end

  it_parses "consume(foo 1,)"
  it_parses "consume(foo &block, 1)"
  it_parses "Matches.new(@matches.try &.[](*args), @cover, @owner, @success)"
  it_parses "value.try &.foo"
  assert_syntax_error "foo(name: 1, 2)"
  assert_syntax_error "foo(&block, 1)"
end
