require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (calls and blocks)" do
  it "parses chained calls with blocks and params" do
    ast = parse_ok("foo(1).bar do |x, y| x end")
    exprs = ast.children(ast.root)[0]
    bin = ast.children(exprs)[0]
    rhs = ast.children(bin)[1]
    ast.node(rhs).kind.should eq(Facet::Compiler::NodeKind::CallWithBlock)
  end

  it_parses "include Foo, Bar"
  it_parses "extend Foo, Bar"
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
  it_parses "foo !false"
  it_parses "!a && b"
  it_parses "foo.bar.baz"
  it_parses "f.x Foo.new"
  it_parses "f.x = Foo.new"
  it_parses "f.x = - 1"
  it_parses "foo bar, out baz"
  it_parses "foo(&bar)"
  it_parses "foo &bar"
  it_parses "foo(&.bar)"
  it_parses "foo &.bar"

  it "resolves block associativity like upstream" do
    ["a b c d e do; end", "a b c d e {}"].each { |code| parse_ok(code) }
    parse_ok("a b c d e do 1 end do 2 end { 3 } do 4 end")
    parse_ok("a b c d e { 1 } { 2 } do 3 end { 4 }")
    parse_ok("a b c d e 1, 2 do; end")
    parse_ok("a b c d e 1, 2 {}")
    parse_ok("a 1, (2), b do end")
    parse_ok("a 1, (2), b {}")
  end
end
