require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (defs and params)" do

  it_parses "def foo(@[Foo] var); end"
  it_parses "def foo(@[Foo] outer inner); end"
  it_parses "def foo(@[Foo]  var); end"
  it_parses "def foo(a, @[Foo] var); end"
  it_parses "def foo(a, @[Foo] &block); end"
  it_parses "def foo(@[Foo] @var); end"
  it_parses "def foo(@[Foo] var : Int32); end"
  it_parses "def foo(@[Foo] @[Bar] var : Int32); end"
  it_parses "def foo(@[Foo] &@block); end"
  it_parses "def foo(@[Foo] *args); end"
  it_parses "def foo(@[Foo] **args); end"
  it_parses <<-CRYSTAL
    def foo(
      @[Foo]
      id : Int32,
      @[Bar] name : String
    ); end
  CRYSTAL

  it_parses "def foo(\n&block\n); end"
  it_parses "def foo(&block :\n Int ->); end"
  it_parses "def foo(&block : Int ->\n); end"
  it_parses "def foo(a, &block : *Int -> ); end"

  it_parses "def foo(x, *args, y = 2); 1; end"
  it_parses "def foo(x, *args, y = 2, w, z = 3); 1; end"
  it_parses "def foo(x, *, y); 1; end"
  it_parses "def foo(x, *, y, &); 1; end"
  it_parses "def foo(**args)\n1\nend"
  it_parses "def foo(x, **args)\n1\nend"
  it_parses "def foo(x, **args, &block)\n1\nend"
  it_parses "def foo(**args)\nargs\nend"
  it_parses "def foo(x = 1, **args)\n1\nend"
  it_parses "def foo(**args : Foo)\n1\nend"

  # Operator defs with extra params/defaults/splats (upstream #10397)
  %w(<= >= == != []= ===).each do |name|
    it_parses "def #{name}(other, file = 1); end"
    it_parses "def #{name}(*args, **opts); end"
    it_parses "def #{name}(*args, **opts, &); end"
  end

  # Disallow constructs inside def
  {
    "def" => "expected identifier for definition name",
    "macro" => "expected identifier for definition name",
    "class" => "expected identifier",
    "struct" => "expected identifier",
    "module" => "expected identifier",
    "fun" => "expected identifier",
    "alias" => "expected identifier",
    "abstract" => "unexpected token after 'abstract'",
    "include" => "include expects at least one argument",
    "extend" => "extend expects at least one argument",
    "lib" => "expected identifier",
  }.each do |kw, msg|
    it_diagnoses "def foo\n#{kw}\nend", msg
  end
end
