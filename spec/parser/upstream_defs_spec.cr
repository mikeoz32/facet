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
  it_parses "def foo(x, *y); 1; end"
  it_parses "def foo(x, *args, y = 2, w, z = 3); 1; end"
  it_parses "def foo(x, *, y); 1; end"
  it_parses "def foo(x, *, y, &); 1; end"
  it_parses "def foo(**args)\n1\nend"
  it_parses "def foo(x, **args)\n1\nend"
  it_parses "def foo(x, **args, &block)\n1\nend"
  it_parses "def foo(**args)\nargs\nend"
  it_parses "def foo(x = 1, **args)\n1\nend"
  it_parses "def foo(**args : Foo)\n1\nend"
  it_parses "def foo(x = 1, *y); 1; end"
  it_parses "def foo(x, *y : Int32); 1; end"
  it_parses "def foo(*y : *T); 1; end"

  it "builds def param shapes" do
    ast = parse_ok("def foo(x, y = 1, *rest, **kw, &blk); end")
    params = def_params(ast)
    params.size.should eq(5)
    params[0..1].each { |p| ast.arena.node(p).kind.should eq(Facet::Compiler::NodeKind::Param) }
  end

  it "builds def types and return unions" do
    ast = parse_ok("def foo(x : Foo?, y : Foo::Bar | Baz) : Foo(Int32)?\nend")
    params = def_params(ast)
    params.size.should eq(2)
    ast.arena.node(ast.arena.children(params[0])[1]).kind.should eq(Facet::Compiler::NodeKind::Binary)
    ast.arena.node(ast.arena.children(params[1])[1]).kind.should eq(Facet::Compiler::NodeKind::Binary)
    ret = def_return(ast)
    ast.arena.node(ret).kind.should eq(Facet::Compiler::NodeKind::Binary)
  end

  # Basic def shapes and setters
  it_parses "def foo\n1\nend"
  it_parses "def downto(n)\n1\nend"
  it_parses "def foo ; 1 ; end"
  it_parses "def foo; end"
  it_parses "def foo(var); end"
  it_parses "def foo(\nvar); end"
  it_parses "def foo(\nvar\n); end"
  it_parses "def foo(var1, var2); end"
  it_parses "def foo; 1; 2; end"
  it_parses "def foo=(value); end"
  it_parses "def foo(n); foo(n -1); end"
  it_parses "def type(type); end"

  it_parses "def self.foo\n1\nend"
  it_parses "def self.foo()\n1\nend"
  it_parses "def self.foo=\n1\nend"
  it_parses "def self.foo=()\n1\nend"
  it_parses "def Foo.foo\n1\nend"
  it_parses "def Foo::Bar.foo\n1\nend"

  it_parses "def foo; a; end"
  it_parses "def foo(a); a; end"
  it_parses "def foo; a = 1; a; end"
  it_parses "def foo; a = 1; a {}; end"
  it_parses "def foo; a = 1; x { a }; end"
  it_parses "def foo; x { |a| a }; end"
  it_parses "def foo; x { |_| 1 }; end"
  it_parses "def foo; x { |a, *b| b }; end"

  it_parses "def foo(var = 1); end"
  it_parses "def foo(var : Int); end"
  it_parses "def foo(var : self); end"
  it_parses "def foo(var : self?); end"
  it_parses "def foo(var : self.class); end"
  it_parses "def foo(var : self*); end"
  it_parses "def foo(var : Int | Double); end"
  it_parses "def foo(var : Int?); end"
  it_parses "def foo(var : Int*); end"
  it_parses "def foo(var : Int**); end"
  it_parses "def foo(var : Int -> Double); end"
  it_parses "def foo(var : (Int, Float -> Double)); end"
  it_parses "def foo(var : (Int, Float) -> Double); end"
  it_parses "def foo(var : Char[256]); end"
  it_parses "def foo(var : Char[N]); end"
  it_parses "def foo(var : Int32 = 1); end"

  it_parses "def foo; yield; end"
  it_parses "def foo; yield 1; end"
  it_parses "def foo; yield 1; yield; end"
  it_parses "def foo; yield(1); end"
  it_parses "def foo(a, b = a); end"

  it_parses "def foo(&block); end"
  it_parses "def foo(&); end"
  it_parses "def foo(&\n); end"
  it_parses "def foo(a, &block); end"
  it_parses "def foo(a, &block : Int -> Double); end"
  it_parses "def foo(a, & : Int -> Double); end"
  it_parses "def foo(a, &block : Int, Float -> Double); end"
  it_parses "def foo(a, &block : Int, self -> Double); end"
  it_parses "def foo(a, &block : -> Double); end"
  it_parses "def foo(a, &block : Int -> ); end"
  it_parses "def foo(a, &block : self -> self); end"
  it_parses "def foo(a, &block : Foo); end"

  it_parses "def foo(@var); end"
  it_parses "def foo(@var); 1; end"
  it_parses "def foo(@var = 1); 1; end"
  it_parses "def foo(@@var); end"
  it_parses "def foo(@@var); 1; end"
  it_parses "def foo(@@var = 1); 1; end"

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
