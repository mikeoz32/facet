require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (basic port)" do

  it "parses basic literals" do
    %w(nil true false 1 +1 -1 1_i64 -1_i64).each do |code|
      parse_ok(code)
    end
  end

  it "parses simple binary expression" do
    ast = parse_ok("1 + 2")
    exprs = ast.children(ast.root)[0]
    expr = ast.children(exprs)[0]
    ast.node(expr).kind.should eq(Facet::Compiler::NodeKind::Binary)
  end

  it "parses def with default parameter" do
    ast = parse_ok("def foo(var = 1); end")
    exprs = ast.children(ast.root)[0]
    def_id = ast.children(exprs)[0]
    ast.node(def_id).kind.should eq(Facet::Compiler::NodeKind::Def)
    params = ast.children(def_id)[1]
    ast.children(params).size.should eq(1)
  end

  it "parses def with type annotation on parameter" do
    ast = parse_ok("def foo(var : Int32); end")
    exprs = ast.children(ast.root)[0]
    def_id = ast.children(exprs)[0]
    params = ast.children(def_id)[1]
    ast.children(params).size.should eq(1)
  end

  it "parses def with splat and double splat" do
    ast = parse_ok("def foo(x, *args, **opts); end")
    exprs = ast.children(ast.root)[0]
    def_id = ast.children(exprs)[0]
    params = ast.children(def_id)[1]
    kinds = ast.children(params).map { |pid| ast.node(pid).kind }
    kinds.should contain(Facet::Compiler::NodeKind::Param)
    kinds.should contain(Facet::Compiler::NodeKind::Splat)
    kinds.should contain(Facet::Compiler::NodeKind::DoubleSplat)
  end

  it "parses calls with named args" do
    ast = parse_ok("foo(bar: 1, baz: 2)")
    exprs = ast.children(ast.root)[0]
    call = ast.children(exprs)[0]
    args = ast.children(call)[1]
    ast.children(args).each do |arg|
      ast.node(arg).kind.should eq(Facet::Compiler::NodeKind::NamedArg)
    end
  end

  it_parses "[1, 2, 3]"
  it_parses "{\"a\" => 1, :b => 2}"

  it_parses "..10"
  it_parses "10.."
  it_parses "1..2"

  it_parses "a = 1; b = 2; c = 3; a-b-c"
  it_parses "a = 1; b = 2; c = 3; a-b -c"
  it_parses "1\n+2"
  it_parses "1;-2"
  it_parses "1 * 2"
  it_parses "2 * (3 + 4)"
  it_parses "()"
  it_parses "(1; 2; 3)"
end
