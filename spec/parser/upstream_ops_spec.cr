require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (operators and edge calls)" do
  it_parses "def +(other); end"
  it_parses "def %(other); end"
  it_parses "def **(other); end"
  it_parses "def [](idx); end"
  it_parses "def []=(idx, val); end"

  it_parses "-foo"
  it_parses "foo ? bar : baz"
  it_parses "case x\nwhen .>(y)\nend"
  it_parses "..10"
  it_parses "10.."
  it_parses "1...2"
  it_parses "1 || 2"
  it_parses "1 && 2"
  it_parses "a ||= 1"
  it_parses "a &&= 1"
  it "builds binary ops for ||, && and op-assign" do
    ast = parse_ok("1 || 2")
    exprs = root_exprs(ast)[0]
    bin = ast.children(exprs)[0]
    binary_op_kind(ast, bin).should eq(Facet::Compiler::TokenKind::OrOr)

    ast2 = parse_ok("a ||= 1")
    exprs2 = root_exprs(ast2)[0]
    bin2 = ast2.children(exprs2)[0]
    binary_op_kind(ast2, bin2).should eq(Facet::Compiler::TokenKind::OrOrEqual)
  end
  it_parses "foo !false"
  it_parses "!a && b"
  it_parses "foo.bar.baz"
  it_parses "f.x Foo.new"
  it_parses "f.x = Foo.new"
  it_parses "f.x = - 1"
  it_parses "b.c ||= 1"
  it_parses "b.c &&= 1"
  it_parses "foo.bar = {} of Int32 => Int32"
  it_parses "1 <= 2 <= 3"
  it_parses "1 == 2 == 3 == 4"

  it_parses "f.x += 2"
  it_parses "f.x -= 2"
  it_parses "f.x *= 2"
  it_parses "f.x /= 2"
  it_parses "f.x //= 2"
  it_parses "f.x %= 2"
  it_parses "f.x |= 2"
  it_parses "f.x &= 2"
  it_parses "f.x ^= 2"
  it_parses "f.x **= 2"
  it_parses "f.x <<= 2"
  it_parses "f.x >>= 2"
  it_parses "f.x &+= 2"
  it_parses "f.x &-= 2"
  it_parses "f.x &*= 2"

  it "parses operator definitions with and without receiver" do
    %w(/ < <= == != =~ !~ > >= + - * / ~ % & | ^ ** ===).each do |op|
      parse_ok("def #{op}; end;")
      parse_ok("def #{op}(); end;")
      parse_ok("def self.#{op}; end;")
      parse_ok("def self.#{op}(); end;")
    end
  end

  it "parses operator calls with args, blocks, and proc pointers" do
    %w(bar + - * / < <= == > >= % | & ^ ** === =~ != []= !~).each do |name|
      parse_ok("foo.#{name}")
      parse_ok("foo.#{name} 1, 2")
      parse_ok("foo.#{name}(1, 2)")
      parse_ok("foo.#{name}(1, 2) { 3 }")
      parse_ok("foo.#{name} do end")
    end

    %w(<< < <= == >> > >= + - * / // % | & ^ ** === =~ !~ &+ &- &* &**).each do |op|
      parse_ok("1 #{op} 2")
      parse_ok("n #{op} 2")
      parse_ok("foo(n #{op} 2)")
      parse_ok("foo(0, n #{op} 2)")
      parse_ok("foo(a: n #{op} 2)")
      parse_ok("foo(z: 0, a: n #{op} 2)")
      parse_ok("def #{op}(); end")
      parse_ok("foo = 1; ->foo.#{op}(Int32)")
      parse_ok("->Foo.#{op}(Int32)")
    end

    %w([] []=).each do |op|
      parse_ok("foo = 1; ->foo.#{op}(Int32)")
      parse_ok("->Foo.#{op}(Int32)")
    end
  end

  it_parses "a = 1; a += 1"
  it_parses "a = 1; a +=\n1"
  it_parses "a.b +=\n1"
  it_parses "a = 1; a -= 1"
  it_parses "a = 1; a -=\n1"
  it_parses "a.b -=\n1"
  it_parses "a = 1; a *= 1"
  it_parses "a = 1; a *=\n1"
  it_parses "a.b *=\n1"
  it_parses "a = 1; a /= 1"
  it_parses "a = 1; a /=\n1"
  it_parses "a.b /=\n1"
  it_parses "a = 1; a //= 1"
  it_parses "a = 1; a //=\n1"
  it_parses "a.b //=\n1"
  it_parses "a = 1; a %= 1"
  it_parses "a = 1; a %=\n1"
  it_parses "a.b %=\n1"
  it_parses "a = 1; a |= 1"
  it_parses "a = 1; a |=\n1"
  it_parses "a.b |=\n1"
  it_parses "a = 1; a &= 1"
  it_parses "a = 1; a &=\n1"
  it_parses "a.b &=\n1"
  it_parses "a = 1; a ^= 1"
  it_parses "a = 1; a ^=\n1"
  it_parses "a.b ^=\n1"
  it_parses "a = 1; a **= 1"
  it_parses "a = 1; a **=\n1"
  it_parses "a.b **=\n1"
  it_parses "a = 1; a <<= 1"
  it_parses "a = 1; a <<=\n1"
  it_parses "a.b <<=\n1"
  it_parses "a = 1; a >>= 1"
  it_parses "a = 1; a >>=\n1"
  it_parses "a.b >>=\n1"
  it_parses "a = 1; a &+= 1"
  it_parses "a = 1; a &+=\n1"
  it_parses "a.b &+=\n1"
  it_parses "a = 1; a &-= 1"
  it_parses "a = 1; a &-=\n1"
  it_parses "a.b &-=\n1"
  it_parses "a = 1; a &*= 1"
  it_parses "a = 1; a &*=\n1"
  it_parses "a.b &*=\n1"
  it_parses "a = 1; a &&= 1"
  it_parses "a = 1; a ||= 1"
  it_parses "a = 1; a[2] &&= 3"
  it_parses "a = 1; a[2] ||= 3"

  it_diagnoses "case 1\nwhen .=(2)", "unexpected token"
  it_diagnoses "case 1\nwhen .+=(2)", "unexpected token"
  it_diagnoses "case 1\nwhen .&&(2)", "unexpected token"
end
