require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (miscellaneous)" do
  it_parses <<-CRYSTAL
    asm("nop")
  CRYSTAL

  it_parses "sizeof(Int32)"
  it_parses "instance_sizeof(Int32)"
  it_parses "alignof(Int32)"
  it_parses "instance_alignof(Int32)"
  it_parses "sizeof(X)"
  it_parses "instance_sizeof(X)"
  it_parses "alignof(X)"
  it_parses "instance_alignof(X)"
  it_parses "offsetof(X, @a)"
  it_parses "offsetof(X, 1)"
  it_parses "sizeof(\n  Int32\n)"
  it_parses "instance_sizeof(\n  Int32\n)"
  it_parses "alignof(\n  Int32\n)"
  it_parses "instance_alignof(\n  Int32\n)"
  it_parses "offsetof(Int32, a)"
  it_parses "offsetof(Int32, @a)"
  it_parses "offsetof(Int32, 1)"
  it_parses "pointerof(@foo)"
  it_parses "pointerof(@a)"
  it_parses "a = 1; pointerof(a)"
  it_parses "offsetof(\n  Foo,\n  @foo\n)"
  it_parses "pointerof(\n  foo\n)"

  %w(
    begin nil true false yield with abstract
    def macro require case select if unless include
    extend class struct module enum while until return
    next break lib fun alias pointerof sizeof
    instance_sizeof offsetof typeof private protected asm
    end self in do else elsif when rescue ensure
  ).each do |keyword|
    it_parses "#{keyword} : Int32"
    it_parses "property #{keyword} : Int32"
  end

  it_parses <<-CRYSTAL
    macro foo
      \\{%
        1
      %}
    end
  CRYSTAL

  it_parses "\n\n__LINE__"
  it_parses "__FILE__"
  it_parses "__DIR__"
  it_parses "puts __FILE__"
  it_parses "puts __DIR__"
  it_parses "puts __LINE__"
  it_parses "puts _"
  it_parses "self"
  it_parses "def foo(x = __LINE__); end"
  it_parses "def foo(x = __FILE__); end"
  it_parses "def foo(x = __DIR__); end"
  it_parses "macro foo(x = __LINE__);end"
  it_parses "1 \\\n + 2"
  it_parses "1\\\n + 2"
  it_parses "1 \\\r\n + 2"
  it_parses "1\\\r\n + 2"
  it_parses %(\"hello " \\\n "world\")
  it_parses %(\"hello "\\\n"world\")
  it_parses %(\"hello " \\\r\n "world\")
  it_parses "puts ~1"
  it_parses "require \"foo\""
  it_parses "require \"foo\"; [1]"
end
