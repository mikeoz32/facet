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
  it_parses "offsetof(Int32, a)"
  it_parses "offsetof(Int32, @a)"
  it_parses "offsetof(Int32, 1)"
  it_parses "pointerof(@foo)"

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
