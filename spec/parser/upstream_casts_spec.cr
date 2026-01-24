require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (casts and typeof)" do
  it_parses "alias Foo = Bar"
  it_parses "alias Foo::Bar = Baz"
  assert_syntax_error "alias Foo?"

  it_parses "def foo\n1\nend\nif 1\nend"

  assert_syntax_error "1 as Bar"
  assert_syntax_error "1 as? Bar"

  it_parses "1.as Bar"
  it_parses "1.as(Bar)"
  it_parses "foo.as(Bar)"
  it_parses "foo.bar.as(Bar)"
  it_parses "call(foo.as Bar, Baz)"
  it_parses "as(Bar)"

  it_parses "1.as? Bar"
  it_parses "1.as?(Bar)"
  it_parses "as?(Bar)"

  it_parses "typeof(1)"
  it_parses "typeof(a = 1); a"
end
