require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (annotations)" do
  it_parses "@[Foo]\nclass Bar; end"
  it_parses "@[Foo, Bar]\ndef baz; end"
  it_parses "def foo(@[Ann] arg : Int32); end"
  it_parses "@[Foo(1, foo: 2)] def bar; end"
  it_parses "@[Foo::Bar] class Baz; end"
  it_parses "@[Foo] def self.bar; end"
  it_parses "@[Foo] property bar"
  it_parses "macro foo(@[Foo] var);end"
  it_parses "macro foo(@[Foo] outer inner);end"
  it_parses "macro foo(@[Foo]  var);end"
  it_parses "macro foo(a, @[Foo] var);end"
  it_parses "macro foo(a, @[Foo] &block);end"
  it_parses "macro foo(@[Foo] *args);end"
  it_parses "macro foo(@[Foo] **args);end"
  it_parses <<-CRYSTAL
    macro foo(
      @[Foo]
      id,
      @[Bar] name
    );end
  CRYSTAL
end
