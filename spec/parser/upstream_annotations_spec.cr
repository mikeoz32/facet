require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (annotations)" do
  it_parses "@[Foo]\nclass Bar; end"
  it_parses "@[Foo, Bar]\ndef baz; end"
  it_parses "def foo(@[Ann] arg : Int32); end"
  it_parses "@[Foo(1, foo: 2)] def bar; end"
  it_parses "@[Foo::Bar] class Baz; end"
end
