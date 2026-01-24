require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (proc pointers and arrows)" do
  it "parses proc pointer syntax" do
    %w(foo ->bar foo ->bar= foo ->self.bar foo ->self.bar= foo ->Bar.baz foo ->Bar.baz= foo ->@bar.baz foo ->@bar.baz= foo ->@@bar.baz foo ->@@bar.baz= foo ->bar(Baz)).each do |code|
      parse_ok(code)
    end
  end

  it "parses lambda literal" do
    parse_ok("->{ }")
  end

  it_parses "-> do end"
  it_parses "-> { }"
  it_parses "->() { }"
  it_parses "->(x : Int32) { }"
  it_parses "->(x : Int32) { x }"
  it_parses "->(x) { x }"
  it_parses "x = 1; ->{ x }"
  it_parses "f ->{ a do\n end\n }"
  it_parses "-> : Int32 { }"
  it_parses "->\n:\nInt32\n{\n}"
  it_parses "->() : Int32 { }"
  it_parses "->() : Int32 do end"
  it_parses "->(x : Int32) : Int32 { }"

  it_parses "->foo"
  it_parses "foo = 1; ->foo"
  it_parses "->Foo.foo"
  it_parses "->@foo.foo"
  it_parses "->@@foo.foo"
  it_parses "->::foo"
  it_parses "->::Foo.foo"
  it_parses "->Foo::Bar::Baz.foo"
  it_parses "->foo(Int32, Float64)"
  it_parses "foo = 1; ->foo.bar(Int32)"
  it_parses "->foo(Void*)"
  it_parses "call ->foo"
  it_parses "[] of ->"
  it_parses "[] of ->\n1"

  it_parses "foo &->bar"
end
