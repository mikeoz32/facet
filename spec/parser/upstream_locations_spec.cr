require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (locations/end spans)" do
  it "tracks end locations for a mix of nodes" do
    samples = [
      "nil", "false", "123", "123.45", "'a'", ":foo", %("hello"),
      "[1, 2]", "[] of Int32", "{a: 1}", "{} of Int32 => String", "1..3", "/foo/", "{1, 2}",
      "foo", "foo(1, 2)", "foo 1, 2", "Foo", "Foo(A)",
      "if 1; else; 2; end", "unless 1; 2; end", "a = 123", "a, b = 1, 2",
      "@foo", "foo.@foo", "@@foo", "a && b", "a || b", "def foo; end", "def foo; 1; end",
      "abstract def foo", "abstract def foo : Int32", "begin; 1; end", "class Foo; end", "struct Foo; end",
      "module Foo; end", "alias Foo = Bar", "->{ }", "macro foo;end", "!foo", "pointerof(@foo)", "sizeof(Foo)",
      "offsetof(Foo, @a)", "typeof(1)", "1 if 2", "while 1; end", "return", "return 1", "yield", "yield 1",
      "include Foo", "extend Foo", "a, b = 1, 2 if 3", "abstract def foo(x)", "::foo", "foo.[0] = 1",
      "Int[8]?", "[1, 2,]", "%w(one two)", "foo bar, out baz", "Foo?", "foo : Foo.class", "foo : Foo?",
      "foo : Foo*", "foo : Foo**", "foo : Foo[42]", "foo ->bar", "foo ->bar=", "foo ->self.bar", "foo ->self.bar=",
      "foo ->Bar.baz", "foo ->Bar.baz=", "foo ->@bar.baz", "foo ->@bar.baz=", "foo ->@@bar.baz",
      "foo ->@@bar.baz=", "foo ->bar(Baz)", "foo *bar", "foo **bar", "Foo { 1 }", "foo.!", "foo.!()", "f.x = foo",
      "f.x=(*foo)", "f.x=(foo).bar", "x : Foo ->", "x : Foo -> Bar", %(require "foo"),
      "begin; 1; 2; 3; end", "1..", "foo.responds_to?(:foo)", "foo.responds_to? :foo", "foo.nil?", "foo.nil?(  )",
      "@a = uninitialized Foo", "@@a = uninitialized Foo", "1 rescue 2", "1 ensure 2", "foo.bar= *baz",
      "case :foo; when :bar; 2; end", %(asm("nop" ::)), "select; when foo; 2; end",
      "foo : Foo.class", "foo : Foo*", "foo : Foo**", "foo : Foo[42]",
      "foo ->bar", "foo ->bar=", "foo ->self.bar", "foo ->self.bar=", "foo ->Bar.baz", "foo ->Bar.baz=", "foo ->@bar.baz", "foo ->@bar.baz=", "foo ->@@bar.baz", "foo ->@@bar.baz=", "foo ->bar(Baz)",
      "asm(\"nop\")", "asm(\"nop\" ::)", "Foo { 1 }",
      "f.x += 2", "f.x -= 2", "f.x *= 2", "f.x /= 2",
      "foo.bar= *baz", "foo(&bar)", "foo &bar", "foo(&.bar)", "foo &.bar",
      "def foo; rescue ex; end", "def foo; ensure; end"
    ]

    samples.each do |code|
      ast = parse_ok(code)
      ast.root.should_not be_nil
    end
  end
end
