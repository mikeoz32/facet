require "../spec_helper"
require "./upstream_support"

extend UpstreamSupport

describe "Parser upstream parity (structures and visibility)" do
  it_parses "class Foo; end"
  it_parses "module Foo; end"
  it_parses "struct Foo; end"
  it_parses "enum Foo; end"
  it_parses "lib LibC; end"
  it_parses "abstract class Foo; end"
  it_parses "abstract struct Foo; end"
  it_parses "abstract module Foo; end"

  it_parses "class Foo < Bar; end"
  it_parses "alias Name = Int32"
  it_parses "type Name = Int32"

  it_parses "private def foo; end"
  it_parses "protected def foo; end"
  it_parses "private macro foo; end"

  it_parses "class Foo; property bar; end"
  it_parses "class Foo; getter bar; end"
  it_parses "class Foo; setter bar; end"
  it_parses "class Foo; property bar : Int32; end"
  it_parses "def initialize(@x : Int32); end"
end
