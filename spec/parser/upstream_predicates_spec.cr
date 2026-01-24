require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (predicates and pseudo-method calls)" do
  it_parses "foo.is_a?(Const)"
  it_parses "foo.is_a?(Foo | Bar)"
  it_parses "foo.is_a? Const"
  it_parses "foo.responds_to?(:foo)"
  it_parses "foo.responds_to? :foo"
  it_parses "if foo.responds_to? :foo\nx = 1\nend"

  it_parses "is_a?(Const)"
  it_parses "responds_to?(:foo)"
  it_parses "nil?"
  it_parses "nil?(  )"
  it_parses "foo.nil?"
  it_parses "foo.nil?(  )"

  it_parses "foo &.nil?"
  it_parses "foo &.baz.qux do\nend"
  it_parses "foo(&.is_a?(T))"
  it_parses "foo(&.responds_to?(:foo))"
  it_parses "foo(&.as?(T))"
  it_parses "foo &.as?(T)"
  it_parses "foo(&.as?(T).bar)"
  it_parses "foo &.as?(T).bar"

  it_parses "foo.!"
  it_parses "foo.!.!"
  it_parses "foo.!(  )"
  it_parses "foo &.!"

  it_parses "1.is_a?(\n  Int32\n)"
  it_parses "1.responds_to?(\n  :foo\n)"
  it_parses "1.nil?(\n)"
  it_parses "1.!(\n)"

  assert_syntax_error "foo.responds_to?", "responds_to? requires an argument"
end
