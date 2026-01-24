require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (hashes, named tuples, tuples)" do
  it_parses "{1 => 2, 3 => 4}"
  it_parses "{1 =>\n2, 3 =>\n4}"
  it_parses %({A::B => 1, C::D => 2})
  assert_syntax_error %({"foo" => 1, "bar": 2}), "can't use 'key: value' syntax in a hash literal"

  it_parses "{a: 1}"
  it_parses "{a: 1, b: 2}"
  it_parses "{A: 1, B: 2}"
  it_parses %({"foo": 1})
  it_parses %({"foo": 1, "bar": 2})

  assert_syntax_error "{\"\": 1}", "named tuple name cannot be empty"
  assert_syntax_error "{a: 1, \"\": 2}", "named tuple name cannot be empty"
  assert_syntax_error "{a: 1, a: 2}", "duplicated key: a"
  assert_syntax_error "{a[0]: 1}", "expecting token '=>', not ':'"
  assert_syntax_error "{a[]: 1}", "expecting token '=>', not ':'"

  it_parses "{} of Int => Double"
  it_parses "{} of Int32 -> Int32 => Int32"

  it_parses "{1}"
  it_parses "{1, 2, 3}"
  it_parses "{A::B}"
  it_parses "{\n1,\n2\n}"
  it_parses "{\n1\n}"
  it_parses "{\n{1}\n}"
  it_parses %({"".id})
end
