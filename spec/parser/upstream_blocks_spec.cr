require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (blocks and binding)" do
  # Block associativity surprises (#15303)
  it_parses "a b c d e do; end"
  it_parses "a b c d e {}"
  it_parses "a b c d e do 1 end do 2 end { 3 } do 4 end"
  it_parses "a b c d e { 1 } { 2 } do 3 end { 4 }"
  it_parses "a b c d e 1, 2 do; end"
  it_parses "a b c d e 1, 2 {}"
  it_parses "a 1, (2), b do end"
  it_parses "a 1, (2), b {}"

  # Blocks attached to calls with regex body
  it_parses "foo do\n//\nend"
  it_parses "foo x do\n//\nend"
  it_parses "foo(x) do\n//\nend"

  # Block parameter shapes and unpacking
  it_parses "foo { 1 }"
  it_parses "foo { |a| 1 }"
  it_parses "foo { |a, b| 1 }"
  it_parses "foo { |a, b, | 1 }"
  it_parses "foo { |(_, c)| c }"
  it_parses "foo { |(_, c, )| c }"
  it_parses "foo { |(a, (b, (c, d)))| }"
  it_parses "foo { |(a, *b, c)| }"

end
