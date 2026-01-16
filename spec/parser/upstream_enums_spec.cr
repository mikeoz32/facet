require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (enums and aliases)" do
  it_parses "enum Color\n  Red\n  Green\n  Blue\nend"
  it_parses "enum Flag\n  A = 1\n  B = 2\nend"
  it_parses "alias Name = Int32"
  it_parses "type Name = Int32"
  it_parses "enum X; @[Foo]; Bar; end"
  it_parses "enum X; private def foo; end; end"
  it_parses "enum X; protected macro foo; end; end"
end
