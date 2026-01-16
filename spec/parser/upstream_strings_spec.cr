require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (strings and percent literals)" do
  it_parses "%q(foo bar)"
  it_parses "%w(foo bar)"
  it_parses "%i(foo bar)"

  it_parses <<-CRYSTAL
    <<-EOF
    hello
    EOF
  CRYSTAL
end
