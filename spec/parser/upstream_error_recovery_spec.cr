require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (error recovery)" do
  it "allows trailing comma in call" do
    parse_ok("foo(1, )")
  end
end
