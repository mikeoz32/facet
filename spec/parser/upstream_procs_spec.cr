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
end
