require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (operators and edge calls)" do
  it_parses "def +(other); end"
  it_parses "def %(other); end"
  it_parses "def **(other); end"
  it_parses "def [](idx); end"
  it_parses "def []=(idx, val); end"

  it_parses "-foo"
  it_parses "foo ? bar : baz"
  it_parses "case x\nwhen .>(y)\nend"
  it_parses "..10"
  it_parses "10.."
  it_parses "1...2"

  it "parses abbreviated operator assignment" do
    %w(+= -= *= /= //= %= |= &= ^= **=).each do |op|
      parse_ok("f.x #{op} 2")
    end

    %w(<<= >>= &+= &-= &*= &**=).each do |op|
      parse_ok("f.x #{op} 2")
    end
  end

  it_diagnoses "case 1\nwhen .=(2)", "unexpected token"
  it_diagnoses "case 1\nwhen .+=(2)", "unexpected token"
  it_diagnoses "case 1\nwhen .&&(2)", "unexpected token"
end
