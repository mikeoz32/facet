require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (error recovery)" do
  it "reports invalid trailing comma in call" do
    source = Facet::Compiler::Source.new("foo(1, )", "err")
    parser = Facet::Compiler::Parser.new(source)
    parser.parse_file
    parser.diagnostics.should_not be_empty
    parser.diagnostics.first.message.should contain("trailing comma")
  end
end
