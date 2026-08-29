require "./spec_helper"

describe Facet do
  it "exposes the shard version" do
    Facet::VERSION.should eq("0.1.5")
  end
end
