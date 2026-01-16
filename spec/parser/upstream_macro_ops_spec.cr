require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (macro operator names)" do
  %w(` << < <= == === != =~ !~ >> > >= + - * / // ~ % & | ^ ** []? []= <=> &+ &- &* &**).each do |name|
    it_parses "macro #{name};end"
  end

  it_diagnoses "macro !;end", "pseudo-method"
end
