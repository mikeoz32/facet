require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (selected errors)" do
  it "rejects wrong param type/default order" do
    source = Facet::Compiler::Source.new("def foo(var = 1 : Int32); end", "err")
    parser = Facet::Compiler::Parser.new(source)
    parser.parse_file
    parser.diagnostics.should_not be_empty
  end

  it "rejects macro unmatched end" do
    source = Facet::Compiler::Source.new("{% if true %} 1 ", "err")
    parser = Facet::Compiler::Parser.new(source)
    parser.parse_file
    parser.diagnostics.should_not be_empty
  end

  it "rejects nesting type/def inside def head" do
    %w(class module struct enum fun alias abstract include extend lib macro).each do |kw|
      source = Facet::Compiler::Source.new("def foo\n#{kw}\nend", "err")
      parser = Facet::Compiler::Parser.new(source)
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects && and || as methods" do
    [
      "foo.&&", "foo.&&()", "foo &.&&", "foo &.&&()",
      "foo.||", "foo.||()", "foo &.||", "foo &.||()",
    ].each do |code|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(code, "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects redefining pseudo-methods" do
    %w(! is_a? as as? responds_to? nil?).each do |name|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("def #{name}; end", "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty

      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("def self.#{name}; end", "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects responds_to? without target" do
    parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("foo.responds_to?", "err"))
    parser.parse_file
    parser.diagnostics.should_not be_empty
  end

  it "rejects include/extend without arguments" do
    %w(include extend).each do |kw|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("#{kw}\n", "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end

  it "rejects obviously unterminated literals and calls" do
    [
      " [1, 2, 3 end",
      " {1 => end",
      " {1, 2, 3 end",
      " (1, 2, 3 end",
      "foo(1, 2, 3 end",
      "foo(foo(&.block)",
    ].each do |code|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(code, "err"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end
end
