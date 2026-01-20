require "../spec_helper"
require "./upstream_support"

include UpstreamSupport

describe "Parser upstream parity (multi-assign / splat)" do
  it_parses "a = 1"
  it_parses "a, b = 1, 2"
  it_parses "a, b = 1"
  it_parses "_, _ = 1, 2"
  it_parses "a[0], a[1] = 1, 2"
  it_parses "a[], a[] = 1, 2"
  it_parses "a.foo, a.bar = 1, 2"
  it_parses "x = 0; a, b = x += 1"
  it_parses "a, b = 1, 2 if 3"

  it_parses "*a = 1"
  it_parses "*a = 1, 2"
  it_parses "*_ = 1, 2"

  it_parses "*a, b = 1"
  it_parses "a, *b = 1"
  it_parses "a, *b = 1, 2"
  it_parses "*a, b = 1, 2, 3, 4"
  it_parses "a, b, *c = 1"
  it_parses "a, b, *c = 1, 2"
  it_parses "_, *_, _, _ = 1, 2, 3"

  it_parses "*a.foo, a.bar = 1"
  it_parses "a.foo, *a.bar = 1"

  it_parses "@a, b = 1, 2"
  it_parses "@@a, b = 1, 2"

  # use ASCII identifiers here to avoid encoding edge cases in tests
  it_parses "a.b, c.d = 1, 2"

  it "diagnoses invalid assignment forms" do
    # specific message expectations
    parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("b? = 1", "diag"))
    parser.parse_file
    parser.diagnostics.should_not be_empty
    parser.diagnostics.first.message.should include("=")

    parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("b! = 1", "diag"))
    parser.parse_file
    parser.diagnostics.should_not be_empty
    parser.diagnostics.first.message.should include("=")

    parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new("a, B = 1, 2", "diag"))
    parser.parse_file
    parser.diagnostics.should_not be_empty
    parser.diagnostics.first.message.should include("constant")

    # errors that just need to produce a diagnostic
    [
      "1 == 2, a = 4",
      "x : String, a = 4",
      "b, 1 == 2, a = 4",
      "a = 1, 2, 3",
      "a = 1, b = 2"
    ].each do |src|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(src, "diag"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end

    # splat-related errors
    [
      "*a",
      "*a if true",
      "*a if true = 2",
      "*a, 1 = 2",
      "*1, a = 2",
      "*a, *b = 1"
    ].each do |src|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(src, "diag"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end

    # multiple assignment mismatch cases
    [
      "*a, b, c, d = 1, 2",
      "a, b, *c, d = 1, 2",
      "*a, b, c, d, e = 1, 2",
      "a, b, c, d, *e = 1, 2, 3"
    ].each do |src|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(src, "diag"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end

    # unexpected splat in RHS
    [
      "a = *1",
      "a = *1, 2",
      "a = 1, *2",
      "a, b = *1",
      "a, b = *1, 2",
      "a, b = 1, *2",
      "a, *b = *1",
      "a, *b = *1, 2",
      "a, *b = 1, *2"
    ].each do |src|
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(src, "diag"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
    end
  end
end
