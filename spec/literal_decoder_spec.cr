require "./spec_helper"

private def parse_literal_nodes(code : String) : Tuple(Facet::Compiler::AstFile, Slice(Facet::Compiler::NodeId))
  source = Facet::Compiler::Source.new(code, "literal_decoder")
  parser = Facet::Compiler::Parser.new(source)
  ast = parser.parse_file
  parser.diagnostics.should be_empty
  expressions = ast.children(ast.root)[0]
  {ast, ast.children(expressions)}
end

private def collect_literal_nodes(
  ast : Facet::Compiler::AstFile,
  node_id : Facet::Compiler::NodeId,
  result = [] of Facet::Compiler::NodeId,
) : Array(Facet::Compiler::NodeId)
  node = ast.node(node_id)
  if {
       Facet::Compiler::NodeKind::LiteralString,
       Facet::Compiler::NodeKind::LiteralChar,
       Facet::Compiler::NodeKind::LiteralRegex,
       Facet::Compiler::NodeKind::LiteralSymbol,
     }.includes?(node.kind)
    result << node_id
  end
  ast.children(node_id).each { |child| collect_literal_nodes(ast, child, result) }
  result
end

describe Facet::Compiler::LiteralDecoder do
  it "decodes Crystal escapes while retaining raw literal spelling" do
    code = <<-'CR'
      "a\n\x42\u0043\u{44 45}\101\h"; '\n'; :"a\n"; %q(\n)
      CR
    ast, nodes = parse_literal_nodes(code)
    string, char, symbol, raw = nodes

    ast.literal_content_string(string).should eq(%q(a\n\x42\u0043\u{44 45}\101\h))
    ast.decoded_literal_string(string).should eq("a\nBCDEAh")
    ast.decoded_literal_string(char).should eq("\n")
    ast.decoded_literal_string(symbol).should eq("a\n")
    ast.decoded_literal_string(raw).should eq(%q(\n))
    ast.literal_style(raw).should eq(Facet::Compiler::LiteralStyle::Raw)
  end

  it "uses percent-word escape rules instead of string escape rules" do
    ast, nodes = parse_literal_nodes(%q{%w(a\ b \n \\)})
    array = nodes[0]
    words = ast.children(array)[0, 3]

    words.map { |word| ast.decoded_literal_string(word) }.to_a
      .should eq(["a b", %q(\n), %q(\)])
    words.each do |word|
      ast.literal_style(word).should eq(Facet::Compiler::LiteralStyle::Word)
    end
  end

  it "dedents heredocs, decodes interpolating bodies, and strips the terminator newline" do
    escaped_source = "<<-TEXT\n  one\\n\n    two\n  TEXT\n"
    escaped_ast, escaped_nodes = parse_literal_nodes(escaped_source)
    escaped = escaped_nodes[0]
    escaped_ast.literal_content_string(escaped).should eq("  one\\n\n    two\n")
    escaped_ast.decoded_literal_string(escaped).should eq("one\n\n  two")

    raw_source = "<<-'TEXT'\n  one\\n\n  TEXT\n"
    raw_ast, raw_nodes = parse_literal_nodes(raw_source)
    raw = raw_nodes[0]
    raw_ast.decoded_literal_string(raw).should eq(%q(one\n))
    raw_ast.literal_style(raw).should eq(Facet::Compiler::LiteralStyle::HeredocRaw)
  end

  it "preserves shifted content payloads imported from interpolations" do
    ast, nodes = parse_literal_nodes(%q{"outer #{"inner\n"}"})
    literals = collect_literal_nodes(ast, nodes[0])
    inner = literals.find { |literal| ast.node_string(literal).starts_with?('"') }.not_nil!

    ast.literal_content_string(inner).should eq(%q(inner\n))
    ast.decoded_literal_string(inner).should eq("inner\n")
    ast.literal_style(inner).should eq(Facet::Compiler::LiteralStyle::Escaped)
  end

  it "normalizes escaped regex delimiters without consuming regex escapes" do
    ast, nodes = parse_literal_nodes(%q{/\s\/\n/})
    regex = nodes[0]

    ast.decoded_literal_string(regex).should eq(%q(\s/\n))
    ast.literal_style(regex).should eq(Facet::Compiler::LiteralStyle::Regex)
  end
end
