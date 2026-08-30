require "./spec_helper"

describe Facet::Compiler::SyntaxTree do
  it "exposes stable declaration roles without leaking child positions" do
    source = Facet::Compiler::Source.new(<<-CRYSTAL, "roles.cr")
      # Generic container.
      class Box(T) < Base
        # Converts a value.
        def transform(value : Int32) : String
          compute(value)
        end
      end
    CRYSTAL
    ast = Facet::Compiler::Parser.new(source).parse_file
    tree = Facet::Compiler::SyntaxTree.new(ast)

    box = tree.nodes(Facet::Compiler::NodeKind::Class).first
    box.name.should eq("Box")
    box.name_span.try { |span| source.text.byte_slice(span.start, span.length) }.should eq("Box")
    box.superclass.try(&.symbol_name).should eq("Base")
    box.body.should_not be_nil
    box.doc.should eq("Generic container.")

    method = tree.nodes(Facet::Compiler::NodeKind::Def).first
    method.name.should eq("transform")
    method.parameters.map(&.name).should eq(["value"])
    method.return_type.try(&.symbol_name).should eq("String")
    method.body.should_not be_nil
    method.doc.should eq("Converts a value.")

    call = tree.nodes(Facet::Compiler::NodeKind::Call).find { |node| node.callee.try(&.symbol_name) == "compute" }
    call.should_not be_nil
    call.not_nil!.arguments.map(&.symbol_name).should eq(["value"])
  end

  it "finds the smallest node and provides its semantic ancestors" do
    source = Facet::Compiler::Source.new("class Box\n  def transform; 1; end\nend")
    tree = Facet::Compiler::SyntaxTree.new(Facet::Compiler::Parser.new(source).parse_file)
    offset = source.text.index("transform").not_nil!

    node = tree.node_at(offset)
    node.should_not be_nil
    node.not_nil!.symbol_name.should eq("transform")
    node.not_nil!.ancestor(Facet::Compiler::NodeKind::Def).try(&.name).should eq("transform")
    node.not_nil!.ancestor(Facet::Compiler::NodeKind::Class).try(&.name).should eq("Box")
  end

  it "converts byte offsets to UTF-8 and UTF-16 editor positions" do
    source = Facet::Compiler::Source.new("a😀b\nnext")
    index = Facet::Compiler::LineIndex.new(source)
    b_offset = "a😀".bytesize

    index.position_at(b_offset, Facet::Compiler::PositionEncoding::Utf8).should eq(
      Facet::Compiler::TextPosition.new(0, 5)
    )
    index.position_at(b_offset, Facet::Compiler::PositionEncoding::Utf16).should eq(
      Facet::Compiler::TextPosition.new(0, 3)
    )
    index.offset_at(
      Facet::Compiler::TextPosition.new(0, 3),
      Facet::Compiler::PositionEncoding::Utf16
    ).should eq(b_offset)
    index.offset_at(Facet::Compiler::TextPosition.new(1, 2)).should eq("a😀b\nne".bytesize)
  end
end
