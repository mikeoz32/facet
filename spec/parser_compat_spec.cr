require "./spec_helper"

describe "Parser compatibility with Crystal surface" do
  it "parses operator defs that look like percent-literals" do
    source = Facet::Compiler::Source.new("def %(x); end\ndef **(x); end\n")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    defs = ast.children(exprs)
    defs.size.should eq(2)
    ast.node(defs[0]).kind.should eq(Facet::Compiler::NodeKind::Def)
    ast.node(defs[1]).kind.should eq(Facet::Compiler::NodeKind::Def)
  end

  it "parses implicit-dot call in when conditions" do
    source = Facet::Compiler::Source.new("case value\nwhen .>(limit)\nend\n")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    case_id = ast.children(exprs)[0]
    ast.node(case_id).kind.should eq(Facet::Compiler::NodeKind::Case)
  end

  it "parses command-style call blocks with braces" do
    source = Facet::Compiler::Source.new("Time.measure { yield }\n")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    expr_id = ast.children(exprs)[0]
    ast.node(expr_id).kind.should eq(Facet::Compiler::NodeKind::Binary)
    rhs_id = ast.children(expr_id)[1]
    ast.node(rhs_id).kind.should eq(Facet::Compiler::NodeKind::CallWithBlock)
  end

  it "parses params with external names and ivars" do
    source = Facet::Compiler::Source.new("def initialize(name @name : String, counter @counter : Int32); end\n")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    def_id = ast.children(exprs)[0]
    params_id = ast.children(def_id)[1]
    params = ast.children(params_id)
    params.size.should eq(2)
    first_param_children = ast.children(params[0])
    first_param_children.size.should be >= 2
    ast.node(first_param_children[1]).kind.should eq(Facet::Compiler::NodeKind::InstanceVar)
  end

  it "parses beginless and endless ranges" do
    source = Facet::Compiler::Source.new("..10\n10..\n")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    ranges = ast.children(exprs)
    ranges.size.should eq(2)

    beginless_children = ast.children(ranges[0])
    ast.node(beginless_children[0]).kind.should eq(Facet::Compiler::NodeKind::Nop)
    ast.node(beginless_children[1]).kind.should_not eq(Facet::Compiler::NodeKind::Nop)

    endless_children = ast.children(ranges[1])
    ast.node(endless_children[0]).kind.should_not eq(Facet::Compiler::NodeKind::Nop)
    ast.node(endless_children[1]).kind.should eq(Facet::Compiler::NodeKind::Nop)
  end
end
