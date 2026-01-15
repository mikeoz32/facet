require "./spec_helper"

describe Facet::Compiler::Parser do
  it "parses identifiers and interns symbols" do
    source = Facet::Compiler::Source.new("foo; bar")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    expr_children = ast.children(exprs)
    expr_children.size.should eq(2)

    first = ast.node(expr_children[0])
    second = ast.node(expr_children[1])
    first.kind.should eq(Facet::Compiler::NodeKind::Ident)
    second.kind.should eq(Facet::Compiler::NodeKind::Ident)

    ast.arena.symbols[first.payload_index].should eq("foo")
    ast.arena.symbols[second.payload_index].should eq("bar")
  end

  it "parses binary expressions" do
    source = Facet::Compiler::Source.new("1 + 2")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    expr = ast.children(exprs)[0]
    node = ast.node(expr)
    node.kind.should eq(Facet::Compiler::NodeKind::Binary)
    ast.arena.operator_kind(node.payload_index).should eq(Facet::Compiler::TokenKind::Plus)
  end

  it "parses calls with arguments" do
    source = Facet::Compiler::Source.new("foo(1, 2)")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    call_id = ast.children(exprs)[0]
    call = ast.node(call_id)
    call.kind.should eq(Facet::Compiler::NodeKind::Call)

    args_id = ast.children(call_id)[1]
    ast.node(args_id).kind.should eq(Facet::Compiler::NodeKind::Args)
    ast.children(args_id).size.should eq(2)
  end

  it "parses named arguments" do
    source = Facet::Compiler::Source.new("foo(bar: 1, baz: 2)")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    call_id = ast.children(exprs)[0]
    args_id = ast.children(call_id)[1]
    args = ast.children(args_id)
    args.size.should eq(2)
    ast.node(args[0]).kind.should eq(Facet::Compiler::NodeKind::NamedArg)
    ast.node(args[1]).kind.should eq(Facet::Compiler::NodeKind::NamedArg)
  end

  it "parses hash and named tuple literals" do
    source = Facet::Compiler::Source.new("{foo: 1} {\"a\" => 2}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    first = ast.children(exprs)[0]
    second = ast.children(exprs)[1]
    ast.node(first).kind.should eq(Facet::Compiler::NodeKind::NamedTuple)
    ast.node(second).kind.should eq(Facet::Compiler::NodeKind::Hash)
  end

  it "parses macro expressions" do
    source = Facet::Compiler::Source.new("{{ 1 + 2 }}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    macro_id = ast.children(exprs)[0]
    ast.node(macro_id).kind.should eq(Facet::Compiler::NodeKind::MacroExpr)
  end

  it "parses macro control blocks" do
    source = Facet::Compiler::Source.new("{% if true %} {% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    macro_id = ast.children(exprs)[0]
    ast.node(macro_id).kind.should eq(Facet::Compiler::NodeKind::MacroControl)
    ast.macro_control_tag(macro_id).should eq(Facet::Compiler::TokenKind::KeywordIf)
  end

  it "parses def params with types, defaults, splats, and block arg" do
    source = Facet::Compiler::Source.new("def foo(x : Int32, y = 1, *args, **opts, &block) : Nil\nend")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    def_id = ast.children(exprs)[0]
    def_node = ast.node(def_id)
    def_node.kind.should eq(Facet::Compiler::NodeKind::Def)

    params_id = ast.children(def_id)[1]
    params = ast.children(params_id)
    params.size.should eq(5)
    ast.node(params[0]).kind.should eq(Facet::Compiler::NodeKind::Param)
    ast.node(params[1]).kind.should eq(Facet::Compiler::NodeKind::Param)
    ast.node(params[2]).kind.should eq(Facet::Compiler::NodeKind::Splat)
    ast.node(params[3]).kind.should eq(Facet::Compiler::NodeKind::DoubleSplat)
    ast.node(params[4]).kind.should eq(Facet::Compiler::NodeKind::BlockParam)
  end

  it "parses nilable and path types in annotations" do
    source = Facet::Compiler::Source.new("def foo(x : Foo?, y : Foo::Bar | Baz) : Foo(Int32)?\nend")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    def_id = ast.children(exprs)[0]

    params_id = ast.children(def_id)[1]
    params = ast.children(params_id)

    x_type_id = ast.children(params[0])[1]
    x_type = ast.node(x_type_id)
    x_type.kind.should eq(Facet::Compiler::NodeKind::Binary)
    ast.arena.operator_kind(x_type.payload_index).should eq(Facet::Compiler::TokenKind::Pipe)

    x_left_id = ast.children(x_type_id)[0]
    x_left = ast.node(x_left_id)
    x_left.kind.should eq(Facet::Compiler::NodeKind::Ident)
    ast.arena.symbols[x_left.payload_index].should eq("Foo")

    y_type_id = ast.children(params[1])[1]
    y_type = ast.node(y_type_id)
    y_type.kind.should eq(Facet::Compiler::NodeKind::Binary)
    ast.arena.operator_kind(y_type.payload_index).should eq(Facet::Compiler::TokenKind::Pipe)

    y_left_id = ast.children(y_type_id)[0]
    ast.node(y_left_id).kind.should eq(Facet::Compiler::NodeKind::Path)

    return_type_id = ast.children(def_id)[2]
    return_type = ast.node(return_type_id)
    return_type.kind.should eq(Facet::Compiler::NodeKind::Binary)
    return_left_id = ast.children(return_type_id)[0]
    ast.node(return_left_id).kind.should eq(Facet::Compiler::NodeKind::TypeApply)
  end

  it "parses macro variables inside macro expressions" do
    source = Facet::Compiler::Source.new("{{ %foo }}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    macro_id = ast.children(exprs)[0]
    body_id = ast.children(macro_id)[0]
    body_exprs = ast.children(body_id)
    body_exprs.size.should eq(1)
    ast.node(body_exprs[0]).kind.should eq(Facet::Compiler::NodeKind::MacroVar)
  end

  it "parses macro control blocks with elsif and else" do
    source = Facet::Compiler::Source.new("{% if true %} 1 {% elsif false %} 2 {% else %} 3 {% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    macro_id = ast.children(exprs)[0]
    ast.node(macro_id).kind.should eq(Facet::Compiler::NodeKind::MacroControl)
  end

  it "parses macro for headers with in" do
    source = Facet::Compiler::Source.new("{% for x in items %} 1 {% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    macro_id = ast.children(exprs)[0]
    ast.node(macro_id).kind.should eq(Facet::Compiler::NodeKind::MacroControl)

    header_id = ast.children(macro_id)[0]
    ast.node(header_id).kind.should eq(Facet::Compiler::NodeKind::MacroForHeader)
  end

  it "parses macro for headers with multiple targets" do
    source = Facet::Compiler::Source.new("{% for x, y in items %} 1 {% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    macro_id = ast.children(exprs)[0]
    header_id = ast.children(macro_id)[0]
    header = ast.node(header_id)
    header.kind.should eq(Facet::Compiler::NodeKind::MacroForHeader)

    targets_id = ast.children(header_id)[0]
    targets = ast.children(targets_id)
    targets.size.should eq(2)
  end

  it "parses nested macro controls mixed with loops" do
    source = Facet::Compiler::Source.new("{% for x in [1,2] %}{% if true %}a{% end %}{% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.should be_empty
    exprs = ast.children(ast.root)[0]
    macro_for = ast.children(exprs)[0]
    ast.node(macro_for).kind.should eq(Facet::Compiler::NodeKind::MacroControl)

    body_id = ast.children(macro_for)[1]
    body_exprs = ast.children(body_id)
    body_exprs.size.should eq(1)
    inner_macro = body_exprs[0]
    ast.node(inner_macro).kind.should eq(Facet::Compiler::NodeKind::MacroControl)
  end

  it "recovers from unmatched macro control end" do
    source = Facet::Compiler::Source.new("{% if true %} 1 ")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.any? { |d| d.message.includes?("expected '{% end %}' to close macro if") }.should be_true
  end

  it "extracts macro for header parts via helper" do
    source = Facet::Compiler::Source.new("{% for x, y in items %} 1 {% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    macro_id = ast.children(ast.children(ast.root)[0])[0]
    header_id = ast.children(macro_id)[0]
    parts = ast.macro_for_header_parts(header_id)
    parts.should_not be_nil
    targets_id, iter_id = parts.not_nil!
    ast.node(targets_id).kind.should eq(Facet::Compiler::NodeKind::Args)
    ast.node(iter_id).kind.should_not eq(Facet::Compiler::NodeKind::Error)

    targets = ast.macro_for_targets(header_id)
    targets.should_not be_nil
    targets.not_nil!.size.should eq(2)
  end

  it "recovers macro for header missing target" do
    source = Facet::Compiler::Source.new("{% for in items %} {% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.any? { |d| d.message.includes?("expected for loop variable") }.should be_true
    macro_id = ast.children(ast.children(ast.root)[0])[0]
    header_id = ast.children(macro_id)[0]
    ast.node(header_id).kind.should eq(Facet::Compiler::NodeKind::MacroForHeader)
  end

  it "recovers macro for header missing 'in'" do
    source = Facet::Compiler::Source.new("{% for x items %} {% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.any? { |d| d.message.includes?("expected 'in' in macro for") }.should be_true
  end

  it "recovers macro for header missing iterable" do
    source = Facet::Compiler::Source.new("{% for x in %} {% end %}")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    ast.diagnostics.any? { |d| d.message.includes?("expected expression after 'in'") }.should be_true
  end

  it "parses require statements" do
    source = Facet::Compiler::Source.new("require \"./foo\"")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    req = ast.children(exprs)[0]
    ast.node(req).kind.should eq(Facet::Compiler::NodeKind::Require)
  end

  it "parses class inheritance" do
    source = Facet::Compiler::Source.new("class Foo < Bar; end")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    cls = ast.children(exprs)[0]
    ast.node(cls).kind.should eq(Facet::Compiler::NodeKind::Class)
    ast.children(cls).size.should eq(3)
  end

  it "parses call blocks with parameters" do
    source = Facet::Compiler::Source.new("foo(1).bar do |x, y| x end")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    binary_id = ast.children(exprs)[0]
    binary = ast.node(binary_id)
    binary.kind.should eq(Facet::Compiler::NodeKind::Binary)
    rhs = ast.children(binary_id)[1]
    ast.node(rhs).kind.should eq(Facet::Compiler::NodeKind::CallWithBlock)
    args = ast.children(rhs)[1]
    ast.children(args).size.should eq(2)
  end

  it "parses if expressions with else" do
    source = Facet::Compiler::Source.new("if true; 1; else; 2; end")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file

    exprs = ast.children(ast.root)[0]
    if_id = ast.children(exprs)[0]
    if_node = ast.node(if_id)
    if_node.kind.should eq(Facet::Compiler::NodeKind::If)

    cond_id = ast.children(if_id)[0]
    ast.node(cond_id).kind.should eq(Facet::Compiler::NodeKind::LiteralBool)

    then_body = ast.children(if_id)[1]
    else_body = ast.children(if_id)[2]
    ast.node(then_body).kind.should eq(Facet::Compiler::NodeKind::Expressions)
    ast.node(else_body).kind.should eq(Facet::Compiler::NodeKind::Expressions)
  end

  it "recovers from unexpected tokens" do
    source = Facet::Compiler::Source.new("if ; end")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file
    ast.diagnostics.should_not be_empty
  end
end
