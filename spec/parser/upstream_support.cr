module UpstreamSupport
  def parse_ok(code : String)
    source = Facet::Compiler::Source.new(code, "upstream_port")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file
    if parser.diagnostics.any?
      first = parser.diagnostics.first
      raise "diagnostic: #{first.message} @ #{first.span.start}"
    end
    ast
  end

  # Small DSL helpers to speed up porting upstream specs.
  macro it_parses(code_literal, description = nil)
    it({{description || "parses " + code_literal.stringify}}) do
      parse_ok({{code_literal}})
    end
  end

  macro it_diagnoses(code_literal, message)
    it("diagnoses snippet") do
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new({{code_literal}}, "diag"))
      parser.parse_file
      parser.diagnostics.should_not be_empty
      parser.diagnostics.first.message.should contain({{message}})
    end
  end

  # Small helpers for AST shape assertions
  def first_def(ast)
    exprs = ast.children(ast.root)[0]
    ast.children(exprs).first
  end

  def def_params(ast, def_node = nil)
    def_node ||= first_def(ast)
    params = ast.children(def_node)[1]
    ast.children(params)
  end

  def def_return(ast, def_node = nil)
    def_node ||= first_def(ast)
    ast.children(def_node)[2]
  end

  def first_macro(ast)
    exprs = ast.children(ast.root)[0]
    ast.children(exprs).find { |id| ast.arena.node(id).kind == Facet::Compiler::NodeKind::MacroDef }
  end

  def macro_params(ast, macro_node = nil)
    macro_node ||= first_macro(ast)
    return [] of Int32 unless macro_node
    args = ast.children(macro_node)[1]
    ast.children(args)
  end

  def root_exprs(ast)
    ast.children(ast.root)
  end

  def node_kind(ast, node_id)
    ast.arena.node(node_id).kind
  end

  def binary_op_kind(ast, node_id)
    node = ast.arena.node(node_id)
    return nil unless node.kind == Facet::Compiler::NodeKind::Binary
    op_id = node.payload_index
    return nil if op_id < 0 || op_id >= ast.arena.operators.size
    ast.arena.operator_kind(op_id)
  end
end
