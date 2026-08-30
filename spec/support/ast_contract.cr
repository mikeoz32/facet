module AstContractSupport
  SYMBOL_PAYLOAD_KINDS = {
    Facet::Compiler::NodeKind::Ident,
    Facet::Compiler::NodeKind::InstanceVar,
    Facet::Compiler::NodeKind::ClassVar,
    Facet::Compiler::NodeKind::Global,
    Facet::Compiler::NodeKind::NamedArg,
    Facet::Compiler::NodeKind::Param,
    Facet::Compiler::NodeKind::Splat,
    Facet::Compiler::NodeKind::DoubleSplat,
    Facet::Compiler::NodeKind::BlockParam,
    Facet::Compiler::NodeKind::MacroVar,
  }

  LITERAL_KINDS = {
    Facet::Compiler::NodeKind::LiteralNumber,
    Facet::Compiler::NodeKind::LiteralString,
    Facet::Compiler::NodeKind::LiteralChar,
    Facet::Compiler::NodeKind::LiteralRegex,
    Facet::Compiler::NodeKind::LiteralSymbol,
    Facet::Compiler::NodeKind::LiteralBool,
    Facet::Compiler::NodeKind::LiteralNil,
    Facet::Compiler::NodeKind::MacroLiteral,
  }

  SEMANTIC_FLAGS = {
    Facet::Compiler::SemanticFlag::Abstract,
    Facet::Compiler::SemanticFlag::Private,
    Facet::Compiler::SemanticFlag::Protected,
    Facet::Compiler::SemanticFlag::Union,
    Facet::Compiler::SemanticFlag::Select,
    Facet::Compiler::SemanticFlag::Exhaustive,
    Facet::Compiler::SemanticFlag::Escaped,
    Facet::Compiler::SemanticFlag::RescueClause,
  }

  def facet_ast(code : String) : Facet::Compiler::AstFile
    source = Facet::Compiler::Source.new(code, "ast_contract")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file
    unless parser.diagnostics.empty?
      details = parser.diagnostics.map { |diagnostic| "#{diagnostic.message}@#{diagnostic.span.start}" }.join("; ")
      raise "AST contract input produced diagnostics: #{details}"
    end
    violations = Facet::Compiler::AstIntegrity.contract_violations(ast)
    unless violations.empty?
      raise "AST contract violation: #{violations.first(8).join("; ")}"
    end
    ast
  end

  def facet_ast_contract(code : String) : String
    ast = facet_ast(code)
    render_contract_node(ast, ast.root)
  end

  def assert_facet_ast(code : String, expected : String) : Nil
    facet_ast_contract(code).should eq(expected)
  end

  private def render_contract_node(ast : Facet::Compiler::AstFile, node_id : Facet::Compiler::NodeId) : String
    node = ast.node(node_id)
    String.build do |io|
      io << node.kind
      render_contract_payload(io, ast, node_id)
      render_contract_flags(io, node)
      children = ast.children(node_id)
      unless children.empty?
        io << '('
        children.each_with_index do |child, index|
          io << ", " unless index == 0
          io << render_contract_node(ast, child)
        end
        io << ')'
      end
    end
  end

  private def render_contract_payload(
    io : IO,
    ast : Facet::Compiler::AstFile,
    node_id : Facet::Compiler::NodeId,
  ) : Nil
    node = ast.node(node_id)
    if SYMBOL_PAYLOAD_KINDS.includes?(node.kind)
      io << '['
      if node.payload_index >= 0
        ast.arena.symbols[node.payload_index].inspect(io)
      else
        io << "anonymous"
      end
      io << ']'
    elsif LITERAL_KINDS.includes?(node.kind)
      io << '['
      ast.node_string(node_id).inspect(io)
      io << ']'
    elsif node.kind == Facet::Compiler::NodeKind::Unary || node.kind == Facet::Compiler::NodeKind::Binary
      io << '[' << ast.arena.operator_kind(node.payload_index) << ']'
    elsif node.kind == Facet::Compiler::NodeKind::MacroControl
      io << '[' << ast.macro_control_tag(node_id) << ']'
    elsif node.kind == Facet::Compiler::NodeKind::Nop && node.span.length > 0
      io << '['
      ast.node_string(node_id).inspect(io)
      io << ']'
    end
  end

  private def render_contract_flags(io : IO, node : Facet::Compiler::Node) : Nil
    semantic = SEMANTIC_FLAGS.select { |flag| node.semantic_flag?(flag) }
    storage_flags = node.flags & 0x00ff_u16
    return if semantic.empty? && storage_flags == 0

    io << '{'
    parts = semantic.map(&.to_s)
    parts << "storage=#{storage_flags}" unless storage_flags == 0
    io << parts.join('|')
    io << '}'
  end
end
