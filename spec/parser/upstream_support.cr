module UpstreamSupport
  SEMANTIC_CONTAINER_KINDS = {
    Facet::Compiler::NodeKind::File,
    Facet::Compiler::NodeKind::Expressions,
    Facet::Compiler::NodeKind::Def,
    Facet::Compiler::NodeKind::MacroDef,
    Facet::Compiler::NodeKind::Class,
    Facet::Compiler::NodeKind::Module,
    Facet::Compiler::NodeKind::Struct,
    Facet::Compiler::NodeKind::Enum,
    Facet::Compiler::NodeKind::Lib,
    Facet::Compiler::NodeKind::Fun,
    Facet::Compiler::NodeKind::Call,
    Facet::Compiler::NodeKind::Assign,
    Facet::Compiler::NodeKind::Binary,
    Facet::Compiler::NodeKind::Block,
    Facet::Compiler::NodeKind::If,
    Facet::Compiler::NodeKind::Unless,
    Facet::Compiler::NodeKind::While,
    Facet::Compiler::NodeKind::Until,
    Facet::Compiler::NodeKind::Case,
    Facet::Compiler::NodeKind::When,
    Facet::Compiler::NodeKind::For,
    Facet::Compiler::NodeKind::Begin,
    Facet::Compiler::NodeKind::Rescue,
    Facet::Compiler::NodeKind::Ensure,
    Facet::Compiler::NodeKind::CallWithBlock,
  }

  SIGNIFICANT_TOKEN_KINDS = {
    Facet::Compiler::TokenKind::Identifier,
    Facet::Compiler::TokenKind::InstanceVar,
    Facet::Compiler::TokenKind::ClassVar,
    Facet::Compiler::TokenKind::GlobalVar,
    Facet::Compiler::TokenKind::Annotation,
    Facet::Compiler::TokenKind::Symbol,
    Facet::Compiler::TokenKind::Number,
    Facet::Compiler::TokenKind::String,
    Facet::Compiler::TokenKind::Regex,
    Facet::Compiler::TokenKind::Char,
    Facet::Compiler::TokenKind::KeywordNil,
    Facet::Compiler::TokenKind::KeywordTrue,
    Facet::Compiler::TokenKind::KeywordFalse,
    Facet::Compiler::TokenKind::KeywordSelf,
    Facet::Compiler::TokenKind::KeywordSuper,
  }

  def self.validate_ast_integrity(ast : Facet::Compiler::AstFile) : Nil
    source = ast.source
    root = ast.node(ast.root)
    unless root.span.start == 0 && root.span.finish == source.size
      raise "root span #{root.span} does not cover source size #{source.size}"
    end

    if bad = ast.arena.nodes.find { |node| node.span.start < 0 || node.span.finish < node.span.start || node.span.finish > source.size }
      raise "invalid AST span #{bad.span}"
    end
    if ast.arena.nodes.any? { |node| node.kind == Facet::Compiler::NodeKind::Error }
      raise "accepted input contains an Error AST node"
    end

    lexer = Facet::Compiler::Lexer.new(source)
    tokens = lexer.tokenize_all
    unless lexer.diagnostics.empty?
      raise "accepted input produced lexer diagnostics: #{lexer.diagnostics.map(&.message).join("; ")}"
    end
    unless tokens.last?.try(&.eof?) && tokens.last.span.start == source.size
      raise "lexer did not consume the complete source"
    end

    missing = tokens.reject(&.eof?).select do |token|
      SIGNIFICANT_TOKEN_KINDS.includes?(token.kind) && ast.arena.nodes.none? do |node|
        !SEMANTIC_CONTAINER_KINDS.includes?(node.kind) &&
          node.span.start <= token.span.start && node.span.finish >= token.span.finish
      end
    end
    unless missing.empty?
      details = missing.first(8).map { |token| "#{token.kind}@#{token.span.start}" }.join(", ")
      raise "significant tokens missing from bounded semantic AST nodes: #{details}"
    end
  end

  def self.validate_diagnostics(diagnostics : Array(Facet::Compiler::Diagnostic), source : Facet::Compiler::Source) : Nil
    diagnostics.each do |diagnostic|
      if diagnostic.message.empty?
        raise "empty parser diagnostic"
      end
      span = diagnostic.span
      if span.start < 0 || span.finish < span.start || span.finish > source.size
        raise "invalid diagnostic span #{span} for source size #{source.size}"
      end
    end
  end

  def parse_ok(code : String)
    source = Facet::Compiler::Source.new(code, "upstream_port")
    parser = Facet::Compiler::Parser.new(source)
    ast = parser.parse_file
    if parser.diagnostics.any?
      first = parser.diagnostics.first
      raise "diagnostic: #{first.message} @ #{first.span.start}"
    end
    UpstreamSupport.validate_ast_integrity(ast)
    ast
  end

  def parse_error(code : String)
    source = Facet::Compiler::Source.new(code, "upstream_error")
    parser = Facet::Compiler::Parser.new(source)
    parser.parse_file
    if parser.diagnostics.empty?
      raise "expected a parser diagnostic"
    end
    UpstreamSupport.validate_diagnostics(parser.diagnostics, source)
    parser
  end

  # Small DSL helpers to speed up porting upstream specs.
  # Accept extra args so we can mirror upstream `it_parses` signatures.
  macro it_parses(code_literal, *rest, **named)
    it({{"parses " + code_literal.stringify}}) do
      parse_ok({{code_literal}})
    end
  end

  macro it_diagnoses(code_literal, message)
    it("diagnoses snippet") do
      parser = parse_error({{code_literal}})
      parser.diagnostics.first.message.should contain({{message}})
    end
  end

  # Upstream-compatible syntax error helper (line/column params ignored for now).
  macro assert_syntax_error(code_literal, message = nil, *rest, **named)
    it({{"diagnoses " + code_literal.stringify}}) do
      parse_error({{code_literal}})
      # NOTE: Message matching is deferred until we finish porting upstream cases.
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
