require "json"
require "../../src/facet/compiler"

record UpstreamSemanticHeader,
  kind : String,
  crystal_version : String,
  crystal_revision : String,
  suites : Array(String),
  example_count : Int32,
  pending_count : Int32,
  contract_count : Int32,
  type_count : Int32,
  error_count : Int32,
  no_errors_count : Int32 do
  include JSON::Serializable
end

record UpstreamSemanticCase,
  id : String,
  kind : String,
  source : String,
  suite : String,
  line : Int32,
  inject_primitives : Bool,
  flags : String?,
  expected_type : String?,
  expected_error : String?,
  expected_code : String?,
  error_line : Int32?,
  error_column : Int32?,
  error_size : Int32? do
  include JSON::Serializable
end

record UpstreamSemanticResult,
  actual_type : String?,
  actual_code : String?,
  actual_line : Int32?,
  actual_column : Int32?,
  complete : Bool,
  parser_clean : Bool do
  def matches?(fixture : UpstreamSemanticCase) : Bool
    return false if fixture.inject_primitives
    return false unless @parser_clean
    case fixture.kind
    when "type"
      @actual_type == fixture.expected_type && @actual_code.nil?
    when "error"
      return false unless @complete
      return false unless fixture.expected_code && @actual_code == fixture.expected_code
      return false if fixture.error_line && @actual_line != fixture.error_line
      return false if fixture.error_column && @actual_column != fixture.error_column
      true
    else
      false
    end
  end
end

module UpstreamSemanticParity
  extend self

  def classification(fixture : UpstreamSemanticCase, result : UpstreamSemanticResult) : String
    return "supported" if result.matches?(fixture)
    return "inject_primitives" if fixture.inject_primitives
    return "target_flags" if fixture.flags
    return "parser" unless result.parser_clean
    return "incomplete" unless result.complete
    return "unsupported_error" if fixture.kind == "error" && fixture.expected_code.nil?
    return "type_mismatch" if fixture.kind == "type"
    "diagnostic_mismatch"
  end

  def load(path : String) : {UpstreamSemanticHeader, Array(UpstreamSemanticCase)}
    lines = File.read_lines(path).reject(&.blank?)
    header = UpstreamSemanticHeader.from_json(lines.shift)
    {header, lines.map { |line| UpstreamSemanticCase.from_json(line) }}
  end

  def evaluate(fixture : UpstreamSemanticCase) : UpstreamSemanticResult
    manager = Facet::Compiler::SourceManager.new
    file_id = manager.add(fixture.source, "semantic-case.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(
      queries,
      Facet::Compiler::RegisteredSourceResolver.new([] of String, nil),
      semantic_options: Facet::Compiler::SemanticOptions.new(fixture.flags.try { |flags| [flags] } || [] of String)
    )
    snapshot = semantic.analyze([file_id])
    tree = queries.syntax(file_id)
    diagnostics = snapshot.diagnostics_for(file_id)
    diagnostic = fixture.kind == "type" ? diagnostics.find(&.confidence.conclusive?) : diagnostics.first?
    actual_type = final_expression(tree.root).try do |node|
      ref = Facet::Compiler::NodeRef.new(file_id, node.id, manager.revision(file_id))
      snapshot.type_of(ref).try { |type_id| semantic.types.display(type_id) }
    end
    position = diagnostic.try { |entry| tree.position_at(entry.span.start) }
    UpstreamSemanticResult.new(
      actual_type,
      diagnostic.try(&.code),
      position.try { |value| value.line + 1 },
      position.try { |value| value.character + 1 },
      snapshot.complete?,
      tree.ast.diagnostics.empty?
    )
  rescue
    UpstreamSemanticResult.new(nil, nil, nil, nil, false, false)
  end

  private def final_expression(node : Facet::Compiler::SyntaxNode) : Facet::Compiler::SyntaxNode?
    if {Facet::Compiler::NodeKind::File, Facet::Compiler::NodeKind::Expressions}.includes?(node.kind)
      child = node.children.reject { |entry| entry.kind == Facet::Compiler::NodeKind::Nop }.last?
      return child.try { |entry| final_expression(entry) }
    end
    node
  end
end
