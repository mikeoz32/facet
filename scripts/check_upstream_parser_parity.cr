require "base64"
require "compiler/crystal/syntax"
require "../src/facet"

# Compares Facet with Crystal::Parser over base64-encoded source inputs captured
# from the upstream parser specs. See upstream_input_trace.patch.

alias F = Facet::Compiler

record Mismatch, index : Int32, expected : String, actual : String, source : String, detail : String

def semantic_leaf?(kind : F::NodeKind) : Bool
  !{
    F::NodeKind::File, F::NodeKind::Expressions,
    F::NodeKind::Def, F::NodeKind::MacroDef, F::NodeKind::Class, F::NodeKind::Module,
    F::NodeKind::Struct, F::NodeKind::Enum, F::NodeKind::Lib, F::NodeKind::Fun,
    F::NodeKind::Call, F::NodeKind::Assign, F::NodeKind::Binary, F::NodeKind::Block,
    F::NodeKind::If, F::NodeKind::Unless, F::NodeKind::While, F::NodeKind::Until,
    F::NodeKind::Case, F::NodeKind::When, F::NodeKind::For, F::NodeKind::Begin,
    F::NodeKind::Rescue, F::NodeKind::Ensure, F::NodeKind::CallWithBlock,
  }.includes?(kind)
end

trace_path = ARGV[0]? || abort "usage: crystal run scripts/check_upstream_parser_parity.cr -- PARSER_TRACE.b64"
inputs = File.read_lines(trace_path).map { |line| Base64.decode_string(line) }.uniq
mismatches = [] of Mismatch
accepted = 0
rejected = 0
facet_clean = 0
invariant_failures = 0
uncovered_tokens = 0

inputs.each_with_index do |text, index|
  crystal_ok = begin
    Crystal::Parser.new(text).parse
    true
  rescue Crystal::SyntaxException
    false
  end

  if crystal_ok
    accepted += 1
  else
    rejected += 1
  end

  source = F::Source.new(text, "upstream-parser-spec-#{index}")
  parser = F::Parser.new(source)
  ast = parser.parse_file
  facet_ok = parser.diagnostics.empty?
  facet_clean += 1 if facet_ok

  unless crystal_ok == facet_ok
    detail = parser.diagnostics.first?.try { |d| "#{d.message} @ #{d.span.start}" } || "no diagnostic"
    mismatches << Mismatch.new(index, crystal_ok ? "accept" : "reject", facet_ok ? "accept" : "reject", text, detail)
    next
  end

  next unless facet_ok
  bad_span = ast.arena.nodes.find do |node|
    node.span.start < 0 || node.span.finish < node.span.start || node.span.finish > source.size
  end
  error_node = ast.arena.nodes.find { |node| node.kind == F::NodeKind::Error }
  if bad_span || error_node
    invariant_failures += 1
    detail = bad_span ? "invalid span #{bad_span.not_nil!.span}" : "arena contains Error node"
    mismatches << Mismatch.new(index, "valid AST", "invalid AST", text, detail)
    next
  end

  significant = [
    F::TokenKind::Identifier,
    F::TokenKind::InstanceVar,
    F::TokenKind::ClassVar,
    F::TokenKind::GlobalVar,
    F::TokenKind::Annotation,
    F::TokenKind::Symbol,
    F::TokenKind::Number,
    F::TokenKind::String,
    F::TokenKind::Regex,
    F::TokenKind::Char,
    F::TokenKind::KeywordNil,
    F::TokenKind::KeywordTrue,
    F::TokenKind::KeywordFalse,
    F::TokenKind::KeywordSelf,
    F::TokenKind::KeywordSuper,
  ]
  tokens = F::Lexer.new(source).tokenize_all
  missing = tokens.reject(&.eof?).select do |token|
    significant.includes?(token.kind) && ast.arena.nodes.none? do |node|
      semantic_leaf?(node.kind) && node.span.start <= token.span.start && node.span.finish >= token.span.finish
    end
  end
  unless missing.empty?
    uncovered_tokens += missing.size
    detail = "semantic tokens without exact AST span: #{missing.first(8).map { |token| "#{token.kind}@#{token.span.start}" }.join(", ")}"
    mismatches << Mismatch.new(index, "covered AST", "uncovered token", text, detail)
  end
end

puts "crystal_version=#{Crystal::VERSION} inputs=#{inputs.size} crystal_accept=#{accepted} crystal_reject=#{rejected} facet_clean=#{facet_clean} mismatches=#{mismatches.size} invariant_failures=#{invariant_failures} uncovered_tokens=#{uncovered_tokens}"
mismatches.first(100).each do |m|
  puts "\n##{m.index} expected=#{m.expected} actual=#{m.actual}: #{m.detail}"
  puts m.source.dump
end
exit(mismatches.empty? ? 0 : 1)
