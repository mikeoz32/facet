require "compiler/crystal/syntax"
require "./support/facet_ast_normalizer"
require "./support/upstream_ast_normalizer"

alias F = Facet::Compiler

# Cross-product oracle for array, tuple, hash, named-tuple, typed collection,
# and splat spellings in standalone, assignment, call-argument, and nested-array
# contexts. Accepted cases also gate the native and semantic AST contracts.
expressions = [
  "[]", "[ ]", "[1]", "[1,]", "[1, 2]", "[1 2]", "[1,,2]", "[1\n, 2]",
  "[*items]", "[**items]", "[1, *items]", "[1, **items]",
  "[] of Int32", "[1] of Int32", "[1, 2] of Int32", "[] of lowercase", "[] of nil", "[] of",
  "{}", "{ }", "{1}", "{1,}", "{1, 2}", "{1 2}", "{1,,2}",
  "{1 => 2}", "{1=>2}", "{1 =>}", "{=> 2}", "{1 => 2,}", "{1 => 2, 3 => 4}",
  "{foo: 1}", "{foo : 1}", "{foo:1}", "{foo :1}", "{foo: 1, bar: 2}",
  %q({"foo": 1}), %q({"": 1}), %q({"foo" : 1}),
  "{*items}", "{**items}", "{1, *items}", "{foo: 1, **items}", "{1 => 2, **items}",
  "{} of String => Int32", "{1 => 2} of Int32 => Int32", "{} of String", "{} of => Int32",
]

contexts = [
  ->(expression : String) { expression },
  ->(expression : String) { "value = #{expression}" },
  ->(expression : String) { "consume(#{expression})" },
  ->(expression : String) { "[#{expression}]" },
]

sources = expressions.flat_map { |expression| contexts.map(&.call(expression)) }.uniq
acceptance_mismatches = [] of String
shape_mismatches = [] of String
unsupported = [] of String

sources.each do |code|
  upstream_ast = begin
    Crystal::Parser.parse(code)
  rescue Crystal::SyntaxException
    nil
  end

  parser = F::Parser.new(F::Source.new(code))
  facet_ast = parser.parse_file
  upstream_accepted = !upstream_ast.nil?
  facet_accepted = parser.diagnostics.empty?
  if upstream_accepted != facet_accepted
    detail = parser.diagnostics.first?.try(&.message) || "accepted"
    acceptance_mismatches << "#{code.inspect} upstream=#{upstream_accepted} facet=#{facet_accepted} #{detail}"
    next
  end
  next unless upstream_ast && facet_accepted

  violations = F::AstIntegrity.contract_violations(facet_ast)
  unless violations.empty?
    shape_mismatches << "#{code.inspect} contract=#{violations.first}"
    next
  end

  begin
    expected = UpstreamAstNormalizer.normalize(upstream_ast)
    actual = FacetAstNormalizer.normalize(facet_ast)
    unless expected == actual
      shape_mismatches << "#{code.inspect} #{expected.first_difference(actual)}"
    end
  rescue error : UnsupportedSemanticAst
    unsupported << "#{code.inspect} #{error.node_kind}"
  end
end

puts "cases=#{sources.size} acceptance_mismatches=#{acceptance_mismatches.size} " \
     "shape_mismatches=#{shape_mismatches.size} unsupported=#{unsupported.size}"
acceptance_mismatches.first(100).each { |line| puts line }
shape_mismatches.first(100).each { |line| puts line }
unsupported.first(30).each { |line| puts line }
exit 1 unless acceptance_mismatches.empty? && shape_mismatches.empty? && unsupported.empty?
