require "compiler/crystal/syntax"
require "./support/facet_ast_normalizer"
require "./support/upstream_ast_normalizer"

alias F = Facet::Compiler

# Cross-product oracle for explicit, command, member, global, named, splat,
# double-splat, block-argument, and shorthand-block calls. Accepted cases gate
# native AST integrity and the common semantic projection.
calls = [
  "foo", "foo()", "foo( )", "foo(1)", "foo(1,)", "foo(1, 2)", "foo(, 1)", "foo(1,, 2)", "foo(1 2)",
  "foo(1\n, 2)", "foo(1,\n2)",
  "foo(name: 1)", "foo(name:1)", "foo(name : 1)", "foo(name :1)",
  "foo(1, name: 2)", "foo(name: 1, 2)", "foo(first: 1, second: 2)", "foo(name: 1, name: 2)",
  "foo(*args)", "foo(**args)", "foo(&block)", "foo(*args, **named, &block)",
  "foo(**named, *args)", "foo(&block, 1)", "foo(*args, 1)", "foo(**named, 1)",
  "foo 1", "foo 1, 2", "foo 1,", "foo name: 1", "foo name : 1", "foo *args", "foo **args", "foo &block",
  "value.foo", "value.foo()", "value.foo(1)", "value.foo 1", "value.foo(name: 1)", "value.foo { |x| x }",
  "value.foo do |x| x end", "value.try &.foo", "value.try &.foo(1)",
  "::foo", "::foo()", "::foo(1)", "foo ::Bar", "value.is_a? ::Foo::Bar",
  "outer(foo &block, 1)", "Matches.new(@matches.try &.[](*args), @cover, @owner, @success)",
]

contexts = [
  ->(call : String) { call },
  ->(call : String) { "result = #{call}" },
  ->(call : String) { "consume(#{call})" },
]

sources = calls.flat_map { |call| contexts.map(&.call(call)) }.uniq
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
