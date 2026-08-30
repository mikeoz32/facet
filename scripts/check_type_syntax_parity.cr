require "compiler/crystal/syntax"
require "./support/facet_ast_normalizer"
require "./support/upstream_ast_normalizer"

alias F = Facet::Compiler

# Cross-product oracle for type-like spellings in six parser contexts. Accepted
# inputs must match Facet's AST contract and the common Crystal/Facet semantic
# projection in addition to matching the upstream acceptance decision.
types = [
  "Int32", "Foo", "Foo::Bar", "::Foo", "::Foo::Bar",
  "foo", "type", "Foo::bar", "::foo", "1", "",
  "_", "self", "self?", "self.class", "self*", "self**",
  "Nil", "nil", "Void", "NoReturn",
  "Foo?", "Foo??", "Foo*", "Foo**", "Foo[3]", "Foo[1 + 2]", "Foo[]", "Foo[ ]",
  "Foo.class", "(Foo).class",
  "Foo(Int32)", "Foo()", "Foo(Int32,)", "Foo(Int32, String)", "Foo(foo)", "foo(Int32)", "Foo(Int32",
  "Int32 | String", "Int32 |", "| Int32", "(Int32 | String)", "Int32 | String | Nil",
  "Int32 -> String", "(Int32, String) -> Bool", "-> Int32", "Int32, String -> Bool",
  "(-> Int32)", "(Int32 ->)", "(Int32 -> String).class",
  "{Int32, String}", "{name: Int32}", "{name : Int32}", "{}", "{Int32,}",
  "typeof(1)", "typeof(foo)", "sizeof(Int32)", "Pointer(Int32)",
]

contexts = [
  ->(type : String) { "value : #{type}" },
  ->(type : String) { "def consume(value : #{type}); end" },
  ->(type : String) { "def produce : #{type}; end" },
  ->(type : String) { "alias Generated = #{type}" },
  ->(type : String) { "uninitialized #{type}" },
  ->(type : String) { "[] of #{type}" },
]

sources = types.flat_map { |type| contexts.map(&.call(type)) }.uniq
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
