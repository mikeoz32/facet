require "compiler/crystal/syntax"
require "json"

# Extracts self-contained `assert_macro(body, expected)` contracts from the
# official Crystal compiler specs. Cases that require an injected AST value,
# program mutation, flags, or an expected error are counted but intentionally
# excluded: those need a richer fixture schema instead of being silently
# treated as passing expansion cases.

record MacroFixtureHeader,
  kind : String,
  crystal_version : String,
  crystal_revision : String,
  suites : Array(String),
  semantic_suites : Array(String),
  example_count : Int32,
  semantic_example_count : Int32,
  assertion_counts : Hash(String, Int32),
  assertion_count : Int32,
  case_count : Int32,
  excluded_count : Int32,
  exclusion_counts : Hash(String, Int32) do
  include JSON::Serializable
end

record MacroFixtureCase,
  suite : String,
  source_file : String,
  line : Int32,
  body : String,
  expected : String do
  include JSON::Serializable
end

class MacroContractCollector < Crystal::Visitor
  getter assertion_count : Int32 = 0
  getter example_count : Int32 = 0
  getter assertion_counts = Hash(String, Int32).new(0)
  getter exclusion_counts = Hash(String, Int32).new(0)
  getter cases = [] of MacroFixtureCase
  @environment_context_depth = 0
  @expected_exception_depth = 0

  def initialize(@suite : String, @source_file : String, @extract_cases : Bool)
  end

  def visit(node : Crystal::ASTNode) : Bool
    true
  end

  def visit(node : Crystal::Call) : Bool
    @example_count += 1 if node.name == "it" && node.obj.nil? && node.block
    @assertion_counts[node.name] += 1 if node.name.starts_with?("assert_")
    if @extract_cases && node.name == "assert_macro"
      @assertion_count += 1

      if @environment_context_depth > 0
        @exclusion_counts["ambient_environment"] += 1
      elsif @expected_exception_depth > 0
        @exclusion_counts["expected_exception"] += 1
      elsif !(node.args.size == 2 && node.named_args.nil? && node.block.nil?)
        @exclusion_counts["requires_context"] += 1
      else
        body = static_string(node.args[0])
        expected = static_string(node.args[1])
        if body && expected
          @cases << MacroFixtureCase.new(
            suite: @suite,
            source_file: @source_file,
            line: node.location.try(&.line_number) || 0,
            body: body,
            expected: expected,
          )
        else
          @exclusion_counts["dynamic_expression"] += 1
        end
      end
    end

    @environment_context_depth += 1 if node.name == "with_env"
    @expected_exception_depth += 1 if node.name == "expect_raises"
    true
  end

  def end_visit(node : Crystal::Call) : Nil
    @environment_context_depth -= 1 if node.name == "with_env"
    @expected_exception_depth -= 1 if node.name == "expect_raises"
  end

  private def static_string(node : Crystal::ASTNode) : String?
    node.as?(Crystal::StringLiteral).try(&.value)
  end
end

checkout = ARGV[0]? || abort "usage: crystal run scripts/generate_upstream_macro_fixture.cr -- CRYSTAL_CHECKOUT OUTPUT.jsonl"
output_path = ARGV[1]? || abort "usage: crystal run scripts/generate_upstream_macro_fixture.cr -- CRYSTAL_CHECKOUT OUTPUT.jsonl"

unless Crystal::VERSION == "1.21.0"
  abort "fixture generation requires Crystal 1.21.0, got #{Crystal::VERSION}"
end

revision = "57cf7da5094db6c5d3c058c6d054a757b5ced19e"
suites = {
  "macro-expander" => "spec/compiler/macro/macro_expander_spec.cr",
  "macro-methods"  => "spec/compiler/macro/macro_methods_spec.cr",
}
semantic_suites = [
  "spec/compiler/semantic/macro_spec.cr",
  "spec/compiler/semantic/macro_overload_spec.cr",
]

cases = [] of MacroFixtureCase
assertion_count = 0
example_count = 0
semantic_example_count = 0
assertion_counts = Hash(String, Int32).new(0)
exclusion_counts = Hash(String, Int32).new(0)
suites.each do |suite, relative_path|
  path = File.join(checkout, relative_path)
  source = File.read(path)
  ast = Crystal::Parser.parse(source)
  collector = MacroContractCollector.new(suite, relative_path, extract_cases: true)
  ast.accept(collector)
  assertion_count += collector.assertion_count
  example_count += collector.example_count
  collector.assertion_counts.each { |name, count| assertion_counts[name] += count }
  collector.exclusion_counts.each { |reason, count| exclusion_counts[reason] += count }
  cases.concat(collector.cases)
end
semantic_suites.each do |relative_path|
  source = File.read(File.join(checkout, relative_path))
  ast = Crystal::Parser.parse(source)
  collector = MacroContractCollector.new("semantic", relative_path, extract_cases: false)
  ast.accept(collector)
  semantic_example_count += collector.example_count
  collector.assertion_counts.each { |name, count| assertion_counts[name] += count }
end

header = MacroFixtureHeader.new(
  kind: "facet-upstream-macro-fixture",
  crystal_version: Crystal::VERSION,
  crystal_revision: revision,
  suites: suites.values,
  semantic_suites: semantic_suites,
  example_count: example_count,
  semantic_example_count: semantic_example_count,
  assertion_counts: assertion_counts,
  assertion_count: assertion_count,
  case_count: cases.size,
  excluded_count: assertion_count - cases.size,
  exclusion_counts: exclusion_counts,
)

File.open(output_path, "w") do |io|
  io.puts header.to_json
  cases.each { |fixture_case| io.puts fixture_case.to_json }
end

puts "crystal_version=#{Crystal::VERSION} examples=#{example_count} semantic_examples=#{semantic_example_count} assertions=#{assertion_count} cases=#{cases.size} excluded=#{assertion_count - cases.size} output=#{output_path}"
