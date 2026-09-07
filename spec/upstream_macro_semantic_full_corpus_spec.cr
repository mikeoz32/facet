require "./spec_helper"
require "../scripts/support/upstream_macro_semantic_parity"

fixture_path = File.expand_path("fixtures/crystal_1_21_macro_semantic_full_events.jsonl", __DIR__)
baseline_path = File.expand_path("fixtures/crystal_1_21_macro_semantic_full_events_supported.txt", __DIR__)
full_semantic_header, full_semantic_cases = UpstreamSemanticMacroParity.load(fixture_path)
supported_full_semantic_indices = File.read_lines(baseline_path).map(&.to_i)

describe "Crystal 1.21 full semantic macro event corpus" do
  it "compares hygienic identifiers by binding identity instead of generated spelling" do
    actual = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new("__value_1 = 1\n__value_1", "actual.cr")
    ).parse_file
    expected = Facet::Compiler::Parser.new(
      Facet::Compiler::Source.new("__temp_32 = 1\n__temp_32", "expected.cr")
    ).parse_file

    FacetAstNormalizer.normalize(actual).should_not eq(FacetAstNormalizer.normalize(expected))
    FacetAstNormalizer.normalize_macro_output(actual).should eq(FacetAstNormalizer.normalize_macro_output(expected))
  end

  it "compares matching caller-local diagnostics only after AST equivalence" do
    expected = "counter += 1\ncounter"
    actual = "counter += 1\n counter"

    UpstreamSemanticMacroParity.equivalent_output?(actual, expected).should be_true
    UpstreamSemanticMacroParity.equivalent_output?("other += 1\nother", expected).should be_false
    UpstreamSemanticMacroParity.equivalent_output?(%(value = "other"), %(value = "expected")).should be_false
    UpstreamSemanticMacroParity.equivalent_output?("value = 2", "value = 1").should be_false
  end

  it "models inline @type as the instance type of a metaclass scope" do
    fixture_case = UpstreamSemanticMacroCase.new(
      kind: "inline",
      invocation: "{{ @type }}|{{ @type.class }}",
      definition: "{{ @type }}|{{ @type.class }}",
      macro_name: nil,
      scope: "Foo+.class",
      flags: [] of String,
      free_vars: {} of String => JSON::Any,
      instance_vars: [] of String,
      resolved_paths: {} of String => JSON::Any,
      scope_class_methods: [] of JSON::Any,
      path_errors: {} of String => String,
      expected: "Foo|Foo.class",
      expected_error_type: nil,
      expected_error_message: nil
    )

    UpstreamSemanticMacroParity.expand(fixture_case, 0).matches?(fixture_case).should be_true
  end

  it "loads every unique expansion context from the official semantic suite" do
    full_semantic_header.kind.should eq("facet-upstream-macro-semantic-full-events")
    full_semantic_header.crystal_version.should eq("1.21.0")
    full_semantic_header.crystal_revision.should eq("57cf7da5094db6c5d3c058c6d054a757b5ced19e")
    full_semantic_header.suites.should eq(["spec/compiler/semantic"])
    full_semantic_header.semantic_example_count.should eq(3288)
    full_semantic_header.pending_count.should eq(9)
    full_semantic_header.raw_event_count.should eq(150_926)
    full_semantic_header.filtered_event_count.should eq(149_094)
    full_semantic_header.duplicate_event_count.should eq(146_358)
    full_semantic_header.event_count.should eq(2736)
    full_semantic_header.call_count.should eq(1077)
    full_semantic_header.inline_count.should eq(1659)
    full_semantic_header.success_count.should eq(2696)
    full_semantic_header.error_count.should eq(40)
    (full_semantic_header.filtered_event_count.not_nil! - full_semantic_header.duplicate_event_count.not_nil!).should eq(
      full_semantic_header.event_count
    )
    full_semantic_cases.size.should eq(full_semantic_header.event_count)
    supported_full_semantic_indices.should eq(supported_full_semantic_indices.sort.uniq)
    supported_full_semantic_indices.size.should eq(2736)
  end

  supported_full_semantic_indices.each do |index|
    fixture_case = full_semantic_cases[index]
    preview = fixture_case.invocation.lines.first?.to_s.strip
    preview = preview[0, Math.min(preview.size, 60)]

    it "matches full semantic macro event #{index}: #{preview.dump}" do
      result = UpstreamSemanticMacroParity.expand(fixture_case, index)
      result.matches?(fixture_case).should be_true
    end
  end

  it "does not regress aggregate full semantic event parity" do
    exact = full_semantic_cases.each_with_index.count do |fixture_case, index|
      UpstreamSemanticMacroParity.expand(fixture_case, index).matches?(fixture_case)
    end
    exact.should be >= supported_full_semantic_indices.size
  end
end
