require "./spec_helper"
require "../scripts/support/upstream_macro_parity"

fixture_path = File.expand_path("fixtures/crystal_1_21_macro.jsonl", __DIR__)
baseline_path = File.expand_path("fixtures/crystal_1_21_macro_supported.txt", __DIR__)
macro_fixture_header, macro_fixture_cases = UpstreamMacroParity.load(fixture_path)
supported_macro_indices = File.read_lines(baseline_path).map(&.to_i)

describe "Crystal 1.21 macro corpus" do
  it "loads the statically extractable official macro contracts" do
    macro_fixture_header.kind.should eq("facet-upstream-macro-fixture")
    macro_fixture_header.crystal_version.should eq("1.21.0")
    macro_fixture_header.crystal_revision.should eq("57cf7da5094db6c5d3c058c6d054a757b5ced19e")
    macro_fixture_header.example_count.should eq(731)
    macro_fixture_header.semantic_example_count.should eq(133)
    macro_fixture_header.assertion_count.should eq(973)
    macro_fixture_header.case_count.should eq(371)
    macro_fixture_header.excluded_count.should eq(602)
    macro_fixture_header.exclusion_counts.values.sum.should eq(macro_fixture_header.excluded_count)
    macro_fixture_header.exclusion_counts.should eq({
      "requires_context"    => 593,
      "dynamic_expression"  => 3,
      "ambient_environment" => 2,
      "expected_exception"  => 4,
    })
    macro_fixture_header.assertion_counts["assert_macro"].should eq(macro_fixture_header.assertion_count)
    macro_fixture_cases.size.should eq(macro_fixture_header.case_count)
    supported_macro_indices.should eq(supported_macro_indices.sort.uniq)
    supported_macro_indices.size.should eq(371)
  end

  supported_macro_indices.each do |index|
    fixture_case = macro_fixture_cases[index]
    preview = fixture_case.body.lines.first?.to_s.strip
    preview = preview[0, Math.min(preview.size, 60)]

    it "matches upstream macro contract #{index}: #{preview.dump}" do
      result = UpstreamMacroParity.expand(fixture_case, index)
      result.diagnostics.should be_empty
      result.actual.should eq(fixture_case.expected)
    end
  end

  it "does not regress aggregate exact parity" do
    exact = macro_fixture_cases.each_with_index.count do |fixture_case, index|
      UpstreamMacroParity.expand(fixture_case, index).matches?(fixture_case)
    end
    exact.should be >= supported_macro_indices.size
  end
end
