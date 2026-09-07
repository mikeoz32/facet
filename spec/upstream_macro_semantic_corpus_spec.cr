require "./spec_helper"
require "../scripts/support/upstream_macro_semantic_parity"

fixture_path = File.expand_path("fixtures/crystal_1_21_macro_semantic_events.jsonl", __DIR__)
baseline_path = File.expand_path("fixtures/crystal_1_21_macro_semantic_events_supported.txt", __DIR__)
semantic_macro_header, semantic_macro_cases = UpstreamSemanticMacroParity.load(fixture_path)
supported_semantic_macro_indices = File.read_lines(baseline_path).map(&.to_i)

describe "Crystal 1.21 semantic macro event corpus" do
  it "loads every expansion event from the official semantic macro specs" do
    semantic_macro_header.kind.should eq("facet-upstream-macro-semantic-events")
    semantic_macro_header.crystal_version.should eq("1.21.0")
    semantic_macro_header.crystal_revision.should eq("57cf7da5094db6c5d3c058c6d054a757b5ced19e")
    semantic_macro_header.suites.should eq([
      "spec/compiler/semantic/macro_spec.cr",
      "spec/compiler/semantic/macro_overload_spec.cr",
    ])
    semantic_macro_header.semantic_example_count.should eq(133)
    semantic_macro_header.event_count.should eq(147)
    semantic_macro_header.call_count.should eq(69)
    semantic_macro_header.inline_count.should eq(78)
    semantic_macro_header.success_count.should eq(131)
    semantic_macro_header.error_count.should eq(16)
    semantic_macro_cases.size.should eq(semantic_macro_header.event_count)
    supported_semantic_macro_indices.should eq(supported_semantic_macro_indices.sort.uniq)
    supported_semantic_macro_indices.size.should eq(127)
  end

  supported_semantic_macro_indices.each do |index|
    fixture_case = semantic_macro_cases[index]
    preview = fixture_case.invocation.lines.first?.to_s.strip
    preview = preview[0, Math.min(preview.size, 60)]

    it "matches upstream semantic macro event #{index}: #{preview.dump}" do
      result = UpstreamSemanticMacroParity.expand(fixture_case, index)
      result.matches?(fixture_case).should be_true
    end
  end

  it "does not regress aggregate semantic event parity" do
    exact = semantic_macro_cases.each_with_index.count do |fixture_case, index|
      UpstreamSemanticMacroParity.expand(fixture_case, index).matches?(fixture_case)
    end
    exact.should be >= supported_semantic_macro_indices.size
  end
end
