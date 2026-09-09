require "./spec_helper"
require "../scripts/support/upstream_semantic_parity"

fixture_path = File.expand_path("fixtures/crystal_1_21_semantic_contracts.jsonl", __DIR__)
baseline_path = File.expand_path("fixtures/crystal_1_21_semantic_supported.txt", __DIR__)
deferred_path = File.expand_path("fixtures/crystal_1_21_semantic_deferred.tsv", __DIR__)
semantic_header, semantic_cases = UpstreamSemanticParity.load(fixture_path)
supported_semantic_indices = File.read_lines(baseline_path).reject(&.blank?).map(&.to_i)
deferred_semantic_rows = File.read_lines(deferred_path).reject(&.blank?).map(&.split('\t'))
deferred_semantic_indices = deferred_semantic_rows.map { |row| row[0].to_i }

describe "Crystal 1.21 semantic contract corpus" do
  it "keeps every captured contract classified as supported or deferred" do
    semantic_header.kind.should eq("facet-upstream-semantic-contracts")
    semantic_header.crystal_version.should eq("1.21.0")
    semantic_header.crystal_revision.should eq("57cf7da5094db6c5d3c058c6d054a757b5ced19e")
    semantic_header.example_count.should eq(449)
    semantic_header.pending_count.should eq(2)
    semantic_header.contract_count.should eq(582)
    semantic_cases.size.should eq(semantic_header.contract_count)
    supported_semantic_indices.should eq(supported_semantic_indices.sort.uniq)
    deferred_semantic_indices.should eq(deferred_semantic_indices.sort.uniq)
    supported_semantic_indices.each { |index| index.should be < semantic_cases.size }
    deferred_semantic_rows.each { |row| row.size.should eq(3) }
    (supported_semantic_indices & deferred_semantic_indices).should be_empty
    (supported_semantic_indices + deferred_semantic_indices).sort.should eq((0...semantic_cases.size).to_a)
  end

  supported_semantic_indices.each do |index|
    fixture = semantic_cases[index]
    preview = fixture.source.lines.first?.to_s.strip
    preview = preview[0, Math.min(preview.size, 60)]

    it "matches semantic contract #{index}: #{preview.dump}" do
      result = UpstreamSemanticParity.evaluate(fixture)
      result.matches?(fixture).should be_true
    end
  end

  it "does not regress the supported semantic denominator" do
    matching = semantic_cases.count { |fixture| UpstreamSemanticParity.evaluate(fixture).matches?(fixture) }
    matching.should be >= supported_semantic_indices.size
  end
end
