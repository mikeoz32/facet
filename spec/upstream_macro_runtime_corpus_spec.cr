require "./spec_helper"
require "../scripts/support/upstream_macro_parity"

fixture_path = File.expand_path("fixtures/crystal_1_21_macro_runtime.jsonl", __DIR__)
baseline_path = File.expand_path("fixtures/crystal_1_21_macro_runtime_supported.txt", __DIR__)
runtime_macro_header, runtime_macro_all_cases = UpstreamMacroParity.load_runtime(fixture_path)
runtime_macro_cases = runtime_macro_all_cases.reject(&.contextual_program)
supported_runtime_macro_indices = File.read_lines(baseline_path).map(&.to_i)

describe "Crystal 1.21 runtime macro corpus" do
  it "loads every executed official evaluator contract" do
    runtime_macro_header.kind.should eq("facet-upstream-macro-runtime-fixture")
    runtime_macro_header.crystal_version.should eq("1.21.0")
    runtime_macro_header.crystal_revision.should eq("57cf7da5094db6c5d3c058c6d054a757b5ced19e")
    runtime_macro_header.case_count.should eq(1017)
    runtime_macro_header.direct_case_count.should eq(900)
    runtime_macro_header.contextual_case_count.should eq(117)
    runtime_macro_header.argument_case_count.should eq(578)
    runtime_macro_header.metadata_argument_count.should eq(10)
    runtime_macro_header.structured_name_argument_count.should eq(198)
    runtime_macro_header.structured_call_argument_count.should eq(21)
    runtime_macro_header.structured_control_flow_argument_count.should eq(28)
    runtime_macro_all_cases.size.should eq(runtime_macro_header.case_count)
    runtime_macro_cases.size.should eq(runtime_macro_header.direct_case_count)
    supported_runtime_macro_indices.should eq(supported_runtime_macro_indices.sort.uniq)
    supported_runtime_macro_indices.size.should eq(609)
  end

  it "retains authoritative structural names and generic variants" do
    class_case = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3574
    end.not_nil!
    class_name = class_case.arguments.first
    class_name.name_source.should eq("::Foo::Bar(A, B, *C, D)")
    class_name.name_without_generic_args_source.should eq("::Foo::Bar")
    class_name.name_kind.should eq("identifier")

    primitive_case = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 2853
    end.not_nil!
    primitive_case.arguments.first.name_source.should eq(":abc")
    primitive_case.arguments.first.name_kind.should eq("symbol")
  end

  it "retains authoritative nested call structure" do
    named_case = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3105
    end.not_nil!
    structure = named_case.arguments.first.structure.not_nil!
    structure.kind.should eq("Crystal::Call")
    structure.fields["receiver"].source.should eq("1")
    structure.collections["named_args"].size.should eq(2)
    first_named = structure.collections["named_args"].first
    first_named.source.should eq("a: 1")
    first_named.fields["name"].source.should eq("a")
    first_named.fields["value"].source.should eq("1")
    structure.booleans["global?"].should be_false

    global_case = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3114
    end.not_nil!
    global_case.arguments.first.structure.not_nil!.booleans["global?"].should be_true
  end

  it "retains authoritative case, select, and exception-handler structure" do
    case_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3181
    end.not_nil!
    case_structure = case_contract.arguments.first.structure.not_nil!
    case_structure.kind.should eq("Crystal::Case")
    case_structure.fields["cond"].source.should eq("1")
    case_structure.collections["whens"].first.collections["conds"].map(&.source).should eq(["2", "3"])
    case_structure.collections["whens"].first.fields["body"].source.should eq("4")
    case_structure.booleans["exhaustive?"].should be_false

    select_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3220
    end.not_nil!
    select_structure = select_contract.arguments.first.structure.not_nil!
    select_structure.kind.should eq("Crystal::Select")
    select_structure.fields.has_key?("cond").should be_false
    select_structure.collections["whens"].first.fields["body"].source.should eq("1")

    handler_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3316
    end.not_nil!
    handler_structure = handler_contract.arguments.first.structure.not_nil!
    handler_structure.kind.should eq("Crystal::ExceptionHandler")
    handler_structure.collections["rescues"].first.fields["name"].source.should eq("ex")
    handler_structure.collections["rescues"].last.collections["types"].map(&.source).should eq(["Char", "String"])
    handler_structure.fields["ensure"].source.should eq("4")

    rescue_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3323
    end.not_nil!
    rescue_contract.arguments.first.structure.not_nil!.nil_fields.should contain("types")
  end

  it "retains upstream AST location and documentation metadata" do
    location_case = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 52
    end.not_nil!
    location = location_case.arguments.first
    location.filename.should eq("foo.cr")
    location.line_number.should eq(1)
    location.column_number.should eq(2)

    doc_case = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 243
    end.not_nil!
    doc_case.arguments.first.doc.should eq("Some docs")
  end

  supported_runtime_macro_indices.each do |index|
    fixture_case = runtime_macro_cases[index]
    preview = fixture_case.body.lines.first?.to_s.strip
    preview = preview[0, Math.min(preview.size, 60)]

    it "matches executed upstream macro contract #{index}: #{preview.dump}" do
      result = UpstreamMacroParity.expand(fixture_case, index)
      result.diagnostics.should be_empty
      result.actual.should eq(fixture_case.expected)
    end
  end

  it "does not regress aggregate runtime parity" do
    exact = runtime_macro_cases.each_with_index.count do |fixture_case, index|
      UpstreamMacroParity.expand(fixture_case, index).matches?(fixture_case)
    end
    exact.should be >= supported_runtime_macro_indices.size
  end
end
