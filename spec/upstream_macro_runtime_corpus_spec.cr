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
    runtime_macro_header.structured_declaration_argument_count.should eq(54)
    runtime_macro_header.structured_type_declaration_argument_count.should eq(63)
    runtime_macro_header.structured_asm_argument_count.should eq(20)
    runtime_macro_all_cases.size.should eq(runtime_macro_header.case_count)
    runtime_macro_cases.size.should eq(runtime_macro_header.direct_case_count)
    supported_runtime_macro_indices.should eq(supported_runtime_macro_indices.sort.uniq)
    supported_runtime_macro_indices.size.should eq(718)
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

  it "retains authoritative function declaration and argument structure" do
    def_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 2798
    end.not_nil!
    def_structure = def_contract.arguments.first.structure.not_nil!
    def_structure.kind.should eq("Crystal::Def")
    def_structure.fields["splat_index"].source.should eq("1")
    def_structure.collections["args"].map(&.source).should eq(["x", "y"])
    def_structure.booleans["accepts_block?"].should be_false

    free_vars_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 2824
    end.not_nil!
    free_vars_contract.arguments.first.structure.not_nil!.collections["free_vars"].map(&.source).should eq(["T"])

    arg_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3123
    end.not_nil!
    arg_structure = arg_contract.arguments.first.structure.not_nil!
    arg_structure.fields["name"].source.should eq("into")
    arg_structure.fields["internal_name"].source.should eq("array")

    fun_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3756
    end.not_nil!
    fun_structure = fun_contract.arguments.first.structure.not_nil!
    fun_structure.kind.should eq("Crystal::FunDef")
    fun_structure.fields["real_name"].source.should eq(%("y.z"))
    fun_structure.collections["args"].last.source.should eq(" : Char")
    fun_structure.booleans["variadic?"].should be_true
    fun_structure.booleans["has_body?"].should be_true
  end

  it "retains authoritative type declaration structure" do
    class_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3587
    end.not_nil!
    class_structure = class_contract.arguments.first.structure.not_nil!
    class_structure.kind.should eq("Crystal::ClassDef")
    class_structure.fields["kind"].source.should eq("class")
    class_structure.fields["superclass"].source.should eq("Parent(*T)")
    class_structure.collections["type_vars"].should be_empty
    class_structure.nil_fields.should contain("splat_index")
    class_structure.booleans["abstract?"].should be_false
    class_structure.booleans["struct?"].should be_false

    module_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3641
    end.not_nil!
    module_structure = module_contract.arguments.first.structure.not_nil!
    module_structure.kind.should eq("Crystal::ModuleDef")
    module_structure.collections["type_vars"].map(&.source).should eq(["A", "B", "C", "D"])
    module_structure.fields["splat_index"].source.should eq("2")

    enum_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3670
    end.not_nil!
    enum_structure = enum_contract.arguments.first.structure.not_nil!
    enum_structure.kind.should eq("Crystal::EnumDef")
    enum_structure.fields["base_type"].source.should eq("::Int32")
    enum_structure.fields["body"].source.should eq("X")

    lib_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3702
    end.not_nil!
    lib_structure = lib_contract.arguments.first.structure.not_nil!
    lib_structure.kind.should eq("Crystal::LibDef")
    lib_structure.fields["body"].kind.should eq("Crystal::FunDef")

    c_struct_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3739
    end.not_nil!
    c_struct_structure = c_struct_contract.arguments.first.structure.not_nil!
    c_struct_structure.kind.should eq("Crystal::CStructOrUnionDef")
    c_struct_structure.fields["kind"].source.should eq("struct")
    c_struct_structure.fields["body"].source.should eq("x : Int")
    c_struct_structure.booleans["union?"].should be_false
  end

  it "retains authoritative asm and operand structure" do
    empty_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3833
    end.not_nil!
    empty_structure = empty_contract.arguments.first.structure.not_nil!
    empty_structure.kind.should eq("Crystal::Asm")
    empty_structure.fields["text"].source.should eq(%("nop"))
    empty_structure.collections["outputs"].should be_empty
    empty_structure.collections["inputs"].should be_empty
    empty_structure.collections["clobbers"].should be_empty
    empty_structure.booleans["volatile?"].should be_false
    empty_structure.booleans["alignstack?"].should be_false
    empty_structure.booleans["intel?"].should be_false
    empty_structure.booleans["can_throw?"].should be_false

    full_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3839
    end.not_nil!
    full_structure = full_contract.arguments.first.structure.not_nil!
    full_structure.collections["outputs"].size.should eq(2)
    first_output = full_structure.collections["outputs"].first
    first_output.kind.should eq("Crystal::AsmOperand")
    first_output.fields["constraint"].source.should eq(%("=r"))
    first_output.fields["exp"].source.should eq("x")
    full_structure.collections["inputs"].map(&.fields["constraint"].source).should eq([%("i"), %("r")])
    full_structure.collections["clobbers"].map(&.source).should eq([%("rax"), %("memory")])
    full_structure.booleans["volatile?"].should be_true
    full_structure.booleans["alignstack?"].should be_true
    full_structure.booleans["intel?"].should be_true
    full_structure.booleans["can_throw?"].should be_true

    operand_contract = runtime_macro_cases.find do |fixture_case|
      fixture_case.source_file.ends_with?("macro_methods_spec.cr") && fixture_case.line == 3884
    end.not_nil!
    operand_structure = operand_contract.arguments.first.structure.not_nil!
    operand_structure.kind.should eq("Crystal::AsmOperand")
    operand_structure.fields["constraint"].source.should eq(%("i"))
    operand_structure.fields["exp"].source.should eq("1")
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
