require "./spec_helper"

private def generic_contract_fixture(source : String)
  manager = Facet::Compiler::SourceManager.new
  entry = manager.add(source, "/workspace/main.cr")
  queries = Facet::Compiler::QueryDb.new(manager)
  semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
  {semantic, semantic.analyze([entry]), entry, queries}
end

describe "Semantic generic signature contracts" do
  {
    "repeated free variable"         => {"def target(x : T, y : T) forall T; end; target(1, 'x')", "facet.argument_type_mismatch"},
    "nested repeated free variable"  => {"class Box(T); end; def target(x : T, y : Box(T)) forall T; end; target(1, Box(Char).new)", "facet.argument_type_mismatch"},
    "repeated variable from tuple"   => {"def target(x : Tuple(T), y : T) forall T; end; target({1}, 'x')", "facet.argument_type_mismatch"},
    "absolute free-variable shadow"  => {"class Item; end; def target(x : ::Item) forall Item; end; target(1)", "facet.argument_type_mismatch"},
    "undefined absolute type"        => {"def target(x : ::Item) forall Item; end; target(1)", "facet.undefined_constant"},
    "ambiguous union free variables" => {"def target(x : T | U) forall T, U; end; target(1)", "facet.ambiguous_free_variable"},
    "nested ambiguous union"         => {"class Box(T); end; def target(x : Box(T | U)) forall T, U; end; target(Box(Int32).new)", "facet.ambiguous_free_variable"},
    "wrong generic arity"            => {"class Box(A, B); end; def target(x : Box(Int32)); end; target(Box(Int32, Int32).new)", "facet.generic_arity"},
    "inherited generic argument"     => {"class Parent(T); end; class Child < Parent(Char); end; def target(x : Parent(Int32)); end; target(Child.new)", "facet.argument_type_mismatch"},
  }.each do |description, contract|
    it "rejects #{description}" do
      _, snapshot, entry, queries = generic_contract_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.map(&.code).should eq([contract[1]])
    end
  end

  {
    "matching repeated free variable"               => {"def target(x : T, y : T) forall T; y; end; target(1, 2)", "Int32"},
    "first union binds all residual members"        => {"def target(x : Nil | T) forall T; T; end; target(1 || 'x' || nil)", "(Char | Int32).class"},
    "union binding accepts compatible second value" => {"def target(x : T, y : T) forall T; y; end; target(1 || 'x', 2)", "Int32"},
    "nil does not capture a free variable"          => {"def target(x : Nil | T, y : T) forall T; T; end; target(nil, 1)", "Int32.class"},
    "generic subtype restriction"                   => {"class Box(T); end; def target(x : Box(Number)); 1; end; target(Box(Int32).new)", "Int32"},
    "generic union restriction"                     => {"class Box(T); end; def target(x : Box(Int32 | Float64)); 1; end; target(Box(Int32).new)", "Int32"},
    "inherited matching restriction"                => {"class Parent(T); end; class Child < Parent(Int32); end; def target(x : Parent(Int32)); 1; end; target(Child.new)", "Int32"},
    "distinct free variables outside a union"       => {"def target(x : T, y : U) forall T, U; {T, U}; end; target(1, 'x')", "Tuple(Int32.class, Char.class)"},
    "absolute real type with shadow name"           => {"class Item; end; def target(x : ::Item) forall Item; 1; end; target(::Item.new)", "Int32"},
    "uninstantiated invalid signature"              => {"def target(x : T | U) forall T, U; end; 1", "Int32"},
  }.each do |description, contract|
    it "accepts #{description}" do
      semantic, snapshot, entry, queries = generic_contract_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
    end
  end

  {
    "plain repeated variable" => {"def target(x : T, y : T) forall T; end\ntarget(1, 'x')", 2, 11},
    "constructor argument"    => {"class Box(T); end\ndef target(x : T, y : Box(T)) forall T; end\ntarget(1, Box(Char).new)", 3, 21},
    "named argument"          => {"def target(a : Int32); end\na = 1 || nil\ntarget(a: a)", 3, 8},
    "splat signature"         => {"def target(x : Int32, *rest); end\ntarget('x')", 2, 1},
  }.each do |description, contract|
    it "anchors #{description} errors at the compiler argument site" do
      _, snapshot, entry, queries = generic_contract_fixture(contract[0])
      snapshot.diagnostics.should_not be_empty
      diagnostic = snapshot.diagnostics.first
      diagnostic.code.should eq("facet.argument_type_mismatch")
      position = queries.syntax(entry).position_at(diagnostic.span.start)
      {position.line + 1, position.character + 1}.should eq({contract[1], contract[2]})
    end
  end

  it "does not let an untyped fallback hide a selected ambiguous union" do
    _, snapshot, _, _ = generic_contract_fixture("def target(x : T | U) forall T, U; end; def target(x); 1; end; target(1)")
    snapshot.diagnostics.map(&.code).should eq(["facet.ambiguous_free_variable"])
  end

  it "does not let an untyped fallback hide an unresolved type restriction" do
    _, snapshot, _, _ = generic_contract_fixture("def target(x : ::MissingType); end; def target(x); 1; end; target(1)")
    snapshot.diagnostics.map(&.code).should eq(["facet.undefined_constant"])
  end

  it "accepts any matching instantiation of an included generic module" do
    _, snapshot, _, _ = generic_contract_fixture("module Included(T); end; class Thing; include Included(Int32); include Included(Char); end; def target(x : Included(Char)); 1; end; target(Thing.new)")
    snapshot.diagnostics.should be_empty
  end

  {
    "undefined unused signature"                      => "def target(x : ::MissingType); end; def target(x : Int32); 1; end; target(1)",
    "ambiguous unused signature"                      => "def target(x : T | U) forall T, U; end; def target(x : Int32); 1; end; target(1)",
    "inconsistent free-variable overload"             => "def target(x : T, y : T) forall T; 'x'; end; def target(x, y); 1; end; target(1, 'x')",
    "unmatched generic head with unresolved argument" => "def target(x : Array(::MissingType)); end; def target(x); 1; end; target(1)",
  }.each do |description, source|
    it "ignores #{description}" do
      _, snapshot, _, _ = generic_contract_fixture(source)
      snapshot.diagnostics.should be_empty
    end
  end

  it "refreshes instantiated signature errors after a provider changes" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./provider\"\ntarget(1, 'x')", "/workspace/main.cr")
    provider = manager.add("def target(x : T, y : T) forall T; 1; end", "/workspace/provider.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    semantic.analyze([entry]).diagnostics.map(&.code).should eq(["facet.argument_type_mismatch"])
    queries.update(provider, "def target(x : T, y : U) forall T, U; 1; end")
    semantic.analyze([entry]).diagnostics.should be_empty
  end
end
