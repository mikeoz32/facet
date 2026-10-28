require "./spec_helper"

private def method_contract_fixture(source : String)
  manager = Facet::Compiler::SourceManager.new
  entry = manager.add(source, "/workspace/main.cr")
  queries = Facet::Compiler::QueryDb.new(manager)
  semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
  {semantic, semantic.analyze([entry]), entry, queries}
end

describe "Semantic method contracts" do
  {
    "top-level return"              => {"def target : Int32; 'x'; end; target", "facet.return_type_mismatch"},
    "instance return"               => {"class Item; def target : Int32; 'x'; end; end; Item.new.target", "facet.return_type_mismatch"},
    "class return"                  => {"class Item; def self.target : Int32; 'x'; end; end; Item.target", "facet.return_type_mismatch"},
    "explicit return"               => {"def target : Int32; return 'x'; end; target", "facet.return_type_mismatch"},
    "branch return"                 => {"def target(flag) : Int32; if flag; 1; else; 'x'; end; end; target(true)", "facet.return_type_mismatch"},
    "early return"                  => {"def target(flag) : Int32; return 'x' if flag; 1; end; target(true)", "facet.return_type_mismatch"},
    "nil fallthrough"               => {"def target(flag) : Int32; return 1 if flag; end; target(true)", "facet.return_type_mismatch"},
    "empty body"                    => {"def target : Int32; end; target", "facet.return_type_mismatch"},
    "nonreturning contract"         => {"def target : NoReturn; 1; end; target", "facet.return_type_mismatch"},
    "generic return"                => {"class Item(T); def target : T; 'x'; end; end; Item(Int32).new.target", "facet.return_type_mismatch"},
    "free-variable return"          => {"def target(x : T) : T forall T; 'x'; end; target(1)", "facet.return_type_mismatch"},
    "alias return"                  => {"alias Result = Int32; def target : Result; 'x'; end; target", "facet.return_type_mismatch"},
    "numeric return is not coerced" => {"def target : Int64; 1; end; target", "facet.return_type_mismatch"},
    "default restriction"           => {"def target(x : Int64 = 'x'); x; end; target", "facet.default_type_mismatch"},
    "tuple default restriction"     => {"def target(x : Tuple(_) = 'x'); x; end; target", "facet.default_type_mismatch"},
    "named omission"                => {"def target(x : Int32 = 'x', y = 1); x; end; target(y: 2)", "facet.default_type_mismatch"},
    "prior parameter default"       => {"def target(x, y : Int32 = x); y; end; target('x')", "facet.default_type_mismatch"},
    "constructor default"           => {"class Item; def initialize(x : Int32 = 'x'); end; end; Item.new", "facet.default_type_mismatch"},
    "overflowing numeric default"   => {"def target(x : UInt8 = 300); x; end; target", "facet.default_type_mismatch"},
    "inherited generic return"      => {"class Parent(T); def target : T; 'x'; end; end; class Child(U) < Parent(Int32); end; Child(Char).new.target", "facet.return_type_mismatch"},
  }.each do |description, contract|
    it "checks #{description}" do
      _, snapshot, entry, queries = method_contract_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.map(&.code).should eq([contract[1]])
    end
  end

  {
    "matching return"                   => {"def target : Int32; 1; end; target", "Int32"},
    "narrow union return"               => {"def target : Int32 | Char; 1; end; target", "Int32"},
    "narrow nilable return"             => {"def target : Int32?; 1; end; target", "Int32"},
    "subclass return"                   => {"class Parent; end; class Child < Parent; end; def target : Parent; Child.new; end; target", "Child"},
    "void discards result"              => {"def target : Void; 1; end; target", "Nil"},
    "nil discards result"               => {"def target : Nil; 1; end; target", "Nil"},
    "nonreturning void body"            => {"def target : Void; while true; end; end; target", "NoReturn"},
    "nonreturning nil body"             => {"def target : Nil; while true; end; end; target", "NoReturn"},
    "annotated recursion"               => {"def target : Int32; target; end; target", "Int32"},
    "nonreturning body"                 => {"def target : Int32; while true; end; end; target", "NoReturn"},
    "nonreturning annotation"           => {"def target : NoReturn; while true; end; end; target", "NoReturn"},
    "matching generic return"           => {"class Item(T); def target(x : T) : T; x; end; end; Item(Int32).new.target(1)", "Int32"},
    "matching free-variable return"     => {"def target(x : T) : T forall T; x; end; target('x')", "Char"},
    "unused bad default"                => {"def target(x : Int32 = 'x'); x; end; target(1)", "Int32"},
    "default union stays narrow"        => {"def target(x : Int32 | Char = 1); x; end; target", "Int32"},
    "numeric default restriction"       => {"def target(x : Int64 = 1); x; end; target", "Int64"},
    "negative numeric default"          => {"def target(x : Int64 = -1); x; end; target", "Int64"},
    "hexadecimal numeric default"       => {"def target(x : UInt8 = 0xff); x; end; target", "UInt8"},
    "underscored numeric default"       => {"def target(x : Int64 = 1_000); x; end; target", "Int64"},
    "float numeric default"             => {"def target(x : Float32 = 1.5); x; end; target", "Float32"},
    "suffixed integer default"          => {"def target(x : Float64 = 1_i64); x; end; target", "Float64"},
    "suffixed float default"            => {"def target(x : Float32 = 1.0_f64); x; end; target", "Float32"},
    "numeric union default"             => {"def target(x : Int64 | Char = 1); x; end; target", "Int64"},
    "inherited generic result"          => {"class Parent(T); def target : T; 1; end; end; class Child(U) < Parent(Int32); end; Child(Char).new.target", "Int32"},
    "inherited generic default"         => {"class Parent(T); def target(x : T = 1); x; end; end; class Child(U) < Parent(Int64); end; Child(Char).new.target", "Int64"},
    "inherited generic type expression" => {"class Parent(T); def target; T; end; end; class Child(U) < Parent(Int32); end; Child(Char).new.target", "Int32.class"},
    "uninstantiated bad method"         => {"def target : Int32; 'x'; end; 1", "Int32"},
    "unknown body remains conservative" => {"def target : Int32; unknown_value; end; target", "Int32"},
  }.each do |description, contract|
    it "preserves #{description}" do
      semantic, snapshot, entry, queries = method_contract_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
    end
  end

  it "does not cache away failures at repeated call sites" do
    _, snapshot, _, _ = method_contract_fixture("def target : Int32; 'x'; end\ntarget\ntarget")
    snapshot.diagnostics.map(&.code).should eq(["facet.return_type_mismatch", "facet.return_type_mismatch"])
    snapshot.diagnostics.map(&.span.start).uniq.size.should eq(2)
  end

  it "keeps argument specializations independent for annotated methods" do
    _, snapshot, _, _ = method_contract_fixture("def target(x) : Int32; x; end\ntarget(1)\ntarget('x')\ntarget(2)")
    snapshot.diagnostics.map(&.code).should eq(["facet.return_type_mismatch"])
  end

  it "bounds polymorphic recursion behind return annotations" do
    _, snapshot, _, _ = method_contract_fixture("def target(x) : Int32; target({x}); end; target(1)")
    snapshot.complete?.should be_false
    snapshot.diagnostics.should be_empty
  end

  {"0xff" => "Float64", "0b101" => "Float32"}.each do |literal, expected|
    it "restricts #{literal} to a floating-point default" do
      semantic, snapshot, entry, queries = method_contract_fixture("def target(x : #{expected} = #{literal}); x; end; target")
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(expected)
    end
  end

  it "refreshes method-contract diagnostics after a required body changes" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./provider\"\ntarget", "/workspace/main.cr")
    provider = manager.add("def target : Int32; 1; end", "/workspace/provider.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    semantic.analyze([entry]).diagnostics.should be_empty
    queries.update(provider, "def target : Int32; 'x'; end")
    semantic.analyze([entry]).diagnostics.map(&.code).should eq(["facet.return_type_mismatch"])
    queries.update(provider, "def target : Char; 'x'; end")
    snapshot = semantic.analyze([entry])
    snapshot.diagnostics.should be_empty
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Char")
  end
end
