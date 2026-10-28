require "./spec_helper"

private def local_fixture(source : String)
  manager = Facet::Compiler::SourceManager.new
  entry = manager.add(source, "/workspace/main.cr")
  queries = Facet::Compiler::QueryDb.new(manager)
  semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
  {semantic, semantic.analyze([entry]), entry, queries}
end

describe "Semantic local declarations" do
  {
    "uninitialized declaration"                      => {"a : Int32\na", "facet.read_before_assignment"},
    "nilable declaration still requires assignment"  => {"a : Int32?\na", "facet.read_before_assignment"},
    "invalid assignment"                             => {"a : Int32\na = true\na", "facet.local_type_mismatch"},
    "invalid initializer"                            => {"a : Int32 = true\na", "facet.local_type_mismatch"},
    "invalid branch assignment"                      => {"a : Int32 = 1\nif true\na = 'x'\nend\na", "facet.local_type_mismatch"},
    "partially initialized restriction"              => {"a : Int32\nflag = true\nif flag\na = 1\nend\na", "facet.local_type_mismatch"},
    "duplicate declaration"                          => {"a : Int32\na : Int32", "facet.duplicate_local"},
    "declaration after assignment"                   => {"a = true\na : Int32", "facet.duplicate_local"},
    "uninitialized redeclaration"                    => {"a = 1\na = uninitialized Float64", "facet.duplicate_local"},
    "mutually exclusive declarations"                => {"flag = true\nif flag\na : Int32 = 1\nelse\na : Int32 = 2\nend", "facet.duplicate_local"},
    "underscore read"                                => {"_", "facet.discard_read"},
    "underscore remains unreadable after assignment" => {"_ = 1\n_", "facet.discard_read"},
    "generic non-instantiated declaration"           => {"class Box(T); end\nx : Box", "facet.generic_type_required"},
    "alias non-instantiated declaration"             => {"class Box(T); end\nalias Item = Box\nx : Item", "facet.generic_type_required"},
    "sibling branch read"                            => {"flag = true\nflag ? (a = 1) : a", "facet.read_before_assignment"},
    "method local read"                              => {"def target\na : Int32\na\nend\ntarget", "facet.read_before_assignment"},
    "method local restriction"                       => {"def target\na : Int32\na = true\nend\ntarget", "facet.local_type_mismatch"},
    "loop assignment restriction"                    => {"a : Int32 = 1\nflag = true\nwhile flag\na = 'x'\nend", "facet.local_type_mismatch"},
    "destructuring restriction"                      => {"a : Int32\na, b = {true, 1}\na", "facet.local_type_mismatch"},
    "parameter redeclaration"                        => {"def target(a); a : Int32; end; target(1)", "facet.duplicate_local"},
    "generic local restriction"                      => {"class Box(T); def target; a : T = 'x'; a; end; end; Box(Int32).new.target", "facet.local_type_mismatch"},
    "free variable local restriction"                => {"def target(x : T) forall T; a : T = 'x'; a; end; target(1)", "facet.local_type_mismatch"},
  }.each do |description, contract|
    it "checks #{description}" do
      _, snapshot, entry, queries = local_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.map(&.code).should eq([contract[1]])
    end
  end

  {
    "initialized declaration"                  => {"a : Int32 = 0\na", "Int32"},
    "assigned declaration"                     => {"a : Int32\na = 0\na", "Int32"},
    "nil initializer"                          => {"a : Int32? = nil\na", "Nil"},
    "union initializer stays narrow"           => {"a : Int32 | Char = 1\na", "Int32"},
    "both branches initialize"                 => {"a : Int32\nflag = true\nif flag\na = 1\nelse\na = 2\nend\na", "Int32"},
    "nilable partial initialization"           => {"a : Int32?\nflag = true\nif flag\na = 1\nend\na", "(Int32 | Nil)"},
    "independent methods"                      => {"def first\na : Int32 = 1\na\nend\ndef second\na : Char = 'x'\na\nend\nfirst\nsecond", "Char"},
    "uninitialized value declaration"          => {"a = uninitialized Float64\na", "Float64"},
    "initialized numeric restriction"          => {"a : Int64 = 1\na", "Int64"},
    "assigned numeric restriction"             => {"a : Int64\na = 1\na", "Int64"},
    "generic instance declaration"             => {"class Box(T); end\nx : Box(Int32)\nx = Box(Int32).new\nx", "Box(Int32)"},
    "discard assignment"                       => {"_ = 1\n2", "Int32"},
    "untyped partial assignment stays nilable" => {"flag = true\nif flag\na = 1\nend\na", "(Int32 | Nil)"},
    "unreachable uninitialized read"           => {"def target\na : Int32\nreturn 1\na\nend\ntarget", "Int32"},
    "required loop initialization"             => {"a : Int32\nwhile true\na = 1\nbreak\nend\na", "Int32"},
    "destructuring initializes"                => {"a : Int32\na, b = {1, 'x'}\na", "Int32"},
    "generic local initialization"             => {"class Box(T); def target(x : T); a : T = x; a; end; end; Box(Int32).new.target(1)", "Int32"},
    "free variable local initialization"       => {"def target(x : T) forall T; a : T = x; a; end; target(1)", "Int32"},
  }.each do |description, contract|
    it "infers #{description}" do
      semantic, snapshot, entry, queries = local_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
    end
  end

  it "reports erroneous specialized locals at every call site" do
    _, snapshot, _, _ = local_fixture("def target(x)\na : Int32\na = x\na\nend\ntarget('x')\ntarget('x')")
    snapshot.diagnostics.map(&.code).should eq(["facet.local_type_mismatch", "facet.local_type_mismatch"])
  end

  {
    "first iteration reads before assignment"   => {"a : Int32?; flag = true; while flag; a; a = 1; end", "facet.read_before_assignment"},
    "restriction survives a terminating branch" => {"def target(flag : Bool); if flag; a : Int32 = 1; return 0; end; a = 'x'; a; end; target(false)", "facet.local_type_mismatch"},
    "restriction reaches sibling assignments"   => {"flag = true; if flag; a : Int32 = 1; else; a = 'x'; end", "facet.local_type_mismatch"},
    "shorthand parameter redeclaration"         => {"class Foo; def initialize(@a : Int32); a : Int32 = 2; end; end; Foo.new(1)", "facet.duplicate_local"},
    "splat parameter redeclaration"             => {"def target(*a); a : Int32 = 1; end; target(1)", "facet.duplicate_local"},
  }.each do |description, contract|
    it "checks #{description}" do
      _, snapshot, _, _ = local_fixture(contract[0])
      snapshot.diagnostics.map(&.code).should eq([contract[1]])
    end
  end

  {
    "enclosing assignment is not a prior binding" => "a = (a : Int32 = 1); a",
    "first iteration initializes before reading"  => "a : Int32?; flag = true; while flag; a = 1; a; end; 1",
  }.each do |description, source|
    it "accepts #{description}" do
      _, snapshot, _, _ = local_fixture(source)
      snapshot.diagnostics.should be_empty
    end
  end

  it "does not diagnose declarations in uncalled method bodies" do
    _, snapshot, _, _ = local_fixture("def target\na : Int32\na\nend\n1")
    snapshot.diagnostics.should be_empty
  end

  it "binds the type of a local declaration for navigation" do
    _, snapshot, entry, queries = local_fixture("alias Count = Int32\na : Count = 1\na")
    node = queries.syntax(entry).nodes(Facet::Compiler::NodeKind::VarDecl).first.declared_type.not_nil!
    ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
    snapshot.definition_of(ref).not_nil!.kind.should eq(Facet::Compiler::SemanticDefinitionKind::Alias)
  end

  it "refreshes local restrictions after a required alias changes" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./types\"\na : Count = 1\na", "/workspace/main.cr")
    provider = manager.add("alias Count = Int32", "/workspace/types.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    semantic.analyze([entry]).diagnostics.should be_empty
    queries.update(provider, "alias Count = Char")
    semantic.analyze([entry]).diagnostics.map(&.code).should eq(["facet.local_type_mismatch"])
    queries.update(provider, "alias Count = Int32 | Char")
    semantic.analyze([entry]).diagnostics.should be_empty
  end
end
