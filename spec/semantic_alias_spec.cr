require "./spec_helper"

private def alias_fixture(source : String)
  manager = Facet::Compiler::SourceManager.new
  entry = manager.add(source, "/workspace/main.cr")
  queries = Facet::Compiler::QueryDb.new(manager)
  semantics = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
  {semantics, semantics.analyze([entry]), entry, queries}
end

describe "Semantic type aliases" do
  it "lets method forall parameters shadow unqualified alias names" do
    semantic, snapshot, entry, queries = alias_fixture("alias NumberAlias = Int32\ndef value(x : NumberAlias) forall NumberAlias; x; end\nvalue('x')")
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Char")
    snapshot.diagnostics.should be_empty
  end

  it "retains absolute aliases when a method forall parameter has the same name" do
    _, snapshot, _, _ = alias_fixture("alias NumberAlias = Int32\ndef value(x : ::NumberAlias) forall NumberAlias; x; end\nvalue('x')")
    snapshot.diagnostics.map(&.code).should eq(["facet.argument_type_mismatch"])
  end

  it "resolves alias targets in declaration scope rather than a caller forall scope" do
    _, snapshot, _, _ = alias_fixture("class Element; end\nalias Selected = Element\ndef value(x : Selected) forall Element; x; end\nvalue(1)")
    snapshot.diagnostics.map(&.code).should eq(["facet.argument_type_mismatch"])
  end

  {
    "scalar alias"                => {"alias Count = Int32\nCount", "Int32.class"},
    "forward alias chain"         => {"alias First = Second\nalias Second = Char\nFirst", "Char.class"},
    "union alias"                 => {"alias Maybe = Int32 | Nil\nMaybe", "(Int32 | Nil).class"},
    "nilable alias"               => {"alias Maybe = Int32?\nMaybe", "(Int32 | Nil).class"},
    "tuple alias"                 => {"alias Pair = {Int32, Char}\nPair", "Tuple(Int32, Char).class"},
    "named tuple alias"           => {"alias Entry = {count: Int32, name: String}\nEntry", "NamedTuple(count: Int32, name: String).class"},
    "proc alias"                  => {"alias Callback = Proc(Int32, Char)\nCallback", "Proc(Int32, Char).class"},
    "proc arrow alias"            => {"alias Callback = Int32 -> Char\nCallback", "Proc(Int32, Char).class"},
    "multiple proc arguments"     => {"alias Callback = Int32, Char -> Bool\nCallback", "Proc(Int32, Char, Bool).class"},
    "nilable proc alias"          => {"alias Callback = (Int32 -> Char)?\nCallback", "(Proc(Int32, Char) | Nil).class"},
    "metaclass union alias"       => {"alias Kinds = Int32.class | Char.class\nKinds", "(Char.class | Int32.class).class"},
    "empty tuple alias"           => {"alias Empty = Tuple()\nEmpty", "Tuple().class"},
    "generic instance alias"      => {"class Box(T); end\nalias IntegerBox = Box(Int32)\nIntegerBox.new", "Box(Int32)"},
    "open generic alias"          => {"class Box(T); end\nalias Container = Box\nContainer(Char).new", "Box(Char)"},
    "nested generic alias"        => {"class Box(T); end\nalias Count = Int32\nalias Nested = Box(Box(Count))\nNested.new", "Box(Box(Int32))"},
    "method restriction alias"    => {"alias Count = Int32\ndef value(x : Count); x; end\nvalue(1)", "Int32"},
    "generic restriction alias"   => {"class Box(T); end\nalias Container = Box\ndef value(x : Container(T)) forall T; x; end\nvalue(Box(Char).new)", "Box(Char)"},
    "declared return alias"       => {"alias Count = Int32\ndef value : Count; 1; end\nvalue", "Int32"},
    "local declared type alias"   => {"alias Count = Int32\nx : Count = 1\nx", "Int32"},
    "cast alias"                  => {"alias Count = Int32\n1.as(Count)", "Int32"},
    "nilable cast alias"          => {"alias Count = Int32\n1.as?(Count)", "(Int32 | Nil)"},
    "uninitialized alias"         => {"alias Count = Int32\nuninitialized Count", "Int32"},
    "namespace shadowing"         => {"alias Count = Char\nmodule Local\nalias Count = Int32\ndef self.value(x : Count); x; end\nend\nLocal.value(1)", "Int32"},
    "absolute alias"              => {"alias Count = Char\nmodule Local\nalias Count = Int32\ndef self.value(x : ::Count); x; end\nend\nLocal.value('x')", "Char"},
    "qualified alias declaration" => {"alias Local::Count = Int32\nLocal::Count", "Int32.class"},
    "alias target lexical scope"  => {"class Item; end\nmodule Local\nclass Item; end\nalias Selected = Item\nend\nLocal::Selected.new", "Local::Item"},
    "alias of namespace"          => {"module Local\nclass Item; end\nend\nalias Space = Local\nSpace::Item.new", "Local::Item"},
    "generic argument alias"      => {"class Box(T); end\nalias Count = Int32\nBox(Count).new", "Box(Int32)"},
  }.each do |description, contract|
    it "resolves #{description}" do
      semantics, snapshot, entry, queries = alias_fixture(contract[0])
      result = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, result.id, queries.manager.revision(entry))
      semantics.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
      snapshot.diagnostics.should be_empty
    end
  end

  it "binds an alias use to its declaration while retaining the canonical type" do
    semantics, snapshot, entry, queries = alias_fixture("alias Count = Int32\nCount")
    result = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, result.id, queries.manager.revision(entry))
    definition = snapshot.definition_of(ref).not_nil!
    definition.kind.should eq(Facet::Compiler::SemanticDefinitionKind::Alias)
    definition.qualified_name.should eq("Count")
    semantics.types.display(definition.type_id).should eq("Int32")
    snapshot.node(definition.node.not_nil!).not_nil!.kind.should eq(Facet::Compiler::NodeKind::Alias)
  end

  it "uses canonical aliases when rejecting an incompatible argument" do
    _, snapshot, _, _ = alias_fixture("alias Count = Int32\ndef value(x : Count); x; end\nvalue('x')")
    snapshot.diagnostics.map(&.code).should eq(["facet.argument_type_mismatch"])
  end

  it "updates required aliases and invalidates old navigation handles" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./types\"\ndef value(x : Count); x; end\nCount", "/workspace/main.cr")
    provider = manager.add("alias Count = Int32", "/workspace/types.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    first = semantic.analyze([entry])
    result = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, result.id, manager.revision(entry))
    old = first.definition_of(ref).not_nil!.node.not_nil!
    semantic.types.display(first.type_of(ref).not_nil!).should eq("Int32.class")
    queries.update(provider, "alias Count = Char")
    second = semantic.analyze([entry])
    semantic.types.display(second.type_of(ref).not_nil!).should eq("Char.class")
    second.node(old).should be_nil
    semantic.types.display(second.methods("Object", "value").first.parameter_types.first).should eq("Char")
    queries.update(provider, "alias Count = Missing")
    third = semantic.analyze([entry], Facet::Compiler::SemanticMode::Strict)
    third.success?.should be_false
    third.diagnostics.map(&.code).should eq(["facet.undefined_type"])
    queries.update(provider, "alias Count = String")
    semantic.analyze([entry]).diagnostics.should be_empty
  end

  it "binds alias references in signature and alias target positions" do
    _, snapshot, entry, queries = alias_fixture("alias Count = Int32\nalias Other = Count\ndef value(x : Other) : Count; 1; end\nvalue(1)")
    tree = queries.syntax(entry)
    uses = tree.nodes(Facet::Compiler::NodeKind::Ident).select { |node| {"Count", "Other"}.includes?(node.text) }
    uses.each do |node|
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      snapshot.definition_of(ref).not_nil!.kind.should eq(Facet::Compiler::SemanticDefinitionKind::Alias)
    end
  end

  {
    "direct"        => "alias Count = Missing\nCount",
    "nested"        => "alias Count = Array(Missing)\nCount",
    "qualified"     => "alias Count = Missing::Type\nCount",
    "single-letter" => "alias Count = Q\nCount",
  }.each do |description, source|
    it "diagnoses #{description} undefined alias targets" do
      semantics, _, entry, _ = alias_fixture(source)
      snapshot = semantics.analyze([entry], Facet::Compiler::SemanticMode::Strict)
      snapshot.success?.should be_false
      snapshot.diagnostics.map(&.code).uniq.should eq(["facet.undefined_type"])
    end
  end

  {
    "alias inheritance"              => {"class Base; def value; 1; end; end\nalias Parent = Base\nclass Child < Parent; end\nChild.new.value", "Int32"},
    "aliased parent fields"          => {"class Base; def initialize(@x : Int32); end; end\nalias Parent = Base\nclass Child < Parent; end\nChild.new(1).@x", "Int32"},
    "alias union overload"           => {"alias Count = Int32 | Char\ndef value(x : Count); x; end\nvalue(1 || 'x')", "(Char | Int32)"},
    "metaclass restriction alias"    => {"alias Kind = Int32.class\ndef value(x : Kind); 'x'; end\nvalue(Int32)", "Char"},
    "generic constructor alias"      => {"class Box(T); def initialize(@x : T); end; def value; @x; end; end\nalias Item = Box(Char)\nItem.new('x').value", "Char"},
    "generic nested namespace"       => {"module Local; class Box(T); end; alias Item = Box(Char); end\nLocal::Item.new", "Local::Box(Char)"},
    "lexical qualified target"       => {"module Local; module Nested; class Item; end; end\nalias Selected = Nested::Item\nend\nLocal::Selected.new", "Local::Nested::Item"},
    "module include alias"           => {"module Feature; def value; 1; end; end\nalias Shared = Feature\nclass Item; include Shared; end\nItem.new.value", "Int32"},
    "absolute namespace alias"       => {"module Space; class Item; end; end\nalias Selected = Space\n::Selected::Item.new", "Space::Item"},
    "namespace alias in restriction" => {"module Space; class Item; end; end\nalias Selected = Space\ndef value(x : ::Selected::Item); 1; end\nvalue(Space::Item.new)", "Int32"},
    "generated alias"                => {"macro make_type; alias Count = Int32; end\nmake_type\nCount", "Int32.class"},
    "class-local alias"              => {"class Outer; alias Count = Int32; def self.value; Count; end; end\nOuter.value", "Int32.class"},
  }.each do |description, contract|
    it "propagates #{description}" do
      semantic, snapshot, entry, queries = alias_fixture(contract[0])
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
      snapshot.diagnostics.should be_empty
    end
  end

  it "keeps aliases from unreachable files out of the snapshot" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("Hidden", "/workspace/main.cr")
    manager.add("alias Hidden = Int32", "/workspace/hidden.cr")
    semantic = Facet::Compiler::SemanticDb.new(Facet::Compiler::QueryDb.new(manager), Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    snapshot = semantic.analyze([entry])
    snapshot.diagnostics.map(&.code).should eq(["facet.undefined_constant"])
  end

  it "rejects an incompatible absolute namespace alias restriction" do
    _, snapshot, _, _ = alias_fixture("module Space; class Item; end; end\nalias Selected = Space\ndef value(x : ::Selected::Item); 1; end\nvalue(1)")
    snapshot.diagnostics.map(&.code).should eq(["facet.argument_type_mismatch"])
  end

  it "does not publish undefined alias targets when a provider is missing" do
    _, snapshot, _, _ = alias_fixture("require \"./missing\"\nalias Selected = External\nSelected")
    snapshot.diagnostics.map(&.code).should eq(["facet.missing_require"])
  end

  it "refreshes generated aliases after a macro provider changes" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./provider\"\nmake_type\nSelected", "/workspace/main.cr")
    provider = manager.add("macro make_type; alias Selected = Int32; end", "/workspace/provider.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    first = semantic.analyze([entry])
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, manager.revision(entry))
    semantic.types.display(first.type_of(ref).not_nil!).should eq("Int32.class")
    queries.update(provider, "macro make_type; alias Selected = Char; end")
    second = semantic.analyze([entry])
    semantic.types.display(second.type_of(ref).not_nil!).should eq("Char.class")
    queries.update(provider, "macro make_type; alias Different = Bool; end")
    third = semantic.analyze([entry])
    third.diagnostics.map(&.code).should eq(["facet.undefined_constant"])
  end

  it "exposes inherited method candidates through an aliased parent" do
    semantic, snapshot, _, _ = alias_fixture("class Base; def value; 1; end; end\nalias Parent = Base\nclass Child < Parent; end\nChild.new")
    snapshot.method_candidates(semantic.types.named("Child"), "value").map(&.owner).should eq(["Base"])
  end

  {"direct" => "alias Loop = Loop\nLoop", "indirect" => "alias First = Second\nalias Second = First\nFirst", "nested" => "alias Loop = Array(Loop)\nLoop"}.each do |description, source|
    it "diagnoses #{description} alias cycles without recursion overflow" do
      semantics, _, entry, _ = alias_fixture(source)
      snapshot = semantics.analyze([entry], Facet::Compiler::SemanticMode::Strict)
      snapshot.success?.should be_false
      snapshot.diagnostics.map(&.code).uniq.should eq(["facet.alias_cycle"])
    end
  end
end
