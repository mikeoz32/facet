require "./spec_helper"

private def call_resolution_fixture(source : String)
  manager = Facet::Compiler::SourceManager.new
  entry = manager.add(source, "/workspace/main.cr")
  queries = Facet::Compiler::QueryDb.new(manager)
  semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
  {semantic, semantic.analyze([entry]), entry, queries}
end

describe "Semantic call resolution" do
  {
    "qualified class method"                    => {"class Item; end; def Item.target; 1; end; Item.target", "Int32"},
    "qualified builtin method"                  => {"def Int.target; 2.5; end; Int.target", "Float64"},
    "qualified alias method"                    => {"class Item; end; alias Other = Item; def Other.target; 1; end; Item.target", "Int32"},
    "qualified namespaced method"               => {"module Space; class Item; end; end; def Space::Item.target; 1; end; Space::Item.target", "Int32"},
    "relative qualified method"                 => {"module Space; class Item; end; def Item.target; 1; end; end; Space::Item.target", "Int32"},
    "top-level method from class method"        => {"def target; 1; end; class Item; def self.run; target; end; end; Item.run", "Int32"},
    "root calls retain root lookup"             => {"def inner; 1; end; def target; inner; end; class Item; def self.inner; 'x'; end; def self.run; target; end; end; Item.run", "Int32"},
    "absolute method bypasses lexical override" => {"def target; 1; end; class Item; def target; 'x'; end; def run; ::target; end; end; Item.new.run", "Int32"},
    "Class instance fallback"                   => {"class Class; def target; 1; end; end; class Item; end; Item.target", "Int32"},
    "Class class fallback"                      => {"class Class; def self.target; 1; end; end; class Item; end; Item.target", "Int32"},
    "Class instance self"                       => {"class Class; def target; self; end; end; class Item; end; Item.target", "Item.class"},
    "instance self"                             => {"class Item; def target; self; end; end; Item.new.target", "Item"},
    "class self"                                => {"class Item; def self.target; self; end; end; Item.target", "Item.class"},
    "root raise is nonreturning"                => {"def target; raise \"stop\"; end; target", "NoReturn"},
  }.each do |description, contract|
    it "resolves #{description}" do
      semantic, snapshot, entry, queries = call_resolution_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
    end
  end

  {
    "bare undefined name"                  => {"missing", "facet.undefined_local"},
    "undefined name in instantiated body"  => {"def target; missing; end; target", "facet.undefined_local"},
    "undefined name through wrappers"      => {"def first; missing; end; def second; first; end; second", "facet.undefined_local"},
    "undefined primitive method"           => {"1.missing", "facet.undefined_method"},
    "undefined method through body"        => {"class Item; end; def target; Item.new.missing; end; target", "facet.undefined_method"},
    "undefined method on nil result"       => {"def target; end; target.missing", "facet.undefined_method"},
    "undefined abstract class method"      => {"abstract class Item; end; Item.missing", "facet.undefined_method"},
    "partial union method"                 => {"class A; def target; 1; end; end; class B; end; x = A.new || B.new; x.target", "facet.undefined_method"},
    "root arity from class method"         => {"def target; 1; end; class Item; def self.run; target(1); end; end; Item.run", "facet.wrong_arity"},
    "root argument type from class method" => {"def target(x : String); 1; end; class Item; def self.run; target(1); end; end; Item.run", "facet.argument_type_mismatch"},
    "Class fallback arity"                 => {"class Class; def target; 1; end; end; class Item; end; Item.target(1)", "facet.wrong_arity"},
    "top-level self"                       => {"self", "facet.no_self"},
    "self in top-level method"             => {"def target; self; end; target", "facet.no_self"},
    "instance method definition receiver"  => {"class Item; end; item = Item.new; def item.target; end", "facet.invalid_method_receiver"},
    "generic instance definition receiver" => {"class Item(T); end; alias Other = Item(Int32); def Other.target; end", "facet.generic_method_receiver"},
    "undefined method definition receiver" => {"def Missing.target; end", "facet.undefined_constant"},
  }.each do |description, contract|
    it "reports #{description}" do
      _, snapshot, entry, queries = call_resolution_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.map(&.code).should eq([contract[1]])
    end
  end

  it "does not lose missing-body diagnostics on repeated calls" do
    _, snapshot, _, _ = call_resolution_fixture("def target; missing; end\ntarget\ntarget")
    snapshot.diagnostics.map(&.code).should eq(["facet.undefined_local", "facet.undefined_local"])
  end

  it "does not publish errors from uncalled bodies" do
    _, snapshot, _, _ = call_resolution_fixture("def target; missing; end; 1")
    snapshot.diagnostics.should be_empty
  end

  {
    "bare Class fallback"                     => {"class Class; def target; 1; end; end; class Item; def self.run; target; end; end; Item.run", "Int32"},
    "Class raise override"                    => {"class Class; def raise; 1; end; end; class Item; def self.run; raise(); end; end; Item.run", "Int32"},
    "bare Class raise override"               => {"class Class; def raise; 1; end; end; class Item; def self.run; raise; end; end; Item.run", "Int32"},
    "lexical global owner fallback"           => {"class Item; end; module Space; def Item.target; 1; end; end; Item.target", "Int32"},
    "lexical ancestor owner fallback"         => {"module Space; class Item; end; module Inner; def Item.target; 1; end; end; end; Space::Item.target", "Int32"},
    "absolute arguments retain lexical scope" => {"def target(x); x; end; module Space; VALUE = 1; def self.run; ::target(VALUE); end; end; Space.run", "Int32"},
    "absolute raise"                          => {"::raise \"stop\"", "NoReturn"},
  }.each do |description, contract|
    it "resolves #{description}" do
      semantic, snapshot, entry, queries = call_resolution_fixture(contract[0])
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
    end
  end

  it "does not expose Object instance methods to absolute root calls" do
    _, snapshot, _, _ = call_resolution_fixture("class Object; def target; 1; end; end; ::target")
    snapshot.diagnostics.map(&.code).should eq(["facet.undefined_method"])
  end

  it "rejects dynamic method declarations" do
    _, snapshot, _, _ = call_resolution_fixture("if true\n  def target; 1; end\nend")
    snapshot.diagnostics.map(&.code).should eq(["facet.dynamic_type_declaration"])
  end

  it "does not let one union member's method_missing hide another member's error" do
    _, snapshot, _, _ = call_resolution_fixture("class A; def method_missing(call); end; end; class B; end; x = A.new || B.new; x.missing")
    snapshot.diagnostics.map(&.code).should eq(["facet.undefined_method"])
  end

  it "binds qualified calls to the declaring method" do
    _, snapshot, entry, queries = call_resolution_fixture("class Item; end; def Item.target; 1; end; Item.target")
    call = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, call.callee.not_nil!.id, queries.manager.revision(entry))
    definition = snapshot.definition_of(ref).not_nil!
    definition.name.should eq("target")
    definition.owner.should eq("Item")
    definition.class_method.should be_true
  end

  it "invalidates qualified call types after the provider changes" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./provider\"\nItem.target", "/workspace/main.cr")
    provider = manager.add("class Item; end; def Item.target; 1; end", "/workspace/provider.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, manager.revision(entry))
    semantic.types.display(semantic.analyze([entry]).type_of(ref).not_nil!).should eq("Int32")
    queries.update(provider, "class Item; end; def Item.target; 'x'; end")
    snapshot = semantic.analyze([entry])
    snapshot.diagnostics.should be_empty
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Char")
  end
end
