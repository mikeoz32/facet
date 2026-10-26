require "./spec_helper"

private def declaration_fixture(source : String)
  manager = Facet::Compiler::SourceManager.new
  entry = manager.add(source, "/workspace/main.cr")
  queries = Facet::Compiler::QueryDb.new(manager)
  semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
  {semantic, semantic.analyze([entry]), entry, queries}
end

describe "Semantic type declaration validation" do
  it "rejects instantiation of builtin abstract Object" do
    _, snapshot, _, _ = declaration_fixture("Object.new")
    snapshot.diagnostics.map(&.code).should eq(["facet.abstract_instantiation"])
  end

  it "rejects primitive struct superclasses of classes" do
    _, snapshot, _, _ = declaration_fixture("class Item < Int32; end")
    snapshot.diagnostics.map(&.code).should eq(["facet.invalid_superclass"])
  end

  it "rejects class superclasses of structs" do
    _, snapshot, _, _ = declaration_fixture("struct Item < Reference; end")
    snapshot.diagnostics.map(&.code).should eq(["facet.invalid_superclass"])
  end

  it "rejects inheritance from concrete structs" do
    _, snapshot, _, _ = declaration_fixture("struct Base; end\nstruct Item < Base; end")
    snapshot.diagnostics.map(&.code).should eq(["facet.invalid_superclass"])
  end

  it "allows concrete structs to extend abstract structs" do
    _, snapshot, _, _ = declaration_fixture("abstract struct Base; end\nstruct Item < Base; end\nItem.new")
    snapshot.diagnostics.should be_empty
  end

  {
    "class reopened as struct"       => {"class Item; end\nstruct Item; end", "facet.type_kind_mismatch"},
    "class reopened as module"       => {"class Item; end\nmodule Item; end", "facet.type_kind_mismatch"},
    "module reopened as class"       => {"module Item; end\nclass Item; end", "facet.type_kind_mismatch"},
    "struct reopened as class"       => {"struct Item; end\nclass Item; end", "facet.type_kind_mismatch"},
    "nongeneric reopened as generic" => {"class Item; end\nclass Item(T); end", "facet.generic_parameter_mismatch"},
    "renamed generic parameter"      => {"class Item(T); end\nclass Item(U); end", "facet.generic_parameter_mismatch"},
    "different generic arity"        => {"class Item(A, B); end\nclass Item(A); end", "facet.generic_parameter_mismatch"},
    "added generic splat"            => {"class Item(T); end\nclass Item(*T); end", "facet.generic_parameter_mismatch"},
    "removed generic splat"          => {"class Item(*T); end\nclass Item(T); end", "facet.generic_parameter_mismatch"},
    "moved generic splat"            => {"class Item(*A, B); end\nclass Item(A, *B); end", "facet.generic_parameter_mismatch"},
    "default superclass changed"     => {"class Base; end\nclass Item; end\nclass Item < Base; end", "facet.superclass_mismatch"},
    "explicit superclass changed"    => {"class First; end\nclass Second; end\nclass Item < First; end\nclass Item < Second; end", "facet.superclass_mismatch"},
    "undefined superclass"           => {"class Item < Missing; end", "facet.undefined_constant"},
    "module superclass"              => {"module Feature; end\nclass Item < Feature; end", "facet.invalid_superclass"},
    "struct superclass of class"     => {"struct ValueType; end\nclass Item < ValueType; end", "facet.invalid_superclass"},
    "metaclass superclass alias"     => {"class Base; end\nalias Meta = Base.class\nclass Item < Meta; end", "facet.invalid_superclass"},
    "self inheritance"               => {"class Item < Item; end", "facet.inheritance_cycle"},
    "abstract new"                   => {"abstract class Item; end\nItem.new", "facet.abstract_instantiation"},
    "abstract allocate"              => {"abstract class Item; end\nItem.allocate", "facet.abstract_instantiation"},
    "abstract generic new"           => {"abstract class Item(T); end\nItem(Int32).new", "facet.abstract_instantiation"},
    "abstract alias new"             => {"abstract class Item; end\nalias Selected = Item\nSelected.new", "facet.abstract_instantiation"},
    "abstract Value allocate"        => {"Value.allocate", "facet.abstract_instantiation"},
    "abstract Number allocate"       => {"Number.allocate", "facet.abstract_instantiation"},
    "conditional class"              => {"if true; class Item; end; end", "facet.dynamic_type_declaration"},
    "conditional module"             => {"unless false; module Item; end; end", "facet.dynamic_type_declaration"},
    "loop class"                     => {"while false; class Item; end; end", "facet.dynamic_type_declaration"},
  }.each do |description, contract|
    it "diagnoses #{description}" do
      semantic, _, entry, queries = declaration_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot = semantic.analyze([entry], Facet::Compiler::SemanticMode::Strict)
      snapshot.success?.should be_false
      snapshot.diagnostics.map(&.code).should eq([contract[1]])
    end
  end

  {
    "class reopen"                  => "class Item; end\nclass Item; def value; 1; end; end\nItem.new.value",
    "generic reopen"                => "class Item(T); end\nclass Item(T); end\nItem(Int32).new",
    "generic omission on reopen"    => "class Item(T); end\nclass Item; end\nItem(Int32).new",
    "same superclass"               => "class Base; end\nclass Item < Base; end\nclass Item < Base; end\nItem.new",
    "omitted superclass on reopen"  => "class Base; end\nclass Item < Base; end\nclass Item; end\nItem.new",
    "aliased superclass"            => "class Base; end\nalias Parent = Base\nclass Item < Base; end\nclass Item < Parent; end\nItem.new",
    "implicit namespace reopened"   => "class Space::Item; end\nmodule Space; end\nSpace::Item.new",
    "concrete abstract subclass"    => "abstract class Base; end\nclass Item < Base; end\nItem.new",
    "abstract class custom factory" => "abstract class Item; def self.new; 1; end; end\nItem.new",
    "static class namespace"        => "class Outer; class Inner; end; end\nOuter::Inner.new",
  }.each do |description, source|
    it "accepts #{description}" do
      _, snapshot, _, _ = declaration_fixture(source)
      snapshot.diagnostics.should be_empty
    end
  end

  it "reports arity before abstract instantiation" do
    _, snapshot, _, _ = declaration_fixture("abstract class Item; def initialize(x); end; end\nItem.new")
    snapshot.diagnostics.map(&.code).should eq(["facet.wrong_arity"])
  end

  it "keeps conflict locations in require execution order" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./base\"\nstruct Item; end", "/workspace/main.cr")
    provider = manager.add("class Item; end", "/workspace/base.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    snapshot = semantic.analyze([entry])
    diagnostic = snapshot.diagnostics.first
    diagnostic.code.should eq("facet.type_kind_mismatch")
    diagnostic.file_id.should eq(entry)
    diagnostic.related.first.file_id.should eq(provider)
    queries.syntax(entry).position_at(diagnostic.span.start).line.should eq(1)
    queries.update(entry, "require \"./base\"\nclass Item; end")
    semantic.analyze([entry]).diagnostics.should be_empty
  end

  it "respects declarations occurring before a require" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("class Item; end\nrequire \"./other\"", "/workspace/main.cr")
    provider = manager.add("struct Item; end", "/workspace/other.cr")
    semantic = Facet::Compiler::SemanticDb.new(Facet::Compiler::QueryDb.new(manager), Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    diagnostic = semantic.analyze([entry]).diagnostics.first
    diagnostic.file_id.should eq(provider)
    diagnostic.related.first.file_id.should eq(entry)
  end

  it "reuses a namespace class declared before a required qualified declaration" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("class Space; end\nrequire \"./item\"\nSpace::Item.new", "/workspace/main.cr")
    manager.add("class Space::Item; end", "/workspace/item.cr")
    semantic = Facet::Compiler::SemanticDb.new(Facet::Compiler::QueryDb.new(manager), Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    semantic.analyze([entry]).diagnostics.should be_empty
  end

  it "detects indirect inheritance cycles without rejecting their dependents as cyclic" do
    _, snapshot, _, _ = declaration_fixture("class First < Second; end\nclass Second < First; end\nclass Third < First; end")
    snapshot.diagnostics.map(&.code).should eq(["facet.inheritance_cycle", "facet.inheritance_cycle"])
  end

  it "keeps missing-provider superclass facts conservative" do
    _, snapshot, _, _ = declaration_fixture("require \"./missing\"\nclass Item < External; end")
    snapshot.diagnostics.map(&.code).should eq(["facet.missing_require"])
  end

  it "does not let a dynamic abstract declaration change a static type" do
    _, snapshot, _, _ = declaration_fixture("class Item; end\nif true; abstract class Item; end; end\nItem.new")
    snapshot.diagnostics.map(&.code).should eq(["facet.dynamic_type_declaration"])
  end

  it "retains abstractness after reopening without the modifier" do
    _, snapshot, _, _ = declaration_fixture("abstract class Item; end\nclass Item; end\nItem.new")
    snapshot.diagnostics.map(&.code).should eq(["facet.abstract_instantiation"])
  end

  it "refreshes abstract metadata after a provider is edited" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./provider\"\nItem.new", "/workspace/main.cr")
    provider = manager.add("abstract class Item; end", "/workspace/provider.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    semantic.analyze([entry]).diagnostics.map(&.code).should eq(["facet.abstract_instantiation"])
    queries.update(provider, "class Item; end")
    semantic.analyze([entry], Facet::Compiler::SemanticMode::Strict).success?.should be_true
  end

  it "does not duplicate declarations through require cycles" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./other\"\nclass First; end", "/workspace/main.cr")
    manager.add("require \"./main\"\nclass Second; end", "/workspace/other.cr")
    semantic = Facet::Compiler::SemanticDb.new(Facet::Compiler::QueryDb.new(manager), Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    semantic.analyze([entry]).diagnostics.should be_empty
  end
end
