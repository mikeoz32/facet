require "./spec_helper"

private def recursion_fixture(source : String)
  manager = Facet::Compiler::SourceManager.new
  entry = manager.add(source, "/workspace/main.cr")
  queries = Facet::Compiler::QueryDb.new(manager)
  semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
  {semantic, semantic.analyze([entry]), entry, queries}
end

describe "Semantic recursive return inference" do
  {
    "direct cycle"                     => {"def recur; recur; end; recur", "NoReturn"},
    "argument cycle"                   => {"def recur(x); recur(x); end; recur(1)", "NoReturn"},
    "mutual cycle"                     => {"def first; second; end; def second; first; end; first", "NoReturn"},
    "three method cycle"               => {"def first; second; end; def second; third; end; def third; first; end; first", "NoReturn"},
    "base branch"                      => {"def recur(x); if x; recur(x); else; 1; end; end; recur(true)", "Int32"},
    "mutual base branch"               => {"def first(x); if x; second(x); else; 1; end; end; def second(x); first(x); end; second(true)", "Int32"},
    "multiple base types"              => {"def first(x); if x; second(x); else; 1; end; end; def second(x); if x; first(x); else; 'x'; end; end; first(true)", "(Char | Int32)"},
    "explicit base return"             => {"def recur(x); return 1 unless x; recur(x); end; recur(true)", "Int32"},
    "nonreturning condition"           => {"def recur; recur; end; if recur; 1; else; 'x'; end", "(Char | Int32)"},
    "nonreturning binary left"         => {"def recur; recur; end; recur + 1", "NoReturn"},
    "nonreturning binary right"        => {"def recur; recur; end; 1 + recur", "NoReturn"},
    "nonreturning unary"               => {"def recur; recur; end; !recur", "NoReturn"},
    "nonreturning argument"            => {"def recur; recur; end; def accept(x); 1; end; accept(recur)", "NoReturn"},
    "nonreturning receiver"            => {"def recur; recur; end; recur.missing", "NoReturn"},
    "specialized arguments"            => {"def recur(x, flag); if flag; recur(x, flag); else; x; end; end; recur(1, true); recur('x', true)", "Char"},
    "specialized receivers"            => {"class Holder(T); def recur(flag); if flag; recur(flag); else; T; end; end; end; Holder(Int32).new.recur(true); Holder(Char).new.recur(true)", "Char.class"},
    "repeated recursive edges"         => {"def recur(x); if x; recur(x); recur(x); else; 1; end; end; recur(true)", "Int32"},
    "arithmetic base branch"           => {"def recur(x); if x > 0; recur(x - 1) + 1; else; 1; end; end; recur(5)", "Int32"},
    "right arithmetic base branch"     => {"def recur(x); if x > 0; 1 + recur(x - 1); else; 1; end; end; recur(5)", "Int32"},
    "annotation boundary"              => {"def recur : Int32; recur; end; recur", "Int32"},
    "overload boundary"                => {"def recur(x : Int32); recur('x'); end; def recur(x : Char); x; end; recur(1)", "Char"},
    "specialization boundary"          => {"def recur(x); if x.is_a?(Int32); recur('x'); else; x; end; end; recur(1)", "Char"},
    "short circuit right"              => {"def recur; recur; end; 1 || recur", "Int32"},
    "unreachable method tail"          => {"def recur; recur; end; def wrapper; recur; 'x'; end; wrapper", "NoReturn"},
    "independent subsequent calls"     => {"def first; second; end; def second; first; end; def answer; 1; end; first; answer", "Int32"},
    "separate mutual roots"            => {"def first(x); if x; second(x); else; 1; end; end; def second(x); if x; first(x); else; 'x'; end; end; first(true); second(true)", "(Char | Int32)"},
    "recursive class method"           => {"class Worker; def self.recur(x); if x; recur(x); else; 1; end; end; end; Worker.recur(true)", "Int32"},
    "nonreturning array element"       => {"def recur; recur; end; [recur]", "Array(NoReturn)"},
    "nonreturning tuple element"       => {"def recur; recur; end; {1, recur}", "Tuple(Int32, NoReturn)"},
    "nonreturning named tuple element" => {"def recur; recur; end; {value: recur}", "NamedTuple(value: NoReturn)"},
    "nonreturning hash key"            => {"def recur; recur; end; {recur => 1}", "Hash(NoReturn, Int32)"},
    "nonreturning hash value"          => {"def recur; recur; end; {1 => recur}", "Hash(Int32, NoReturn)"},
    "nonreturning range endpoint"      => {"def recur; recur; end; 1..recur", "NoReturn"},
    "nonreturning index receiver"      => {"def recur; recur; end; recur[0]", "NoReturn"},
    "nonreturning index argument"      => {"def recur; recur; end; {1}[recur]", "NoReturn"},
  }.each do |description, contract|
    it "infers #{description}" do
      semantic, snapshot, entry, queries = recursion_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
    end
  end

  it "retains body errors at every recursive call site" do
    _, snapshot, _, _ = recursion_fixture("def accept(x : Int32); 1; end\ndef recur(x); if x; recur(x); else; accept('x'); end; end\nrecur(true)\nrecur(true)")
    snapshot.diagnostics.map(&.code).should eq(["facet.argument_type_mismatch", "facet.argument_type_mismatch"])
  end

  it "does not memoize suppressed loop-probe diagnostics" do
    _, snapshot, _, _ = recursion_fixture("def accept(x : Int32); 1; end; def wrapper(x); accept(x); end; def root; while false; wrapper('x'); end; end; root")
    snapshot.diagnostics.map(&.code).should eq(["facet.argument_type_mismatch"])
  end

  it "invalidates component memoization after instance-variable writes" do
    semantic, snapshot, entry, queries = recursion_fixture("class Box; def initialize; @x = 1; end; def value; @x; end; def change; @x = 'x'; end; def run; while false; value; change; end; value; end; end; Box.new.run")
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("(Char | Int32)")
    snapshot.diagnostics.should be_empty
  end

  it "replays constant-initializer errors at the recursive call and initializer" do
    _, snapshot, _, _ = recursion_fixture("def accept(x : Int32); 1; end; def recur(flag); if flag; recur(flag); else; C; end; end; recur(true); C = accept('x')")
    snapshot.diagnostics.map(&.code).should eq(["facet.argument_type_mismatch", "facet.argument_type_mismatch"])
    snapshot.diagnostics.map(&.span.start).uniq.size.should eq(2)
  end

  it "bounds structurally growing recursive return types" do
    semantic, snapshot, entry, queries = recursion_fixture("def recur(x); if x; {recur(x)}; else; 1; end; end; recur(true)")
    snapshot.complete?.should be_false
    snapshot.diagnostics.should be_empty
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Unknown")
  end

  it "bounds polymorphic recursion with expanding argument types" do
    _, snapshot, _, _ = recursion_fixture("def recur(x); recur({x}); end; recur(1)")
    snapshot.complete?.should be_false
    snapshot.diagnostics.should be_empty
  end

  it "does not reanalyze shared acyclic callees exponentially" do
    source = "def leaf(x); x; end\n"
    previous = "leaf"
    8.times do |index|
      name = "level#{index}"
      source += "def #{name}(x); #{previous}(x); #{previous}(x); end\n"
      previous = name
    end
    source += "#{previous}(1)"
    semantic, snapshot, _, _ = recursion_fixture(source)
    snapshot.diagnostics.should be_empty
    semantic.stats.body_inference_executions.should be < 150
  end

  {
    "binary"         => "recur + missing()",
    "call arguments" => "accept(recur, missing())",
  }.each do |description, expression|
    it "still checks #{description} operand diagnostics" do
      _, snapshot, _, _ = recursion_fixture("def recur; recur; end; def accept(x, y); 1; end; #{expression}")
      snapshot.diagnostics.map(&.code).should eq(["facet.undefined_method"])
    end
  end

  it "invalidates a recursive component after a required body changes" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./provider\"\nfirst(true)", "/workspace/main.cr")
    provider = manager.add("def first(x); if x; second(x); else; 1; end; end\ndef second(x); first(x); end", "/workspace/provider.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, manager.revision(entry))
    semantic.types.display(semantic.analyze([entry]).type_of(ref).not_nil!).should eq("Int32")
    queries.update(provider, "def first(x); if x; second(x); else; 'x'; end; end\ndef second(x); first(x); end")
    snapshot = semantic.analyze([entry])
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Char")
    snapshot.diagnostics.should be_empty
  end
end
