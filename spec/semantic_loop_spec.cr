require "./spec_helper"

private def loop_fixture(source : String)
  manager = Facet::Compiler::SourceManager.new
  entry = manager.add(source, "/workspace/main.cr")
  queries = Facet::Compiler::QueryDb.new(manager)
  semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
  {semantic, semantic.analyze([entry]), entry, queries}
end

describe "Semantic loop flow" do
  {
    "break guard narrows exit"                 => {"x = 1 || 'x'; while true; break if x.is_a?(Int32); end; x", "Int32"},
    "next guard narrows exit"                  => {"x = 1 || 'x'; while true; next unless x.is_a?(Int32); break; end; x", "Int32"},
    "while exit narrows nilability"            => {"x = 1 || nil; while x; x = nil; end; x", "Nil"},
    "until exit narrows truthiness"            => {"x = nil || 1; until x; x = 1; end; x", "Int32"},
    "break bypasses condition narrowing"       => {"x = 1 || nil; while x; break; end; x", "(Int32 | Nil)"},
    "condition assignment survives"            => {"while x = nil; end; x", "Nil"},
    "infinite loop in one branch"              => {"flag = true; if flag; while true; end; else; 1; end", "Int32"},
    "nonreturning condition"                   => {"def endless; while true; end; end; while endless; break 1; end", "NoReturn"},
    "nonreturning call argument"               => {"def endless; while true; end; end; def use(x); 1; end; use(endless)", "NoReturn"},
    "nonreturning receiver"                    => {"def endless; while true; end; end; endless.missing", "NoReturn"},
    "nested next stays local"                  => {"flag = true; while true; while flag; next; end; break 'x'; end", "Char"},
    "caller loop state survives a method loop" => {"def inner; while true; break 'x'; end; end; while true; inner; break 1; end", "Int32"},
  }.each do |description, contract|
    it "propagates #{description}" do
      semantic, snapshot, entry, queries = loop_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      Facet::Compiler::AstIntegrity.contract_violations(queries.syntax(entry).ast).should be_empty
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
    end
  end

  it "publishes widened node types from the converged loop state" do
    semantic, snapshot, entry, queries = loop_fixture("x = 1\nflag = true\nwhile flag\n  seen = x\n  x = 'x'\nend\nseen")
    tree = queries.syntax(entry)
    assignment = tree.nodes(Facet::Compiler::NodeKind::Assign).find { |node| node.target.try(&.text) == "seen" }.not_nil!
    ref = Facet::Compiler::NodeRef.new(entry, assignment.value.not_nil!.id, queries.manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("(Char | Int32)")
  end

  it "does not cache speculative method diagnostics away" do
    _, snapshot, _, _ = loop_fixture("def accept(x : Int32); 1; end\ndef wrapper(x); accept(x); end\nx = 1\nwhile false\nwrapper(x)\nx = 'x'\nend")
    snapshot.diagnostics.map(&.code).should eq(["facet.argument_type_mismatch"])
  end

  it "bounds structurally growing loop facts and exposes incompleteness" do
    semantic, snapshot, entry, queries = loop_fixture("x = 1\nflag = true\nwhile flag\nx = {x}\nend\nx")
    snapshot.complete?.should be_false
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Unknown")
    snapshot.diagnostics.should be_empty
  end

  it "invalidates loop return types after a required method changes" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add("require \"./provider\"\nwhile true\nbreak result\nend", "/workspace/main.cr")
    provider = manager.add("def result; 1; end", "/workspace/provider.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(queries, Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil))
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, manager.revision(entry))
    first = semantic.analyze([entry])
    semantic.types.display(first.type_of(ref).not_nil!).should eq("Int32")
    queries.update(provider, "def result; 'x'; end")
    second = semantic.analyze([entry])
    semantic.types.display(second.type_of(ref).not_nil!).should eq("Char")
    second.diagnostics.should be_empty
  end

  it "narrows loop-body receivers before checking calls" do
    _, snapshot, _, _ = loop_fixture("class Item; def value; 1; end; end\nx = Item.new || nil\nwhile x\nx.value\nx = nil\nend")
    snapshot.diagnostics.should be_empty
  end

  it "keeps assignments after next out of loop-carried facts" do
    semantic, snapshot, entry, queries = loop_fixture("x = 1\nflag = true\nwhile flag\nnext\nx = 'x'\nend\nx")
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Int32")
  end

  it "keeps inner infinite loops from reaching an outer break" do
    semantic, snapshot, entry, queries = loop_fixture("while true\nwhile true\nnext\nend\nbreak 1\nend")
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("NoReturn")
  end

  it "retains explicit method returns from possibly empty loops" do
    semantic, snapshot, entry, queries = loop_fixture("def target(flag); while flag; return 'x'; end; end\ntarget(true)")
    node = queries.syntax(entry).root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("(Char | Nil)")
  end

  {
    "empty while"                             => {"while false; end", "Nil"},
    "empty until"                             => {"until true; end", "Nil"},
    "body is not the loop result"             => {"while false; 1; end", "Nil"},
    "literal infinite while"                  => {"while true; end", "NoReturn"},
    "literal infinite until"                  => {"until false; end", "NoReturn"},
    "while break value"                       => {"while true; break 1; end", "Int32"},
    "until break value"                       => {"until false; break 'x'; end", "Char"},
    "valueless break"                         => {"while true; break; end", "Nil"},
    "possible zero iterations"                => {"flag = true; while flag; break 1; end", "(Int32 | Nil)"},
    "false condition retains break types"     => {"while false; break 1; end", "(Int32 | Nil)"},
    "until true retains break types"          => {"until true; break 1; end", "(Int32 | Nil)"},
    "multiple break values"                   => {"flag = true; while true; if flag; break 1; else; break 'x'; end; end", "(Char | Int32)"},
    "next value is not an exit value"         => {"while true; next 1; end", "NoReturn"},
    "conditional next and break"              => {"flag = true; while true; next if flag; break 'x'; end", "Char"},
    "nested breaks stay local"                => {"while true; while true; break 'x'; end; break 1; end", "Int32"},
    "nested loop result can be broken"        => {"while true; value = while true; break 'x'; end; break value; end", "Char"},
    "fallthrough assignments widen"           => {"x = 1; while false; x = 'x'; end; x", "(Char | Int32)"},
    "until assignments widen"                 => {"x = 1; until true; x = 'x'; end; x", "(Char | Int32)"},
    "new loop locals are nilable"             => {"while false; x = 'x'; end; x", "(Char | Nil)"},
    "break exit keeps assignment"             => {"x = 1; while true; x = 'x'; break; end; x", "Char"},
    "unreachable statements after break"      => {"x = 1; while true; break; x = 'x'; end; x", "Int32"},
    "next carries loop state"                 => {"x = 1; flag = true; while flag; x = 'x'; next; end; x", "(Char | Int32)"},
    "loop-carried value before assignment"    => {"x = 1; y = nil; while false; y = x; x = 'x'; end; y", "(Char | Int32 | Nil)"},
    "return inside loop"                      => {"def target; while true; return 'x'; end; end; target", "Char"},
    "infinite method stops fallthrough"       => {"def target; while true; next; end; 1; end; target", "NoReturn"},
    "return and break combine"                => {"def target(flag); while true; return 'x' if flag; break 1; end; end; target(true)", "(Char | Int32)"},
    "callee returns do not break caller loop" => {"def inner; return 'x'; end; while true; inner; break 1; end", "Int32"},
    "multiple loops keep separate results"    => {"first = while true; break 1; end; second = until false; break 'x'; end; {first, second}", "Tuple(Int32, Char)"},
    "loop introduced local on break"          => {"while true; x = 1; break; end; x", "Int32"},
  }.each do |description, contract|
    it "infers #{description}" do
      semantic, snapshot, entry, queries = loop_fixture(contract[0])
      queries.syntax(entry).ast.diagnostics.should be_empty
      Facet::Compiler::AstIntegrity.contract_violations(queries.syntax(entry).ast).should be_empty
      snapshot.diagnostics.should be_empty
      node = queries.syntax(entry).root.children.last.children.last
      ref = Facet::Compiler::NodeRef.new(entry, node.id, queries.manager.revision(entry))
      semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(contract[1])
    end
  end

  it "treats NoReturn as the bottom of unions" do
    types = Facet::Compiler::TypeStore.new
    types.display(types.union([types.named("NoReturn"), types.named("Int32")])).should eq("Int32")
    types.display(types.union([types.named("NoReturn")])).should eq("NoReturn")
  end
end
