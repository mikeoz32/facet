require "./spec_helper"

private def semantic_fixture(
  sources : Hash(String, String),
  entry : String,
  roots : Array(String) = ["/workspace", "/stdlib"],
  prelude : String? = "prelude",
  semantic_options : Facet::Compiler::SemanticOptions = Facet::Compiler::SemanticOptions.new,
)
  manager = Facet::Compiler::SourceManager.new
  ids = {} of String => Facet::Compiler::FileId
  sources.each do |filename, source|
    ids[filename] = manager.add(source, filename)
  end
  queries = Facet::Compiler::QueryDb.new(manager)
  resolver = Facet::Compiler::RegisteredSourceResolver.new(roots, prelude)
  semantic = Facet::Compiler::SemanticDb.new(queries, resolver, semantic_options: semantic_options)
  {semantic, semantic.analyze([ids[entry]]), ids, queries}
end

describe Facet::Compiler::SemanticDb do
  it "analyzes the require-reachable project, dependencies, and prelude" do
    semantic, snapshot, ids, _ = semantic_fixture({
      "/workspace/main.cr"        => %(require "./models/user"\nuser = User.new\nuser.name\nuser.missing\n),
      "/workspace/models/user.cr" => "class User\n  def name : String\n    \"A\"\n  end\nend\n",
      "/workspace/unreachable.cr" => "class Hidden; end\n",
      "/stdlib/prelude.cr"        => "class String; end\n",
    }, "/workspace/main.cr")

    snapshot.graph.reachable.should contain(ids["/workspace/main.cr"])
    snapshot.graph.reachable.should contain(ids["/workspace/models/user.cr"])
    snapshot.graph.reachable.should contain(ids["/stdlib/prelude.cr"])
    snapshot.graph.reachable.should_not contain(ids["/workspace/unreachable.cr"])

    diagnostics = snapshot.diagnostics_for(ids["/workspace/main.cr"])
    diagnostics.map(&.code).should eq(["facet.undefined_method"])
    diagnostics.first.message.should contain("missing")
    diagnostics.first.confidence.provisional?.should be_true
    snapshot.methods("User", "name").size.should eq(1)
    semantic.types.display(snapshot.methods("User", "name").first.return_type).should eq("String")
  end

  it "resolves relative, rooted, glob, and directory requires" do
    _, snapshot, ids, _ = semantic_fixture({
      "/workspace/main.cr"            => %(require "./models/*"\nrequire "support"\n),
      "/workspace/models/a.cr"        => "class A; end\n",
      "/workspace/models/b.cr"        => "class B; end\n",
      "/workspace/support/helpers.cr" => "module Helpers; end\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    snapshot.diagnostics.should be_empty
    snapshot.graph.reachable.should eq(Set{
      ids["/workspace/main.cr"],
      ids["/workspace/models/a.cr"],
      ids["/workspace/models/b.cr"],
      ids["/workspace/support/helpers.cr"],
    })
  end

  it "uses the first exact require match before directory and later-root candidates" do
    _, snapshot, ids, _ = semantic_fixture({
      "/workspace/main.cr"           => %(require "support"\n),
      "/workspace/support.cr"        => "class Exact; end\n",
      "/workspace/support/helper.cr" => "class DirectoryCandidate; end\n",
      "/dependency/support.cr"       => "class LaterRoot; end\n",
    }, "/workspace/main.cr", ["/workspace", "/dependency"], nil)

    snapshot.diagnostics.should be_empty
    snapshot.graph.reachable.should eq(Set{
      ids["/workspace/main.cr"],
      ids["/workspace/support.cr"],
    })
  end

  it "reports missing requires and keeps a tolerant partial snapshot" do
    _, snapshot, ids, _ = semantic_fixture({
      "/workspace/main.cr" => %(require "./missing"\nclass Present; end\n),
    }, "/workspace/main.cr", ["/workspace"], nil)

    snapshot.complete?.should be_false
    snapshot.success?.should be_true
    snapshot.diagnostics_for(ids["/workspace/main.cr"]).map(&.code).should eq(["facet.missing_require"])
  end

  it "makes strict success depend on semantic errors" do
    manager = Facet::Compiler::SourceManager.new
    entry = manager.add(%(require "./missing"\n), "/workspace/main.cr")
    queries = Facet::Compiler::QueryDb.new(manager)
    semantic = Facet::Compiler::SemanticDb.new(
      queries,
      Facet::Compiler::RegisteredSourceResolver.new(["/workspace"], nil)
    )

    semantic.analyze([entry], Facet::Compiler::SemanticMode::Tolerant).success?.should be_true
    semantic.analyze([entry], Facet::Compiler::SemanticMode::Strict).success?.should be_false
  end

  it "suppresses undefined-method diagnostics for unknown receivers and method_missing" do
    _, snapshot, ids, _ = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        unknown.value

        class Dynamic
          def method_missing(call)
          end
        end

        Dynamic.new.generated
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    snapshot.diagnostics_for(ids["/workspace/main.cr"]).should be_empty
    snapshot.completeness_reasons.should contain(Facet::Compiler::SemanticCompletenessReason::UnknownType)
  end

  it "looks methods up through inheritance and included modules" do
    _, snapshot, ids, _ = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        module Named
          def name : String
            "ok"
          end
        end

        class Parent
          def parent_value : Int32
            1
          end
        end

        class Child < Parent
          include Named
        end

        child = Child.new
        child.name
        child.parent_value
        child.nope
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    diagnostics = snapshot.diagnostics_for(ids["/workspace/main.cr"])
    diagnostics.map(&.message).should eq(["undefined method 'nope' for Child"])
  end

  it "records inferred node types and rejects stale node references" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => "class Item; end\nitem = Item.new\nitem\n",
    }, "/workspace/main.cr", ["/workspace"], nil)
    file_id = ids["/workspace/main.cr"]
    tree = queries.syntax(file_id)
    item = tree.nodes(Facet::Compiler::NodeKind::Ident).last
    ref = Facet::Compiler::NodeRef.new(file_id, item.id, queries.manager.revision(file_id))

    type_id = snapshot.type_of(ref).not_nil!
    semantic.types.display(type_id).should eq("Item")

    queries.update(file_id, "class Item; end\nitem = Item.new\nitem\n# changed\n")
    stale = Facet::Compiler::NodeRef.new(file_id, item.id, queries.manager.revision(file_id))
    snapshot.type_of(stale).should be_nil
  end

  it "caches unchanged semantic and per-file queries across unrelated edits" do
    semantic, first, ids, queries = semantic_fixture({
      "/workspace/main.cr"      => %(require "./used"\nUsed.new.ok\n),
      "/workspace/used.cr"      => "class Used\n  def ok : Bool; true; end\nend\n",
      "/workspace/unrelated.cr" => "class Unrelated; end\n",
    }, "/workspace/main.cr", ["/workspace"], nil)
    executions = semantic.stats.analysis_executions

    second = semantic.analyze([ids["/workspace/main.cr"]])
    second.same?(first).should be_true
    semantic.stats.analysis_executions.should eq(executions)

    queries.update(ids["/workspace/unrelated.cr"], "class Unrelated; def changed; end; end\n")
    third = semantic.analyze([ids["/workspace/main.cr"]])
    third.same?(first).should be_true
    semantic.stats.analysis_cache_hits.should be >= 2
  end

  it "indexes methods generated by a macro before checking calls" do
    _, snapshot, ids, _ = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        macro add_generated
          def generated : Int32
            42
          end
        end

        class Item
          add_generated
        end

        Item.new.generated
        Item.new.absent
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    generated = snapshot.methods("Item", "generated")
    generated.size.should eq(1)
    generated.first.generated.should be_true
    diagnostics = snapshot.diagnostics_for(ids["/workspace/main.cr"])
    diagnostics.map(&.message).should eq([
      "undefined method 'absent' for Item",
    ])
    diagnostics.first.confidence.conclusive?.should be_true
  end

  it "substitutes receiver generic arguments into method return types" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class Box(T)
          def value : T
            raise "abstract"
          end
        end

        box = Box(String).new
        box.value
      CR
      "/stdlib/prelude.cr" => "class String; end\n",
    }, "/workspace/main.cr")

    tree = queries.syntax(ids["/workspace/main.cr"])
    call = tree.nodes(Facet::Compiler::NodeKind::Binary).find { |node| node.call_name == "value" }.not_nil!
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], call.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("String")
  end

  it "preserves compact numeric suffixes and canonical nil-last unions" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => %({1i8, 1u16, 2.3f32, 2.3f64, 1 || "" || nil}\n),
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Int8, UInt16, Float32, Float64, (Int32 | String | Nil))"
    )
  end

  it "preserves explicit nested and union generic arguments" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => "class Box(T); end\nBox(Box(Int32 | Float64)).new\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    call = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], call.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Box(Box(Float64 | Int32))")
  end

  it "resolves a bare zero-argument call through Object methods" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => "def answer; 42; end\nanswer\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    call = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], call.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Int32")
  end

  it "specializes untyped parameters from call arguments and defaults" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => "def identity(value); value; end\ndef defaulted(value = 'x'); value; end\n{identity(42), defaulted}\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Tuple(Int32, Char)")
  end

  it "reuses identical call-site specializations within a semantic snapshot" do
    semantic, _, _, _ = semantic_fixture({
      "/workspace/main.cr" => "def identity(value); cheeky = value; cheeky; end\n{identity(1), identity(2)}\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    # One specialized pass serves both calls; the second execution records the
    # method body itself for editor queries.
    semantic.stats.body_inference_executions.should eq(2)
  end

  it "keeps self-return inference specific to each generic receiver" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => "class Box(T); def itself; self; end; end\n{Box(Int32).new.itself, Box(Char).new.itself}\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Tuple(Box(Int32), Box(Char))")
  end

  it "prefers exact arity and parameter types during overload selection" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def choose(value : Int32); 1; end
        def choose(value : Char); 'x'; end
        def count(a, b); 1; end
        def count(a, b, c = 0); 'x'; end
        {choose(1), choose('x'), count(1, 2)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil, Facet::Compiler::SemanticOptions.new(["preview_overload_order"]))

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Tuple(Int32, Char, Int32)")
  end

  it "orders untyped preview overloads by positional signature specificity" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def optional(x, y, z = 0); 1; end
        def optional(x, y, z = 0, w = 0); 'x'; end
        def required(x, y = 0); 'x'; end
        def required(x, y, z = 0); 1; end
        def bounded(x, y, *rest); 'x'; end
        def bounded(x, y, z = 0); 1; end
        def later_splat(x, y, *rest); 'x'; end
        def later_splat(x, y, z = 0, *rest); 1; end
        {optional(1, 2), required(1, 2), bounded(1, 2), later_splat(1, 2)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil, Facet::Compiler::SemanticOptions.new(["preview_overload_order"]))

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Tuple(Int32, Int32, Int32, Int32)")
  end

  it "diagnoses only when every closed member of a union lacks the method" do
    _, snapshot, ids, _ = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class A
          def shared : Bool; true; end
        end
        class B; end

        both = flag ? A.new : B.new
        both.shared
        both.absent
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    snapshot.diagnostics_for(ids["/workspace/main.cr"]).map(&.message).should eq([
      "undefined method 'absent' for (A | B)",
    ])
  end
end
