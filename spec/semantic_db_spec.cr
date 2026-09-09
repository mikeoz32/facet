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

  it "binds method forall variables through values, metaclasses, and generic returns" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class Box(T); end
        def reflected(x : Free) forall Free; Free; end
        def reflected_class(x : Free.class) forall Free; Free; end
        def wrap(x : T) forall T; Box(T).new; end
        def passthrough(x : U) : U forall U; x; end
        {reflected(1), reflected_class(Int32), wrap('x'), passthrough(1)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Int32.class, Int32.class, Box(Char), Int32)"
    )
  end

  it "binds method forall variables from defaults and tuple restrictions" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def default_type(x, y : U = nil) forall U; U; end
        def default_class(x : Free.class = Int32) forall Free; Free; end
        def tuple_second(x : {X, Y}) forall X, Y; Y; end
        def tuple_value(x : {_, _}); x; end
        {default_type(1), default_class, tuple_second({1, 2.5}), tuple_value({1, 2.5})}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Nil.class, Int32.class, Float64.class, Tuple(Int32, Float64))"
    )
  end

  it "parenthesizes a union metaclass inferred through forall" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def union_type(x : T?) forall T; T; end
        union_type(1 || "" || nil)
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("(Int32 | String).class")
  end

  it "binds forall variables through blocks, splats, generic includes, and nested unions" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        module Included(T); end
        class Direct
          include Included(Int32)
        end
        class Generic(T)
          include Included(T)
        end
        def from_block(&block : -> Free) forall Free; yield; Free; end
        def from_include(x : Included(T)) forall T; T; end
        def from_splat(**x : **T) forall T; T; end
        def nested(x : (Nil | T).class) forall T; T; end
        def remainder(x : T | (Int32 | String)) forall T; T; end
        {
          from_block { 1 },
          from_include(Direct.new),
          from_include(Generic(Char).new),
          from_splat(**{a: 1, b: ""}),
          nested(String?),
          remainder(1 || "" || 'a'),
        }
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Int32.class, Int32.class, Char.class, NamedTuple(a: Int32, b: String).class, String.class, Char.class)"
    )
  end

  it "resolves constants through lexical scope, methods, ancestors, and absolute paths" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        TOP = 2.5
        module Shared
          FLAG = true
        end
        class Parent
          VALUE = 1
        end
        class Child < Parent
          include Shared
          LOCAL = 2.5
          def value; VALUE; end
          def self.local; LOCAL; end
        end
        {Child.new.value, Child.local, Child::FLAG, ::TOP}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Int32, Float64, Bool, Float64)"
    )
    semantic.types.display(snapshot.constant("Child::LOCAL").not_nil!.type_id).should eq("Float64")
  end

  it "resolves nested constant assignment paths and forall metaclass paths" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class Box
          Inner::VALUE = 'x'
        end
        struct Int32
          MARK = 'm'
        end
        def mark(x : U) forall U; U::MARK; end
        {Box::Inner::VALUE, mark(1)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Tuple(Char, Char)")
  end

  it "types enum members as their owning enum" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => "lib LibC\n  enum Status\n    Ready = 1\n  end\nend\nLibC::Status::Ready\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("LibC::Status")
  end

  it "creates module types for implicit constant namespaces" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => "Config::VALUE = 1\nConfig\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Config:Module")
  end

  it "invalidates referenced constants across required-file edits" do
    semantic, first, ids, queries = semantic_fixture({
      "/workspace/main.cr"      => %(require "./constants"\nConfig::VALUE\n),
      "/workspace/constants.cr" => "module Config\n  VALUE = 1\nend\n",
    }, "/workspace/main.cr", ["/workspace"], nil)
    entry = ids["/workspace/main.cr"]
    first_tree = queries.syntax(entry)
    first_result = first_tree.root.children.last.children.last
    first_ref = Facet::Compiler::NodeRef.new(entry, first_result.id, queries.manager.revision(entry))
    semantic.types.display(first.type_of(first_ref).not_nil!).should eq("Int32")

    queries.update(ids["/workspace/constants.cr"], "module Config\n  VALUE = \"changed\"\nend\n")
    second = semantic.analyze([entry])
    second_tree = queries.syntax(entry)
    second_result = second_tree.root.children.last.children.last
    second_ref = Facet::Compiler::NodeRef.new(entry, second_result.id, queries.manager.revision(entry))
    semantic.types.display(second.type_of(second_ref).not_nil!).should eq("String")
    semantic.types.display(second.constant("Config::VALUE").not_nil!.type_id).should eq("String")
  end

  it "reports unresolved constants at the method call site" do
    _, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class Foo
          class Foo
          end
          def self.value
            Foo::Foo
          end
        end
        Foo.value
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    diagnostics = snapshot.diagnostics_for(ids["/workspace/main.cr"])
    diagnostics.size.should eq(1)
    diagnostic = diagnostics.first
    diagnostic.code.should eq("facet.undefined_constant")
    diagnostic.message.should eq("undefined constant Foo::Foo")
    position = queries.syntax(ids["/workspace/main.cr"]).position_at(diagnostic.span.start)
    {position.line + 1, position.character + 1}.should eq({8, 7})
  end

  it "isolates constant values from top-level locals and diagnoses cycles" do
    _, isolated, isolated_ids, isolated_queries = semantic_fixture({
      "/workspace/main.cr" => "local = 1; VALUE = local; VALUE\n",
    }, "/workspace/main.cr", ["/workspace"], nil)
    diagnostics = isolated.diagnostics_for(isolated_ids["/workspace/main.cr"])
    diagnostics.size.should eq(1)
    diagnostic = diagnostics.first
    diagnostic.code.should eq("facet.undefined_local")
    position = isolated_queries.syntax(isolated_ids["/workspace/main.cr"]).position_at(diagnostic.span.start)
    {position.line + 1, position.character + 1}.should eq({1, 20})

    _, cyclic, cyclic_ids, cyclic_queries = semantic_fixture({
      "/workspace/main.cr" => "VALUE = VALUE.next\nVALUE\n",
    }, "/workspace/main.cr", ["/workspace"], nil)
    diagnostics = cyclic.diagnostics_for(cyclic_ids["/workspace/main.cr"])
    diagnostics.size.should eq(1)
    diagnostic = diagnostics.first
    diagnostic.code.should eq("facet.constant_cycle")
    position = cyclic_queries.syntax(cyclic_ids["/workspace/main.cr"]).position_at(diagnostic.span.start)
    {position.line + 1, position.character + 1}.should eq({1, 1})
  end

  it "reports constants used as declaration and generic types" do
    _, declaration, declaration_ids, declaration_queries = semantic_fixture({
      "/workspace/main.cr" => "VALUE = 1\nitem : VALUE\n",
    }, "/workspace/main.cr", ["/workspace"], nil)
    diagnostics = declaration.diagnostics_for(declaration_ids["/workspace/main.cr"])
    diagnostics.size.should eq(1)
    diagnostic = diagnostics.first
    diagnostic.code.should eq("facet.constant_as_type")
    position = declaration_queries.syntax(declaration_ids["/workspace/main.cr"]).position_at(diagnostic.span.start)
    {position.line + 1, position.character + 1}.should eq({2, 8})

    _, generic, generic_ids, generic_queries = semantic_fixture({
      "/workspace/main.cr" => "Box = Box(Int32).new\nBox\n",
    }, "/workspace/main.cr", ["/workspace"], nil)
    diagnostics = generic.diagnostics_for(generic_ids["/workspace/main.cr"])
    diagnostics.size.should eq(1)
    diagnostic = diagnostics.first
    diagnostic.code.should eq("facet.constant_as_type")
    position = generic_queries.syntax(generic_ids["/workspace/main.cr"]).position_at(diagnostic.span.start)
    {position.line + 1, position.character + 1}.should eq({1, 7})
  end

  it "reports a constant parameter restriction when the method is instantiated" do
    _, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => "VALUE = 1\ndef consume(value : VALUE); end\nconsume(1)\n",
    }, "/workspace/main.cr", ["/workspace"], nil)

    diagnostics = snapshot.diagnostics_for(ids["/workspace/main.cr"])
    diagnostics.size.should eq(1)
    diagnostic = diagnostics.first
    diagnostic.code.should eq("facet.constant_as_type")
    position = queries.syntax(ids["/workspace/main.cr"]).position_at(diagnostic.span.start)
    {position.line + 1, position.character + 1}.should eq({3, 1})
  end

  it "narrows truthy assignments and merges conditional branch bindings" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def guarded
          value = 1 if 1
          return 2 unless value && value
          value
        end
        a = 1 || nil
        b = 2 || nil
        pair = if !a || !b
                 {1, 2}
               else
                 {a, b}
               end
        {guarded, pair}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Int32, Tuple(Int32, Int32))"
    )
  end

  it "narrows is_a? alternatives through ancestors" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class Parent; end
        class Left < Parent
          def value; 1; end
        end
        class Right < Parent
          def value; 'r'; end
        end
        item = Left.new.as(Parent)
        if item.is_a?(Left) || item.is_a?(Right)
          item.value
        else
          nil
        end
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("(Char | Int32 | Nil)")
  end

  it "combines explicit returns with the surviving guard environment" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def guarded
          value = 1 || 2.0 || 'x'
          if !value.is_a?(Int32) && !value.is_a?(Float64)
            return true
          end
          value
        end
        guarded
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("(Bool | Float64 | Int32)")
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

  it "orders preview overloads by restriction subsumption and named specificity" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def typed_optional(x : Int32 = 0); 1; end
        def typed_optional(*args); 'x'; end
        def typed_optional_reversed(*args); 'x'; end
        def typed_optional_reversed(x : Int32 = 0); 1; end

        def named_optional(*, x : Int32 = 0); 1; end
        def named_optional(**opts); 'x'; end
        def named_optional_reversed(**opts); 'x'; end
        def named_optional_reversed(*, x : Int32 = 0); 1; end

        def named_required(*, n); 1; end
        def named_required(*, n, **rest); 'x'; end
        def named_required_reversed(*, n, **rest); 'x'; end
        def named_required_reversed(*, n); 1; end

        {typed_optional, typed_optional_reversed, named_optional,
         named_optional_reversed, named_required(n: 0), named_required_reversed(n: 0)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil, Facet::Compiler::SemanticOptions.new(["preview_overload_order"]))

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Int32, Int32, Int32, Int32, Int32, Int32)"
    )
  end

  it "keeps the first preview overload when strictness dimensions conflict" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def restricted(x : Int32, *args : Number); 1; end
        def restricted(*args : Int); 'x'; end
        def restricted_reversed(*args : Int); 1; end
        def restricted_reversed(x : Int32, *args : Number); 'x'; end

        def mixed(x, *, y = 0); 1; end
        def mixed(x = 0, *, y); 'x'; end
        def mixed_reversed(x = 0, *, y); 1; end
        def mixed_reversed(x, *, y = 0); 'x'; end

        def named(*, x, y = 0); 1; end
        def named(*, y, x = 0); 'x'; end
        def named_reversed(*, y, x = 0); 1; end
        def named_reversed(*, x, y = 0); 'x'; end

        {restricted(1, 2, 3), restricted_reversed(1, 2, 3),
         mixed(1, y: 2), mixed_reversed(1, y: 2),
         named(x: 1, y: 2), named_reversed(x: 1, y: 2)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil, Facet::Compiler::SemanticOptions.new(["preview_overload_order"]))

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Int32, Int32, Int32, Int32, Int32, Int32)"
    )
  end

  it "selects overloads by block presence and nominal restrictions" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def block_choice; yield; 1; end
        def block_choice; 2.5; end
        def numeric(value : Int); 2.5; end
        def numeric(value : Float); 1; end
        {block_choice, block_choice { nil }, numeric(1), numeric(1.5)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Float64, Int32, Float64, Int32)"
    )
  end

  it "respects explicit class new methods and initializer instance-variable types" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class Factory(T)
          def self.new
            1
          end
        end

        class Defaults
          def initialize(@self_value = self, @named_value = 'a')
          end

          def self_value
            @self_value
          end

          def named_value
            @named_value
          end
        end

        class Declared
          @value : Int32

          def initialize(@value = fallback)
          end

          def value
            @value
          end

          def fallback
            1
          end
        end

        {Factory(Int32).new, Defaults.new.self_value,
         Defaults.new(named_value: 'b').named_value, Declared.new.value}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple(Int32, Defaults, Char, Int32)"
    )
  end

  it "types declarations as nil even when their bodies contain proc values" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class CallbackHolder
          def self.callback(value)
          end

          @callback : Proc(String, Nil) = ->callback(String)
        end
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq("Nil")
  end

  it "types Union type expressions as normalized metaclasses" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        struct Union
          def self.types
            T
          end
        end

        {Union(Int32, String), Union(Int32, Int32), Union(Int32, String).types}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple((Int32 | String).class, Int32.class, Tuple(Int32, String).class)"
    )
  end

  it "dispatches union arguments and named arguments per overload" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        class Parent; end
        class Child < Parent; end
        def choose(value : Child); 1; end
        def choose(value); 2.5; end
        def named(a : Int32, b : Int32); true; end
        def named(b : Int32, a : Nil); 'x'; end
        value = nil || Parent.new || Child.new
        named_value = 1 || nil
        {choose(value), named(a: named_value, b: 2)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple((Float64 | Int32), (Bool | Char))"
    )
  end

  it "matches structural tuple restrictions and replaces duplicate signatures" do
    semantic, snapshot, ids, queries = semantic_fixture({
      "/workspace/main.cr" => <<-CR,
        def tuple_size(value : {X, Y}) forall X, Y; 1; end
        def tuple_size(value : {X, Y, Z}) forall X, Y, Z; 'x'; end
        def replaced(value : String.class); 1; end
        def replaced(value : ::String.class); 'x'; end
        tuple = {1, 2} || {1, 2, 3}
        {tuple_size(tuple), replaced(String)}
      CR
    }, "/workspace/main.cr", ["/workspace"], nil)

    tree = queries.syntax(ids["/workspace/main.cr"])
    result = tree.root.children.last.children.last
    ref = Facet::Compiler::NodeRef.new(ids["/workspace/main.cr"], result.id, queries.manager.revision(ids["/workspace/main.cr"]))
    semantic.types.display(snapshot.type_of(ref).not_nil!).should eq(
      "Tuple((Char | Int32), Char)"
    )
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
