require "./spec_helper"

describe Facet::Compiler::QueryDb do
  it "caches parse/index/expand per file" do
    mgr = Facet::Compiler::SourceManager.new
    fid = mgr.add("macro foo; end\n{{ foo }}")
    db = Facet::Compiler::QueryDb.new(mgr)

    expanded1 = db.expand(fid)
    expanded2 = db.expand(fid)
    expanded1.source.text.should eq(expanded2.source.text)
  end

  it "invalidates on source change" do
    mgr = Facet::Compiler::SourceManager.new
    fid = mgr.add("macro foo; end\n{{ foo }}")
    db = Facet::Compiler::QueryDb.new(mgr)

    db.expand(fid)
    mgr.update(fid, "macro foo; end\n{{ foo }}\n{{ foo }}")
    expanded = db.expand(fid)
    expanded.source.text.should contain("foo")
  end

  it "returns parser diagnostics once" do
    mgr = Facet::Compiler::SourceManager.new
    fid = mgr.add("def foo(", "broken.cr")
    db = Facet::Compiler::QueryDb.new(mgr)

    direct_parser = Facet::Compiler::Parser.new(mgr.source(fid))
    direct_ast = direct_parser.parse_file
    cached_ast = db.parse(fid)

    direct_ast.diagnostics.should_not be_empty
    cached_ast.diagnostics.map(&.message).should eq(direct_ast.diagnostics.map(&.message))
  end

  it "invalidates expand when macro provider changes" do
    mgr = Facet::Compiler::SourceManager.new
    macro_fid = mgr.add("macro foo; 1; end")
    use_fid = mgr.add("{{ foo }}")
    db = Facet::Compiler::QueryDb.new(mgr)

    db.expand(use_fid)
    mgr.update(macro_fid, "macro foo; 2; end")
    expanded = db.expand(use_fid)
    expanded.source.text.should contain("2")
  end

  it "invalidates ordinary macro calls when their provider changes" do
    mgr = Facet::Compiler::SourceManager.new
    macro_fid = mgr.add("macro answer\n1\nend", "macro.cr")
    use_fid = mgr.add("answer", "use.cr")
    db = Facet::Compiler::QueryDb.new(mgr)

    db.expand(use_fid).source.text.strip.should eq("1")
    db.update(macro_fid, "macro answer\n2\nend").should be_true
    db.expand(use_fid).source.text.strip.should eq("2")
  end

  it "tracks expansion consumers invalidated by a macro provider edit" do
    mgr = Facet::Compiler::SourceManager.new
    macro_fid = mgr.add("macro answer\n1\nend", "macro.cr")
    use_fid = mgr.add("answer", "use.cr")
    unrelated_fid = mgr.add("value = 1", "unrelated.cr")
    db = Facet::Compiler::QueryDb.new(mgr)

    db.expand(use_fid)
    db.expand(unrelated_fid)
    db.pending_expansion_file_ids.should be_empty
    db.update(macro_fid, "macro answer\n2\nend").should be_true

    db.pending_expansion_file_ids.should contain(macro_fid)
    db.pending_expansion_file_ids.should contain(use_fid)
    db.pending_expansion_file_ids.should_not contain(unrelated_fid)
    db.expand(use_fid).source.text.strip.should eq("2")
    db.pending_expansion_file_ids.should_not contain(use_fid)
  end

  it "invalidates an unresolved ordinary call when a provider appears" do
    mgr = Facet::Compiler::SourceManager.new
    use_fid = mgr.add("answer", "use.cr")
    db = Facet::Compiler::QueryDb.new(mgr)

    db.expand(use_fid).source.text.should eq("answer")
    macro_fid, _ = db.upsert("macro answer\n42\nend", "macro.cr")
    db.expand(macro_fid)

    db.pending_expansion_file_ids.should contain(use_fid)
    db.expand(use_fid).source.text.strip.should eq("42")
  end

  it "does not queue newly registered files before an expansion is materialized" do
    manager = Facet::Compiler::SourceManager.new
    queries = Facet::Compiler::QueryDb.new(manager)

    file_id, changed = queries.upsert("value = 1", "new.cr")

    changed.should be_true
    file_id.should be >= 0
    queries.pending_expansion_file_ids.should be_empty
  end

  it "does not recompute queries for unchanged source bytes" do
    mgr = Facet::Compiler::SourceManager.new
    fid = mgr.add("value = 1", "same.cr")
    db = Facet::Compiler::QueryDb.new(mgr)

    first = db.parse(fid)
    second = db.parse(fid)
    first.arena.same?(second.arena).should be_true
    db.stats.parse_executions.should eq(1)
    db.stats.parse_cache_hits.should eq(1)

    revision = mgr.revision(fid)
    db.update(fid, "value = 1").should be_false
    mgr.revision(fid).should eq(revision)
    db.parse(fid)
    db.stats.parse_executions.should eq(1)

    first_tree = db.syntax(fid)
    second_tree = db.syntax(fid)
    first_tree.same?(second_tree).should be_true
    db.stats.syntax_executions.should eq(1)
    db.stats.syntax_cache_hits.should eq(1)
  end

  it "keeps expansions cached across unrelated edits" do
    mgr = Facet::Compiler::SourceManager.new
    macro_fid = mgr.add("macro answer; 42; end", "macro.cr")
    use_fid = mgr.add("{{ answer }}", "use.cr")
    unrelated_fid = mgr.add("value = 1", "unrelated.cr")
    db = Facet::Compiler::QueryDb.new(mgr)

    db.expand(use_fid).source.text.should contain("42")
    executions = db.stats.expand_executions
    db.update(unrelated_fid, "value = 2").should be_true
    db.expand(use_fid).source.text.should contain("42")

    db.stats.expand_executions.should eq(executions)
    db.stats.expand_cache_hits.should be > 0
    db.stats.parse_executions.should be >= 3
    macro_fid.should be >= 0
  end

  it "invalidates type-aware macro consumers when indexed declarations change" do
    mgr = Facet::Compiler::SourceManager.new
    type_fid = mgr.add("class Item\n  def old_name; end\nend", "item.cr")
    macro_fid = mgr.add(<<-CR, "macros.cr")
      macro method_names(type)
        {{type.resolve.methods.map { |method| method.name }.join(",")}}
      end
    CR
    use_fid = mgr.add("names = method_names(Item)", "use.cr")
    db = Facet::Compiler::QueryDb.new(mgr)

    db.expand(use_fid).source.text.should match(/names =\s+"old_name"/)
    db.pending_expansion_file_ids.should be_empty

    db.update(type_fid, "class Item\n  def old_name; end\n  def new_name; end\nend").should be_true
    db.pending_expansion_file_ids.should contain(use_fid)
    db.expand(use_fid).source.text.should match(/names =\s+"old_name,new_name"/)
    db.pending_expansion_file_ids.should_not contain(use_fid)
    macro_fid.should be >= 0
  end

  it "invalidates type-aware macro consumers when annotation values change" do
    mgr = Facet::Compiler::SourceManager.new
    type_fid = mgr.add(<<-CR, "item.cr")
      annotation Label
      end

      @[Label(name: "old")]
      class Item
      end
    CR
    macro_fid = mgr.add(<<-CR, "macros.cr")
      macro label(type)
        {{type.resolve.annotation(Label)[:name]}}
      end
    CR
    use_fid = mgr.add("name = label(Item)", "use.cr")
    db = Facet::Compiler::QueryDb.new(mgr)

    db.expand(use_fid).source.text.should match(/name =\s+"old"/)
    db.pending_expansion_file_ids.should be_empty

    db.update(type_fid, <<-CR).should be_true
      annotation Label
      end

      @[Label(name: "new")]
      class Item
      end
    CR
    db.pending_expansion_file_ids.should contain(use_fid)
    db.expand(use_fid).source.text.should match(/name =\s+"new"/)
    db.pending_expansion_file_ids.should_not contain(use_fid)
    macro_fid.should be >= 0
  end

  it "applies byte edits and preserves stable named file ids" do
    mgr = Facet::Compiler::SourceManager.new
    db = Facet::Compiler::QueryDb.new(mgr)
    fid, added = db.upsert("value = 10", "edit.cr")
    added.should be_true

    db.apply_edit(fid, Facet::Compiler::Span.new(8, 10), "20").should be_true
    mgr.source(fid).text.should eq("value = 20")
    db.parse(fid).diagnostics.should be_empty

    same_fid, changed = db.upsert("value = 30", "edit.cr")
    same_fid.should eq(fid)
    changed.should be_true
    mgr.source(fid).text.should eq("value = 30")
  end
end
