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
