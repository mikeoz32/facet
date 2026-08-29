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
    db.invalidate(fid)
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
    db.invalidate(macro_fid)
    expanded = db.expand(use_fid)
    expanded.source.text.should contain("2")
  end
end
