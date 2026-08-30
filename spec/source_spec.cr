require "./spec_helper"

describe Facet::Compiler::Source do
  it "defaults to real source" do
    src = Facet::Compiler::Source.new("foo")
    src.kind.should eq(Facet::Compiler::SourceKind::Real)
    src.expanded_from.should be_nil
  end

  it "maps virtual source back to origin" do
    root = Facet::Compiler::Source.new("root text", "root.cr")
    site_span = Facet::Compiler::Span.new(2, 6)
    virtual = Facet::Compiler::Source.new("virt", nil, Facet::Compiler::SourceKind::Virtual, Facet::Compiler::ExpansionSite.new(root, site_span))

    mapped_source, mapped_span, chain = virtual.map_to_origin(Facet::Compiler::Span.new(0, 4))
    mapped_source.filename.should eq("root.cr")
    mapped_span.should eq(site_span)
    chain.size.should eq(1)
    chain.first.span.should eq(site_span)
  end

  it "tracks monotonic revisions only for changed bytes" do
    manager = Facet::Compiler::SourceManager.new
    file_id = manager.add("one", "sample.cr")
    first_revision = manager.revision(file_id)

    manager.update(file_id, "one").should be_false
    manager.revision(file_id).should eq(first_revision)

    manager.update(file_id, "two").should be_true
    manager.revision(file_id).should be > first_revision
    manager.workspace_revision.should eq(manager.revision(file_id))
  end
end
