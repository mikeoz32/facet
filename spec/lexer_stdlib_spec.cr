require "./spec_helper"

describe "Lexer stdlib scan" do
  it "lexes the Crystal stdlib without diagnostics" do
    stdlib_root = find_stdlib_root
    files = Dir.glob("#{stdlib_root}/**/*.cr")

    failures = [] of String
    files.each do |path|
      source = Facet::Compiler::Source.new(File.read(path), path)
      lexer = Facet::Compiler::Lexer.new(source)
      loop do
        token = lexer.next_token
        break if token.eof?
      end
      if lexer.diagnostics.any?
        first = lexer.diagnostics.first
        failures << "#{path}: #{first.message}"
      end
    end

    if failures.any?
      fail "lexer diagnostics (first #{[failures.size, 20].min}):\n#{failures.first(20).join("\n")}"
    end
  end
end

private def find_stdlib_root : String
  output = IO::Memory.new
  Process.run("crystal", ["env"], output: output)
  crystal_path_line = output.to_s.lines.find(&.starts_with?("CRYSTAL_PATH="))
  raise "CRYSTAL_PATH not found in `crystal env` output" unless crystal_path_line

  paths = crystal_path_line.split("=", 2)[1].split(':')
  root = paths.find do |path|
    File.exists?(File.join(path, "prelude.cr"))
  end
  raise "stdlib root not found in CRYSTAL_PATH" unless root

  root
end
