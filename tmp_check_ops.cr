require "./src/facet/compiler"
include Facet::Compiler
ops = %w(/ < <= == != =~ !~ > >= + - * / ~ % & | ^ ** ===)
ops.each do |op|
  ["def #{op}; end;", "def #{op}(); end;", "def self.#{op}; end;", "def self.#{op}(); end;"].each do |src|
    parser = Parser.new(Source.new(src, "tmp"))
    parser.parse_file
    if parser.diagnostics.any?
      puts "#{op.inspect} src=#{src.inspect}: #{parser.diagnostics.first.message} @ #{parser.diagnostics.first.span.start}"
    end
  end
end
