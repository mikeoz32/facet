require "./src/facet/compiler"
include Facet::Compiler
ops = %w(+ - * / // % | & ^ ** << >> &+ &- &*)
ops.each do |op|
  ["a = 1; a #{op}= 1", "a = 1; a #{op}=\n1", "a.b #{op}=\n1"].each do |src|
    parser = Parser.new(Source.new(src, "tmp"))
    parser.parse_file
    if parser.diagnostics.any?
      diag = parser.diagnostics.first
      puts "#{op} src=#{src.inspect} -> #{diag.message} @ #{diag.span.start}"
      break
    end
  end
end
["a = 1; a &&= 1", "a = 1; a ||= 1", "a = 1; a[2] &&= 3", "a = 1; a[2] ||= 3"].each do |src|
  parser = Parser.new(Source.new(src, "tmp"))
  parser.parse_file
  if parser.diagnostics.any?
    diag = parser.diagnostics.first
    puts "extra src=#{src.inspect} -> #{diag.message} @ #{diag.span.start}"
  end
end
