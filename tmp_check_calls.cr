require "./src/facet/compiler"
include Facet::Compiler
ops = %w(bar + - * / < <= == > >= % | & ^ ** === =~ != []= !~)
ops.each do |name|
  strings = [
    "foo.#{name}",
    "foo.#{name} 1, 2",
    "foo.#{name}(1, 2)",
    "foo.#{name}(1, 2) { 3 }",
    "foo.#{name} do end",
  ]
  strings.each do |src|
    parser = Parser.new(Source.new(src, "tmp"))
    parser.parse_file
    if parser.diagnostics.any?
      diag = parser.diagnostics.first
      puts "#{name} src=#{src.inspect} -> #{diag.message} @ #{diag.span.start}"
      break
    end
  end
end

puts "\nproc pointers"
ops2 = %w(<< < <= == >> > >= + - * / // % | & ^ ** === =~ !~ &+ &- &* &**)
ops2.each do |op|
  [
    "1 #{op} 2",
    "n #{op} 2",
    "foo(n #{op} 2)",
    "foo(0, n #{op} 2)",
    "foo(a: n #{op} 2)",
    "foo(z: 0, a: n #{op} 2)",
    "def #{op}(); end",
    "foo = 1; ->foo.#{op}(Int32)",
    "->Foo.#{op}(Int32)",
  ].each do |src|
    parser = Parser.new(Source.new(src, "tmp"))
    parser.parse_file
    if parser.diagnostics.any?
      diag = parser.diagnostics.first
      puts "#{op} src=#{src.inspect} -> #{diag.message} @ #{diag.span.start}"
      break
    end
  end
end

ops3 = %w([] []=)
ops3.each do |op|
  ["foo = 1; ->foo.#{op}(Int32)", "->Foo.#{op}(Int32)"].each do |src|
    parser = Parser.new(Source.new(src, "tmp"))
    parser.parse_file
    if parser.diagnostics.any?
      diag = parser.diagnostics.first
      puts "#{op} src=#{src.inspect} -> #{diag.message} @ #{diag.span.start}"
      break
    end
  end
end
