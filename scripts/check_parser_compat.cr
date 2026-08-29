require "../src/facet"

module ParserCompat
  extend self

  CHILD_FLAG = "--parse-file"

  record DiagnosticFailure,
    path : String,
    message : String,
    line : Int32,
    column : Int32,
    source_line : String
  record CrashFailure, path : String, exit_code : Int32

  def run(args : Array(String)) : Int32
    if args.first? == CHILD_FLAG
      return parse_one(args[1]? || abort "missing file path")
    end

    roots = args.empty? ? [stdlib_root] : args
    files = roots.flat_map { |root| source_files(root) }.uniq.sort
    abort "no Crystal files found" if files.empty?

    executable = Process.executable_path || abort "cannot resolve scanner executable"
    diagnostics = [] of DiagnosticFailure
    crashes = [] of CrashFailure
    clean = 0

    files.each do |path|
      output = IO::Memory.new
      status = Process.run(
        executable,
        [CHILD_FLAG, path],
        output: output,
        error: Process::Redirect::Close
      )

      case status.exit_code
      when 0
        clean += 1
      when 2
        message, line, column, source_line = output.to_s.chomp.split('\t', 4)
        diagnostics << DiagnosticFailure.new(path, message, line.to_i, column.to_i, source_line)
      else
        crashes << CrashFailure.new(path, status.exit_code)
      end
    end

    print_summary(files, clean, diagnostics, crashes)
    diagnostics.empty? && crashes.empty? ? 0 : 1
  end

  private def parse_one(path : String) : Int32
    source = Facet::Compiler::Source.new(File.read(path), path)
    parser = Facet::Compiler::Parser.new(source)
    parser.parse_file
    if diagnostic = parser.diagnostics.first?
      line, column = line_and_column(source, diagnostic.span.start)
      source_line = source.text.lines[line - 1]?.try(&.chomp) || ""
      puts [diagnostic.message, line, column, source_line].join('\t')
      return 2
    end
    0
  rescue ex
    puts "#{ex.class}: #{ex.message}"
    3
  end

  private def line_and_column(source : Facet::Compiler::Source, offset : Int32) : {Int32, Int32}
    line = 1
    column = 1
    limit = Math.min(offset, source.bytes.size)
    limit.times do |index|
      if source.bytes[index] == '\n'.ord
        line += 1
        column = 1
      else
        column += 1
      end
    end
    {line, column}
  end

  private def source_files(path : String) : Array(String)
    if File.file?(path)
      path.ends_with?(".cr") ? [File.expand_path(path)] : [] of String
    else
      Dir.glob(File.join(File.expand_path(path), "**", "*.cr"))
    end
  end

  private def stdlib_root : String
    output = IO::Memory.new
    status = Process.run("crystal", ["env"], output: output)
    abort "crystal env failed" unless status.success?

    crystal_path = output.to_s.lines
      .find(&.starts_with?("CRYSTAL_PATH="))
      .try(&.split("=", 2)[1]) || abort "CRYSTAL_PATH not found"

    crystal_path.split(':').find do |path|
      File.exists?(File.join(path, "prelude.cr"))
    end || abort "Crystal stdlib not found"
  end

  private def print_summary(
    files : Array(String),
    clean : Int32,
    diagnostics : Array(DiagnosticFailure),
    crashes : Array(CrashFailure)
  )
    clean_percent = clean * 100.0 / files.size
    puts "files=#{files.size} clean=#{clean} diagnostics=#{diagnostics.size} " \
         "crashes=#{crashes.size} clean_percent=#{clean_percent.round(2)}"

    unless crashes.empty?
      puts "\nCrashes:"
      crashes.each { |failure| puts "  exit=#{failure.exit_code} #{failure.path}" }
    end

    groups = diagnostics.group_by(&.message).to_a.sort_by { |message, failures| {-failures.size, message} }
    unless groups.empty?
      puts "\nDiagnostic groups:"
      group_limit = (ENV["GROUPS"]? || "30").to_i
      sample_limit = (ENV["SAMPLES"]? || "3").to_i
      groups.first(group_limit).each do |message, failures|
        puts "  #{failures.size.to_s.rjust(4)}  #{message}"
        failures.first(sample_limit).each do |failure|
          puts "        #{failure.path}:#{failure.line}:#{failure.column}"
          puts "          #{failure.source_line.strip}"
        end
      end
    end
  end
end

exit ParserCompat.run(ARGV)
