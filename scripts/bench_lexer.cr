require "../src/facet"
require "compiler/crystal/syntax/ast"
require "compiler/crystal/syntax/virtual_file"
require "compiler/crystal/syntax/exception"
require "compiler/crystal/syntax/lexer"
require "compiler/crystal/syntax/parser"

class BenchLexer < Crystal::Lexer
  def wants_regex=(value : Bool)
    @wants_regex = value
  end
end

private def regex_allowed?(last_type : Crystal::Token::Kind?, last_value : Crystal::Keyword | String | Nil) : Bool
  return true unless last_type

  if last_type.ident?
    case last_value
    when String
      return last_value.ends_with?('?') || last_value.ends_with?('!')
    when Crystal::Keyword
      case last_value
      when Crystal::Keyword::DEF,
           Crystal::Keyword::MACRO,
           Crystal::Keyword::ALIAS,
           Crystal::Keyword::SELF,
           Crystal::Keyword::NIL,
           Crystal::Keyword::TRUE,
           Crystal::Keyword::FALSE,
           Crystal::Keyword::SUPER
        return false
      else
        return true
      end
    else
      return false
    end
  end

  return false if last_type.const? || last_type.instance_var? || last_type.class_var?
  return false if last_type.number? || last_type.string? || last_type.char? || last_type.symbol?
  return false if last_type.op_rparen? || last_type.op_rsquare? || last_type.op_rcurly?
  return false if last_type.delimiter_end? || last_type.string_array_end?

  true
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

private def bench(name : String, iterations : Int32, disable_gc : Bool, &block : -> Tuple(Int64, Int64))
  GC.collect
  GC.disable if disable_gc
  start = Time.monotonic
  total_tokens = 0_i64
  total_bytes = 0_i64
  begin
    iterations.times do
      tokens, bytes = yield
      total_tokens += tokens
      total_bytes += bytes
    end
  ensure
    GC.enable if disable_gc
  end
  elapsed = Time.monotonic - start

  seconds = elapsed.total_seconds
  mib = total_bytes.to_f / 1024.0 / 1024.0
  throughput = seconds > 0 ? (mib / seconds) : 0.0
  per_iter = seconds / iterations
  token_info = total_tokens > 0 ? "tokens=#{total_tokens}" : "tokens=n/a"
  puts "#{name}: #{seconds.round(3)}s total (#{per_iter.round(3)}s/iter), #{throughput.round(2)} MiB/s, #{token_info}"
end

private def run_facet(files : Array(String)) : Tuple(Int64, Int64)
  tokens = 0_i64
  bytes = 0_i64
  files.each do |path|
    text = File.read(path)
    bytes += text.bytesize
    lexer = Facet::Compiler::Lexer.new(Facet::Compiler::Source.new(text, path))
    loop do
      token = lexer.next_token
      tokens += 1
      break if token.eof?
    end
    if lexer.diagnostics.any?
      first = lexer.diagnostics.first
      loc = lexer.line_and_column(first.span.start)
      raise "#{path}:#{loc[0]}:#{loc[1]} #{first.message}"
    end
  end
  {tokens, bytes}
end

private def run_crystal(files : Array(String), mode : String) : Tuple(Int64, Int64)
  tokens = 0_i64
  bytes = 0_i64
  files.each do |path|
    text = File.read(path)
    bytes += text.bytesize
    begin
      case mode
      when "plain"
        lexer = Crystal::Lexer.new(text)
        lexer.comments_enabled = false
        lexer.doc_enabled = false
        lexer.count_whitespace = false
        lexer.filename = path
        loop do
          lexer.next_token
          tokens += 1
          break if lexer.token.type.eof?
        end
      when "highlight"
        lexer = Crystal::Lexer.new(text)
        lexer.comments_enabled = true
        lexer.doc_enabled = false
        lexer.count_whitespace = true
        lexer.wants_raw = true
        lexer.filename = path
        loop do
          lexer.next_token
          tokens += 1
          break if lexer.token.type.eof?
        end
      when "heuristic"
        lexer = BenchLexer.new(text)
        lexer.comments_enabled = false
        lexer.doc_enabled = false
        lexer.count_whitespace = false
        lexer.wants_regex = true
        lexer.slash_is_regex = true
        lexer.filename = path
        last_type = nil
        last_value = nil
        loop do
          wants_regex = regex_allowed?(last_type, last_value)
          lexer.wants_regex = wants_regex
          lexer.slash_is_regex = wants_regex
          lexer.next_token
          tokens += 1
          break if lexer.token.type.eof?
          unless lexer.token.type.space? || lexer.token.type.newline?
            last_type = lexer.token.type
            value = lexer.token.value
            if value.is_a?(String) || value.is_a?(Crystal::Keyword)
              last_value = value
            else
              last_value = nil
            end
          end
        end
      when "parser"
        parser = Crystal::Parser.new(text)
        parser.filename = path
        parser.parse
      else
        raise "unknown CRYSTAL_MODE=#{mode} (expected plain|highlight|heuristic|parser)"
      end
    rescue e : Crystal::SyntaxException
      raise "#{path}:#{e.line_number}:#{e.column_number} #{e.message}"
    end
  end
  {tokens, bytes}
end

stdlib_root = find_stdlib_root
files = Dir.glob("#{stdlib_root}/**/*.cr").sort
iterations = (ENV["ITER"]? || "1").to_i
disable_gc = ENV["DISABLE_GC"]? == "1"
crystal_mode = ENV["CRYSTAL_MODE"]? || "parser"

puts "stdlib root: #{stdlib_root}"
puts "files: #{files.size}, iterations: #{iterations}"
puts "gc: #{disable_gc ? "disabled" : "enabled"}"
puts "crystal lexer mode: #{crystal_mode}"

bench("Facet::Lexer", iterations, disable_gc) { run_facet(files) }
bench("Crystal::Lexer", iterations, disable_gc) { run_crystal(files, crystal_mode) }
