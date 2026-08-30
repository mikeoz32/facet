require "base64"
require "set"
require "../src/facet"

# Verifies that Facet fully consumes every unique input exercised by the
# upstream Crystal lexer specs, without unknown tokens or non-trivia gaps.

alias F = Facet::Compiler

record Failure, index : Int32, source : String, detail : String

def trivia_only?(bytes : Bytes, start_pos : Int32, finish_pos : Int32) : Bool
  i = start_pos
  while i < finish_pos
    byte = bytes[i]
    if byte == ' '.ord || byte == '\t'.ord || byte == '\r'.ord || byte == '\n'.ord
      i += 1
    elsif byte == '\\'.ord && i + 1 < bytes.size && bytes[i + 1] == '{'.ord
      i += 2
    elsif byte == '\\'.ord && i + 1 < finish_pos && (bytes[i + 1] == '\n'.ord || bytes[i + 1] == '\r'.ord)
      i += bytes[i + 1] == '\r'.ord && i + 2 < finish_pos && bytes[i + 2] == '\n'.ord ? 3 : 2
    elsif byte == '#'.ord
      if i + 1 < finish_pos && bytes[i + 1] == '='.ord
        i += 2
        depth = 1
        while i < finish_pos && depth > 0
          if i + 1 < finish_pos && bytes[i] == '#'.ord && bytes[i + 1] == '='.ord
            depth += 1
            i += 2
          elsif i + 1 < finish_pos && bytes[i] == '='.ord && bytes[i + 1] == '#'.ord
            depth -= 1
            i += 2
          else
            i += 1
          end
        end
        return false unless depth == 0
      else
        i += 1
        while i < finish_pos && bytes[i] != '\n'.ord
          i += 1
        end
      end
    else
      return false
    end
  end
  true
end

trace_path = ARGV[0]? || abort "usage: crystal run scripts/check_upstream_lexer_coverage.cr -- LEXER_TRACE.b64"
inputs = File.read_lines(trace_path).map { |line| Base64.decode_string(line) }.uniq
expected_errors = if error_trace = ARGV[1]?
                    File.read_lines(error_trace).map { |line| Base64.decode_string(line) }.to_set
                  else
                    nil
                  end
# These inputs depend on mutable upstream lexer state or on how many tokens the
# test consumes. An input-only trace cannot reproduce that context:
# slash_is_regex is false for `/` and `/=`, while `<<-EOS\n` only scans the
# heredoc opener instead of asking the lexer to consume the missing body.
state_dependent_diagnostics = {"/", "/=", "<<-EOS\n"}.to_set
failures = [] of Failure
diagnostics = 0
diagnostic_inputs = [] of Failure
diagnostic_mismatches = 0
structural_failures = 0

inputs.each_with_index do |text, index|
  source = F::Source.new(text, "upstream-lexer-spec-#{index}")
  lexer = F::Lexer.new(source)
  begin
    tokens = lexer.tokenize_all
    unless lexer.diagnostics.empty?
      diagnostics += 1
      diagnostic_inputs << Failure.new(index, text, lexer.diagnostics.map(&.message).join("; "))
    end
    if expected = expected_errors
      expected_error = expected.includes?(text)
      actual_error = !lexer.diagnostics.empty?
      if !state_dependent_diagnostics.includes?(text) && expected_error != actual_error
        diagnostic_mismatches += 1
        detail = expected_error ? "expected a lexer diagnostic" : "unexpected lexer diagnostic"
        failures << Failure.new(index, text, detail)
      end
    end
    if tokens.empty? || !tokens.last.eof?
      structural_failures += 1
      failures << Failure.new(index, text, "missing EOF token")
    elsif tokens.last.span.start != source.size
      structural_failures += 1
      failures << Failure.new(index, text, "EOF at #{tokens.last.span.start}, source ends at #{source.size}")
    elsif token = tokens.find { |item| item.kind == F::TokenKind::Unknown }
      structural_failures += 1
      failures << Failure.new(index, text, "unknown token at #{token.span.start}")
    elsif token = tokens.find { |item| item.span.start < 0 || item.span.finish < item.span.start || item.span.finish > source.size }
      structural_failures += 1
      failures << Failure.new(index, text, "invalid token span #{token.span}")
    else
      previous_end = 0
      tokens.each do |item|
        if item.span.start > previous_end && !trivia_only?(source.bytes, previous_end, item.span.start)
          structural_failures += 1
          failures << Failure.new(index, text, "non-trivia bytes skipped at #{previous_end}..#{item.span.start}")
          break
        end
        previous_end = Math.max(previous_end, item.span.finish)
      end
    end
  rescue ex
    structural_failures += 1
    failures << Failure.new(index, text, "#{ex.class}: #{ex.message}")
  end
end

puts "inputs=#{inputs.size} fully_consumed=#{inputs.size - structural_failures} diagnostics=#{diagnostics} state_dependent_diagnostics=#{state_dependent_diagnostics.count { |text| inputs.includes?(text) }} diagnostic_mismatches=#{diagnostic_mismatches} failures=#{failures.size}"
failures.first(100).each do |failure|
  puts "\n##{failure.index}: #{failure.detail}"
  puts failure.source.dump
end
if ENV["SHOW_DIAGNOSTICS"]? == "1"
  diagnostic_inputs.each do |entry|
    puts "\nD##{entry.index}: #{entry.detail}"
    puts entry.source.dump
  end
end
exit(failures.empty? ? 0 : 1)
