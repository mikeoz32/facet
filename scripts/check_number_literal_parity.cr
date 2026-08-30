require "compiler/crystal/syntax"
require "../src/facet"

alias F = Facet::Compiler

# Generated acceptance matrix for numeric bases, separators, decimal/exponent
# forms, valid width suffixes, and plausible invalid suffixes. This complements
# the captured upstream parser suite with cross-product cases.
cores = [
  "0", "1", "12", "1_000", "1__0", "1_", "_1",
  "0b0", "0b1", "0b1010", "0b2", "0b", "0b_1",
  "0o0", "0o7", "0o755", "0o8", "0o", "0o_1",
  "0x0", "0xA", "0xdead_beef", "0xG", "0x", "0x_1",
  "0x1.0p0", "0x1p4", "0x1.0", "0x1p", "0x1p+", "0x1p-2",
  "1.0", "1.", ".1", "1.2.3", "1_0.2_5", "1._0",
  "1e0", "1e+2", "1e-2", "1e", "1e+", "1_e2", "1e2_0",
]
suffixes = [
  "", "_i8", "_i16", "_i32", "_i64", "_i128",
  "_u8", "_u16", "_u32", "_u64", "_u128", "_f32", "_f64",
  "i8", "I32", "int", "u256", "f16", "_I32", "_int", "_u256", "_f16",
]

mismatches = [] of Tuple(String, Bool, Bool, String)
cores.each do |core|
  suffixes.each do |suffix|
    code = core + suffix
    upstream = begin
      Crystal::Parser.parse(code)
      true
    rescue Crystal::SyntaxException
      false
    end
    parser = F::Parser.new(F::Source.new(code))
    parser.parse_file
    facet = parser.diagnostics.empty?
    if upstream != facet
      detail = parser.diagnostics.first?.try(&.message) || "accepted"
      mismatches << {code, upstream, facet, detail}
    end
  end
end

puts "cases=#{cores.size * suffixes.size} mismatches=#{mismatches.size}"
mismatches.first(120).each { |code, upstream, facet, detail| puts "#{code.inspect} upstream=#{upstream} facet=#{facet} #{detail}" }
exit 1 unless mismatches.empty?
