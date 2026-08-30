require "compiler/crystal/syntax"
require "../src/facet"

# Exhaustively compares every ASCII letter prefix and supported delimiter over
# representative percent-literal bodies. This guards against accepting lexer
# extensions (such as `%s` or `%I`) that Crystal::Parser rejects.
openings = {'(' => ')', '[' => ']', '{' => '}', '<' => '>', '|' => '|'}
prefixes = [""] + ('a'..'z').map(&.to_s) + ('A'..'Z').map(&.to_s)
bodies = ["foo", "a b", "\\n", %q(#{1})]
mismatches = [] of Tuple(String, Bool, Bool, String)

prefixes.each do |prefix|
  openings.each do |opening, closing|
    bodies.each do |body|
      code = "%#{prefix}#{opening}#{body}#{closing}"
      upstream = begin
        Crystal::Parser.parse(code)
        true
      rescue Crystal::SyntaxException
        false
      end
      parser = Facet::Compiler::Parser.new(Facet::Compiler::Source.new(code))
      parser.parse_file
      facet = parser.diagnostics.empty?
      if upstream != facet
        detail = parser.diagnostics.first?.try(&.message) || "accepted"
        mismatches << {code, upstream, facet, detail}
      end
    end
  end
end

puts "cases=#{prefixes.size * openings.size * bodies.size} mismatches=#{mismatches.size}"
mismatches.first(100).each { |code, upstream, facet, detail| puts "#{code.inspect} upstream=#{upstream} facet=#{facet} #{detail}" }
exit 1 unless mismatches.empty?
