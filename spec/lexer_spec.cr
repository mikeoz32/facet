require "./spec_helper"

describe Facet::Compiler::Lexer do
  it "lexes identifiers and keywords" do
    source = Facet::Compiler::Source.new("if foo else")
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::KeywordIf,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::KeywordElse,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "lexes all keywords" do
    keywords = Facet::Compiler::Keywords::TABLE.keys.sort
    source = Facet::Compiler::Source.new(keywords.join(" "))
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    expected = keywords.map { |kw| Facet::Compiler::Keywords::TABLE[kw] } + [Facet::Compiler::TokenKind::Eof]
    kinds.should eq(expected)
  end

  it "lexes numeric literals" do
    source = Facet::Compiler::Source.new("123 0x1f 0b1010 0o755 3.14 2e10 1_000 0xDEAD_BEEF 0x1.fp2 42_u64 1.0_f64 0b1_001 0o7_77")
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Number,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "skips line comments" do
    source = Facet::Compiler::Source.new("foo # comment\nbar")
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "skips block comments with nesting" do
    source = Facet::Compiler::Source.new("foo #= outer #= inner =# end =# bar")
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "lexes string literals with escapes" do
    source = Facet::Compiler::Source.new(%("a\\\"b" "c"))
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::String,
      Facet::Compiler::TokenKind::String,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "lexes char literals" do
    source = Facet::Compiler::Source.new(%('a' '\\n'))
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::Char,
      Facet::Compiler::TokenKind::Char,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "lexes common operators" do
    source = Facet::Compiler::Source.new("a==b c!=d e===f g<=>h i<<j k>>l m**n o=~p q!~r s&.t u=>v")
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::EqualEqual,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::BangEqual,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::TripleEqual,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Spaceship,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::ShiftLeft,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::ShiftRight,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::StarStar,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Match,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::NotMatch,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::SafeNav,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::HashRocket,
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "reports unterminated string literals" do
    source = Facet::Compiler::Source.new(%("unterminated))
    lexer = Facet::Compiler::Lexer.new(source)
    lexer.tokenize_all
    lexer.diagnostics.size.should eq(1)
    lexer.diagnostics.first.message.should eq("unterminated string literal")
  end

  it "reports unterminated char literals" do
    source = Facet::Compiler::Source.new(%('x))
    lexer = Facet::Compiler::Lexer.new(source)
    lexer.tokenize_all
    lexer.diagnostics.size.should eq(1)
    lexer.diagnostics.first.message.should eq("unterminated char literal")
  end

  it "reports unterminated block comments" do
    source = Facet::Compiler::Source.new("foo #= unterminated")
    lexer = Facet::Compiler::Lexer.new(source)
    lexer.tokenize_all
    lexer.diagnostics.size.should eq(1)
    lexer.diagnostics.first.message.should eq("unterminated block comment")
  end

  it "reports invalid numeric literals" do
    source = Facet::Compiler::Source.new("1__2 1e 0x 0x1.fp 0b 0o")
    lexer = Facet::Compiler::Lexer.new(source)
    lexer.tokenize_all
    messages = lexer.diagnostics.map(&.message)
    messages.should contain("invalid underscore placement in numeric literal")
    messages.should contain("invalid exponent in numeric literal")
    messages.should contain("invalid hex literal")
    messages.should contain("invalid binary literal")
    messages.should contain("invalid octal literal")
  end

  it "reports invalid escape sequences" do
    source = Facet::Compiler::Source.new(%("bad\\q" "bad\\x1" '\\q'))
    lexer = Facet::Compiler::Lexer.new(source)
    lexer.tokenize_all
    messages = lexer.diagnostics.map(&.message)
    messages.should contain("invalid escape sequence")
    messages.should contain("invalid hex escape sequence")
  end

  it "lexes percent literals" do
    source = Facet::Compiler::Source.new(%(%(a "b") %q{c "d"} %Q(e #{1}) %w(foo bar) %r{a+}))
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::String,
      Facet::Compiler::TokenKind::String,
      Facet::Compiler::TokenKind::String,
      Facet::Compiler::TokenKind::String,
      Facet::Compiler::TokenKind::Regex,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "handles interpolation with nested strings" do
    source = Facet::Compiler::Source.new(%("a #{"b"} c"))
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::String,
      Facet::Compiler::TokenKind::Eof,
    ])
    lexer.diagnostics.should be_empty
  end

  it "lexes regex literals" do
    source = Facet::Compiler::Source.new(%(r = /a\\d+/i))
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Assign,
      Facet::Compiler::TokenKind::Regex,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "lexes heredoc literals" do
    source = Facet::Compiler::Source.new("msg = <<-MSG\nhello 'world'\nMSG\n")
    lexer = Facet::Compiler::Lexer.new(source)
    kinds = lexer.tokenize_all.map(&.kind)
    kinds.should eq([
      Facet::Compiler::TokenKind::Identifier,
      Facet::Compiler::TokenKind::Assign,
      Facet::Compiler::TokenKind::String,
      Facet::Compiler::TokenKind::Eof,
    ])
  end

  it "reports unterminated escape sequences" do
    source = Facet::Compiler::Source.new(%("bad\\))
    lexer = Facet::Compiler::Lexer.new(source)
    lexer.tokenize_all
    messages = lexer.diagnostics.map(&.message)
    messages.should contain("unterminated escape sequence")
  end

  it "tracks line and column for tokens" do
    source = Facet::Compiler::Source.new("a\n  b\nc")
    lexer = Facet::Compiler::Lexer.new(source)
    tokens = lexer.tokenize_all
    lexer.token_location(tokens[0]).should eq({1, 1})
    lexer.token_location(tokens[1]).should eq({2, 3})
    lexer.token_location(tokens[2]).should eq({3, 1})
    lexer.text(tokens[1]).should eq("b")
  end
end
