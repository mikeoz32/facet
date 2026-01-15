module Facet
  module Compiler
    module Operators
      MULTI_3 = {
        "..." => TokenKind::DotDotDot,
        "===" => TokenKind::TripleEqual,
        "<=>" => TokenKind::Spaceship,
        "//=" => TokenKind::SlashSlashEqual,
      } of String => TokenKind

      MULTI_2 = {
        "==" => TokenKind::EqualEqual,
        "!=" => TokenKind::BangEqual,
        "<=" => TokenKind::LessEqual,
        ">=" => TokenKind::GreaterEqual,
        "&&" => TokenKind::AndAnd,
        "||" => TokenKind::OrOr,
        "->" => TokenKind::Arrow,
        "=>" => TokenKind::HashRocket,
        "**" => TokenKind::StarStar,
        "<<" => TokenKind::ShiftLeft,
        ">>" => TokenKind::ShiftRight,
        "//" => TokenKind::SlashSlash,
        "=~" => TokenKind::Match,
        "!~" => TokenKind::NotMatch,
        "&." => TokenKind::SafeNav,
        "::" => TokenKind::DoubleColon,
        ".." => TokenKind::DotDot,
        "+=" => TokenKind::PlusEqual,
        "-=" => TokenKind::MinusEqual,
        "*=" => TokenKind::StarEqual,
        "/=" => TokenKind::SlashEqual,
        "%=" => TokenKind::PercentEqual,
      } of String => TokenKind

      SINGLE = {
        '+'.ord.to_u8 => TokenKind::Plus,
        '-'.ord.to_u8 => TokenKind::Minus,
        '*'.ord.to_u8 => TokenKind::Star,
        '/'.ord.to_u8 => TokenKind::Slash,
        '%'.ord.to_u8 => TokenKind::Percent,
        '^'.ord.to_u8 => TokenKind::Caret,
        '&'.ord.to_u8 => TokenKind::Ampersand,
        '|'.ord.to_u8 => TokenKind::Pipe,
        '!'.ord.to_u8 => TokenKind::Bang,
        '~'.ord.to_u8 => TokenKind::Tilde,
        '='.ord.to_u8 => TokenKind::Assign,
        '<'.ord.to_u8 => TokenKind::Less,
        '>'.ord.to_u8 => TokenKind::Greater,
        '('.ord.to_u8 => TokenKind::LParen,
        ')'.ord.to_u8 => TokenKind::RParen,
        '{'.ord.to_u8 => TokenKind::LBrace,
        '}'.ord.to_u8 => TokenKind::RBrace,
        '['.ord.to_u8 => TokenKind::LBracket,
        ']'.ord.to_u8 => TokenKind::RBracket,
        ','.ord.to_u8 => TokenKind::Comma,
        '.'.ord.to_u8 => TokenKind::Dot,
        ':'.ord.to_u8 => TokenKind::Colon,
        ';'.ord.to_u8 => TokenKind::Semicolon,
        '?'.ord.to_u8 => TokenKind::Question,
      } of UInt8 => TokenKind

      def self.match(bytes : Bytes, index : Int32) : Tuple(TokenKind, Int32)?
        remaining = bytes.size - index

        if remaining >= 3
          if kind = MULTI_3[String.new(bytes[index, 3])]?
            return {kind, 3}
          end
        end

        if remaining >= 2
          if kind = MULTI_2[String.new(bytes[index, 2])]?
            return {kind, 2}
          end
        end

        if kind = SINGLE[bytes[index]]?
          return {kind, 1}
        end

        nil
      end
    end
  end
end
