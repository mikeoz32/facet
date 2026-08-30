module Facet
  module Compiler
    class Lexer
      SPACE      = 0x20_u8
      TAB        = 0x09_u8
      CR         = 0x0d_u8
      LF         = 0x0a_u8
      BANG       = 0x21_u8
      HASH       = 0x23_u8
      DOLLAR     = 0x24_u8
      EQUAL      = 0x3d_u8
      PERCENT    = 0x25_u8
      DQUOTE     = 0x22_u8
      AT         = 0x40_u8
      COLON      = 0x3a_u8
      QUESTION   = 0x3f_u8
      SQUOTE     = 0x27_u8
      BACKSLASH  = 0x5c_u8
      UNDERSCORE = 0x5f_u8
      ZERO       = 0x30_u8
      ONE        = 0x31_u8
      SEVEN      = 0x37_u8
      NINE       = 0x39_u8
      SLASH      = 0x2f_u8
      UPPER_A    = 0x41_u8
      UPPER_B    = 0x42_u8
      UPPER_F    = 0x46_u8
      UPPER_O    = 0x4f_u8
      UPPER_Z    = 0x5a_u8
      UPPER_E    = 0x45_u8
      UPPER_P    = 0x50_u8
      UPPER_X    = 0x58_u8
      LOWER_A    = 0x61_u8
      LOWER_B    = 0x62_u8
      LOWER_E    = 0x65_u8
      LOWER_F    = 0x66_u8
      LOWER_N    = 0x6e_u8
      LOWER_O    = 0x6f_u8
      LOWER_P    = 0x70_u8
      LOWER_R    = 0x72_u8
      LOWER_T    = 0x74_u8
      LOWER_U    = 0x75_u8
      LOWER_V    = 0x76_u8
      LOWER_X    = 0x78_u8
      LOWER_Z    = 0x7a_u8
      LBRACKET   = 0x5b_u8
      RBRACKET   = 0x5d_u8
      LPAREN     = 0x28_u8
      RPAREN     = 0x29_u8
      LT         = 0x3c_u8
      GT         = 0x3e_u8
      LBRACE     = 0x7b_u8
      RBRACE     = 0x7d_u8
      DOT        = 0x2e_u8
      PLUS       = 0x2b_u8
      STAR       = 0x2a_u8
      MINUS      = 0x2d_u8
      TILDE      = 0x7e_u8
      BACKTICK   = 0x60_u8
      AMPERSAND  = 0x26_u8
      PIPE       = 0x7c_u8

      getter source : Source
      getter line_starts : Array(Int32)
      getter diagnostics : Array(Diagnostic)
      property parser_mode : Bool

      def initialize(@source : Source)
        @bytes = @source.bytes
        @i = 0
        @line_starts = [0] of Int32
        @diagnostics = [] of Diagnostic
        @last_token_kind = TokenKind::Eof
        @last_token_span = nil
        @previous_token_kind = TokenKind::Eof
        @parser_mode = false
        @multi_heredoc_header_finishes = {} of Int32 => Int32
        @multi_heredoc_full_spans = {} of Int32 => Span
        @multi_heredoc_body_bounds = {} of Int32 => Tuple(Int32, Int32)
        @multi_heredoc_line_skips = {} of Int32 => Int32
        @active_multi_heredoc_line_end = nil.as(Int32?)
        scan_multi_heredoc_layouts
      end

      def heredoc_full_span(header_start : Int32) : Span?
        @multi_heredoc_full_spans[header_start]?
      end

      def heredoc_body_bounds(header_start : Int32) : Tuple(Int32, Int32)?
        @multi_heredoc_body_bounds[header_start]?
      end

      def next_token : Token
        skip_trivia
        n = @bytes.size
        return record_token(eof_token) if @i >= n

        byte = @bytes[@i]
        if byte == BACKSLASH && @i + 1 < n && @bytes[@i + 1] == LBRACE
          @i += 1
          return next_token
        end
        if byte == AT
          if token = scan_annotation_or_variable
            return record_token(token)
          end
        end

        if byte == COLON
          if token = scan_symbol_literal
            return record_token(token)
          end
        end

        if byte == DOLLAR
          if token = scan_global_variable
            return record_token(token)
          end
        end

        if ident_start?(byte)
          return record_token(scan_identifier)
        end

        if ascii_digit?(byte)
          return record_token(scan_number)
        end

        if byte == DQUOTE
          return record_token(scan_string)
        end

        if byte == BACKTICK
          if backtick_allowed?
            return record_token(scan_backtick)
          end
        end

        if byte == SQUOTE
          return record_token(scan_char)
        end

        if byte == PERCENT
          unless @last_token_kind == TokenKind::KeywordDef ||
                 @last_token_kind == TokenKind::KeywordMacro ||
                 @last_token_kind == TokenKind::Dot ||
                 @last_token_kind == TokenKind::SafeNav ||
                 @last_token_kind == TokenKind::DoubleColon
            if token = scan_percent_literal
              return record_token(token)
            end
          end
        end

        if byte == LT
          if token = scan_heredoc
            return record_token(token)
          end
        end

        if byte == SLASH && regex_allowed?
          return record_token(scan_regex)
        end

        if byte == DOT && @i + 1 < n && ascii_digit?(@bytes[@i + 1])
          @diagnostics << Diagnostic.new(Span.new(@i, @i + 1), ".1 style number literal is not supported; put 0 before the dot")
        end

        if byte == AMPERSAND && @i + 2 < n && @bytes[@i + 1] == MINUS && @bytes[@i + 2] == GT
          start = @i
          @i += 1
          return record_token(Token.new(TokenKind::Ampersand, Span.new(start, @i)))
        end

        if token = scan_operator_or_punct
          return record_token(token)
        end

        start = @i
        @i += 1
        record_token(Token.new(TokenKind::Unknown, Span.new(start, @i)))
      end

      def tokenize_all : Array(Token)
        tokens = [] of Token
        loop do
          token = next_token
          tokens << token
          break if token.eof?
        end
        tokens
      end

      private def eof_token
        Token.new(TokenKind::Eof, Span.new(@i, @i))
      end

      private def record_token(token : Token) : Token
        @previous_token_kind = @last_token_kind
        @last_token_kind = token.kind
        @last_token_span = token.span
        token
      end

      private def skip_trivia
        n = @bytes.size
        while @i < n
          case @bytes[@i]
          when BACKSLASH
            if @i + 1 < n
              j = @i + 1
              while j < n && (@bytes[j] == SPACE || @bytes[j] == TAB)
                j += 1
              end
              if j < n && @bytes[j] == LF
                @i = j + 1
                @line_starts << @i
                next
              elsif j < n && @bytes[j] == CR
                if j + 1 < n && @bytes[j + 1] == LF
                  @i = j + 2
                else
                  @i = j + 1
                end
                @line_starts << @i
                next
              elsif j > @i + 1
                @i = j
                next
              end
            end
            break
          when SPACE, TAB
            @i += 1
          when CR
            if @i + 1 < n && @bytes[@i + 1] == LF && skip_active_multi_heredocs_at?(@i + 1)
              next
            end
            start = @i
            @i += 1
            if @i < n && @bytes[@i] == LF
              @i += 1
              @line_starts << @i
            else
              @diagnostics << Diagnostic.new(Span.new(start, @i), "unexpected carriage return")
            end
          when LF
            if skip_active_multi_heredocs_at?(@i)
              next
            end
            @i += 1
            @line_starts << @i
          when HASH
            if @i + 1 < n && @bytes[@i + 1] == EQUAL
              skip_block_comment
            else
              skip_line_comment
            end
          else
            break
          end
        end
      end

      private def skip_active_multi_heredocs_at?(newline_position : Int32) : Bool
        return false unless @active_multi_heredoc_line_end == newline_position
        return false unless target = @multi_heredoc_line_skips[newline_position]?
        position = newline_position
        while position < target
          @line_starts << position + 1 if @bytes[position] == LF
          position += 1
        end
        @i = target
        @active_multi_heredoc_line_end = nil
        true
      end

      private def skip_line_comment
        n = @bytes.size
        while @i < n
          byte = @bytes[@i]
          @i += 1
          if byte == LF
            @line_starts << @i
            break
          end
        end
      end

      private def skip_block_comment
        n = @bytes.size
        depth = 1
        @i += 2
        while @i < n
          byte = @bytes[@i]
          if byte == HASH && @i + 1 < n && @bytes[@i + 1] == EQUAL
            depth += 1
            @i += 2
          elsif byte == EQUAL && @i + 1 < n && @bytes[@i + 1] == HASH
            depth -= 1
            @i += 2
            break if depth == 0
          else
            @i += 1
            @line_starts << @i if byte == LF
          end
        end

        if depth > 0
          @diagnostics << Diagnostic.new(Span.new(@i, @i), "unterminated block comment")
        end
      end

      private def scan_identifier : Token
        start = @i
        n = @bytes.size
        @i += 1
        while @i < n && ident_continue?(@bytes[@i])
          @i += 1
        end
        if @i < n
          byte = @bytes[@i]
          if byte == QUESTION
            if @i + 1 >= n || @bytes[@i + 1] != EQUAL
              @i += 1
            end
          elsif byte == BANG
            if @i + 1 >= n || (@bytes[@i + 1] != EQUAL && @bytes[@i + 1] != TILDE)
              @i += 1
            end
          end
        end

        kind = Keywords.lookup(@bytes, start, @i - start) || TokenKind::Identifier
        Token.new(kind, Span.new(start, @i))
      end

      private def scan_annotation_or_variable : Token?
        n = @bytes.size
        start = @i
        if @i + 1 < n && @bytes[@i + 1] == LBRACKET
          @i += 1
          return Token.new(TokenKind::Annotation, Span.new(start, @i))
        end
        if @i + 2 < n && @bytes[@i + 1] == BACKSLASH && @bytes[@i + 2] == LBRACE
          @i += 2
          return Token.new(TokenKind::InstanceVar, Span.new(start, @i))
        end
        at_count = 0
        while @i < n && @bytes[@i] == AT && at_count < 2
          @i += 1
          at_count += 1
        end
        return nil if at_count == 0 || @i >= n || !ident_start?(@bytes[@i])

        @i += 1
        while @i < n && ident_continue?(@bytes[@i])
          @i += 1
        end
        if @i < n
          byte = @bytes[@i]
          if byte == QUESTION
            if @i + 1 >= n || @bytes[@i + 1] != EQUAL
              @i += 1
            end
          elsif byte == BANG
            if @i + 1 >= n || (@bytes[@i + 1] != EQUAL && @bytes[@i + 1] != TILDE)
              @i += 1
            end
          end
        end

        kind = at_count == 1 ? TokenKind::InstanceVar : TokenKind::ClassVar
        Token.new(kind, Span.new(start, @i))
      end

      private def scan_global_variable : Token?
        n = @bytes.size
        start = @i
        @i += 1
        return nil if @i >= n || !(ident_start?(@bytes[@i]) || ascii_digit?(@bytes[@i]) || @bytes[@i] == TILDE || @bytes[@i] == QUESTION)
        if @bytes[@i] == ZERO && @i + 1 < n && (ascii_digit?(@bytes[@i + 1]) || @bytes[@i + 1] == QUESTION)
          @diagnostics << Diagnostic.new(Span.new(start, @i + 2), "invalid global match-data index")
        end
        @i += 1
        while @i < n && ident_continue?(@bytes[@i])
          @i += 1
        end
        if @i < n
          byte = @bytes[@i]
          if byte == QUESTION
            if @i + 1 >= n || @bytes[@i + 1] != EQUAL
              @i += 1
            end
          elsif byte == BANG
            if @i + 1 >= n || (@bytes[@i + 1] != EQUAL && @bytes[@i + 1] != TILDE)
              @i += 1
            end
          end
        end
        Token.new(TokenKind::GlobalVar, Span.new(start, @i))
      end

      private def scan_symbol_literal : Token?
        n = @bytes.size
        start = @i
        return nil if @i + 1 >= n
        return nil if @parser_mode && start > 0 && ident_continue?(@bytes[start - 1])
        next_byte = @bytes[@i + 1]
        return nil if next_byte == COLON

        # Quoted symbol, e.g. :"foo" or :"\\u{61}"
        if next_byte == DQUOTE
          @i += 2
          terminated = false
          while @i < n
            byte = @bytes[@i]
            @i += 1
            if byte == BACKSLASH
              consume_escape(@i - 1, allow_unknown: true, allow_octal: true)
            elsif byte == DQUOTE
              terminated = true
              break
            elsif byte == LF
              @line_starts << @i
            end
          end
          unless terminated
            @diagnostics << Diagnostic.new(Span.new(start + 1, start + 1), "unterminated quoted symbol")
          end
          return Token.new(TokenKind::Symbol, Span.new(start, @i))
        end

        # Special bracketed symbols like :[], :[]?, :[]=
        if next_byte == LBRACKET
          @i += 2
          if @i < n && @bytes[@i] == RBRACKET
            @i += 1
            if @i < n && (@bytes[@i] == QUESTION || @bytes[@i] == EQUAL)
              @i += 1
            end
          end
          return Token.new(TokenKind::Symbol, Span.new(start, @i))
        end

        # Operator symbols (:+, :**, etc.)
        if match = Operators.match(@bytes, @i + 1)
          kind, length = match
          if kind >= TokenKind::Plus && kind <= TokenKind::AmpersandStarStar
            @i += 1 + length
            if @i < n && ascii_digit?(@bytes[@i])
              @diagnostics << Diagnostic.new(Span.new(start, @i + 1), "operator symbol cannot be followed by a number")
            end
            return Token.new(TokenKind::Symbol, Span.new(start, @i))
          end
        end

        return nil unless ident_start?(next_byte)
        @i += 2
        while @i < n && ident_continue?(@bytes[@i])
          @i += 1
        end
        if @i < n
          byte = @bytes[@i]
          if byte == QUESTION
            if @i + 1 >= n || @bytes[@i + 1] != EQUAL
              @i += 1
            end
          elsif byte == BANG
            if @i + 1 >= n || (@bytes[@i + 1] != EQUAL && @bytes[@i + 1] != TILDE)
              @i += 1
            end
          elsif byte == EQUAL
            @i += 1
          end
        end
        Token.new(TokenKind::Symbol, Span.new(start, @i))
      end

      private def scan_number : Token
        start = @i
        n = @bytes.size
        underscore_invalid = false
        trailing_underscore = false

        if @bytes[@i] == ZERO && @i + 1 < n
          next_byte = @bytes[@i + 1]
          if next_byte == LOWER_X || next_byte == UPPER_X
            floating = false
            @i += 2
            hex_count, hex_ok, hex_trailing = scan_hex_digits
            underscore_invalid ||= !hex_ok
            trailing_underscore = hex_trailing
            if hex_count == 0
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid hex literal")
            end
            suffix_consumed = consume_numeric_suffix(trailing_underscore)
            underscore_invalid ||= trailing_underscore && !suffix_consumed
            if underscore_invalid
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid underscore placement in numeric literal")
            end
            validate_numeric_literal(start, @i, 16, floating, suffix_consumed)
            return Token.new(TokenKind::Number, Span.new(start, @i))
          elsif next_byte == LOWER_B || next_byte == UPPER_B
            @i += 2
            bin_count, bin_ok, bin_trailing = scan_bin_digits
            underscore_invalid ||= !bin_ok
            trailing_underscore = bin_trailing
            if bin_count == 0
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid binary literal")
            end
            suffix_consumed = consume_numeric_suffix(trailing_underscore)
            underscore_invalid ||= trailing_underscore && !suffix_consumed
            if underscore_invalid
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid underscore placement in numeric literal")
            end
            validate_numeric_literal(start, @i, 2, false, suffix_consumed)
            return Token.new(TokenKind::Number, Span.new(start, @i))
          elsif next_byte == LOWER_O || next_byte == UPPER_O
            @i += 2
            oct_count, oct_ok, oct_trailing = scan_oct_digits
            underscore_invalid ||= !oct_ok
            trailing_underscore = oct_trailing
            if oct_count == 0
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid octal literal")
            end
            suffix_consumed = consume_numeric_suffix(trailing_underscore)
            underscore_invalid ||= trailing_underscore && !suffix_consumed
            if underscore_invalid
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid underscore placement in numeric literal")
            end
            validate_numeric_literal(start, @i, 8, false, suffix_consumed)
            return Token.new(TokenKind::Number, Span.new(start, @i))
          end
        end

        floating = false
        int_count, int_ok, int_trailing = scan_decimal_digits
        underscore_invalid ||= !int_ok
        trailing_underscore = int_trailing
        if @i + 1 < n && @bytes[@i] == DOT && ascii_digit?(@bytes[@i + 1])
          floating = true
          @i += 1
          frac_count, frac_ok, frac_trailing = scan_decimal_digits
          underscore_invalid ||= !frac_ok
          trailing_underscore = frac_trailing
          if frac_count == 0
            @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid numeric literal")
          end
        end

        if @i < n && (@bytes[@i] == LOWER_E || @bytes[@i] == UPPER_E)
          floating = true
          @i += 1
          if @i < n && (@bytes[@i] == PLUS || @bytes[@i] == MINUS)
            @i += 1
          end
          exp_count, exp_ok, exp_trailing = scan_decimal_digits
          underscore_invalid ||= !exp_ok
          trailing_underscore = exp_trailing
          if exp_count == 0
            @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid exponent in numeric literal")
          end
        end
        suffix_consumed = consume_numeric_suffix(trailing_underscore)
        underscore_invalid ||= trailing_underscore && !suffix_consumed
        if int_count == 0
          @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid numeric literal")
        end
        if underscore_invalid
          @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid underscore placement in numeric literal")
        end

        validate_numeric_literal(start, @i, 10, floating, suffix_consumed)

        Token.new(TokenKind::Number, Span.new(start, @i))
      end

      private def validate_numeric_literal(start : Int32, finish : Int32, base : Int32, floating : Bool, has_suffix : Bool) : Nil
        raw = String.new(@bytes[start, finish - start])
        normalized = raw.delete('_')
        suffix = has_suffix ? numeric_suffix(normalized) : nil
        core = suffix ? normalized[0, normalized.bytesize - suffix.bytesize] : normalized

        if message = invalid_numeric_suffix_message(finish)
          @diagnostics << Diagnostic.new(Span.new(start, invalid_numeric_suffix_finish(finish)), message)
        end

        if raw.includes?("_.")
          @diagnostics << Diagnostic.new(Span.new(start, finish), "underscore cannot appear before the decimal point")
        end

        if floating
          if suffix && (suffix.not_nil!.starts_with?('i') || suffix.not_nil!.starts_with?('u'))
            @diagnostics << Diagnostic.new(Span.new(start, finish), "invalid integer suffix for decimal number")
          end
          return
        end

        if suffix && suffix.not_nil!.starts_with?('f')
          if base == 2
            @diagnostics << Diagnostic.new(Span.new(start, finish), "binary float literal is not supported")
          elsif base == 8
            @diagnostics << Diagnostic.new(Span.new(start, finish), "octal float literal is not supported")
          end
          return
        end

        if base == 10 && core.bytesize > 1 && core.starts_with?('0')
          @diagnostics << Diagnostic.new(Span.new(start, finish), "octal constants must be prefixed with 0o")
        end

        digits = case base
                 when 16, 8, 2
                   core.bytesize >= 2 ? core[2..] : ""
                 else
                   core
                 end
        magnitude = digits.to_u128?(base)
        unless magnitude
          @diagnostics << Diagnostic.new(Span.new(start, finish), "integer literal is out of range")
          return
        end

        negative = negative_numeric_literal?(start)
        if suffix
          suffix_value = suffix.not_nil!
          bits = suffix_value[1..].to_i
          if suffix_value.starts_with?('u')
            max = bits == 128 ? UInt128::MAX : (1_u128 << bits) - 1
            if negative || magnitude > max
              @diagnostics << Diagnostic.new(Span.new(start, finish), "integer literal does not fit #{suffix_value}")
            end
          else
            limit = 1_u128 << (bits - 1)
            max = negative ? limit : limit - 1
            if magnitude > max
              @diagnostics << Diagnostic.new(Span.new(start, finish), "integer literal does not fit #{suffix_value}")
            end
          end
        else
          max = negative ? (1_u128 << 63) : UInt64::MAX.to_u128
          if magnitude > max
            @diagnostics << Diagnostic.new(Span.new(start, finish), "integer literal does not fit the default integer types")
          end
        end
      end

      private def numeric_suffix(normalized : String) : String?
        {"i128", "u128", "i64", "u64", "i32", "u32", "f32", "f64", "i16", "u16", "i8", "u8"}.find do |suffix|
          normalized.ends_with?(suffix)
        end
      end

      private def invalid_numeric_suffix_message(position : Int32) : String?
        return nil if position >= @bytes.size
        prefix = @bytes[position]
        case prefix
        when 'i'.ord.to_u8          then "invalid int suffix"
        when 'u'.ord.to_u8          then "invalid uint suffix"
        when 'f'.ord.to_u8, UPPER_F then "invalid float suffix"
        else
          return nil unless ident_start?(prefix)
          finish = invalid_numeric_suffix_finish(position)
          text = String.new(@bytes[position, finish - position])
          "unexpected token: \"#{text}\""
        end
      end

      private def invalid_numeric_suffix_finish(position : Int32) : Int32
        i = position + 1
        while i < @bytes.size && ident_continue?(@bytes[i])
          i += 1
        end
        i
      end

      private def negative_numeric_literal?(start : Int32) : Bool
        span = @last_token_span
        return false unless @last_token_kind == TokenKind::Minus && span && span.not_nil!.finish == start
        !token_can_end_expression?(@previous_token_kind)
      end

      private def token_can_end_expression?(kind : TokenKind) : Bool
        case kind
        when TokenKind::Identifier, TokenKind::InstanceVar, TokenKind::ClassVar, TokenKind::GlobalVar,
             TokenKind::Symbol, TokenKind::Number, TokenKind::String, TokenKind::Regex, TokenKind::Char,
             TokenKind::KeywordFalse, TokenKind::KeywordNil, TokenKind::KeywordSelf, TokenKind::KeywordSuper,
             TokenKind::KeywordTrue, TokenKind::RParen, TokenKind::RBracket, TokenKind::RBrace, TokenKind::KeywordEnd
          true
        else
          false
        end
      end

      private def scan_decimal_digits : Tuple(Int32, Bool, Bool)
        n = @bytes.size
        count = 0
        ok = true
        last_underscore = false
        while @i < n
          byte = @bytes[@i]
          if ascii_digit?(byte)
            @i += 1
            count += 1
            last_underscore = false
          elsif byte == UNDERSCORE
            ok = false if count == 0 || last_underscore
            last_underscore = true
            @i += 1
          else
            break
          end
        end
        {count, ok, last_underscore}
      end

      private def scan_hex_digits : Tuple(Int32, Bool, Bool)
        n = @bytes.size
        count = 0
        ok = true
        last_underscore = false
        while @i < n
          byte = @bytes[@i]
          if hex_digit?(byte)
            @i += 1
            count += 1
            last_underscore = false
          elsif byte == UNDERSCORE
            ok = false if count == 0 || last_underscore
            last_underscore = true
            @i += 1
          else
            break
          end
        end
        {count, ok, last_underscore}
      end

      private def scan_bin_digits : Tuple(Int32, Bool, Bool)
        n = @bytes.size
        count = 0
        ok = true
        last_underscore = false
        while @i < n
          byte = @bytes[@i]
          if byte == ZERO || byte == ONE
            @i += 1
            count += 1
            last_underscore = false
          elsif byte == UNDERSCORE
            ok = false if count == 0 || last_underscore
            last_underscore = true
            @i += 1
          else
            break
          end
        end
        {count, ok, last_underscore}
      end

      private def scan_oct_digits : Tuple(Int32, Bool, Bool)
        n = @bytes.size
        count = 0
        ok = true
        last_underscore = false
        while @i < n
          byte = @bytes[@i]
          if byte >= ZERO && byte <= SEVEN
            @i += 1
            count += 1
            last_underscore = false
          elsif byte == UNDERSCORE
            ok = false if count == 0 || last_underscore
            last_underscore = true
            @i += 1
          else
            break
          end
        end
        {count, ok, last_underscore}
      end

      private def scan_string : Token
        start = @i
        n = @bytes.size
        terminated = false
        @i += 1
        while @i < n
          byte = @bytes[@i]
          @i += 1
          if byte == BACKSLASH
            consume_escape(@i - 1, allow_unknown: true, allow_octal: true)
          elsif byte == HASH && @i < n && @bytes[@i] == LBRACE
            @i += 1
            skip_interpolation
          elsif byte == DQUOTE
            terminated = true
            break
          elsif byte == LF
            @line_starts << @i
          end
        end
        unless terminated
          @diagnostics << Diagnostic.new(Span.new(@i, @i), "Unterminated string literal")
        end
        Token.new(TokenKind::String, Span.new(start, @i))
      end

      private def scan_backtick : Token
        start = @i
        n = @bytes.size
        terminated = false
        @i += 1
        while @i < n
          byte = @bytes[@i]
          @i += 1
          if byte == BACKSLASH
            @i += 1 if @i < n
          elsif byte == HASH && @i < n && @bytes[@i] == LBRACE
            @i += 1
            skip_interpolation
          elsif byte == BACKTICK
            terminated = true
            break
          elsif byte == LF
            @line_starts << @i
          end
        end
        unless terminated
          @diagnostics << Diagnostic.new(Span.new(@i, @i), "Unterminated command literal")
        end
        Token.new(TokenKind::String, Span.new(start, @i))
      end

      private def scan_char : Token
        start = @i
        n = @bytes.size
        terminated = false
        @i += 1
        if @i < n && @bytes[@i] == SQUOTE
          @diagnostics << Diagnostic.new(Span.new(start, start + 1), "invalid empty char literal (did you mean '\\''?)")
        end
        while @i < n
          byte = @bytes[@i]
          @i += 1
          if byte == BACKSLASH
            consume_escape(@i - 1, char_literal: true)
          elsif byte == SQUOTE
            terminated = true
            break
          elsif byte == LF
            @line_starts << @i
          end
        end
        unless terminated
          @diagnostics << Diagnostic.new(Span.new(start, @i), "unterminated char literal")
        end
        Token.new(TokenKind::Char, Span.new(start, @i))
      end

      private def scan_operator_or_punct : Token?
        if match = Operators.match(@bytes, @i)
          kind, length = match
          start = @i
          @i += length
          return Token.new(kind, Span.new(start, @i))
        end
        nil
      end

      private def consume_escape(
        start : Int32,
        allow_unknown : Bool = false,
        allow_octal : Bool = false,
        char_literal : Bool = false,
      )
        n = @bytes.size
        if @i >= n
          @diagnostics << Diagnostic.new(Span.new(start, @i), "unterminated escape sequence")
          return
        end

        byte = @bytes[@i]
        @i += 1
        case byte
        when SQUOTE, BACKSLASH, LOWER_N, LOWER_T, LOWER_R, LOWER_B, LOWER_F, LOWER_V, LOWER_E, LOWER_A
          return
        when DQUOTE, HASH, PERCENT, LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE, LT, GT, PIPE
          if char_literal
            add_invalid_char_escape(start, byte)
          end
          return
        when LF
          @line_starts << @i
          return
        when CR
          if @i < n && @bytes[@i] == LF
            @i += 1
          end
          @line_starts << @i
          return
        when LOWER_X
          if char_literal
            add_invalid_char_escape(start, byte)
          elsif !consume_fixed_hex_digits(2)
            @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid hex escape sequence")
          end
        when LOWER_U
          if @i < n && @bytes[@i] == LBRACE
            @i += 1
            valid = true
            loop do
              codepoint_start = @i
              digits = 0
              while @i < n && hex_digit?(@bytes[@i])
                @i += 1
                digits += 1
              end
              valid = false if digits == 0 || digits > 6
              valid = false if digits > 0 && !valid_unicode_codepoint?(codepoint_start, @i)
              if @i < n && @bytes[@i] == RBRACE
                @i += 1
                break
              elsif @i < n && @bytes[@i] == SPACE
                @i += 1
                next
              else
                @diagnostics << Diagnostic.new(Span.new(start, @i), "unterminated unicode escape sequence")
                break
              end
            end
            unless valid
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid unicode escape sequence")
            end
          else
            codepoint_start = @i
            unless consume_fixed_hex_digits(4) && valid_unicode_codepoint?(codepoint_start, @i)
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid unicode escape sequence")
            end
          end
        when ZERO..SEVEN
          if allow_octal
            consume_octal_escape(start, byte)
          elsif byte != ZERO
            add_invalid_char_escape(start, byte)
          end
        else
          unless allow_unknown
            if char_literal
              add_invalid_char_escape(start, byte)
            else
              @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid escape sequence")
            end
          end
        end
      end

      private def add_invalid_char_escape(start : Int32, byte : UInt8) : Nil
        @diagnostics << Diagnostic.new(
          Span.new(start, @i),
          "invalid char escape sequence '\\#{byte.unsafe_chr}'"
        )
      end

      private def consume_octal_escape(start : Int32, first_byte : UInt8) : Nil
        n = @bytes.size
        value = (first_byte - ZERO).to_i
        read = 0
        while read < 2 && @i < n
          byte = @bytes[@i]
          break unless byte >= ZERO && byte <= SEVEN
          value = value * 8 + (byte - ZERO).to_i
          @i += 1
          read += 1
        end
        if value > 0xff
          @diagnostics << Diagnostic.new(Span.new(start, @i), "octal escape value is too large")
        end
      end

      private def valid_unicode_codepoint?(start : Int32, finish : Int32) : Bool
        return false if finish <= start
        value = String.new(@bytes[start, finish - start]).to_i(16)
        value <= 0x10ffff && !(value >= 0xd800 && value <= 0xdfff)
      end

      private def consume_octal_digits(max : Int32)
        n = @bytes.size
        read = 0
        while read < max && @i < n
          byte = @bytes[@i]
          break unless byte >= ZERO && byte <= SEVEN
          @i += 1
          read += 1
        end
      end

      private def consume_fixed_hex_digits(count : Int32) : Bool
        n = @bytes.size
        read = 0
        while read < count && @i < n && hex_digit?(@bytes[@i])
          @i += 1
          read += 1
        end
        read == count
      end

      private def consume_hex_digits_until(terminator : UInt8) : Int32
        n = @bytes.size
        digits = 0
        while @i < n
          byte = @bytes[@i]
          if byte == terminator
            break
          elsif hex_digit?(byte)
            digits += 1
            @i += 1
          else
            break
          end
        end
        digits
      end

      private def consume_numeric_suffix(trailing_underscore : Bool) : Bool
        n = @bytes.size
        original = @i
        if trailing_underscore
          return false unless @i < n && ascii_alpha?(@bytes[@i])
        else
          if @i + 1 < n && @bytes[@i] == UNDERSCORE && ascii_alpha?(@bytes[@i + 1])
            @i += 1
          else
            suffix_start = @i
            return false unless suffix_start < n
            prefix = @bytes[suffix_start]
            return false unless prefix == 'i'.ord.to_u8 || prefix == 'u'.ord.to_u8 || prefix == 'f'.ord.to_u8
            @i += 1
            while @i < n && ascii_digit?(@bytes[@i])
              @i += 1
            end
            suffix = String.new(@bytes[suffix_start, @i - suffix_start])
            unless valid_numeric_suffix?(suffix)
              @i = suffix_start
              return false
            end
            return true
          end
        end

        suffix_start = @i
        while @i < n
          byte = @bytes[@i]
          if ascii_alpha?(byte) || ascii_digit?(byte)
            @i += 1
          else
            break
          end
        end
        suffix = String.new(@bytes[suffix_start, @i - suffix_start])
        return true if valid_numeric_suffix?(suffix)
        @i = original
        false
      end

      private def valid_numeric_suffix?(suffix : String) : Bool
        {"i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64"}.includes?(suffix)
      end

      private def scan_percent_literal : Token?
        n = @bytes.size
        return nil if @i + 1 >= n

        start = @i
        if start >= 2 && @bytes[start - 1] == LBRACE && @bytes[start - 2] == BACKSLASH
          return nil
        end
        type = percent_literal_type(@bytes[@i + 1])
        delimiter_index = @i + 1
        if type
          delimiter_index += 1
          return nil if delimiter_index >= n
        end

        delimiter = @bytes[delimiter_index]
        if after_macro_control_end?(start) && type.nil? &&
           !{LPAREN, LBRACKET, LBRACE, LT}.includes?(delimiter)
          return nil
        end
        if delimiter == EQUAL || ident_start?(delimiter) || delimiter == SPACE || delimiter == TAB || delimiter == CR || delimiter == LF
          return nil
        end
        if delimiter == RPAREN || delimiter == RBRACKET || delimiter == RBRACE || delimiter == GT
          return nil
        end

        closing, nested = percent_literal_closing(delimiter)
        return nil unless closing

        @i = delimiter_index + 1
        body_start = @i
        terminated = scan_percent_body(
          delimiter,
          closing,
          nested,
          percent_literal_interpolates?(type),
          type != 'q'.ord.to_u8
        )
        unless terminated
          @diagnostics << Diagnostic.new(Span.new(@i, @i), unterminated_percent_literal_message(type))
        end
        body_finish = terminated ? @i - 1 : @i
        validate_regex_body(body_start, body_finish, start) if type == 'r'.ord.to_u8
        validate_string_array_interpolation(body_start, body_finish) if type == 'W'.ord.to_u8
        if type == 'r'.ord.to_u8
          consume_regex_options
        end
        kind = type == 'r'.ord.to_u8 ? TokenKind::Regex : TokenKind::String
        Token.new(kind, Span.new(start, @i))
      end

      private def after_macro_control_end?(position : Int32) : Bool
        i = position - 1
        while i >= 0
          byte = @bytes[i]
          break unless byte == SPACE || byte == TAB || byte == CR || byte == LF
          i -= 1
        end
        return false if i < 1 || @bytes[i] != RBRACE
        @bytes[i - 1] == PERCENT
      end

      private def validate_regex_body(start_pos : Int32, finish_pos : Int32, diagnostic_start : Int32) : Nil
        i = start_pos
        in_class = false
        while i < finish_pos
          byte = @bytes[i]
          if byte == BACKSLASH
            if i + 1 < finish_pos && @bytes[i + 1] == LOWER_U
              offset = i - start_pos + 2
              @diagnostics << Diagnostic.new(
                Span.new(diagnostic_start, diagnostic_start),
                "invalid regex: PCRE2 does not support \\F, \\L, \\l, \\N{name}, \\U, or \\u at #{offset}"
              )
            end
            i += 2
            next
          elsif byte == LBRACKET
            in_class = true
          elsif byte == RBRACKET
            in_class = false
          end
          i += 1
        end
        if in_class
          @diagnostics << Diagnostic.new(
            Span.new(diagnostic_start, diagnostic_start),
            "invalid regex: missing terminating ] for character class at #{finish_pos - start_pos}"
          )
        end
      end

      private def validate_string_array_interpolation(start_pos : Int32, finish_pos : Int32) : Nil
        i = start_pos
        while i + 2 < finish_pos
          if @bytes[i] == HASH && @bytes[i + 1] == LBRACE
            j = i + 2
            while j < finish_pos && (@bytes[j] == SPACE || @bytes[j] == TAB)
              j += 1
            end
            if j < finish_pos && @bytes[j] == STAR
              depth = 1
              k = j + 1
              while k < finish_pos && depth > 0
                depth += 1 if @bytes[k] == LBRACE
                depth -= 1 if @bytes[k] == RBRACE
                k += 1
              end
              isolated_start = i == start_pos || @bytes[i - 1] == SPACE || @bytes[i - 1] == TAB || @bytes[i - 1] == CR || @bytes[i - 1] == LF
              isolated_end = k >= finish_pos || @bytes[k] == SPACE || @bytes[k] == TAB || @bytes[k] == CR || @bytes[k] == LF
              unless isolated_start && isolated_end
                diagnostic_pos = isolated_start ? k : j
                @diagnostics << Diagnostic.new(Span.new(diagnostic_pos, diagnostic_pos), "splat interpolation must be the only piece in a string array element")
              end
              i = k
              next
            end
          end
          i += 1
        end
      end

      private def scan_percent_body(opening : UInt8, closing : UInt8, nested : Bool, interpolate : Bool, escapes : Bool) : Bool
        n = @bytes.size
        depth = nested ? 1 : 0
        while @i < n
          byte = @bytes[@i]
          @i += 1
          if escapes && byte == BACKSLASH
            @i += 1 if @i < n
            next
          end
          if interpolate && byte == HASH && @i < n && @bytes[@i] == LBRACE
            @i += 1
            skip_interpolation
            next
          end
          if nested
            if byte == opening
              depth += 1
            elsif byte == closing
              depth -= 1
              return true if depth == 0
            end
          else
            return true if byte == closing
          end
          @line_starts << @i if byte == LF
        end
        false
      end

      private def percent_literal_type(byte : UInt8) : UInt8?
        case byte
        when 'q'.ord.to_u8, 'Q'.ord.to_u8, 'w'.ord.to_u8, 'W'.ord.to_u8, 'x'.ord.to_u8,
             'r'.ord.to_u8, 'i'.ord.to_u8
          byte
        else
          nil
        end
      end

      private def unterminated_percent_literal_message(type : UInt8?) : String
        case type
        when 'w'.ord.to_u8, 'W'.ord.to_u8
          "Unterminated string array literal"
        when 'i'.ord.to_u8, 'I'.ord.to_u8
          "Unterminated symbol array literal"
        when 'r'.ord.to_u8
          "Unterminated regular expression"
        when 'x'.ord.to_u8
          "Unterminated command literal"
        else
          "Unterminated string literal"
        end
      end

      private def percent_literal_interpolates?(type : UInt8?) : Bool
        return true if type.nil?
        case type
        when 'Q'.ord.to_u8, 'W'.ord.to_u8, 'x'.ord.to_u8, 'r'.ord.to_u8
          true
        else
          false
        end
      end

      private def percent_literal_closing(delimiter : UInt8) : Tuple(UInt8, Bool)?
        case delimiter
        when LPAREN
          {RPAREN, true}
        when LBRACKET
          {RBRACKET, true}
        when LBRACE
          {RBRACE, true}
        when LT
          {GT, true}
        else
          {delimiter, false}
        end
      end

      private def scan_multi_heredoc_layouts : Nil
        n = @bytes.size
        line_start = 0
        while line_start < n
          line_end = line_start
          while line_end < n && @bytes[line_end] != LF
            line_end += 1
          end
          break if line_end >= n

          headers = [] of Tuple(Int32, Int32, String, Bool)
          position = line_start
          while position + 3 < line_end
            unless @bytes[position] == LT && @bytes[position + 1] == LT &&
                   @bytes[position + 2] == MINUS
              position += 1
              next
            end
            indented = true
            cursor = position + 3
            quote = if cursor < line_end && @bytes[cursor] == SQUOTE
                      value = @bytes[cursor]
                      cursor += 1
                      value
                    end
            label_start = cursor
            if quote
              while cursor < line_end && @bytes[cursor] != quote
                cursor += 1
              end
              unless cursor < line_end
                position += 1
                next
              end
              label = String.new(@bytes[label_start, cursor - label_start])
              cursor += 1
              headers << {position, cursor, label, indented}
            else
              unless cursor < line_end && (ascii_alpha?(@bytes[cursor]) || @bytes[cursor] == UNDERSCORE)
                position += 1
                next
              end
              cursor += 1
              while cursor < line_end &&
                    (ascii_alpha?(@bytes[cursor]) || ascii_digit?(@bytes[cursor]) || @bytes[cursor] == UNDERSCORE)
                cursor += 1
              end
              headers << {position, cursor, String.new(@bytes[label_start, cursor - label_start]), indented}
            end
            position = cursor
          end

          if headers.size >= 2
            body_cursor = line_end + 1
            layouts = [] of Tuple(Int32, Int32, Int32, Int32, Int32)
            complete = true
            headers.each do |header_start, header_finish, label, indented|
              body_start = body_cursor
              found = false
              while body_cursor <= n
                closing_start = body_cursor
                closing_end = closing_start
                while closing_end < n && @bytes[closing_end] != LF
                  closing_end += 1
                end
                content_end = closing_end
                content_end -= 1 if content_end > closing_start && @bytes[content_end - 1] == CR
                compare_start = closing_start
                if indented
                  while compare_start < content_end &&
                        (@bytes[compare_start] == SPACE || @bytes[compare_start] == TAB)
                    compare_start += 1
                  end
                end
                if content_end - compare_start == label.bytesize &&
                   @bytes[compare_start, label.bytesize] == label.to_slice
                  full_finish = closing_end < n ? closing_end + 1 : closing_end
                  layouts << {header_start, header_finish, body_start, closing_start, full_finish}
                  body_cursor = full_finish
                  found = true
                  break
                end
                break if closing_end >= n
                body_cursor = closing_end + 1
              end
              unless found
                complete = false
                break
              end
            end
            if complete
              layouts.each do |header_start, header_finish, body_start, body_finish, full_finish|
                @multi_heredoc_header_finishes[header_start] = header_finish
                @multi_heredoc_full_spans[header_start] = Span.new(header_start, full_finish)
                @multi_heredoc_body_bounds[header_start] = {body_start, body_finish}
              end
              @multi_heredoc_line_skips[line_end] = body_cursor
            end
          end
          line_start = line_end + 1
        end
      end

      private def scan_heredoc : Token?
        n = @bytes.size
        if finish = @multi_heredoc_header_finishes[@i]?
          start = @i
          @i = finish
          line_end = finish
          while line_end < n && @bytes[line_end] != LF
            line_end += 1
          end
          @active_multi_heredoc_line_end = line_end
          return Token.new(TokenKind::String, Span.new(start, finish))
        end
        return nil unless @i + 1 < n && @bytes[@i + 1] == LT
        return nil if @i + 2 >= n
        next_byte = @bytes[@i + 2]
        return nil unless next_byte == MINUS

        start = @i
        @i += 2

        indented = false
        if @i < n && @bytes[@i] == MINUS
          indented = true
          @i += 1
        end

        quote = nil
        if @i < n && @bytes[@i] == SQUOTE
          quote = @bytes[@i]
          @i += 1
        end

        label = ""
        if quote
          label_start = @i
          while @i < n
            byte = @bytes[@i]
            break if byte == quote || byte == LF || byte == CR
            @i += 1
          end
          if @i >= n || @bytes[@i] == LF || @bytes[@i] == CR
            message = quote == SQUOTE ? "expecting closing single quote" : "expecting closing double quote"
            @diagnostics << Diagnostic.new(Span.new(@i, @i), message)
            return Token.new(TokenKind::String, Span.new(start, @i))
          end
          label = String.new(@bytes[label_start, @i - label_start])
          @i += 1
        else
          label_start = @i
          unless @i < n && (ascii_alpha?(@bytes[@i]) || @bytes[@i] == UNDERSCORE)
            @diagnostics << Diagnostic.new(Span.new(start, Math.min(@i + 1, n)), "heredoc identifier starts with invalid character")
            @i = n
            return Token.new(TokenKind::String, Span.new(start, @i))
          end
          while @i < n
            byte = @bytes[@i]
            break unless ascii_alpha?(byte) || ascii_digit?(byte) || byte == UNDERSCORE
            @i += 1
          end
          if @i == label_start
            @diagnostics << Diagnostic.new(Span.new(start, @i), "heredoc identifier starts with invalid character")
            @i = n
            return Token.new(TokenKind::String, Span.new(start, @i))
          end
          label = String.new(@bytes[label_start, @i - label_start])
        end

        while @i < n && @bytes[@i] != LF
          @i += 1
        end
        if @i >= n
          @diagnostics << Diagnostic.new(Span.new(@i, @i), "Unexpected EOF on heredoc identifier")
          return Token.new(TokenKind::String, Span.new(start, @i))
        end
        if @i < n && @bytes[@i] == LF
          @i += 1
          @line_starts << @i
        end

        terminated = false
        closing_indent = 0
        line_indents = [] of Tuple(Int32, Int32)
        while @i < n
          line_start = @i
          while @i < n && @bytes[@i] != LF
            @i += 1
          end
          line_end = @i
          line_end_no_cr = line_end
          if line_end_no_cr > line_start && @bytes[line_end_no_cr - 1] == CR
            line_end_no_cr -= 1
          end

          compare_start = line_start
          compare_length = line_end_no_cr - line_start
          indent_count = 0
          scan_pos = line_start
          if indented
            while scan_pos < line_end_no_cr
              byte = @bytes[scan_pos]
              break unless byte == SPACE || byte == TAB
              indent_count += 1
              scan_pos += 1
            end
            compare_start = scan_pos
            compare_length = line_end_no_cr - scan_pos
          end
          if compare_length == label.bytesize &&
             @bytes[compare_start, compare_length] == label.to_slice
            terminated = true
            closing_indent = indented ? indent_count : 0
            if @i < n && @bytes[@i] == LF
              @i += 1
              @line_starts << @i
            end
            break
          end
          if indented
            blank = scan_pos >= line_end_no_cr
            line_indents << {line_start, indent_count} unless blank
          end
          if @i < n && @bytes[@i] == LF
            @i += 1
            @line_starts << @i
          end
        end

        if @parser_mode && terminated && indented && closing_indent > 0
          line_indents.each do |line_start, indent|
            if indent < closing_indent
              @diagnostics << Diagnostic.new(Span.new(line_start, line_start), "heredoc line must have an indent greater than or equal to #{closing_indent}")
              break
            end
          end
        end

        unless terminated
          @diagnostics << Diagnostic.new(Span.new(@i, @i), "Unterminated heredoc: can't find \"#{label}\" anywhere before the end of file")
        end

        Token.new(TokenKind::String, Span.new(start, @i))
      end

      private def scan_regex : Token
        start = @i
        n = @bytes.size
        @i += 1
        in_class = false
        paren_depth = 0
        invalid_regex_offset : Int32? = nil
        terminated = false
        while @i < n
          byte = @bytes[@i]
          @i += 1
          if byte == BACKSLASH
            if @i < n && @bytes[@i] == LOWER_U
              offset = @i - start
              @diagnostics << Diagnostic.new(
                Span.new(start, start),
                "invalid regex: PCRE2 does not support \\F, \\L, \\l, \\N{name}, \\U, or \\u at #{offset}"
              )
            end
            @i += 1 if @i < n
          elsif byte == LBRACKET
            in_class = true
          elsif byte == RBRACKET
            in_class = false
          elsif byte == LPAREN && !in_class
            paren_depth += 1
          elsif byte == RPAREN && !in_class
            paren_depth -= 1
            invalid_regex_offset ||= @i - start - 2 if paren_depth < 0
          elsif byte == HASH && @i < n && @bytes[@i] == LBRACE
            @i += 1
            skip_interpolation
          elsif byte == SLASH && !in_class
            terminated = true
            break
          elsif byte == LF
            @line_starts << @i
          end
        end

        unless terminated
          if in_class
            @diagnostics << Diagnostic.new(
              Span.new(start, start),
              "invalid regex: missing terminating ] for character class at #{@i - start - 2}"
            )
          else
            @diagnostics << Diagnostic.new(Span.new(@i, @i), "Unterminated regular expression")
          end
        end
        if offset = invalid_regex_offset
          @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid regex: unmatched closing parenthesis at #{offset}")
        elsif paren_depth != 0
          @diagnostics << Diagnostic.new(Span.new(start, @i), "invalid regex")
        end

        consume_regex_options

        Token.new(TokenKind::Regex, Span.new(start, @i))
      end

      private def consume_regex_options : Nil
        while @i < @bytes.size && ascii_alpha?(@bytes[@i])
          byte = @bytes[@i]
          unless {'i'.ord.to_u8, 'm'.ord.to_u8, 'x'.ord.to_u8}.includes?(byte)
            @diagnostics << Diagnostic.new(
              Span.new(@i, @i + 1),
              "unknown regex option: #{byte.unsafe_chr}"
            )
          end
          @i += 1
        end
      end

      private def skip_interpolation
        n = @bytes.size
        depth = 1
        while @i < n
          byte = @bytes[@i]
          if byte == DQUOTE
            skip_quoted(DQUOTE, "unterminated string literal")
            next
          elsif byte == SQUOTE
            skip_quoted(SQUOTE, "unterminated char literal")
            next
          elsif byte == LT && @i + 1 < n && @bytes[@i + 1] == LT
            @diagnostics << Diagnostic.new(Span.new(@i, @i + 2), "heredoc cannot be used inside interpolation")
            @i += 2
            next
          elsif byte == LBRACE
            depth += 1
            @i += 1
            next
          elsif byte == RBRACE
            depth -= 1
            @i += 1
            break if depth == 0
            next
          else
            @i += 1
            @line_starts << @i if byte == LF
          end
        end

        if depth > 0
          @diagnostics << Diagnostic.new(Span.new(@i, @i), "Unterminated string interpolation")
        end
      end

      private def skip_quoted(quote : UInt8, error_message : String)
        n = @bytes.size
        start = @i
        @i += 1
        while @i < n
          byte = @bytes[@i]
          @i += 1
          if byte == BACKSLASH
            @i += 1 if @i < n
          elsif byte == quote
            return
          elsif byte == LF
            @line_starts << @i
          end
        end
        @diagnostics << Diagnostic.new(Span.new(start, @i), error_message)
      end

      private def ascii_digit?(byte : UInt8) : Bool
        byte >= ZERO && byte <= NINE
      end

      private def ascii_alpha?(byte : UInt8) : Bool
        (byte >= UPPER_A && byte <= UPPER_Z) || (byte >= LOWER_A && byte <= LOWER_Z)
      end

      private def ascii_alnum?(byte : UInt8) : Bool
        ascii_alpha?(byte) || ascii_digit?(byte)
      end

      private def regex_allowed? : Bool
        slash_op = slash_starts_operator?
        following_space = next_byte_space?
        next_ident = next_byte_ident_start?
        newline = newline_since_last_token?
        case @last_token_kind
        when TokenKind::Identifier
          return true if newline
          return true if identifier_ends_with_predicate?
          return false if slash_op
          return false unless spaced_since_last_token?
          return false if following_space
          return false if next_ident && !regex_closing_delim_ahead?
          true
        when TokenKind::InstanceVar,
             TokenKind::ClassVar,
             TokenKind::GlobalVar
          return true if newline
          return false if slash_op
          return false unless spaced_since_last_token?
          return false if following_space
          return false if next_ident && !regex_closing_delim_ahead?
          true
        when TokenKind::Number,
             TokenKind::String,
             TokenKind::Regex,
             TokenKind::Char,
             TokenKind::RParen,
             TokenKind::RBracket,
             TokenKind::RBrace,
             TokenKind::Dot,
             TokenKind::DoubleColon,
             TokenKind::SafeNav,
             TokenKind::KeywordEnd,
             TokenKind::KeywordRescue,
             TokenKind::KeywordEnsure,
             TokenKind::KeywordDef,
             TokenKind::KeywordMacro,
             TokenKind::KeywordAlias,
             TokenKind::KeywordSelf,
             TokenKind::KeywordNil,
             TokenKind::KeywordTrue,
             TokenKind::KeywordFalse,
             TokenKind::KeywordSuper
          return true if newline_since_last_token?
          false
        when TokenKind::KeywordBreak, TokenKind::KeywordReturn, TokenKind::KeywordNext
          return false if slash_op || following_space
          true
        else
          true
        end
      end

      private def backtick_allowed? : Bool
        case @last_token_kind
        when TokenKind::KeywordDef,
             TokenKind::KeywordMacro,
             TokenKind::Dot,
             TokenKind::SafeNav,
             TokenKind::DoubleColon
          false
        else
          true
        end
      end

      private def identifier_ends_with_predicate? : Bool
        span = @last_token_span
        return false unless span
        return false if span.finish <= span.start
        last = @bytes[span.finish - 1]
        last == QUESTION || last == BANG
      end

      private def spaced_since_last_token? : Bool
        return true unless span = @last_token_span
        i = span.finish
        return false if i >= @i
        while i < @i
          byte = @bytes[i]
          return false unless byte == SPACE || byte == TAB || byte == CR || byte == LF
          i += 1
        end
        true
      end

      private def newline_since_last_token? : Bool
        return false unless span = @last_token_span
        i = span.finish
        return false if i >= @i
        last_non_space = nil
        while i < @i
          byte = @bytes[i]
          if byte == LF
            return true unless last_non_space == BACKSLASH
            last_non_space = nil
          elsif byte == CR
            return true unless last_non_space == BACKSLASH
            last_non_space = nil
          elsif byte == SPACE || byte == TAB
            # skip
          else
            last_non_space = byte
          end
          i += 1
        end
        false
      end

      private def ident_preceded_by_dot_or_colon? : Bool
        return false unless span = @last_token_span
        return false if span.start == 0
        prev = @bytes[span.start - 1]
        prev == DOT || prev == COLON
      end

      private def slash_starts_operator? : Bool
        n = @bytes.size
        return false if @i + 1 >= n
        nxt = @bytes[@i + 1]
        nxt == EQUAL || nxt == SLASH
      end

      private def next_byte_space? : Bool
        n = @bytes.size
        return false if @i + 1 >= n
        byte = @bytes[@i + 1]
        byte == SPACE || byte == TAB || byte == CR || byte == LF
      end

      private def next_byte_ident_start? : Bool
        n = @bytes.size
        return false if @i + 1 >= n
        ident_start?(@bytes[@i + 1])
      end

      private def regex_closing_delim_ahead? : Bool
        n = @bytes.size
        j = @i + 1
        escaped = false
        while j < n
          byte = @bytes[j]
          if escaped
            escaped = false
          elsif byte == BACKSLASH
            escaped = true
          elsif byte == SLASH
            return true
          elsif byte == LF
            return false
          end
          j += 1
        end
        false
      end

      private def hex_digit?(byte : UInt8) : Bool
        ascii_digit?(byte) ||
          (byte >= UPPER_A && byte <= UPPER_F) ||
          (byte >= LOWER_A && byte <= LOWER_F)
      end

      private def ident_start?(byte : UInt8) : Bool
        byte >= 0x80_u8 || ascii_alpha?(byte) || byte == UNDERSCORE
      end

      private def ident_continue?(byte : UInt8) : Bool
        ident_start?(byte) || ascii_digit?(byte)
      end

      def text(token : Token) : String
        String.new(@bytes[token.span.start, token.span.length])
      end

      def line_and_column(offset : Int32) : Tuple(Int32, Int32)
        idx = @line_starts.bsearch_index { |x| x > offset }
        line_index = idx ? idx - 1 : @line_starts.size - 1
        line = line_index + 1
        column = offset - @line_starts[line_index] + 1
        {line, column}
      end

      def token_location(token : Token) : Tuple(Int32, Int32)
        line_and_column(token.span.start)
      end
    end
  end
end
