module Facet
  module Compiler
    enum LiteralStyle : UInt8
      Source
      Escaped
      Raw
      Word
      Regex
      HeredocEscaped
      HeredocRaw
    end

    module LiteralDecoder
      extend self

      def decode(content : String, style : LiteralStyle, syntax : String = "", delimiter : UInt8 = 0_u8) : String
        case style
        when LiteralStyle::Escaped
          decode_escapes(content)
        when LiteralStyle::Word
          decode_word(content, delimiter)
        when LiteralStyle::Regex
          decode_regex(content, delimiter)
        when LiteralStyle::HeredocEscaped
          decode_escapes(dedent_heredoc(content, syntax))
        when LiteralStyle::HeredocRaw
          dedent_heredoc(content, syntax)
        else
          content
        end
      end

      private def decode_escapes(content : String) : String
        bytes = content.to_slice
        String.build do |io|
          index = 0
          while index < bytes.size
            byte = bytes[index]
            unless byte == '\\'.ord.to_u8 && index + 1 < bytes.size
              io.write_byte(byte)
              index += 1
              next
            end

            escaped = bytes[index + 1]
            index += 2
            case escaped
            when 'a'.ord.to_u8 then io.write_byte(0x07_u8)
            when 'b'.ord.to_u8 then io.write_byte(0x08_u8)
            when 'e'.ord.to_u8 then io.write_byte(0x1b_u8)
            when 'f'.ord.to_u8 then io.write_byte(0x0c_u8)
            when 'n'.ord.to_u8 then io.write_byte('\n'.ord.to_u8)
            when 'r'.ord.to_u8 then io.write_byte('\r'.ord.to_u8)
            when 't'.ord.to_u8 then io.write_byte('\t'.ord.to_u8)
            when 'v'.ord.to_u8 then io.write_byte(0x0b_u8)
            when '\n'.ord.to_u8
              index = skip_horizontal_space(bytes, index)
            when '\r'.ord.to_u8
              index += 1 if index < bytes.size && bytes[index] == '\n'.ord.to_u8
              index = skip_horizontal_space(bytes, index)
            when 'x'.ord.to_u8
              value, finish = read_hex(bytes, index, 2)
              if value && finish == index + 2
                io.write_byte(value.to_u8)
                index = finish
              else
                io << 'x'
              end
            when 'u'.ord.to_u8
              index = decode_unicode_escape(io, bytes, index)
            when '0'.ord.to_u8..'7'.ord.to_u8
              value = (escaped - '0'.ord.to_u8).to_i32
              read = 0
              while read < 2 && index < bytes.size && bytes[index].in?('0'.ord.to_u8..'7'.ord.to_u8)
                value = value * 8 + (bytes[index] - '0'.ord.to_u8)
                index += 1
                read += 1
              end
              io.write_byte(value.to_u8)
            else
              io.write_byte(escaped)
            end
          end
        end
      end

      private def decode_unicode_escape(io : IO, bytes : Bytes, index : Int32) : Int32
        if index < bytes.size && bytes[index] == '{'.ord.to_u8
          index += 1
          loop do
            index = skip_horizontal_space(bytes, index)
            break if index >= bytes.size
            if bytes[index] == '}'.ord.to_u8
              return index + 1
            end
            value, finish = read_hex(bytes, index, 6)
            return index unless value && finish > index
            io << value.chr
            index = finish
          end
          index
        else
          value, finish = read_hex(bytes, index, 4)
          if value && finish == index + 4
            io << value.chr
            finish
          else
            io << 'u'
            index
          end
        end
      end

      private def decode_word(content : String, delimiter : UInt8) : String
        bytes = content.to_slice
        closing = closing_delimiter(delimiter)
        String.build do |io|
          index = 0
          while index < bytes.size
            byte = bytes[index]
            unless byte == '\\'.ord.to_u8 && index + 1 < bytes.size
              io.write_byte(byte)
              index += 1
              next
            end
            escaped = bytes[index + 1]
            if escaped == '\n'.ord.to_u8
              index = skip_horizontal_space(bytes, index + 2)
            elsif escaped == '\r'.ord.to_u8
              index += 2
              index += 1 if index < bytes.size && bytes[index] == '\n'.ord.to_u8
              index = skip_horizontal_space(bytes, index)
            elsif escaped.unsafe_chr.whitespace? || escaped == delimiter || escaped == closing || escaped == '\\'.ord.to_u8
              io.write_byte(escaped)
              index += 2
            else
              io.write_byte(byte)
              io.write_byte(escaped)
              index += 2
            end
          end
        end
      end

      private def decode_regex(content : String, delimiter : UInt8) : String
        bytes = content.to_slice
        closing = closing_delimiter(delimiter)
        String.build do |io|
          index = 0
          while index < bytes.size
            byte = bytes[index]
            if byte == '\\'.ord.to_u8 && index + 1 < bytes.size
              escaped = bytes[index + 1]
              if escaped == '/'.ord.to_u8 || escaped == closing
                io.write_byte(escaped)
              else
                io.write_byte(byte)
                io.write_byte(escaped)
              end
              index += 2
            else
              io.write_byte(byte)
              index += 1
            end
          end
        end
      end

      private def dedent_heredoc(content : String, syntax : String) : String
        marker = syntax.starts_with?("<<~") ? '~' : '-'
        closing_start = syntax.rindex('\n', Math.max(syntax.bytesize - 2, 0))
        return content unless closing_start
        closing_line = syntax.byte_slice(closing_start + 1, syntax.bytesize - closing_start - 1)
        indent = leading_whitespace(closing_line)
        width = if marker == '~'
                  content.lines(chomp: false)
                    .reject { |line| line.strip.empty? }
                    .map { |line| leading_whitespace(line).bytesize }
                    .min? || indent.bytesize
                else
                  indent.bytesize
                end
        decoded = if width == 0
                    content
                  else
                    content.lines(chomp: false).map do |line|
                      remove = Math.min(leading_whitespace(line).bytesize, width)
                      line.byte_slice(remove, line.bytesize - remove)
                    end.join
                  end
        decoded = decoded.byte_slice(0, decoded.bytesize - 1) if decoded.ends_with?('\n')
        decoded = decoded.byte_slice(0, decoded.bytesize - 1) if decoded.ends_with?('\r')
        decoded
      end

      private def leading_whitespace(value : String) : String
        finish = 0
        bytes = value.to_slice
        while finish < bytes.size && {' '.ord.to_u8, '\t'.ord.to_u8}.includes?(bytes[finish])
          finish += 1
        end
        value.byte_slice(0, finish)
      end

      private def skip_horizontal_space(bytes : Bytes, index : Int32) : Int32
        while index < bytes.size && {' '.ord.to_u8, '\t'.ord.to_u8}.includes?(bytes[index])
          index += 1
        end
        index
      end

      private def read_hex(bytes : Bytes, start : Int32, limit : Int32) : Tuple(Int32?, Int32)
        index = start
        value = 0
        count = 0
        while index < bytes.size && count < limit
          digit = hex_value(bytes[index])
          break unless digit
          value = value * 16 + digit
          index += 1
          count += 1
        end
        {count == 0 ? nil : value, index}
      end

      private def hex_value(byte : UInt8) : Int32?
        case byte
        when '0'.ord.to_u8..'9'.ord.to_u8 then (byte - '0'.ord.to_u8).to_i32
        when 'a'.ord.to_u8..'f'.ord.to_u8 then (byte - 'a'.ord.to_u8 + 10).to_i32
        when 'A'.ord.to_u8..'F'.ord.to_u8 then (byte - 'A'.ord.to_u8 + 10).to_i32
        else                                   nil
        end
      end

      private def closing_delimiter(opening : UInt8) : UInt8
        case opening
        when '('.ord.to_u8 then ')'.ord.to_u8
        when '['.ord.to_u8 then ']'.ord.to_u8
        when '{'.ord.to_u8 then '}'.ord.to_u8
        when '<'.ord.to_u8 then '>'.ord.to_u8
        else                    opening
        end
      end
    end
  end
end
