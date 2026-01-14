module Facet
  module Compiler
    class TokenStream
      def initialize(@lexer : Lexer, @window : Int32 = 4)
        @buffer = [] of Token
        @index = 0
        @marks = [] of Int32
      end

      def peek(n : Int32 = 0) : Token
        ensure_buffer(n)
        @buffer[@index + n]
      end

      def next : Token
        token = peek(0)
        @index += 1
        compact
        token
      end

      def mark : Int32
        @marks << @index
        @index
      end

      def rewind(mark : Int32)
        if idx = @marks.rindex(mark)
          @marks.delete_at(idx)
        end
        @index = mark
      end

      def commit(mark : Int32)
        if idx = @marks.rindex(mark)
          @marks.delete_at(idx)
        end
        compact
      end

      private def ensure_buffer(n : Int32)
        target = @index + n
        while target >= @buffer.size
          @buffer << @lexer.next_token
        end
      end

      private def compact
        min_index = @marks.empty? ? @index : { @index, @marks.min }.min
        drop_before = min_index - @window
        return if drop_before <= 0
        @buffer = @buffer[drop_before, @buffer.size - drop_before]
        @index -= drop_before
        @marks.map! { |m| m - drop_before }
      end
    end
  end
end
