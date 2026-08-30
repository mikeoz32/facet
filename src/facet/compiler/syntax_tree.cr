require "set"

module Facet
  module Compiler
    enum PositionEncoding
      Utf8
      Utf16
    end

    record TextPosition, line : Int32, character : Int32

    # Immutable line map shared by syntax queries. Facet spans are byte based;
    # this class is the single conversion boundary for editor protocols.
    class LineIndex
      getter source : Source
      getter line_starts : Array(Int32)

      def initialize(@source : Source)
        @line_starts = [0] of Int32
        @source.bytes.each_with_index do |byte, index|
          @line_starts << index + 1 if byte == '\n'.ord.to_u8
        end
      end

      def position_at(byte_offset : Int32, encoding : PositionEncoding = PositionEncoding::Utf16) : TextPosition
        offset = byte_offset.clamp(0, @source.size)
        line = line_for(offset)
        line_start = @line_starts[line]
        character = if encoding == PositionEncoding::Utf8
                      offset - line_start
                    else
                      utf16_units(line_start, offset)
                    end
        TextPosition.new(line, character)
      end

      def offset_at(position : TextPosition, encoding : PositionEncoding = PositionEncoding::Utf16) : Int32
        return 0 if position.line < 0
        return @source.size if position.line >= @line_starts.size

        line_start = @line_starts[position.line]
        line_finish = content_finish(position.line)
        target = Math.max(position.character, 0)
        return Math.min(line_start + target, line_finish) if encoding == PositionEncoding::Utf8

        text = @source.text.byte_slice(line_start, line_finish - line_start)
        units = 0
        bytes = 0
        text.each_char do |char|
          width = char.ord > 0xffff ? 2 : 1
          break if units + width > target
          units += width
          bytes += char.to_s.bytesize
          break if units >= target
        end
        line_start + bytes
      end

      def line_text(line : Int32) : String
        return "" if line < 0 || line >= @line_starts.size
        start = @line_starts[line]
        finish = content_finish(line)
        @source.text.byte_slice(start, finish - start)
      end

      private def line_for(offset : Int32) : Int32
        low = 0
        high = @line_starts.size - 1
        while low <= high
          middle = (low + high) // 2
          if @line_starts[middle] <= offset
            low = middle + 1
          else
            high = middle - 1
          end
        end
        Math.max(high, 0)
      end

      private def content_finish(line : Int32) : Int32
        finish = line + 1 < @line_starts.size ? @line_starts[line + 1] : @source.size
        finish -= 1 if finish > @line_starts[line] && @source.bytes[finish - 1] == '\n'.ord.to_u8
        finish -= 1 if finish > @line_starts[line] && @source.bytes[finish - 1] == '\r'.ord.to_u8
        finish
      end

      private def utf16_units(start : Int32, finish : Int32) : Int32
        return 0 if finish <= start
        text = @source.text.byte_slice(start, finish - start)
        units = 0
        text.each_char { |char| units += char.ord > 0xffff ? 2 : 1 }
        units
      end
    end

    # Indexed facade over Facet's compact arena. It preserves the native AST
    # representation while giving LSP and compiler passes stable named queries
    # instead of making them depend on raw child positions.
    class SyntaxTree
      getter ast : AstFile
      getter line_index : LineIndex

      def initialize(@ast : AstFile)
        @line_index = LineIndex.new(@ast.source)
        @parents = Array(Array(NodeId)).new(@ast.arena.nodes.size) { [] of NodeId }
        @depths = Array(Int32).new(@ast.arena.nodes.size, -1)
        @reachable = [] of NodeId
        index_tree
      end

      def root : SyntaxNode
        SyntaxNode.new(self, @ast.root)
      end

      def node(node_id : NodeId) : SyntaxNode
        SyntaxNode.new(self, node_id)
      end

      def reachable_nodes : Array(SyntaxNode)
        @reachable.map { |node_id| node(node_id) }
      end

      def nodes(kind : NodeKind) : Array(SyntaxNode)
        @reachable.compact_map do |node_id|
          @ast.node(node_id).kind == kind ? node(node_id) : nil
        end
      end

      def parents(node_id : NodeId) : Array(SyntaxNode)
        @parents[node_id].map { |parent_id| node(parent_id) }
      end

      def parent(node_id : NodeId) : SyntaxNode?
        @parents[node_id].first?.try { |parent_id| node(parent_id) }
      end

      # Returns the smallest reachable semantic node containing the byte
      # offset. When nodes share a span, the deepest node wins.
      def node_at(byte_offset : Int32) : SyntaxNode?
        return nil unless byte_offset.in?(0..@ast.source.size)
        candidate = @reachable.select do |node_id|
          raw = @ast.node(node_id)
          next false if {NodeKind::Nop, NodeKind::Error}.includes?(raw.kind)
          contains?(raw.span, byte_offset)
        end.min_by? do |node_id|
          raw = @ast.node(node_id)
          {raw.span.length, -@depths[node_id], @ast.children(node_id).size}
        end
        candidate.try { |node_id| node(node_id) }
      end

      def position_at(byte_offset : Int32, encoding : PositionEncoding = PositionEncoding::Utf16) : TextPosition
        @line_index.position_at(byte_offset, encoding)
      end

      def offset_at(position : TextPosition, encoding : PositionEncoding = PositionEncoding::Utf16) : Int32
        @line_index.offset_at(position, encoding)
      end

      def qualified_name(node_id : NodeId) : String?
        raw = @ast.node(node_id)
        case raw.kind
        when NodeKind::Ident, NodeKind::Const, NodeKind::InstanceVar, NodeKind::ClassVar,
             NodeKind::Global, NodeKind::Param, NodeKind::Splat, NodeKind::DoubleSplat,
             NodeKind::BlockParam, NodeKind::NamedArg, NodeKind::MacroVar
          return nil unless raw.payload_index.in?(0...@ast.arena.symbols.entries.size)
          @ast.arena.symbols[raw.payload_index]
        when NodeKind::TypeApply
          child = @ast.children(node_id).first?
          child ? qualified_name(child) : nil
        when NodeKind::Path
          children = @ast.children(node_id)
          return nil if children.empty?
          names = children.compact_map { |child| qualified_name(child) }
          return nil if names.empty?
          return "::#{names[1..].join("::")}" if names.first == "::"
          separator = path_separator(children.first, children.last)
          names.join(separator)
        when NodeKind::Binary
          return nil unless raw.payload_index.in?(0...@ast.arena.operators.size)
          operator = @ast.arena.operator_kind(raw.payload_index)
          return nil unless {TokenKind::Dot, TokenKind::SafeNav, TokenKind::DoubleColon}.includes?(operator)
          children = @ast.children(node_id)
          return nil unless children.size == 2
          left = qualified_name(children[0])
          right = qualified_name(children[1])
          return nil unless left && right
          separator = operator == TokenKind::DoubleColon ? "::" : "."
          "#{left}#{separator}#{right}"
        else
          nil
        end
      end

      def leading_doc(node_id : NodeId) : String?
        start = @ast.node(node_id).span.start
        line = @line_index.position_at(start, PositionEncoding::Utf8).line - 1
        comments = [] of String
        while line >= 0
          text = @line_index.line_text(line)
          stripped = text.lstrip
          break unless stripped.starts_with?('#') && !stripped.starts_with?("#=")
          value = stripped.byte_slice(1, stripped.bytesize - 1)
          value = value.byte_slice(1, value.bytesize - 1) if value.starts_with?(' ')
          comments << value
          line -= 1
        end
        return nil if comments.empty?
        comments.reverse.join('\n')
      end

      private def index_tree : Nil
        seen = Set(NodeId).new
        stack = [{@ast.root, 0}]
        until stack.empty?
          node_id, depth = stack.pop
          @depths[node_id] = Math.max(@depths[node_id], depth)
          next if seen.includes?(node_id)
          seen << node_id
          @reachable << node_id
          children = @ast.children(node_id)
          children.each { |child| @parents[child] << node_id unless @parents[child].includes?(node_id) }
          children.reverse_each { |child| stack << {child, depth + 1} }
        end
      end

      private def contains?(span : Span, offset : Int32) : Bool
        return span.start == offset if span.length == 0
        span.start <= offset && (offset < span.finish || (offset == @ast.source.size && span.finish == offset))
      end

      private def path_separator(first : NodeId, last : NodeId) : String
        left = @ast.node(first).span.finish
        right = @ast.node(last).span.start
        return "::" if right <= left
        between = @ast.source.text.byte_slice(left, right - left)
        between.includes?('.') ? "." : "::"
      end
    end

    struct SyntaxNode
      getter tree : SyntaxTree
      getter id : NodeId

      def initialize(@tree : SyntaxTree, @id : NodeId)
      end

      def raw : Node
        @tree.ast.node(@id)
      end

      def kind : NodeKind
        raw.kind
      end

      def span : Span
        raw.span
      end

      def text : String
        @tree.ast.node_string(@id)
      end

      def children : Array(SyntaxNode)
        @tree.ast.children(@id).map { |child| @tree.node(child) }.to_a
      end

      def child(index : Int32) : SyntaxNode?
        child_id = @tree.ast.children(@id)[index]?
        child_id.try { |value| @tree.node(value) }
      end

      def parent : SyntaxNode?
        @tree.parent(@id)
      end

      def parents : Array(SyntaxNode)
        @tree.parents(@id)
      end

      def ancestors : Array(SyntaxNode)
        values = [] of SyntaxNode
        seen = Set(NodeId).new
        current = parent
        while current && !seen.includes?(current.id)
          seen << current.id
          values << current
          current = current.parent
        end
        values
      end

      def ancestor(kind : NodeKind) : SyntaxNode?
        ancestors.find { |candidate| candidate.kind == kind }
      end

      def descendants(kind : NodeKind? = nil) : Array(SyntaxNode)
        values = [] of SyntaxNode
        seen = Set(NodeId).new
        stack = children.reverse
        until stack.empty?
          current = stack.pop
          next if seen.includes?(current.id)
          seen << current.id
          values << current if kind.nil? || current.kind == kind
          stack.concat(current.children.reverse)
        end
        values
      end

      def symbol_name : String?
        @tree.qualified_name(@id)
      end

      def name_node : SyntaxNode?
        case kind
        when NodeKind::Def, NodeKind::MacroDef, NodeKind::Fun,
             NodeKind::Class, NodeKind::Module, NodeKind::Struct, NodeKind::Enum, NodeKind::Lib,
             NodeKind::Alias, NodeKind::TypeDef, NodeKind::AnnotationDef
          child(0)
        when NodeKind::Param
          candidate = child(children.size == 4 ? 1 : 0)
          candidate && candidate.kind != NodeKind::Nop ? candidate : nil
        when NodeKind::Splat, NodeKind::DoubleSplat, NodeKind::BlockParam,
             NodeKind::NamedArg, NodeKind::Ident, NodeKind::Const, NodeKind::InstanceVar,
             NodeKind::ClassVar, NodeKind::Global, NodeKind::MacroVar
          self
        else
          nil
        end
      end

      def external_name_node : SyntaxNode?
        kind == NodeKind::Param && children.size == 4 ? present_child(0) : nil
      end

      def external_name : String?
        external_name_node.try(&.symbol_name)
      end

      def external_name_span : Span?
        external_name_node.try(&.span)
      end

      def name : String?
        name_node.try(&.symbol_name)
      end

      def name_span : Span?
        target = name_node
        return nil unless target
        target = target.child(0) if target.kind == NodeKind::TypeApply
        return nil unless target
        node = target.raw
        if node.kind == NodeKind::NamedArg && node.payload_index.in?(0...@tree.ast.arena.symbols.entries.size)
          name = @tree.ast.arena.symbols[node.payload_index]
          return Span.new(node.span.start, Math.min(node.span.start + name.bytesize, node.span.finish))
        end
        if {NodeKind::Splat, NodeKind::DoubleSplat, NodeKind::BlockParam}.includes?(node.kind) &&
           node.payload_index.in?(0...@tree.ast.arena.symbols.entries.size)
          name = @tree.ast.arena.symbols[node.payload_index]
          prefix = node.kind == NodeKind::DoubleSplat ? 2 : 1
          start = Math.min(node.span.start + prefix, node.span.finish)
          return Span.new(start, Math.min(start + name.bytesize, node.span.finish))
        end
        if node.kind == NodeKind::MacroVar &&
           node.payload_index.in?(0...@tree.ast.arena.symbols.entries.size)
          name = @tree.ast.arena.symbols[node.payload_index]
          finish = node.span.finish
          return Span.new(Math.max(finish - name.bytesize, node.span.start), finish)
        end
        target.span
      end

      def body : SyntaxNode?
        index = case kind
                when NodeKind::Def, NodeKind::MacroDef                                                  then 3
                when NodeKind::Fun                                                                      then 4
                when NodeKind::Class, NodeKind::Module, NodeKind::Struct, NodeKind::Enum, NodeKind::Lib then 2
                when NodeKind::AnnotationDef                                                            then 1
                when NodeKind::Block, NodeKind::CallWithBlock                                           then 2
                when NodeKind::While, NodeKind::Until                                                   then 1
                when NodeKind::For                                                                      then 2
                when NodeKind::If, NodeKind::Unless                                                     then 1
                else                                                                                         -1
                end
        index >= 0 ? present_child(index) : nil
      end

      def condition : SyntaxNode?
        case kind
        when NodeKind::If, NodeKind::Unless, NodeKind::While, NodeKind::Until,
             NodeKind::Case, NodeKind::When
          present_child(0)
        else
          nil
        end
      end

      def parameters : Array(SyntaxNode)
        index = case kind
                when NodeKind::Def, NodeKind::MacroDef, NodeKind::Fun then 1
                when NodeKind::Block, NodeKind::CallWithBlock         then 0
                else                                                       -1
                end
        return [] of SyntaxNode if index < 0
        child(index).try(&.children) || [] of SyntaxNode
      end

      def return_type : SyntaxNode?
        {NodeKind::Def, NodeKind::MacroDef, NodeKind::Fun, NodeKind::Block}.includes?(kind) ? present_child(2) : nil
      end

      def superclass : SyntaxNode?
        {NodeKind::Class, NodeKind::Struct, NodeKind::Enum}.includes?(kind) ? present_child(1) : nil
      end

      def callee : SyntaxNode?
        case kind
        when NodeKind::Call, NodeKind::CallWithBlock
          child(0)
        when NodeKind::Binary
          member_access? ? child(1).try { |right| right.kind == NodeKind::Call ? right.callee : right } : nil
        else
          nil
        end
      end

      def call_name : String?
        callee.try(&.symbol_name)
      end

      def arguments : Array(SyntaxNode)
        if kind == NodeKind::Call
          child(1).try(&.children) || [] of SyntaxNode
        elsif kind == NodeKind::CallWithBlock
          callee.try(&.arguments) || [] of SyntaxNode
        elsif kind == NodeKind::Binary && member_access?
          child(1).try { |right| right.kind == NodeKind::Call ? right.arguments : [] of SyntaxNode } || [] of SyntaxNode
        else
          [] of SyntaxNode
        end
      end

      def named_arguments : Array(SyntaxNode)
        arguments.select { |argument| argument.kind == NodeKind::NamedArg }
      end

      def receiver : SyntaxNode?
        if kind == NodeKind::Binary
          return member_access? ? child(0) : nil
        end
        call = kind == NodeKind::CallWithBlock ? callee : self
        return nil unless call && call.kind == NodeKind::Call
        target = call.callee
        return nil unless target && target.kind == NodeKind::Binary
        operator = target.raw.payload_index
        return nil unless operator.in?(0...@tree.ast.arena.operators.size)
        kind = @tree.ast.arena.operator_kind(operator)
        {TokenKind::Dot, TokenKind::SafeNav, TokenKind::DoubleColon}.includes?(kind) ? target.child(0) : nil
      end

      def target : SyntaxNode?
        {NodeKind::Assign, NodeKind::VarDecl}.includes?(kind) ? child(0) : nil
      end

      def value : SyntaxNode?
        case kind
        when NodeKind::Assign, NodeKind::NamedArg
          child(kind == NodeKind::Assign ? 1 : 0)
        when NodeKind::VarDecl
          present_child(2)
        when NodeKind::Param
          present_child(children.size == 4 ? 3 : 2)
        else
          nil
        end
      end

      def declared_type : SyntaxNode?
        case kind
        when NodeKind::VarDecl
          present_child(1)
        when NodeKind::Param
          present_child(children.size == 4 ? 2 : 1)
        when NodeKind::Splat, NodeKind::DoubleSplat, NodeKind::BlockParam
          present_child(0)
        else
          nil
        end
      end

      def semantic_flag?(flag : SemanticFlag) : Bool
        raw.semantic_flag?(flag)
      end

      def doc : String?
        @tree.leading_doc(@id)
      end

      private def present_child(index : Int32) : SyntaxNode?
        value = child(index)
        value && value.kind != NodeKind::Nop ? value : nil
      end

      private def member_access? : Bool
        return false unless kind == NodeKind::Binary
        operator = raw.payload_index
        return false unless operator.in?(0...@tree.ast.arena.operators.size)
        {
          TokenKind::Dot,
          TokenKind::SafeNav,
          TokenKind::DoubleColon,
        }.includes?(@tree.ast.arena.operator_kind(operator))
      end
    end
  end
end
