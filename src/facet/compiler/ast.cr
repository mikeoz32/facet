module Facet
  module Compiler
    alias NodeId = Int32
    alias SymbolId = Int32
    alias OperatorId = Int32
    alias LiteralId = Int32

    enum NodeKind
      File
      Expressions
      Error
      Nop
      Ident
      Const
      InstanceVar
      ClassVar
      Global
      LiteralNumber
      LiteralString
      LiteralChar
      LiteralRegex
      LiteralSymbol
      LiteralBool
      LiteralNil
      Call
      Assign
      Unary
      Binary
      Block
      Args
      NamedArg
      Param
      Splat
      DoubleSplat
      BlockParam
      If
      Unless
      While
      Until
      Case
      When
      For
      Break
      Next
      Return
      Yield
      Def
      MacroDef
      Class
      Module
      Struct
      Enum
      Lib
      Fun
      Alias
      TypeDef
      Begin
      Rescue
      Ensure
      Array
      Hash
      Tuple
      NamedTuple
      Range
      Index
      CallWithBlock
      Path
      TypeApply
      MacroExpr
      MacroControl
      MacroForHeader
      MacroVar
      Require
    end

    enum LiteralKind
      Number
      String
      Char
      Regex
      Symbol
    end

    struct LiteralPayload
      getter kind : LiteralKind

      def initialize(@kind : LiteralKind)
      end
    end

    struct Node
      getter kind : NodeKind
      getter span : Span
      getter first_child : Int32
      getter child_count : Int16
      getter flags : UInt16
      getter payload_index : Int32

      def initialize(
        @kind : NodeKind,
        @span : Span,
        @first_child : Int32,
        @child_count : Int16,
        @flags : UInt16,
        @payload_index : Int32
      )
      end
    end

    class SymbolTable
      getter entries : Array(String)

      def initialize
        @entries = [] of String
        @index = {} of String => SymbolId
      end

      def intern(value : String) : SymbolId
        if id = @index[value]?
          return id
        end
        id = @entries.size.to_i32
        @entries << value
        @index[value] = id
        id
      end

      def [](id : SymbolId) : String
        @entries[id]
      end
    end

    class AstArena
      getter nodes : Array(Node)
      getter edges : Array(NodeId)
      getter symbols : SymbolTable
      getter literals : Array(LiteralPayload)
      getter operators : Array(TokenKind)

      def initialize
        @nodes = [] of Node
        @edges = [] of NodeId
        @symbols = SymbolTable.new
        @literals = [] of LiteralPayload
        @operators = [] of TokenKind
      end

      def node(node_id : NodeId) : Node
        @nodes[node_id]
      end

      def add_node(
        kind : NodeKind,
        span : Span,
        children : Array(NodeId) = [] of NodeId,
        payload_index : Int32 = -1,
        flags : UInt16 = 0_u16
      ) : NodeId
        first_child = @edges.size
        children.each { |child| @edges << child }
        node = Node.new(
          kind,
          span,
          first_child,
          children.size.to_i16,
          flags,
          payload_index
        )
        @nodes << node
        (@nodes.size - 1).to_i32
      end

      def children(node_id : NodeId) : Slice(NodeId)
        node = @nodes[node_id]
        return Slice(NodeId).new(0) if node.child_count == 0
        Slice(NodeId).new(@edges.to_unsafe + node.first_child, node.child_count.to_i32)
      end

      def add_operator(kind : TokenKind) : OperatorId
        id = @operators.size.to_i32
        @operators << kind
        id
      end

      def operator_kind(id : OperatorId) : TokenKind
        @operators[id]
      end

      def add_literal(kind : LiteralKind) : LiteralId
        id = @literals.size.to_i32
        @literals << LiteralPayload.new(kind)
        id
      end

      def literal(id : LiteralId) : LiteralPayload
        @literals[id]
      end

      def add_ident(span : Span, symbol_id : SymbolId) : NodeId
        add_node(NodeKind::Ident, span, payload_index: symbol_id)
      end

      def add_literal_node(kind : LiteralKind, span : Span) : NodeId
        literal_id = add_literal(kind)
        node_kind = case kind
                    when LiteralKind::Number then NodeKind::LiteralNumber
                    when LiteralKind::String then NodeKind::LiteralString
                    when LiteralKind::Char   then NodeKind::LiteralChar
                    when LiteralKind::Regex  then NodeKind::LiteralRegex
                    when LiteralKind::Symbol then NodeKind::LiteralSymbol
                    else
                      NodeKind::Error
                    end
        add_node(node_kind, span, payload_index: literal_id)
      end

      def add_unary(op : TokenKind, span : Span, expr : NodeId) : NodeId
        op_id = add_operator(op)
        add_node(NodeKind::Unary, span, [expr], payload_index: op_id)
      end

      def add_binary(op : TokenKind, span : Span, left : NodeId, right : NodeId) : NodeId
        op_id = add_operator(op)
        add_node(NodeKind::Binary, span, [left, right], payload_index: op_id)
      end

      def add_named_arg(name_id : SymbolId, span : Span, value : NodeId) : NodeId
        add_node(NodeKind::NamedArg, span, [value], payload_index: name_id)
      end

      def macro_control_tag(node_id : NodeId) : TokenKind
        node = @nodes[node_id]
        return TokenKind::Unknown unless node.kind == NodeKind::MacroControl
        value = node.payload_index
        return TokenKind::Unknown if value < 0 || value >= TokenKind.values.size
        TokenKind.new(value)
      end

      def macro_for_header_parts(node_id : NodeId) : Tuple(NodeId, NodeId)?
        node = @nodes[node_id]
        return nil unless node.kind == NodeKind::MacroForHeader
        slice = children(node_id)
        return nil if slice.size < 2
        {slice[0], slice[1]}
      end

      def macro_for_targets(node_id : NodeId) : Slice(NodeId)?
        parts = macro_for_header_parts(node_id)
        return nil unless parts
        children(parts[0])
      end

      def set_ident_symbol(node_id : NodeId, symbol_id : SymbolId)
        node = @nodes[node_id]
        return unless node.kind == NodeKind::Ident
        @nodes[node_id] = Node.new(
          node.kind,
          node.span,
          node.first_child,
          node.child_count,
          node.flags,
          symbol_id
        )
      end
    end

    struct AstFile
      getter source : Source
      getter root : NodeId
      getter arena : AstArena
      getter diagnostics : Array(Diagnostic)

      def initialize(
        @source : Source,
        @root : NodeId,
        @arena : AstArena,
        @diagnostics : Array(Diagnostic)
      )
      end

      def node(node_id : NodeId) : Node
        @arena.node(node_id)
      end

      def children(node_id : NodeId) : Slice(NodeId)
        @arena.children(node_id)
      end

      def node_text(node_id : NodeId) : Slice(UInt8)
        span = @arena.node(node_id).span
        return Slice(UInt8).new(0) if span.length == 0
        @source.bytes[span.start, span.length]
      end

      def node_string(node_id : NodeId) : String
        String.new(node_text(node_id))
      end

      def macro_control_tag(node_id : NodeId) : TokenKind
        @arena.macro_control_tag(node_id)
      end

      def macro_for_header_parts(node_id : NodeId) : Tuple(NodeId, NodeId)?
        @arena.macro_for_header_parts(node_id)
      end

      def macro_for_targets(node_id : NodeId) : Slice(NodeId)?
        @arena.macro_for_targets(node_id)
      end
    end
  end
end
