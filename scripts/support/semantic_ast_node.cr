class SemanticAstNode
  getter kind : String
  getter payload : String?
  getter flags : Array(String)
  getter children : Array(SemanticAstNode)

  def initialize(
    @kind : String,
    @children = [] of SemanticAstNode,
    @payload : String? = nil,
    @flags = [] of String,
  )
  end

  def render : String
    String.build do |io|
      io << @kind
      io << '[' << @payload.not_nil!.inspect << ']' if @payload
      unless @flags.empty?
        io << '{' << @flags.join('|') << '}'
      end
      unless @children.empty?
        io << '('
        @children.each_with_index do |child, index|
          io << ", " unless index == 0
          io << child.render
        end
        io << ')'
      end
    end
  end

  def ==(other : SemanticAstNode) : Bool
    @kind == other.kind && @payload == other.payload && @flags == other.flags && @children == other.children
  end

  def first_difference(other : SemanticAstNode, path = "root") : String?
    return "#{path}: kind #{@kind} != #{other.kind}" unless @kind == other.kind
    return "#{path}/#{@kind}: payload #{@payload.inspect} != #{other.payload.inspect}" unless @payload == other.payload
    return "#{path}/#{@kind}: flags #{@flags} != #{other.flags}" unless @flags == other.flags
    unless @children.size == other.children.size
      return "#{path}/#{@kind}: child count #{@children.size} != #{other.children.size}"
    end
    @children.each_with_index do |child, index|
      if difference = child.first_difference(other.children[index], "#{path}/#{@kind}[#{index}]")
        return difference
      end
    end
    nil
  end
end

class UnsupportedSemanticAst < Exception
  getter node_kind : String

  def initialize(@node_kind : String)
    super("unsupported semantic AST node: #{@node_kind}")
  end
end
