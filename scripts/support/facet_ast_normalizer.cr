require "../../src/facet"
require "./semantic_ast_node"

module FacetAstNormalizer
  extend self

  SYMBOL_PAYLOAD_KINDS = {
    Facet::Compiler::NodeKind::Ident,
    Facet::Compiler::NodeKind::InstanceVar,
    Facet::Compiler::NodeKind::ClassVar,
    Facet::Compiler::NodeKind::Global,
    Facet::Compiler::NodeKind::NamedArg,
    Facet::Compiler::NodeKind::Param,
    Facet::Compiler::NodeKind::Splat,
    Facet::Compiler::NodeKind::DoubleSplat,
    Facet::Compiler::NodeKind::BlockParam,
    Facet::Compiler::NodeKind::MacroVar,
  }

  SEMANTIC_FLAGS = {
    Facet::Compiler::SemanticFlag::Abstract,
    Facet::Compiler::SemanticFlag::Private,
    Facet::Compiler::SemanticFlag::Protected,
    Facet::Compiler::SemanticFlag::Union,
    Facet::Compiler::SemanticFlag::Select,
    Facet::Compiler::SemanticFlag::Exhaustive,
    Facet::Compiler::SemanticFlag::RescueClause,
  }

  INFIX_METHODS = {
    "+"   => "Plus",
    "-"   => "Minus",
    "*"   => "Star",
    "/"   => "Slash",
    "//"  => "SlashSlash",
    "%"   => "Percent",
    "^"   => "Caret",
    "&"   => "Ampersand",
    "|"   => "Pipe",
    "**"  => "StarStar",
    "<<"  => "ShiftLeft",
    ">>"  => "ShiftRight",
    "=="  => "EqualEqual",
    "!="  => "BangEqual",
    "<"   => "Less",
    "<="  => "LessEqual",
    ">"   => "Greater",
    ">="  => "GreaterEqual",
    "===" => "TripleEqual",
    "<=>" => "Spaceship",
    "=~"  => "Match",
    "!~"  => "NotMatch",
    "&+"  => "AmpersandPlus",
    "&-"  => "AmpersandMinus",
    "&*"  => "AmpersandStar",
    "&**" => "AmpersandStarStar",
  }

  SPECIAL_PARAMETER_NAMES = {
    "abstract", "alias", "alignof", "annotation", "as", "asm", "begin", "break", "case", "class",
    "def", "do", "else", "elsif", "end", "ensure", "enum", "extend", "false", "for", "fun", "if",
    "in", "include", "instance_alignof", "instance_sizeof", "is_a?", "lib", "macro", "module", "next",
    "nil", "of", "offsetof", "out", "pointerof", "private", "protected", "require", "rescue",
    "responds_to?", "return", "select", "self", "sizeof", "struct", "super", "then", "true", "type",
    "typeof", "uninitialized", "union", "unless", "until", "verbatim", "when", "while", "with", "yield",
  }

  def normalize(ast : Facet::Compiler::AstFile) : SemanticAstNode
    root = normalize_node(ast, ast.root)
    if root.kind == "File" && root.children.size == 1
      expressions = root.children.first
      if expressions.kind == "Expressions" && expressions.children.size == 1 && expressions.children.first.kind == "Nop"
        return sem("File", [sem("Expressions")])
      end
    end
    root
  end

  private def normalize_node(ast : Facet::Compiler::AstFile, node_id : Facet::Compiler::NodeId) : SemanticAstNode
    node = ast.node(node_id)
    children = ast.children(node_id).map { |child| normalize_node(ast, child) }.to_a
    payload = if SYMBOL_PAYLOAD_KINDS.includes?(node.kind) && node.payload_index >= 0
                ast.arena.symbols[node.payload_index]
              elsif node.kind == Facet::Compiler::NodeKind::Unary || node.kind == Facet::Compiler::NodeKind::Binary
                ast.arena.operator_kind(node.payload_index).to_s
              elsif node.kind == Facet::Compiler::NodeKind::MacroControl
                ast.macro_control_tag(node_id).to_s
              end
    flags = SEMANTIC_FLAGS.select { |flag| node.semantic_flag?(flag) }.map(&.to_s)
    storage = node.flags & 0x00ff_u16
    storage = 0_u16 if node.kind == Facet::Compiler::NodeKind::Tuple
    storage = 0_u16 if node.kind == Facet::Compiler::NodeKind::CallWithBlock
    if node.kind == Facet::Compiler::NodeKind::Asm
      flags << "Volatile" if (storage & 0x01) != 0
      flags << "AlignStack" if (storage & 0x02) != 0
      flags << "Intel" if (storage & 0x04) != 0
      flags << "CanThrow" if (storage & 0x08) != 0
    else
      flags << "Storage#{storage}" unless storage == 0
    end
    if node.kind == Facet::Compiler::NodeKind::Call && ast.children(node_id).size == 2
      args = ast.node(ast.children(node_id)[1])
      if args.kind == Facet::Compiler::NodeKind::Args && args.span.length > 0 &&
         ast.source.bytes[args.span.start] == '('.ord.to_u8
        flags << "InternalExplicitParens"
      end
    end
    if node.kind == Facet::Compiler::NodeKind::Unary && payload == "Star" && ast.children(node_id).size == 1
      child = ast.node(ast.children(node_id).first)
      flags << "InternalPrefixStar" if node.span.start < child.span.start
    end
    if node.kind == Facet::Compiler::NodeKind::StringInterpolation && ast.node_string(node_id).starts_with?("<<")
      raw_children = ast.children(node_id).to_a
      closing_indent = heredoc_closing_indent(ast.node_string(node_id))
      while !children.empty? && children.first.kind == "LiteralString" &&
            ignorable_heredoc_leading_literal?(ast.node_string(raw_children.first), closing_indent)
        children.shift
        raw_children.shift
      end
      while !children.empty? && children.last.kind == "LiteralString" && ast.node_string(raw_children.last).strip.empty?
        children.pop
        raw_children.pop
      end
    end
    canonicalize(SemanticAstNode.new(node.kind.to_s, children, payload, flags))
  end

  private def canonicalize(node : SemanticAstNode) : SemanticAstNode
    if node.kind == "Path" && node.children.size == 2
      left = node.children[0]
      right = node.children[1]
      if left.kind == "Ident" && left.payload == "::"
        return add_global_root(right)
      end
    end

    if node.kind == "Def" && node.children.size == 5
      return expand_sigil_parameters(node)
    end

    if node.kind == "MacroControl" && node.payload == "KeywordElsif"
      return sem("MacroControl", node.children, "KeywordIf", node.flags)
    end

    if node.kind == "Expressions" && node.children.size == 1 &&
       node.children.first.kind == "Expressions" && node.children.first.children.empty?
      return sem("Expressions", [sem("Nop")])
    end

    if node.kind == "Expressions"
      flattened = [] of SemanticAstNode
      node.children.each do |child|
        child.kind == "Expressions" ? flattened.concat(child.children) : flattened << child
      end
      return sem(node.kind, collapse_macro_literal_runs(flattened), node.payload, node.flags)
    end

    if node.kind == "StringInterpolation"
      children = collapse_literal_runs(node.children)
      return sem("LiteralString") if children.all? { |child| child.kind == "LiteralString" }
      return sem(node.kind, children, node.payload, node.flags)
    end

    if node.kind == "Annotation" && node.children.size == 2
      value = node.children[0]
      if value.kind == "Call" && value.children.size == 2 && value.children[1].kind == "Args" && value.children[1].children.empty?
        return sem("Annotation", [value.children[0], node.children[1]], node.payload, node.flags)
      end
    end

    if {"Splat", "DoubleSplat"}.includes?(node.kind) && node.children.size == 1
      child = normalize_splat_restriction(node.children.first)
      return sem(node.kind, [child], node.payload, node.flags) unless child == node.children.first
    end

    if node.kind == "Destructure"
      children = node.children.map do |child|
        if {"Splat", "DoubleSplat"}.includes?(child.kind) && child.children.size == 1
          sem(child.kind, child.children, flags: child.flags)
        else
          child
        end
      end
      return sem("Destructure", children, node.payload, node.flags) unless children == node.children
    end

    if node.kind == "Module" && !node.children.empty?
      module_name = node.children.first
      if module_name.kind == "TypeApply" && module_name.children.size == 2 && module_name.children[1].kind == "Args"
        args = module_name.children[1].children.map do |arg|
          arg.kind == "Splat" && arg.children.size == 1 ? arg.children.first : arg
        end
        children = node.children.dup
        children[0] = sem("TypeApply", [module_name.children[0], sem("Args", args)])
        return sem(node.kind, children, node.payload, node.flags)
      end
    end

    if node.kind == "If" && node.children.size == 3
      else_node = node.children[2]
      if else_node.kind == "Expressions"
        replacement = if else_node.children.empty?
                        sem("Nop")
                      elsif else_node.children.size == 1 && {"If", "Unless", "Ternary"}.includes?(else_node.children.first.kind)
                        else_node.children.first
                      end
        return sem("If", [node.children[0], node.children[1], replacement.not_nil!], node.payload, node.flags) if replacement
      end
    end

    if node.kind == "Unless" && node.children.size == 3
      else_node = node.children[2]
      if else_node.kind == "Expressions" && else_node.children.empty?
        return sem("Unless", [node.children[0], node.children[1], sem("Nop")], node.payload, node.flags)
      end
    end

    if node.kind == "TypeApply" && node.children.size == 2 && node.children[1].kind == "Args"
      args = node.children[1].children.map { |arg| normalize_splat_restriction(arg) }
      return sem("TypeApply", [node.children[0], sem("Args", args)], node.payload, node.flags) unless args == node.children[1].children
    end

    if node.kind == "MacroControl" && node.children.size == 3
      else_node = node.children[2]
      if else_node.kind == "Expressions" && else_node.children.size == 1 && else_node.children.first.kind == "MacroControl"
        return sem("MacroControl", [node.children[0], node.children[1], else_node.children.first], node.payload, node.flags)
      end
    end

    if node.kind == "ProcType" && node.children.size == 2 && node.children.first.kind == "Args"
      args = node.children.first.children.map { |arg| normalize_splat_restriction(arg) }
      return sem("ProcType", [sem("Args", args), node.children[1]], node.payload, node.flags)
    end

    if node.kind == "Assign" && node.children.size == 2
      target = node.children[0]
      if target.kind == "CallWithBlock" && target.children.size == 3 && target.children[0].kind == "Index"
        return sem("Assign", [target.children[0], node.children[1]], node.payload, node.flags)
      end
      if target.kind == "Binary" && target.payload == "Dot" && target.children.size == 2
        member = target.children[1]
        if member.kind == "Ident" && (name = member.payload)
          return sem("Assign", [sem("Index", [target.children[0]]), node.children[1]]) if name == "[]"
          setter = sem("Call", [sem("Ident", payload: "#{name}="), sem("Args", [node.children[1]])])
          return sem("Binary", [target.children[0], setter], payload: "Dot")
        end
      end
    end

    if node.kind == "Binary" && node.payload == "Dot" && node.children.size == 2
      receiver = node.children[0]
      member = node.children[1]
      if member.kind == "Call" && member.children.size == 2 && member.children[1].kind == "Args" && member.children[1].children.empty?
        callee = member.children[0]
        if callee.kind == "Ident" && callee.payload == "nil?"
          return sem("Binary", [receiver, callee], payload: "Dot")
        end
        if callee.kind == "Ident" && {"!", "~"}.includes?(callee.payload)
          if receiver.kind == "Ident" && receiver.payload == "."
            operator = callee.payload == "!" ? "Bang" : "Tilde"
            return sem("Unary", [receiver], payload: operator)
          end
          return sem("Binary", [receiver, callee], payload: "Dot")
        end
      end
      if receiver.kind == "Ident" && receiver.payload == "self"
        if member.kind == "Call" && member.children.first?.try(&.payload).in?({"is_a?", "responds_to?"})
          return member
        elsif member.kind == "Ident" && member.payload == "nil?"
          return member
        end
      end
    end

    if node.kind == "Ident" && (name = node.payload) && name.starts_with?('.') && name.size > 1
      return normalize_implicit_ident(name)
    end

    if node.kind == "Unary" && {"AmpersandPlus", "AmpersandMinus"}.includes?(node.payload) && node.children.size == 1
      method = node.payload == "AmpersandPlus" ? "&+" : "&-"
      return sem("Binary", [node.children.first, sem("Ident", payload: method)], payload: "Dot")
    end

    if node.kind == "Call" && node.children.size == 2
      callee = node.children[0]
      args = node.children[1]
      if args.kind == "Args"
        if args.children.empty? && callee.kind == "Unary" && {"Bang", "Tilde"}.includes?(callee.payload) &&
           callee.children.first?.try(&.payload) == "."
          return callee
        end
        if args.children.empty? && callee.kind == "Ident"
          return callee if callee.payload == "nil?"
          return normalize_implicit_ident(callee.payload.not_nil!) if callee.payload.try(&.in?({".!", ".~"}))
        end
        if index = args.children.index { |arg| shorthand_arg_expression(arg) }
          return normalize_shorthand_block(callee, args, index, node.flags.includes?("InternalExplicitParens"))
        end
        if callee.kind == "Ident" && (name = callee.payload) && name.starts_with?('.')
          return normalize_implicit_call(name, args)
        end
        if callee.kind == "Binary" && callee.payload == "Dot" && callee.children.size == 2
          left = callee.children[0]
          right = callee.children[1]
          if left.kind == "Ident" && left.payload == "." && right.kind == "Ident" && (method = right.payload)
            return normalize_implicit_call(".#{method}", args)
          end
          if right.kind == "Ident" && right.payload == "[]"
            return sem("Index", [left] + args.children)
          end
          if right.kind == "Ident" && right.payload == "[]?"
            return sem("Index", [left] + args.children, flags: ["Storage1"])
          end
          if right.kind == "Ident" && right.payload == "[]=" && !args.children.empty?
            return sem("Assign", [sem("Index", [left] + args.children[0...-1]), args.children.last])
          end
          if right.kind == "Ident" && (operator = INFIX_METHODS[right.payload]?) && args.children.size == 1 &&
             args.children.first.kind != "NamedArg" &&
             !(args.children.first.kind == "Unary" && args.children.first.payload == "Ampersand")
            return sem("Binary", [left, args.children.first], payload: operator)
          end
          if right.kind == "Ident" && {"~", "!"}.includes?(right.payload) && args.children.empty?
            return callee
          end
          return callee if right.kind == "Ident" && right.payload == "nil?" && args.children.empty?
          member = sem("Call", [callee.children[1], args])
          return sem("Binary", [callee.children[0], member], payload: "Dot")
        end
      end
      return sem(node.kind, node.children, node.payload, node.flags.reject(&.==("InternalExplicitParens")))
    end

    if node.kind == "Index" && node.children.size > 1
      if index = node.children[1..].index { |arg| shorthand_arg_expression(arg) }
        actual_index = index + 1
        shorthand = shorthand_arg_expression(node.children[actual_index]).not_nil!
        remaining = node.children.dup
        remaining.delete_at(actual_index)
        block_var = sem("Ident", payload: "__arg0")
        body = apply_shorthand(block_var, shorthand)
        return sem("CallWithBlock", [sem("Index", remaining, node.payload, node.flags), sem("Args", [block_var]), sem("Expressions", [body])])
      end
    end

    if node.kind == "CallWithBlock" && node.children.size == 3
      call = node.children[0]
      return call if call.kind == "Assign"
      body = node.children[2]
      if body.kind == "Expressions" && body.children.size == 1 && body.children.first.kind == "Begin"
        return sem("CallWithBlock", [call, node.children[1], body.children.first], node.payload, node.flags)
      end
      if call.kind == "Binary" && call.payload == "Dot" && call.children.size == 2
        with_block = sem("CallWithBlock", [call.children[1], node.children[1], node.children[2]])
        return sem("Binary", [call.children[0], with_block], payload: "Dot")
      end
      if call.kind == "Call" && call.children.size == 2
        callee = call.children[0]
        if callee.kind == "Binary" && callee.payload == "Dot" && callee.children.size == 2
          member_call = sem("Call", [callee.children[1], call.children[1]])
          with_block = sem("CallWithBlock", [member_call, node.children[1], node.children[2]])
          return sem("Binary", [callee.children[0], with_block], payload: "Dot")
        end
      end
    end

    node
  end

  private def add_global_root(node : SemanticAstNode) : SemanticAstNode
    if node.kind == "Path" && !node.children.empty?
      children = node.children.dup
      children[0] = add_global_root(children[0])
      sem(node.kind, children, node.payload, node.flags)
    else
      sem(node.kind, node.children, node.payload, node.flags + ["GlobalRoot"])
    end
  end

  private def expand_sigil_parameters(node : SemanticAstNode) : SemanticAstNode
    params = node.children[1]
    body = node.children[3]
    return node unless params.kind == "Args"
    assignments = [] of SemanticAstNode
    normalized_params = params.children.map_with_index do |parameter, index|
      normalized, assignment = expand_sigil_parameter(parameter, index)
      assignments << assignment if assignment
      normalized
    end
    return node if assignments.empty?

    if body.kind == "Expressions"
      body = sem("Expressions", assignments + body.children)
    elsif body.kind == "Begin" && !body.children.empty? && body.children.first.kind == "Expressions"
      first = sem("Expressions", assignments + body.children.first.children)
      body = sem("Begin", [first] + body.children[1..], body.payload, body.flags)
    end
    children = node.children.dup
    children[1] = sem("Args", normalized_params)
    children[3] = body
    sem(node.kind, children, node.payload, node.flags)
  end

  private def expand_sigil_parameter(node : SemanticAstNode, index : Int32) : Tuple(SemanticAstNode, SemanticAstNode?)
    if node.kind == "Annotation" && node.children.size == 2
      parameter, assignment = expand_sigil_parameter(node.children[1], index)
      return {sem("Annotation", [node.children[0], parameter], node.payload, node.flags), assignment}
    end
    name = node.payload
    return {node, nil} unless {"Param", "BlockParam"}.includes?(node.kind) && name && name.starts_with?('@')

    stripped = name.starts_with?("@@") ? name[2..] : name[1..]
    children = node.children.dup
    internal_name = SPECIAL_PARAMETER_NAMES.includes?(stripped) ? "__arg#{index}" : stripped
    if node.kind == "BlockParam"
      parameter = sem("BlockParam", children, internal_name, node.flags)
      target_kind = name.starts_with?("@@") ? "ClassVar" : "InstanceVar"
      assignment = sem("Assign", [sem(target_kind, payload: name), sem("Ident", payload: internal_name)])
      return {parameter, assignment}
    elsif children.size == 4
      children[1] = sem("Ident", payload: internal_name)
    elsif internal_name == stripped
      children[0] = sem("Ident", payload: internal_name) unless children.empty?
    else
      children = [sem("Ident", payload: stripped), sem("Ident", payload: internal_name)] + children[1..]
    end
    parameter = sem("Param", children, internal_name, node.flags)
    target_kind = name.starts_with?("@@") ? "ClassVar" : "InstanceVar"
    assignment = sem("Assign", [sem(target_kind, payload: name), sem("Ident", payload: internal_name)])
    {parameter, assignment}
  end

  private def normalize_splat_restriction(node : SemanticAstNode) : SemanticAstNode
    return node unless node.kind == "Unary" && node.payload == "Star" && node.children.size == 1
    child = node.children.first
    if child.kind == "Unary" && child.payload == "Star" && child.children.size == 1 && child.flags.includes?("InternalPrefixStar")
      sem("DoubleSplat", [child.children.first])
    elsif node.flags.includes?("InternalPrefixStar")
      sem("Splat", [child])
    else
      node
    end
  end

  private def heredoc_closing_indent(text : String) : Int32
    finish = text.bytesize
    finish -= 1 if finish > 0 && text.byte_at(finish - 1) == '\n'.ord
    finish -= 1 if finish > 0 && text.byte_at(finish - 1) == '\r'.ord
    line_start = text.rindex('\n', finish - 1).try { |index| index + 1 } || 0
    indent = 0
    while line_start + indent < finish
      byte = text.byte_at(line_start + indent)
      break unless byte == ' '.ord || byte == '\t'.ord
      indent += 1
    end
    indent
  end

  private def ignorable_heredoc_leading_literal?(text : String, closing_indent : Int32) : Bool
    return false unless text.strip.empty?
    return false if text.includes?('\n') || text.includes?('\r')
    text.bytesize <= closing_indent
  end

  private def normalize_shorthand_block(
    callee : SemanticAstNode,
    args : SemanticAstNode,
    index : Int32,
    explicit_parens : Bool,
  ) : SemanticAstNode
    shorthand = shorthand_arg_expression(args.children[index]).not_nil!
    remaining = [] of SemanticAstNode
    args.children.each_with_index { |arg, arg_index| remaining << arg unless arg_index == index }
    call = if remaining.empty?
             explicit_parens ? sem("Call", [callee, sem("Args")]) : callee
           else
             sem("Call", [callee, sem("Args", remaining)])
           end
    block_var = sem("Ident", payload: "__arg0")
    body = apply_shorthand(block_var, shorthand)
    sem("CallWithBlock", [call, sem("Args", [block_var]), sem("Expressions", [body])])
  end

  private def apply_shorthand(receiver : SemanticAstNode, expression : SemanticAstNode) : SemanticAstNode
    if expression.kind == "Ident"
      return receiver if expression.payload == "."
      return sem("Unary", [receiver], payload: "Bang") if expression.payload == "!"
      return sem("Binary", [receiver, expression], payload: "Dot")
    end
    if expression.kind == "Binary" && expression.children.size == 2
      if expression.payload == "Dot"
        left = apply_shorthand(receiver, expression.children[0])
        return sem("Binary", [left, expression.children[1]], payload: "Dot")
      end
      left = apply_shorthand(receiver, expression.children[0])
      return sem("Binary", [left, expression.children[1]], payload: expression.payload, flags: expression.flags)
    end
    if expression.kind == "Index" && !expression.children.empty?
      base = apply_shorthand(receiver, expression.children[0])
      return sem("Index", [base] + expression.children[1..], expression.payload, expression.flags)
    end
    if expression.kind == "Assign" && expression.children.size == 2
      target = expression.children[0]
      value = expression.children[1]
      if target.kind == "Ident" && (name = target.payload)
        setter = sem("Call", [sem("Ident", payload: "#{name}="), sem("Args", [value])])
        return sem("Binary", [receiver, setter], payload: "Dot")
      elsif target.kind == "Index" && !target.children.empty?
        base = apply_shorthand(receiver, target.children[0])
        return sem("Assign", [sem("Index", [base] + target.children[1..], target.payload, target.flags), value])
      end
    end
    if expression.kind == "Call" && expression.children.size == 2
      callee = expression.children[0]
      args = expression.children[1]
      if callee.kind == "Ident" && args.kind == "Args" && (name = callee.payload)
        if operator = INFIX_METHODS[name]?
          return sem("Binary", [receiver, args.children.first], payload: operator) if args.children.size == 1
        elsif name == "[]"
          return sem("Index", [receiver] + args.children)
        elsif name == "[]=" && !args.children.empty?
          return sem("Assign", [sem("Index", [receiver] + args.children[0...-1]), args.children.last])
        end
        return sem("Binary", [receiver, expression], payload: "Dot")
      end
    end
    sem("Binary", [receiver, expression], payload: "Dot")
  end

  private def shorthand_arg_expression(node : SemanticAstNode) : SemanticAstNode?
    if node.kind == "Unary" && node.payload == "SafeNav" && node.children.size == 1
      return node.children.first
    end
    if node.kind == "Assign" && node.children.size == 2
      target = node.children[0]
      if target.kind == "Unary" && target.payload == "SafeNav" && target.children.size == 1
        return sem("Assign", [target.children.first, node.children[1]])
      end
    end
    if node.kind == "Binary" && node.children.size == 2
      if left = shorthand_arg_expression(node.children[0])
        return sem("Binary", [left, node.children[1]], node.payload, node.flags)
      end
    end
    if node.kind == "Index" && !node.children.empty?
      if base = shorthand_arg_expression(node.children[0])
        return sem("Index", [base] + node.children[1..], node.payload, node.flags)
      end
    end
    nil
  end

  private def normalize_implicit_ident(name : String) : SemanticAstNode
    receiver = sem("Ident", payload: ".")
    method = name[1..]
    return sem("Unary", [receiver], payload: "Bang") if method == "!"
    sem("Binary", [receiver, sem("Ident", payload: method)], payload: "Dot")
  end

  private def normalize_implicit_call(name : String, args : SemanticAstNode) : SemanticAstNode
    receiver = sem("Ident", payload: ".")
    method = name[1..]
    if args.children.empty? && {"!", "~"}.includes?(method)
      operator = method == "!" ? "Bang" : "Tilde"
      return sem("Unary", [receiver], payload: operator)
    end
    if operator = INFIX_METHODS[method]?
      return sem("Binary", [receiver, args.children.first], payload: operator) if args.children.size == 1
    elsif method == "[]"
      return sem("Index", [receiver] + args.children)
    elsif method == "[]=" && !args.children.empty?
      return sem("Assign", [sem("Index", [receiver] + args.children[0...-1]), args.children.last])
    end
    sem("Binary", [receiver, sem("Call", [sem("Ident", payload: method), args])], payload: "Dot")
  end

  private def collapse_literal_runs(children : Array(SemanticAstNode)) : Array(SemanticAstNode)
    collapsed = [] of SemanticAstNode
    children.each do |child|
      next if child.kind == "LiteralString" && collapsed.last?.try(&.kind) == "LiteralString"
      collapsed << child
    end
    collapsed
  end

  private def collapse_macro_literal_runs(children : Array(SemanticAstNode)) : Array(SemanticAstNode)
    collapsed = [] of SemanticAstNode
    children.each do |child|
      next if child.kind == "MacroLiteral" && collapsed.last?.try(&.kind) == "MacroLiteral"
      collapsed << child
    end
    collapsed
  end

  private def sem(
    kind : String,
    children = [] of SemanticAstNode,
    payload : String? = nil,
    flags = [] of String,
  ) : SemanticAstNode
    SemanticAstNode.new(kind, children.to_a, payload, flags)
  end
end
