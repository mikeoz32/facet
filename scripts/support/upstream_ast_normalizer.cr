require "compiler/crystal/syntax"
require "./semantic_ast_node"

module UpstreamAstNormalizer
  extend self

  INFIX_OPERATORS = {
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

  OP_ASSIGN_OPERATORS = {
    "+"   => "PlusEqual",
    "-"   => "MinusEqual",
    "*"   => "StarEqual",
    "/"   => "SlashEqual",
    "//"  => "SlashSlashEqual",
    "%"   => "PercentEqual",
    "|"   => "PipeEqual",
    "&"   => "AmpersandEqual",
    "^"   => "CaretEqual",
    "**"  => "StarStarEqual",
    "<<"  => "ShiftLeftEqual",
    ">>"  => "ShiftRightEqual",
    "&+"  => "AmpersandPlusEqual",
    "&-"  => "AmpersandMinusEqual",
    "&*"  => "AmpersandStarEqual",
    "&**" => "AmpersandStarStarEqual",
    "||"  => "OrOrEqual",
    "&&"  => "AndAndEqual",
  }

  def normalize(node : Crystal::ASTNode) : SemanticAstNode
    SemanticAstNode.new("File", [as_expressions(node)])
  end

  private def n(
    kind : String,
    children = [] of SemanticAstNode,
    payload : String? = nil,
    flags = [] of String,
  ) : SemanticAstNode
    SemanticAstNode.new(kind, children.to_a, payload, flags)
  end

  private def as_expressions(node : Crystal::ASTNode) : SemanticAstNode
    normalized = normalize_node(node)
    return normalized if normalized.kind == "Expressions"
    return n("Expressions") if normalized.kind == "Nop"
    n("Expressions", [normalized])
  end

  private def optional(node : Crystal::ASTNode?) : SemanticAstNode
    node ? normalize_node(node) : n("Nop")
  end

  private def normalize_node(node : Crystal::ASTNode) : SemanticAstNode
    case node
    when Crystal::Nop
      n("Nop")
    when Crystal::Expressions
      children = collapse_macro_literal_runs(node.expressions.map { |child| normalize_node(child) })
      if node.keyword.paren?
        children.size == 1 ? children.first : n("Expressions", children)
      elsif node.keyword.begin?
        body = n("Expressions", children)
        n("Begin", [body, n("Nop"), n("Nop"), n("Nop")])
      else
        n("Expressions", children)
      end
    when Crystal::NilLiteral
      n("LiteralNil")
    when Crystal::BoolLiteral
      flags = node.value ? ["Storage1"] : [] of String
      n("LiteralBool", flags: flags)
    when Crystal::NumberLiteral
      n("LiteralNumber")
    when Crystal::CharLiteral
      n("LiteralChar")
    when Crystal::StringLiteral
      n("LiteralString")
    when Crystal::StringInterpolation
      children = collapse_literal_runs(node.expressions.map { |expression| normalize_node(expression) })
      children.all? { |child| child.kind == "LiteralString" } ? n("LiteralString") : n("StringInterpolation", children)
    when Crystal::SymbolLiteral
      n("LiteralSymbol")
    when Crystal::RegexLiteral
      node.value.is_a?(Crystal::StringInterpolation) ? n("LiteralRegex", [normalize_node(node.value)]) : n("LiteralRegex")
    when Crystal::Var
      node.name.starts_with?('$') ? n("Global", payload: node.name) : n("Ident", payload: node.name)
    when Crystal::InstanceVar
      n("InstanceVar", payload: node.name)
    when Crystal::ClassVar
      n("ClassVar", payload: node.name)
    when Crystal::Global
      n("Global", payload: node.name)
    when Crystal::Self
      n("Ident", payload: "self")
    when Crystal::ImplicitObj
      n("Ident", payload: ".")
    when Crystal::Underscore
      n("Ident", payload: "_")
    when Crystal::Path
      normalize_path(node)
    when Crystal::ArrayLiteral
      normalize_array(node)
    when Crystal::HashLiteral
      normalize_hash(node)
    when Crystal::NamedTupleLiteral
      entries = node.entries.map do |entry|
        n("NamedArg", [normalize_node(entry.value)], payload: entry.key)
      end
      n("NamedTuple", entries)
    when Crystal::RangeLiteral
      flags = node.exclusive? ? ["Storage1"] : [] of String
      n("Range", [normalize_node(node.from), normalize_node(node.to)], flags: flags)
    when Crystal::TupleLiteral
      n("Tuple", node.elements.map { |element| normalize_node(element) })
    when Crystal::MultiAssign
      targets = if node.targets.size == 1
                  normalize_node(node.targets.first)
                else
                  n("Tuple", node.targets.map { |target| normalize_node(target) })
                end
      values = if node.values.size == 1
                 normalize_node(node.values.first)
               else
                 n("Tuple", node.values.map { |value| normalize_node(value) })
               end
      n("Assign", [targets, values])
    when Crystal::NamedArgument
      n("NamedArg", [normalize_node(node.value)], payload: node.name)
    when Crystal::Arg
      value = n("Ident", payload: node.name)
      node.default_value ? n("Assign", [value, normalize_node(node.default_value.not_nil!)]) : value
    when Crystal::Assign
      n("Assign", [normalize_node(node.target), normalize_node(node.value)], flags: visibility_flags(node.target))
    when Crystal::OpAssign
      payload = OP_ASSIGN_OPERATORS[node.op]? || raise UnsupportedSemanticAst.new("Crystal::OpAssign(#{node.op})")
      n("Binary", [normalize_node(node.target), normalize_node(node.value)], payload: payload)
    when Crystal::And
      n("Binary", [normalize_node(node.left), normalize_node(node.right)], payload: "AndAnd")
    when Crystal::Or
      n("Binary", [normalize_node(node.left), normalize_node(node.right)], payload: "OrOr")
    when Crystal::Call
      normalize_call(node)
    when Crystal::ReadInstanceVar
      n("Binary", [normalize_node(node.obj), n("InstanceVar", payload: node.name)], payload: "Dot")
    when Crystal::If
      kind = node.ternary? ? "Ternary" : "If"
      then_node = node.ternary? ? normalize_node(node.then) : as_expressions(node.then)
      else_node = if node.ternary?
                    normalize_node(node.else)
                  elsif node.else.is_a?(Crystal::If) || node.else.is_a?(Crystal::Unless)
                    normalize_node(node.else)
                  elsif node.else.is_a?(Crystal::Nop)
                    n("Nop")
                  else
                    as_expressions(node.else)
                  end
      n(kind, [normalize_node(node.cond), then_node, else_node])
    when Crystal::Unless
      else_node = node.else.is_a?(Crystal::Nop) ? n("Nop") : as_expressions(node.else)
      n("Unless", [normalize_node(node.cond), as_expressions(node.then), else_node])
    when Crystal::While
      n("While", [normalize_node(node.cond), as_expressions(node.body)])
    when Crystal::Until
      n("Until", [normalize_node(node.cond), as_expressions(node.body)])
    when Crystal::TypeDeclaration
      n("VarDecl", [normalize_node(node.var), normalize_node(node.declared_type), optional(node.value)])
    when Crystal::Generic
      normalize_generic(node)
    when Crystal::Union
      normalize_union(node)
    when Crystal::ProcNotation
      inputs = node.inputs || [] of Crystal::ASTNode
      n("ProcType", [n("Args", inputs.map { |input| normalize_node(input) }), optional(node.output)])
    when Crystal::Metaclass
      n("Path", [normalize_node(node.name), n("Ident", payload: "class")])
    when Crystal::Def
      normalize_def(node)
    when Crystal::Macro
      normalize_macro(node)
    when Crystal::ClassDef
      normalize_class(node)
    when Crystal::ModuleDef
      name = declaration_name(node.name, node.type_vars)
      n("Module", [name, n("Nop"), as_expressions(node.body)], flags: visibility_flags(node))
    when Crystal::AnnotationDef
      n("AnnotationDef", [normalize_path(node.name), n("Expressions")])
    when Crystal::When
      normalize_when(node)
    when Crystal::Case
      whens = n("Expressions", node.whens.map { |item| normalize_when(item) })
      flags = node.exhaustive? ? ["Exhaustive"] : [] of String
      else_node = node.else ? as_expressions(node.else.not_nil!) : n("Nop")
      n("Case", [optional(node.cond), whens, else_node], flags: flags)
    when Crystal::Select
      whens = n("Expressions", node.whens.map { |item| normalize_when(item) })
      else_node = node.else ? as_expressions(node.else.not_nil!) : n("Nop")
      n("Case", [n("Nop"), whens, else_node], flags: ["Select"])
    when Crystal::ExceptionHandler
      normalize_exception_handler(node)
    when Crystal::ProcLiteral
      normalize_proc_literal(node)
    when Crystal::ProcPointer
      normalize_proc_pointer(node)
    when Crystal::LibDef
      n("Lib", [normalize_path(node.name), n("Nop"), as_expressions(node.body)], flags: visibility_flags(node))
    when Crystal::FunDef
      normalize_fun(node)
    when Crystal::TypeDef
      n("TypeDef", [n("Ident", payload: node.name), normalize_node(node.type_spec)])
    when Crystal::CStructOrUnionDef
      flags = node.union? ? ["Union"] : [] of String
      n("Struct", [n("Ident", payload: node.name), n("Nop"), as_expressions(node.body)], flags: flags)
    when Crystal::EnumDef
      body = n("Expressions", node.members.map { |member| normalize_node(member) })
      n("Enum", [normalize_path(node.name), optional(node.base_type), body], flags: visibility_flags(node))
    when Crystal::ExternalVar
      children = [n("Global", payload: "$#{node.name}"), normalize_node(node.type_spec), n("Nop")]
      children << n("Ident", payload: node.real_name.not_nil!) if node.real_name
      n("VarDecl", children)
    when Crystal::Alias
      n("Alias", [normalize_path(node.name), normalize_node(node.value)], flags: visibility_flags(node))
    when Crystal::Include
      n("Call", [n("Ident", payload: "include"), n("Args", [normalize_node(node.name)])])
    when Crystal::Extend
      n("Call", [n("Ident", payload: "extend"), n("Args", [normalize_node(node.name)])])
    when Crystal::Cast
      normalize_keyword_call(node.obj, "as", node.to)
    when Crystal::NilableCast
      normalize_keyword_call(node.obj, "as?", node.to)
    when Crystal::IsA
      if node.nil_check?
        obj = node.obj
        if obj.is_a?(Crystal::Var) && obj.name == "self"
          n("Ident", payload: "nil?")
        else
          n("Binary", [normalize_node(node.obj), n("Ident", payload: "nil?")], payload: "Dot")
        end
      else
        normalize_keyword_call(node.obj, "is_a?", node.const)
      end
    when Crystal::RespondsTo
      normalize_keyword_call(node.obj, "responds_to?", n("LiteralSymbol"))
    when Crystal::PointerOf
      normalize_prefix_call("pointerof", node.exp)
    when Crystal::SizeOf
      normalize_prefix_call("sizeof", node.exp)
    when Crystal::InstanceSizeOf
      normalize_prefix_call("instance_sizeof", node.exp)
    when Crystal::AlignOf
      normalize_prefix_call("alignof", node.exp)
    when Crystal::InstanceAlignOf
      normalize_prefix_call("instance_alignof", node.exp)
    when Crystal::Out
      n("Unary", [normalize_node(node.exp)], payload: "KeywordOut")
    when Crystal::OffsetOf
      normalize_prefix_call("offsetof", node.offsetof_type, node.offset)
    when Crystal::TypeOf
      args = node.expressions.map { |exp| normalize_node(exp) }
      n("Call", [n("Ident", payload: "typeof"), n("Args", args)])
    when Crystal::UninitializedVar
      value = n("Call", [n("Ident", payload: "uninitialized"), n("Args", [normalize_node(node.declared_type)])])
      n("Assign", [normalize_node(node.var), value])
    when Crystal::Return
      normalize_control("Return", node.exp)
    when Crystal::Break
      normalize_control("Break", node.exp)
    when Crystal::Next
      normalize_control("Next", node.exp)
    when Crystal::Yield
      children = node.exps.map { |exp| normalize_node(exp) }
      if scope = node.scope
        n("Yield", [normalize_node(scope)] + children, flags: ["Storage1"])
      else
        n("Yield", children)
      end
    when Crystal::Not
      if node.location == node.exp.location
        n("Binary", [normalize_node(node.exp), n("Ident", payload: "!")], payload: "Dot")
      else
        n("Unary", [normalize_node(node.exp)], payload: "Bang")
      end
    when Crystal::Splat
      n("Splat", [normalize_node(node.exp)])
    when Crystal::DoubleSplat
      n("DoubleSplat", [normalize_node(node.exp)])
    when Crystal::VisibilityModifier
      child = normalize_node(node.exp)
      flags = child.flags + [node.modifier.to_s]
      n(child.kind, child.children, child.payload, flags)
    when Crystal::Require
      n("Require", [n("LiteralString")])
    when Crystal::Annotation
      normalize_annotation(node)
    when Crystal::MacroLiteral
      n("MacroLiteral")
    when Crystal::MacroExpression
      body = as_expressions(node.exp)
      node.output? ? n("MacroExpr", [body]) : n("MacroControl", [body], payload: "Unknown")
    when Crystal::MacroIf
      if node.cond.is_a?(Crystal::BoolLiteral) && node.cond.location.nil?
        n("MacroControl", [n("Expressions"), as_expressions(node.then)], payload: "KeywordBegin")
      else
        tag = node.is_unless? ? "KeywordUnless" : "KeywordIf"
        then_source = node.is_unless? ? node.else : node.then
        else_source = node.is_unless? ? node.then : node.else
        else_node = if else_source.is_a?(Crystal::Nop)
                      n("Nop")
                    elsif else_source.is_a?(Crystal::MacroIf)
                      normalize_node(else_source)
                    else
                      as_expressions(else_source)
                    end
        n("MacroControl", [as_expressions(node.cond), as_expressions(then_source), else_node], payload: tag)
      end
    when Crystal::MacroFor
      targets = n("Args", node.vars.map { |var| normalize_node(var) })
      header = n("MacroForHeader", [targets, normalize_node(node.exp)])
      n("MacroControl", [header, as_expressions(node.body)], payload: "KeywordFor")
    when Crystal::MacroVar
      children = node.exps.try(&.map { |exp| normalize_node(exp) }) || [] of SemanticAstNode
      n("MacroVar", children, payload: node.name)
    when Crystal::MacroVerbatim
      n("MacroControl", [n("Nop"), n("MacroLiteral")], payload: "KeywordVerbatim")
    when Crystal::MagicConstant
      n("Ident", payload: magic_constant_name(node.name))
    when Crystal::Asm
      normalize_asm(node)
    else
      raise UnsupportedSemanticAst.new(node.class.to_s)
    end
  end

  private def normalize_path(node : Crystal::Path) : SemanticAstNode
    return n("LiteralNil") if node.names.size == 1 && node.names.first == "Nil"
    parts = node.names.map { |name| n("Ident", payload: name) }
    if node.global?
      first = parts.first
      parts[0] = n(first.kind, first.children, first.payload, first.flags + ["GlobalRoot"])
    end
    return parts.first if parts.size == 1
    parts[1..].reduce(parts.first) { |left, right| n("Path", [left, right]) }
  end

  private def normalize_array(node : Crystal::ArrayLiteral) : SemanticAstNode
    children = node.elements.map { |element| normalize_node(element) }
    flags = [] of String
    if type = node.of
      children << normalize_node(type)
      flags << "Storage1"
    end
    if name = node.name
      n("TypeApply", [normalize_node(name), n("Tuple", children)])
    else
      n("Array", children, flags: flags)
    end
  end

  private def normalize_hash(node : Crystal::HashLiteral) : SemanticAstNode
    children = node.entries.map do |entry|
      n("Binary", [normalize_node(entry.key), normalize_node(entry.value)], payload: "HashRocket")
    end
    flags = [] of String
    if type = node.of
      children << normalize_node(type.key)
      children << normalize_node(type.value)
      flags << "Storage1"
    end
    literal = n("Hash", children, flags: flags)
    node.name ? n("TypeApply", [normalize_node(node.name.not_nil!), literal]) : literal
  end

  private def normalize_annotation(node : Crystal::Annotation) : SemanticAstNode
    n("Annotation", [normalize_annotation_value(node), n("Nop")])
  end

  private def normalize_annotation_value(node : Crystal::Annotation) : SemanticAstNode
    args = node.args.map { |arg| normalize_node(arg) }
    if named = node.named_args
      args.concat(named.map { |arg| normalize_node(arg) })
    end
    args.empty? ? normalize_path(node.path) : n("Call", [normalize_path(node.path), n("Args", args)])
  end

  private def normalize_asm(node : Crystal::Asm) : SemanticAstNode
    children = [n("LiteralString")]
    children << normalize_asm_operands(node.outputs)
    children << normalize_asm_operands(node.inputs)
    children << n("Args", (node.clobbers || [] of String).map { n("LiteralString") })
    flags = [] of String
    flags << "Volatile" if node.volatile?
    flags << "AlignStack" if node.alignstack?
    flags << "Intel" if node.intel?
    flags << "CanThrow" if node.can_throw?
    n("Asm", children, flags: flags)
  end

  private def normalize_asm_operands(operands : Array(Crystal::AsmOperand)?) : SemanticAstNode
    n("Args", (operands || [] of Crystal::AsmOperand).map do |operand|
      n("AsmOperand", [n("LiteralString"), normalize_node(operand.exp)])
    end)
  end

  private def magic_constant_name(kind : Crystal::Token::Kind) : String
    case kind
    when .magic_line?     then "__LINE__"
    when .magic_end_line? then "__END_LINE__"
    when .magic_file?     then "__FILE__"
    when .magic_dir?      then "__DIR__"
    else                       raise UnsupportedSemanticAst.new("Crystal::MagicConstant(#{kind})")
    end
  end

  private def normalize_generic(node : Crystal::Generic) : SemanticAstNode
    if node.suffix.bracket?
      element = node.type_vars.first? || raise UnsupportedSemanticAst.new("Crystal::Generic(BracketEmpty)")
      dimensions = node.type_vars.skip(1).map { |type| normalize_node(type) }
      return n("TypeApply", [normalize_node(element), n("Args", dimensions)])
    end

    args = node.type_vars.map { |type| normalize_node(type) }
    if named = node.named_args
      args.concat(named.map { |arg| normalize_node(arg) })
    end
    name = node.name
    if name.is_a?(Crystal::Path)
      return n("Tuple", args) if name.names.last? == "Tuple"
      return n("NamedTuple", args) if name.names.last? == "NamedTuple"
    end
    generic = n("TypeApply", [normalize_node(node.name), n("Args", args)])
    case node.suffix
    when .none?
      generic
    when .question?
      name = node.name
      if name.is_a?(Crystal::Path) && name.names.last? == "Union" && !node.type_vars.empty?
        types = node.type_vars.reject { |type| type.is_a?(Crystal::Path) && type.names.last? == "Nil" }
        value = types.map { |type| normalize_node(type) }.reduce { |left, right| n("Binary", [left, right], payload: "Pipe") }
        n("Binary", [value, n("LiteralNil")], payload: "Pipe")
      else
        n("Binary", [generic, n("LiteralNil")], payload: "Pipe")
      end
    when .asterisk?
      name = node.name
      if name.is_a?(Crystal::Path) && name.names.last? == "Pointer" && node.type_vars.size == 1
        n("Unary", [normalize_node(node.type_vars.first)], payload: "Star")
      else
        n("Unary", [generic], payload: "Star")
      end
    else
      raise UnsupportedSemanticAst.new("Crystal::Generic(#{node.suffix})")
    end
  end

  private def normalize_union(node : Crystal::Union) : SemanticAstNode
    types = node.types.map { |type| normalize_node(type) }
    return types.first if types.size == 1
    types[1..].reduce(types.first) { |left, right| n("Binary", [left, right], payload: "Pipe") }
  end

  private def normalize_arg(node : Crystal::Arg) : SemanticAstNode
    name = node.name.empty? ? n("Nop") : n("Ident", payload: node.name)
    type = optional(node.restriction)
    default = optional(node.default_value)
    children = if node.external_name == node.name
                 [name, type, default]
               else
                 [n("Ident", payload: node.external_name), name, type, default]
               end
    parameter = n("Param", children, payload: node.name)
    if annotations = node.parsed_annotations
      annotations.reverse_each do |annot|
        parameter = n("Annotation", [normalize_annotation_value(annot), parameter])
      end
    end
    parameter
  end

  private def normalize_params(
    args : Array(Crystal::Arg),
    splat_index : Int32?,
    double_splat : Crystal::Arg?,
    block_arg : Crystal::Arg?,
  ) : SemanticAstNode
    children = args.map_with_index do |arg, index|
      if splat_index == index
        payload = arg.name.empty? ? nil : arg.name
        wrap_arg_annotations(n("Splat", [optional(arg.restriction)], payload: payload), arg)
      else
        normalize_arg(arg)
      end
    end
    if arg = double_splat
      payload = arg.name.empty? ? nil : arg.name
      children << wrap_arg_annotations(n("DoubleSplat", [optional(arg.restriction)], payload: payload), arg)
    end
    if arg = block_arg
      block_children = arg.restriction ? [normalize_node(arg.restriction.not_nil!)] : [] of SemanticAstNode
      payload = arg.name.empty? ? nil : arg.name
      children << wrap_arg_annotations(n("BlockParam", block_children, payload: payload), arg)
    end
    n("Args", children)
  end

  private def wrap_arg_annotations(node : SemanticAstNode, arg : Crystal::Arg) : SemanticAstNode
    wrapped = node
    if annotations = arg.parsed_annotations
      annotations.reverse_each do |annot|
        wrapped = n("Annotation", [normalize_annotation_value(annot), wrapped])
      end
    end
    wrapped
  end

  private def normalize_def(node : Crystal::Def) : SemanticAstNode
    name = if receiver = node.receiver
             n("Path", [normalize_node(receiver), n("Ident", payload: node.name)])
           else
             n("Ident", payload: node.name)
           end
    params = normalize_params(node.args, node.splat_index, node.double_splat, node.block_arg)
    body = node.abstract? ? n("Nop") : normalize_body(node.body)
    forall = if vars = node.free_vars
               n("Args", vars.map { |var| n("Ident", payload: var) })
             else
               n("Nop")
             end
    flags = visibility_flags(node)
    flags << "Abstract" if node.abstract?
    n("Def", [name, params, optional(node.return_type), body, forall], flags: flags)
  end

  private def normalize_macro(node : Crystal::Macro) : SemanticAstNode
    params = normalize_params(node.args, node.splat_index, node.double_splat, node.block_arg)
    children = [n("Ident", payload: node.name), params, n("Nop"), normalize_body(node.body), n("Nop")]
    n("MacroDef", children, flags: visibility_flags(node))
  end

  private def normalize_class(node : Crystal::ClassDef) : SemanticAstNode
    name = declaration_name(node.name, node.type_vars, node.splat_index)
    kind = node.struct? ? "Struct" : "Class"
    flags = visibility_flags(node)
    flags << "Abstract" if node.abstract?
    n(kind, [name, optional(node.superclass), as_expressions(node.body)], flags: flags)
  end

  private def declaration_name(path : Crystal::Path, type_vars : Array(String)?, splat_index : Int32? = nil) : SemanticAstNode
    name = normalize_path(path)
    return name unless vars = type_vars
    args = vars.map_with_index do |var, index|
      ident = n("Ident", payload: var)
      splat_index == index ? n("Splat", [ident]) : ident
    end
    n("TypeApply", [name, n("Args", args)])
  end

  private def normalize_body(node : Crystal::ASTNode) : SemanticAstNode
    normalized = normalize_node(node)
    normalized.kind == "Begin" ? normalized : as_expressions(node)
  end

  private def normalize_when(node : Crystal::When) : SemanticAstNode
    conds = n("Expressions", node.conds.map { |cond| normalize_node(cond) })
    n("When", [conds, as_expressions(node.body)])
  end

  private def normalize_exception_handler(node : Crystal::ExceptionHandler) : SemanticAstNode
    rescues = node.rescues
    if node.suffix && rescues && rescues.size == 1 && !rescues.first.name && !rescues.first.types && !node.else && !node.ensure
      return n("Rescue", [normalize_node(node.body), normalize_node(rescues.first.body)])
    end
    if node.suffix && node.ensure && !rescues && !node.else
      return n("Ensure", [normalize_node(node.body), normalize_node(node.ensure.not_nil!)])
    end

    rescue_node = if rescues
                    n("Expressions", rescues.map { |clause| normalize_rescue_clause(clause) })
                  else
                    n("Nop")
                  end
    else_node = node.else ? as_expressions(node.else.not_nil!) : n("Nop")
    ensure_node = node.ensure ? n("Ensure", [as_expressions(node.ensure.not_nil!)]) : n("Nop")
    n("Begin", [as_expressions(node.body), rescue_node, else_node, ensure_node])
  end

  private def normalize_rescue_clause(node : Crystal::Rescue) : SemanticAstNode
    type = if types = node.types
             normalized = types.map { |item| normalize_node(item) }
             normalized[1..].reduce(normalized.first) { |left, right| n("Binary", [left, right], payload: "Pipe") }
           end
    header = if name = node.name
               type ? n("VarDecl", [n("Ident", payload: name), type, n("Nop")]) : n("Ident", payload: name)
             else
               type || n("Nop")
             end
    n("Rescue", [header, as_expressions(node.body)], flags: ["RescueClause"])
  end

  private def normalize_proc_literal(node : Crystal::ProcLiteral) : SemanticAstNode
    definition = node.def
    params = normalize_params(definition.args, definition.splat_index, definition.double_splat, definition.block_arg)
    n("Block", [params, optional(definition.return_type), normalize_body(definition.body)])
  end

  private def normalize_proc_pointer(node : Crystal::ProcPointer) : SemanticAstNode
    flags = node.global? ? ["GlobalRoot"] : [] of String
    target = n("Ident", payload: node.name, flags: flags)
    target = n("Path", [normalize_node(node.obj.not_nil!), target]) if node.obj
    unless node.args.empty?
      target = n("Call", [target, n("Args", node.args.map { |arg| normalize_node(arg) })])
    end
    n("Unary", [target], payload: "Arrow")
  end

  private def normalize_fun(node : Crystal::FunDef) : SemanticAstNode
    params = n("Args", node.args.map { |arg| normalize_arg(arg) })
    params.children << n("Param") if node.varargs?
    external = node.real_name == node.name ? n("Nop") : n("Ident", payload: node.real_name)
    body = node.body ? as_expressions(node.body.not_nil!) : n("Nop")
    n("Fun", [n("Ident", payload: node.name), params, optional(node.return_type), external, body])
  end

  private def normalize_keyword_call(obj : Crystal::ASTNode, name : String, arg : Crystal::ASTNode | SemanticAstNode) : SemanticAstNode
    normalized_arg = arg.is_a?(SemanticAstNode) ? arg : normalize_node(arg)
    member = n("Call", [n("Ident", payload: name), n("Args", [normalized_arg])])
    return member if obj.is_a?(Crystal::Var) && obj.name == "self"
    n("Binary", [normalize_node(obj), member], payload: "Dot")
  end

  private def normalize_prefix_call(name : String, *args : Crystal::ASTNode) : SemanticAstNode
    normalized = args.map { |arg| normalize_node(arg) }
    n("Call", [n("Ident", payload: name), n("Args", normalized)])
  end

  private def normalize_call(node : Crystal::Call) : SemanticAstNode
    if obj = node.obj
      if node.name == "[]" || node.name == "[]?"
        indices = node.args.map { |arg| normalize_node(arg) }
        if named = node.named_args
          indices.concat(named.map { |arg| normalize_node(arg) })
        end
        flags = node.name == "[]?" ? ["Storage1"] : [] of String
        index = n("Index", [normalize_node(obj)] + indices, flags: flags)
        if block = node.block
          return n("CallWithBlock", [index, normalize_block_args(block), normalize_body(block.body)])
        end
        return index
      elsif node.name == "[]=" && !node.args.empty?
        indices = node.args[0...-1].map { |arg| normalize_node(arg) }
        if named = node.named_args
          indices.concat(named.map { |arg| normalize_node(arg) })
        end
        index = n("Index", [normalize_node(obj)] + indices)
        return n("Assign", [index, normalize_node(node.args.last)])
      end
      if (operator = INFIX_OPERATORS[node.name]?) && node.args.size == 1 && !node.block && !node.block_arg && !node.named_args
        return n("Binary", [normalize_node(obj), normalize_node(node.args.first)], payload: operator)
      end
      if {"-", "+"}.includes?(node.name) && node.args.empty? && node.location != obj.location
        payload = node.name == "-" ? "Minus" : "Plus"
        return n("Unary", [normalize_node(obj)], payload: payload)
      end
      if node.name == "-@" && node.args.empty?
        return n("Unary", [normalize_node(obj)], payload: "Minus")
      elsif node.name == "+@" && node.args.empty?
        return n("Unary", [normalize_node(obj)], payload: "Plus")
      elsif node.name == "~" && node.args.empty?
        if node.location == obj.location
          return n("Binary", [normalize_node(obj), n("Ident", payload: "~")], payload: "Dot")
        end
        return n("Unary", [normalize_node(obj)], payload: "Tilde")
      end
    end

    args = node.args.map { |arg| normalize_node(arg) }
    if named = node.named_args
      args.concat(named.map { |arg| normalize_node(arg) })
    end
    args << n("Unary", [normalize_node(node.block_arg.not_nil!)], payload: "Ampersand") if node.block_arg

    callee_flags = node.global? ? ["GlobalRoot"] : [] of String
    member = if args.empty? && !node.has_parentheses?
               n("Ident", payload: node.name, flags: callee_flags)
             else
               n("Call", [n("Ident", payload: node.name, flags: callee_flags), n("Args", args)])
             end
    call = node.obj ? n("Binary", [normalize_node(node.obj.not_nil!), member], payload: "Dot") : member
    return call unless block = node.block

    block_args = normalize_block_args(block)
    block_body = normalize_body(block.body)
    block_call = n("CallWithBlock", [node.obj ? member : call, block_args, block_body])
    node.obj ? n("Binary", [normalize_node(node.obj.not_nil!), block_call], payload: "Dot") : block_call
  end

  private def normalize_block_args(block : Crystal::Block) : SemanticAstNode
    unpacks = block.unpacks
    n("Args", block.args.map_with_index do |arg, index|
      if unpack = unpacks.try(&.[index]?)
        normalize_block_unpack(unpack)
      elsif block.splat_index == index
        n("Splat", [n("Ident", payload: arg.name)], payload: arg.name)
      else
        n("Ident", payload: arg.name)
      end
    end)
  end

  private def normalize_control(kind : String, exp : Crystal::ASTNode?) : SemanticAstNode
    if exp.is_a?(Crystal::TupleLiteral) && exp.elements.size == 1
      n(kind, [normalize_node(exp.elements.first)])
    elsif exp
      n(kind, [normalize_node(exp)])
    else
      n(kind)
    end
  end

  private def normalize_block_unpack(node : Crystal::Expressions) : SemanticAstNode
    children = node.expressions.map do |expression|
      expression.is_a?(Crystal::Expressions) ? normalize_block_unpack(expression) : normalize_node(expression)
    end
    n("Destructure", children)
  end

  private def visibility_flags(node : Crystal::ASTNode) : Array(String)
    visibility = case node
                 when Crystal::Path      then node.visibility
                 when Crystal::Call      then node.visibility
                 when Crystal::Def       then node.visibility
                 when Crystal::Macro     then node.visibility
                 when Crystal::ClassDef  then node.visibility
                 when Crystal::ModuleDef then node.visibility
                 when Crystal::LibDef    then node.visibility
                 when Crystal::EnumDef   then node.visibility
                 when Crystal::Alias     then node.visibility
                 else                         Crystal::Visibility::Public
                 end
    visibility.public? ? [] of String : [visibility.to_s]
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
end
