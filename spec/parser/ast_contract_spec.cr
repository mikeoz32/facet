require "../spec_helper"
require "../support/ast_contract"

include AstContractSupport

AST_CONTRACT_CASES = [
  {
    name:     "leaf payloads",
    source:   %(local; @ivar; @@class_var; 1; "text"; 'c'; /rx/; :symbol; true; nil),
    expected: %q(File(Expressions(Ident["local"], InstanceVar["@ivar"], ClassVar["@@class_var"], LiteralNumber["1"], LiteralString["\"text\""], LiteralChar["'c'"], LiteralRegex["/rx/"], LiteralSymbol[":symbol"], LiteralBool["true"]{storage=1}, LiteralNil["nil"]))),
  },
  {
    name:     "expressions and calls",
    source:   %(result = -left + right * 2; foo(1, named: "x"); values[0]; 1...3; flag ? yes : no),
    expected: %q(File(Expressions(Assign(Ident["result"], Binary[Plus](Unary[Minus](Ident["left"]), Binary[Star](Ident["right"], LiteralNumber["2"]))), Call(Ident["foo"], Args(LiteralNumber["1"], NamedArg["named"](LiteralString["\"x\""]))), Index(Ident["values"], LiteralNumber["0"]), Range{storage=1}(LiteralNumber["1"], LiteralNumber["3"]), Ternary(Ident["flag"], Ident["yes"], Ident["no"])))),
  },
  {
    name:     "collections",
    source:   %([1, 2]; {1 => 2}; {one: 1}; {1, "two"}; [1] of Int32),
    expected: %q(File(Expressions(Array(LiteralNumber["1"], LiteralNumber["2"]), Hash(Binary[HashRocket](LiteralNumber["1"], LiteralNumber["2"])), NamedTuple(NamedArg["one"](LiteralNumber["1"])), Tuple(LiteralNumber["1"], LiteralString["\"two\""]), Array{storage=1}(LiteralNumber["1"], Ident["Int32"])))),
  },
  {
    name:     "definitions, parameters, and types",
    source:   %(def transform(x : Foo::Bar = value, *rest : Int32, **options, &block : Int32 -> String) : Array(String) | Nil; yield x; end),
    expected: %q(File(Expressions(Def(Ident["transform"], Args(Param["x"](Ident["x"], Path(Ident["Foo"], Ident["Bar"]), Ident["value"]), Splat["rest"](Ident["Int32"]), DoubleSplat["options"](Nop), BlockParam["block"](ProcType(Args(Ident["Int32"]), Ident["String"]))), Binary[Pipe](TypeApply(Ident["Array"], Args(Ident["String"])), Ident["Nil"]), Expressions(Yield(Ident["x"])), Nop)))),
  },
  {
    name:     "aliases, type definitions, and require",
    source:   %(alias Name = Foo::Bar; type Handle = Pointer(Int32); require "./dep"),
    expected: %q(File(Expressions(Alias(Ident["Name"], Path(Ident["Foo"], Ident["Bar"])), TypeDef(Ident["Handle"], TypeApply(Ident["Pointer"], Args(Ident["Int32"]))), Require(LiteralString["\"./dep\""])))),
  },
  {
    name:     "type declarations and annotations",
    source:   %(annotation Marker; end; @[Marker] class Box(T) < Base; end; module Mix; end; struct Point; end; enum Color : UInt8; Red; end),
    expected: %q(File(Expressions(AnnotationDef(Ident["Marker"], Expressions), Annotation(Ident["Marker"], Class(TypeApply(Ident["Box"], Args(Ident["T"])), Ident["Base"], Expressions)), Module(Ident["Mix"], Nop, Expressions), Struct(Ident["Point"], Nop, Expressions), Enum(Ident["Color"], Ident["UInt8"], Expressions(Ident["Red"]))))),
  },
  {
    name:     "foreign declarations",
    source:   %(lib LibC; fun puts(value : UInt8*) : Int32; end),
    expected: %q(File(Expressions(Lib(Ident["LibC"], Nop, Expressions(Fun(Ident["puts"], Args(Param["value"](Ident["value"], Unary[Star](Ident["UInt8"]), Nop)), Ident["Int32"], Nop, Nop)))))),
  },
  {
    name:     "control flow",
    source:   %(if ready; work; else; stop; end; unless done; retry; end; while active; break; end; until ready; next; end; for x, y in items; return x; end),
    expected: %q(File(Expressions(If(Ident["ready"], Expressions(Ident["work"]), Expressions(Ident["stop"])), Unless(Ident["done"], Expressions(Ident["retry"]), Nop), While(Ident["active"], Expressions(Break)), Until(Ident["ready"], Expressions(Next)), For(Args(Ident["x"], Ident["y"]), Ident["items"], Expressions(Return(Ident["x"])))))),
  },
  {
    name:     "exception handlers",
    source:   %(begin; risky; rescue; recover; else; success; ensure; cleanup; end),
    expected: %q(File(Expressions(Begin(Expressions(Ident["risky"]), Rescue(Expressions(Ident["recover"])), Rescue(Expressions(Ident["success"])), Ensure(Expressions(Ident["cleanup"])))))),
  },
  {
    name:     "proc blocks and destructuring",
    source:   %(callback = ->(x : Int32) { x }; items.each { |head, (left, (right, *rest))| left }),
    expected: %q(File(Expressions(Assign(Ident["callback"], Block(Args(Param["x"](Ident["x"], Ident["Int32"], Nop)), Expressions(Ident["x"]))), Binary[Dot](Ident["items"], CallWithBlock(Ident["each"], Args(Ident["head"], Destructure(Ident["left"], Destructure(Ident["right"], Splat["rest"](Ident["rest"])))), Expressions(Ident["left"])))))),
  },
  {
    name:     "macro structure",
    source:   %(macro render(x);before {{ x }} {% if x %}yes{% else %}no{% end %} after;end; {% for key, value in pairs %}{{ %key }}{% end %}),
    expected: %q(File(Expressions(MacroDef(Ident["render"], Args(Param["x"](Ident["x"], Nop, Nop)), Nop, Expressions(Call(Call(Call(Ident["before"], Args(MacroExpr(Expressions(Ident["x"])))), Args(MacroControl[KeywordIf](Expressions(Ident["x"]), Expressions(MacroLiteral["yes"]), Expressions(MacroLiteral["no"])))), Args(Ident["after"]))), Nop), MacroControl[KeywordFor](MacroForHeader(Args(Ident["key"], Ident["value"]), Ident["pairs"]), Expressions(MacroExpr(Expressions(MacroVar["key"]))))))),
  },
  {
    name:     "compact declaration flags",
    source:   %(abstract class Box(T) < Base; end; union Value; field : Int32; end; private def run; end; protected macro build; end; def value=(new_value); end),
    expected: %q(File(Expressions(Class{Abstract}(TypeApply(Ident["Box"], Args(Ident["T"])), Ident["Base"], Expressions), Struct{Union}(Ident["Value"], Nop, Expressions(VarDecl(Ident["field"], Ident["Int32"], Nop))), Def{Private}(Ident["run"], Args, Nop, Expressions, Nop), MacroDef{Protected}(Ident["build"], Args, Nop, Expressions, Nop), Def(Ident["value="], Args(Param["new_value"](Ident["new_value"], Nop, Nop)), Nop, Expressions, Nop)))),
  },
  {
    name:     "case variants",
    source:   %(case value; when 1; one; else; other; end; case value; in Int32; typed; end; select; when channel.receive; value; else; nil; end),
    expected: %q(File(Expressions(Case(Ident["value"], Expressions(When(Expressions(LiteralNumber["1"]), Expressions(Ident["one"]))), Expressions(Ident["other"])), Case{Exhaustive}(Ident["value"], Expressions(When(Expressions(Ident["Int32"]), Expressions(Ident["typed"]))), Nop), Case{Select}(Nop, Expressions(When(Expressions(Binary[Dot](Ident["channel"], Ident["receive"])), Expressions(Ident["value"]))), Expressions(LiteralNil["nil"]))))),
  },
  {
    name:     "verbatim and escaped macro literals",
    source:   %(macro raw; {% verbatim do %}{{ untouched }}{% end %}; \\{{ escaped }}; end),
    expected: %q(File(Expressions(MacroDef(Ident["raw"], Args, Nop, Expressions(MacroControl[KeywordVerbatim](Nop, MacroLiteral["{{ untouched }}"]), MacroLiteral["\\{{ escaped }}"]{Escaped}), Nop)))),
  },
]

describe "Facet AST contract" do
  AST_CONTRACT_CASES.each do |contract|
    it contract[:name] do
      assert_facet_ast(contract[:source], contract[:expected])
    end
  end

  it "preserves semantic distinctions represented as compact flags" do
    cases = [
      {"abstract def run", Facet::Compiler::NodeKind::Def, Facet::Compiler::SemanticFlag::Abstract},
      {"abstract class Box; end", Facet::Compiler::NodeKind::Class, Facet::Compiler::SemanticFlag::Abstract},
      {"abstract struct Box; end", Facet::Compiler::NodeKind::Struct, Facet::Compiler::SemanticFlag::Abstract},
      {"abstract module Box; end", Facet::Compiler::NodeKind::Module, Facet::Compiler::SemanticFlag::Abstract},
      {"private def run; end", Facet::Compiler::NodeKind::Def, Facet::Compiler::SemanticFlag::Private},
      {"protected macro run; end", Facet::Compiler::NodeKind::MacroDef, Facet::Compiler::SemanticFlag::Protected},
      {"union Value; end", Facet::Compiler::NodeKind::Struct, Facet::Compiler::SemanticFlag::Union},
      {"case value; in Int32; value; end", Facet::Compiler::NodeKind::Case, Facet::Compiler::SemanticFlag::Exhaustive},
      {"select; when channel.receive; value; end", Facet::Compiler::NodeKind::Case, Facet::Compiler::SemanticFlag::Select},
    ]

    cases.each do |code, kind, flag|
      ast = facet_ast(code)
      expressions = ast.children(ast.root)[0]
      node = ast.node(ast.children(expressions)[0])
      node.kind.should eq(kind)
      node.semantic_flag?(flag).should be_true
    end
  end

  it "preserves macro literal bytes between control tags" do
    ast = facet_ast("{% if true %}\n  value\n{% end %}")
    expressions = ast.children(ast.root)[0]
    control = ast.children(expressions)[0]
    body = ast.children(control)[1]
    literal = ast.children(body)[0]

    ast.node(literal).kind.should eq(Facet::Compiler::NodeKind::MacroLiteral)
    ast.node_string(literal).should eq("\n  value\n")

    verbatim = facet_ast("{% verbatim do %}\n  {{ untouched }}\n{% end %}")
    expressions = verbatim.children(verbatim.root)[0]
    control = verbatim.children(expressions)[0]
    literal = verbatim.children(control)[1]
    verbatim.node_string(literal).should eq("\n  {{ untouched }}\n")
  end

  it "does not let Nop or container spans hide a significant token" do
    source = Facet::Compiler::Source.new("lost")
    arena = Facet::Compiler::AstArena.new
    nop = arena.add_node(Facet::Compiler::NodeKind::Nop, Facet::Compiler::Span.new(0, source.size))
    expressions = arena.add_node(Facet::Compiler::NodeKind::Expressions, Facet::Compiler::Span.new(0, source.size), [nop])
    root = arena.add_node(Facet::Compiler::NodeKind::File, Facet::Compiler::Span.new(0, source.size), [expressions])
    ast = Facet::Compiler::AstFile.new(source, root, arena, [] of Facet::Compiler::Diagnostic)
    tokens = Facet::Compiler::Lexer.new(source).tokenize_all

    missing = Facet::Compiler::AstIntegrity.missing_semantic_tokens(ast, tokens)
    missing.size.should eq(1)
    missing.first.kind.should eq(Facet::Compiler::TokenKind::Identifier)
  end

  it "covers every node kind produced by accepted syntax" do
    covered = [] of Facet::Compiler::NodeKind
    AST_CONTRACT_CASES.each do |contract|
      ast = facet_ast(contract[:source])
      covered.concat(ast.arena.nodes.map(&.kind))
    end

    intentionally_nonsemantic = {
      Facet::Compiler::NodeKind::Error,
      Facet::Compiler::NodeKind::Const,
      Facet::Compiler::NodeKind::Global,
    }
    expected = Facet::Compiler::NodeKind.values.reject { |kind| intentionally_nonsemantic.includes?(kind) }
    (expected - covered.uniq).should be_empty
  end
end
