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
    expected: %q(File(Expressions(Def(Ident["transform"], Args(Param["x"](Ident["x"], Path(Ident["Foo"], Ident["Bar"]), Ident["value"]), Splat["rest"](Ident["Int32"]), DoubleSplat["options"](Nop), BlockParam["block"](ProcType(Args(Ident["Int32"]), Ident["String"]))), Binary[Pipe](TypeApply(Ident["Array"], Args(Ident["String"])), LiteralNil["Nil"]), Expressions(Yield(Ident["x"])), Nop)))),
  },
  {
    name:     "aliases, type definitions, and require",
    source:   %(alias Name = Foo::Bar; type Handle = Pointer(Int32); require "./dep"),
    expected: %q(File(Expressions(Alias(Ident["Name"], Path(Ident["Foo"], Ident["Bar"])), TypeDef(Ident["Handle"], TypeApply(Ident["Pointer"], Args(Ident["Int32"]))), Require(LiteralString["\"./dep\""])))),
  },
  {
    name:     "type-context edge forms",
    source:   %(value : ::Foo::Bar; alias Generated = Foo[]; uninitialized Foo(Int32); uninitialized { name : Int32 }),
    expected: %q|File(Expressions(VarDecl(Ident["value"], Path(Ident["::"], Path(Ident["Foo"], Ident["Bar"])), Nop), Index(Alias(Ident["Generated"], Ident["Foo"])), Call(Ident["uninitialized"], Args(TypeApply(Ident["Foo"], Args(Ident["Int32"])))), CallWithBlock{storage=1}(Ident["uninitialized"], Args, Expressions(VarDecl(Ident["name"], Ident["Int32"], Nop)))))|,
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
    expected: %q(File(Expressions(Begin(Expressions(Ident["risky"]), Expressions(Rescue{RescueClause}(Nop, Expressions(Ident["recover"]))), Expressions(Ident["success"]), Ensure(Expressions(Ident["cleanup"])))))),
  },
  {
    name:     "multiple rescue clause headers",
    source:   %(begin; risky; rescue ex : Foo | Bar; one; rescue Baz; two; rescue fallback; three; end),
    expected: %q(File(Expressions(Begin(Expressions(Ident["risky"]), Expressions(Rescue{RescueClause}(VarDecl(Ident["ex"], Binary[Pipe](Ident["Foo"], Ident["Bar"]), Nop), Expressions(Ident["one"])), Rescue{RescueClause}(Ident["Baz"], Expressions(Ident["two"])), Rescue{RescueClause}(Ident["fallback"], Expressions(Ident["three"]))), Nop, Nop)))),
  },
  {
    name:     "proc blocks and destructuring",
    source:   %(callback = ->(x : Int32) { x }; items.each { |head, (left, (right, *rest))| left }),
    expected: %q(File(Expressions(Assign(Ident["callback"], Block(Args(Param["x"](Ident["x"], Ident["Int32"], Nop)), Nop, Expressions(Ident["x"]))), Binary[Dot](Ident["items"], CallWithBlock{storage=1}(Ident["each"], Args(Ident["head"], Destructure(Ident["left"], Destructure(Ident["right"], Splat["rest"](Ident["rest"])))), Expressions(Ident["left"])))))),
  },
  {
    name:     "proc return types and external variables",
    source:   %(callback = ->(x : Int32) : String { x.to_s }; lib LibC; $errno = Foo : Int32; end),
    expected: %q(File(Expressions(Assign(Ident["callback"], Block(Args(Param["x"](Ident["x"], Ident["Int32"], Nop)), Ident["String"], Expressions(Binary[Dot](Ident["x"], Ident["to_s"])))), Lib(Ident["LibC"], Nop, Expressions(VarDecl(Global["$errno"], Ident["Int32"], Nop, Ident["Foo"])))))),
  },
  {
    name:     "macro structure",
    source:   %(macro render(x);before {{ x }} {% if x %}yes{% else %}no{% end %} after;end; {% for key, value in pairs %}{{ %key }}{% end %}),
    expected: %q(File(Expressions(MacroDef(Ident["render"], Args(Param["x"](Ident["x"], Nop, Nop)), Nop, Expressions(MacroLiteral["before "], MacroExpr(Expressions(Ident["x"])), MacroLiteral[" "], MacroControl[KeywordIf](Expressions(Ident["x"]), Expressions(MacroLiteral["yes"]), Expressions(MacroLiteral["no"])), MacroLiteral[" after;"]), Nop), MacroControl[KeywordFor](MacroForHeader(Args(Ident["key"], Ident["value"]), Ident["pairs"]), Expressions(MacroExpr(Expressions(MacroVar["key"]))))))),
  },
  {
    name:     "compact declaration flags",
    source:   %(abstract class Box(T) < Base; end; union Value; field : Int32; end; private def run; end; protected macro build; end; def value=(new_value); end),
    expected: %q(File(Expressions(Class{Abstract}(TypeApply(Ident["Box"], Args(Ident["T"])), Ident["Base"], Expressions), Struct{Union}(Ident["Value"], Nop, Expressions(VarDecl(Ident["field"], Ident["Int32"], Nop))), Def{Private}(Ident["run"], Args, Nop, Expressions, Nop), MacroDef{Protected}(Ident["build"], Args, Nop, Expressions(MacroLiteral[" "]), Nop), Def(Ident["value="], Args(Param["new_value"](Ident["new_value"], Nop, Nop)), Nop, Expressions, Nop)))),
  },
  {
    name:     "case variants",
    source:   %(case value; when 1; one; else; other; end; case value; in Int32; typed; end; select; when channel.receive; value; else; nil; end),
    expected: %q(File(Expressions(Case(Ident["value"], Expressions(When(Expressions(LiteralNumber["1"]), Expressions(Ident["one"]))), Expressions(Ident["other"])), Case{Exhaustive}(Ident["value"], Expressions(When(Expressions(Ident["Int32"]), Expressions(Ident["typed"]))), Nop), Case{Select}(Nop, Expressions(When(Expressions(Binary[Dot](Ident["channel"], Ident["receive"])), Expressions(Ident["value"]))), Expressions(LiteralNil["nil"]))))),
  },
  {
    name:     "verbatim and escaped macro literals",
    source:   %(macro raw; {% verbatim do %}{{ untouched }}{% end %}; \\{{ escaped }}; end),
    expected: %q(File(Expressions(MacroDef(Ident["raw"], Args, Nop, Expressions(MacroLiteral[" "], MacroControl[KeywordVerbatim](Nop, MacroLiteral["{{ untouched }}"]), MacroLiteral["; \\"], MacroLiteral["\\{{ escaped }}"]{Escaped}, MacroLiteral["; "]), Nop)))),
  },
  {
    name:     "interpolation and inline assembly",
    source:   %q("a#{b}"; asm("nop" : "a"(0) : "b"(1))),
    expected: %q(File(Expressions(StringInterpolation(LiteralString["a"], Ident["b"]), Asm(LiteralString["\"nop\""], Args(AsmOperand(LiteralString["\"a\""], LiteralNumber["0"])), Args(AsmOperand(LiteralString["\"b\""], LiteralNumber["1"])), Args)))),
  },
  {
    name:     "source-backed literal forms",
    source:   "__FILE__; __LINE__; $1; %w(one two); %i(foo bar); \"a\" \\\n\"b\"",
    expected: %q(File(Expressions(LiteralString["__FILE__"], LiteralNumber["__LINE__"], Index(Global["$~"], LiteralNumber["1"]), Array{storage=1}(LiteralString["one"], LiteralString["two"], Path(Ident["::"], Ident["String"])), Array{storage=1}(LiteralSymbol["foo"], LiteralSymbol["bar"], Path(Ident["::"], Ident["Symbol"])), StringInterpolation(LiteralString["\"a\""], LiteralString["\"b\""])))),
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

  it "retains distinct content spans for multiple heredocs on one header line" do
    ast = facet_ast("<<-ONE; <<-TWO\na\nONE\nb\nTWO\n")
    expressions = ast.children(ast.root)[0]
    first, second = ast.children(expressions)

    ast.node(first).kind.should eq(Facet::Compiler::NodeKind::LiteralString)
    ast.node(second).kind.should eq(Facet::Compiler::NodeKind::LiteralString)
    ast.literal_content_string(first).should eq("a\n")
    ast.literal_content_string(second).should eq("b\n")
    ast.literal_content_span(first).should_not eq(ast.literal_content_span(second))
  end

  it "retains source-backed content spans for static literal consumers" do
    ast = facet_ast(%q('c'; '\n'; :foo; :"bar baz"; require "./dep"; asm("nop" : "=r"(x) :: "memory")))
    expressions = ast.children(ast.root)[0]
    char, escaped_char, symbol, quoted_symbol, require_node, asm_node = ast.children(expressions)

    ast.literal_content_string(char).should eq("c")
    ast.literal_content_string(escaped_char).should eq(%q(\n))
    ast.literal_content_string(symbol).should eq("foo")
    ast.literal_content_string(quoted_symbol).should eq("bar baz")

    require_literal = ast.children(require_node)[0]
    ast.literal_content_string(require_literal).should eq("./dep")

    asm_children = ast.children(asm_node)
    ast.literal_content_string(asm_children[0]).should eq("nop")
    output = ast.children(asm_children[1])[0]
    ast.literal_content_string(ast.children(output)[0]).should eq("=r")
    clobber = ast.children(asm_children[3])[0]
    ast.literal_content_string(clobber).should eq("memory")
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

  it "does not let unreachable arena nodes hide a significant token" do
    source = Facet::Compiler::Source.new("lost")
    arena = Facet::Compiler::AstArena.new
    arena.add_ident(Facet::Compiler::Span.new(0, source.size), arena.symbols.intern("lost"))
    nop = arena.add_node(Facet::Compiler::NodeKind::Nop, Facet::Compiler::Span.new(0, source.size))
    expressions = arena.add_node(Facet::Compiler::NodeKind::Expressions, Facet::Compiler::Span.new(0, source.size), [nop])
    root = arena.add_node(Facet::Compiler::NodeKind::File, Facet::Compiler::Span.new(0, source.size), [expressions])
    ast = Facet::Compiler::AstFile.new(source, root, arena, [] of Facet::Compiler::Diagnostic)
    tokens = Facet::Compiler::Lexer.new(source).tokenize_all

    Facet::Compiler::AstIntegrity.missing_semantic_tokens(ast, tokens).map(&.kind).should eq([
      Facet::Compiler::TokenKind::Identifier,
    ])
  end

  it "rejects malformed child roles in the native AST contract" do
    source = Facet::Compiler::Source.new("")
    arena = Facet::Compiler::AstArena.new
    nop = arena.add_node(Facet::Compiler::NodeKind::Nop, Facet::Compiler::Span.new(0, 0))
    root = arena.add_node(Facet::Compiler::NodeKind::File, Facet::Compiler::Span.new(0, 0), [nop])
    ast = Facet::Compiler::AstFile.new(source, root, arena, [] of Facet::Compiler::Diagnostic)

    violations = Facet::Compiler::AstIntegrity.contract_violations(ast)
    violations.should contain("File##{root} child 0 is Nop; expected Expressions")
  end

  it "rejects cycles and mismatched literal payloads" do
    empty_source = Facet::Compiler::Source.new("")
    cyclic_arena = Facet::Compiler::AstArena.new
    nop = cyclic_arena.add_node(Facet::Compiler::NodeKind::Nop, Facet::Compiler::Span.new(0, 0))
    expressions = cyclic_arena.add_node(Facet::Compiler::NodeKind::Expressions, Facet::Compiler::Span.new(0, 0), [nop])
    cyclic_arena.edges[cyclic_arena.node(expressions).first_child] = expressions
    root = cyclic_arena.add_node(Facet::Compiler::NodeKind::File, Facet::Compiler::Span.new(0, 0), [expressions])
    cyclic_ast = Facet::Compiler::AstFile.new(empty_source, root, cyclic_arena, [] of Facet::Compiler::Diagnostic)

    Facet::Compiler::AstIntegrity.contract_violations(cyclic_ast)
      .should contain("node #{expressions} forms a cycle in the reachable AST")

    source = Facet::Compiler::Source.new("1")
    payload_arena = Facet::Compiler::AstArena.new
    payload = payload_arena.add_literal(Facet::Compiler::LiteralKind::Number)
    literal = payload_arena.add_node(Facet::Compiler::NodeKind::LiteralString, Facet::Compiler::Span.new(0, 1), payload_index: payload)
    expressions = payload_arena.add_node(Facet::Compiler::NodeKind::Expressions, Facet::Compiler::Span.new(0, 1), [literal])
    root = payload_arena.add_node(Facet::Compiler::NodeKind::File, Facet::Compiler::Span.new(0, 1), [expressions])
    payload_ast = Facet::Compiler::AstFile.new(source, root, payload_arena, [] of Facet::Compiler::Diagnostic)

    Facet::Compiler::AstIntegrity.contract_violations(payload_ast)
      .should contain("LiteralString##{literal} has Number literal payload; expected String")

    content_arena = Facet::Compiler::AstArena.new
    content_payload = content_arena.add_literal(
      Facet::Compiler::LiteralKind::String,
      Facet::Compiler::Span.new(0, 2)
    )
    content_literal = content_arena.add_node(
      Facet::Compiler::NodeKind::LiteralString,
      Facet::Compiler::Span.new(0, 1),
      payload_index: content_payload
    )
    expressions = content_arena.add_node(
      Facet::Compiler::NodeKind::Expressions,
      Facet::Compiler::Span.new(0, 1),
      [content_literal]
    )
    root = content_arena.add_node(
      Facet::Compiler::NodeKind::File,
      Facet::Compiler::Span.new(0, 1),
      [expressions]
    )
    content_ast = Facet::Compiler::AstFile.new(
      source,
      root,
      content_arena,
      [] of Facet::Compiler::Diagnostic
    )

    Facet::Compiler::AstIntegrity.contract_violations(content_ast)
      .any? { |violation| violation.includes?("LiteralString##{content_literal} has invalid literal content span") }
      .should be_true
  end

  it "covers every node kind produced by accepted syntax" do
    covered = [] of Facet::Compiler::NodeKind
    AST_CONTRACT_CASES.each do |contract|
      ast = facet_ast(contract[:source])
      reachable = Facet::Compiler::AstIntegrity.reachable_node_ids(ast)
      covered.concat(reachable.map { |node_id| ast.node(node_id).kind })
    end

    intentionally_nonsemantic = {
      Facet::Compiler::NodeKind::Error,
      Facet::Compiler::NodeKind::Const,
    }
    expected = Facet::Compiler::NodeKind.values.reject { |kind| intentionally_nonsemantic.includes?(kind) }
    (expected - covered.uniq).should be_empty
  end
end
