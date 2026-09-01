# Facet

Facet is an experimental Crystal language frontend written in Crystal. Version
0.1.5 includes a standalone lexer, tolerant parser, compact arena-backed AST,
diagnostics, macro expansion primitives, and an incremental query cache.

Facet is not a drop-in replacement for the Crystal compiler yet. It currently
targets parser tooling, editor integrations, and the frontend foundation needed
for future name resolution, type checking, and compilation stages.

## Current capabilities

- Crystal lexer with byte-accurate source spans and stdlib coverage tests.
- Parser for the broad Crystal syntax surface, including types, FFI, annotations,
  macros, blocks, calls, literals, assignments, and control flow.
- Error nodes and diagnostics so tooling can keep working on incomplete source.
- Compact `AstArena` representation with interned symbols and source-backed text.
- Multi-file macro indexing and partial macro expansion with origin tracking,
  lexical environments, control flow, and hygienic macro variables.
- `SourceManager` and `QueryDb` caching for parse, syntax, index, and expansion
  queries with automatic revision-based invalidation.
- `SyntaxTree` / `SyntaxNode` named declaration, callee/receiver/argument,
  parameter type/default, body, control-flow condition, traversal, cursor lookup,
  documentation, and UTF-16 position queries for editor and compiler consumers.
- Compatibility checks against the actual upstream Crystal 1.21 lexer/parser
  spec inputs. Facet currently matches Crystal::Parser acceptance/rejection on
  all 4,378 unique parser inputs. Every accepted input must retain each
  significant token in a reachable semantic AST node and pass the native AST
  schema for child roles, arity, payloads, flags, spans, and graph integrity.
  All 3,437 accepted inputs also match a committed common semantic projection
  covering construct shape, names, operators, child order, and semantic flags.
  The lexer fully consumes all 690 unique inputs without unknown tokens or
  non-trivia gaps. Lexer diagnostic presence also matches on all 687 inputs
  whose upstream state can be reconstructed from source text alone.
- A committed Crystal 1.21 parser fixture makes all 4,378 upstream parser
  inputs permanent native Facet specs. It also retains the upstream AST inspect
  oracle for accepted inputs and the exact message/location oracle for rejected
  inputs so deeper parity can be tightened without another Crystal checkout.
- A clean parser baseline across all 1,625 files in the Crystal 1.21
  standard-library source tree.
- A committed Crystal 1.21 macro contract corpus. Facet matches exact expansion
  text for 718/900 portable contracts captured from the executing official
  evaluator specs; all 371 statically self-contained contracts remain exact.
  Another 117 runtime contracts requiring mutated compiler program state and
  133 semantic examples remain explicit; see [macro parity](MACRO_PARITY.md).

Not implemented yet: complete compiler semantics, name and overload resolution,
type inference/checking, require graph resolution, lowering, code generation, and
binary production. APIs may change while those layers are designed.

## Installation

Add Facet to `shard.yml`:

```yaml
dependencies:
  facet:
    github: mikeoz32/facet
```

Then install dependencies:

```bash
shards install
```

Facet requires Crystal 1.18.2 or newer.

## Parsing source

```crystal
require "facet/compiler"

source = Facet::Compiler::Source.new("class Greeter; end", "greeter.cr")
parser = Facet::Compiler::Parser.new(source)
ast = parser.parse_file

parser.diagnostics.each do |diagnostic|
  puts "#{diagnostic.message} at #{diagnostic.span.start}"
end

root = ast.node(ast.root)
puts root.kind
```

`AstFile` owns the source, root node ID, arena, and diagnostics. Nodes refer to
children by integer IDs; use `AstFile#node`, `AstFile#children`, and
`AstFile#node_string` to inspect them.

Literal nodes retain their outer syntax span and, when the value occupies a
different source range, a separate payload-backed content span. Use
`AstFile#literal_content_span`, `AstFile#literal_content`, or
`AstFile#literal_content_string` to inspect the raw source-backed body. Use
`AstFile#decoded_literal_string` for its Crystal value. Literal payloads retain
the spelling style because `"\n"`, `%q(\n)`, and `%w(\n)` share similar source
bytes but obey different escape rules. The decoder also handles character and
quoted-symbol escapes, regex delimiters, heredoc indentation, and imported
literal payloads inside `#{...}`. Distinct content spans remain necessary for
multiple heredocs declared on one header line, whose outer syntax spans overlap
while their bodies remain separate.

`Facet::Compiler::AstIntegrity.contract_violations(ast)` validates Facet's
native AST contract.
It traverses only nodes reachable from the root: arena nodes left behind by
parser speculation cannot satisfy semantic-token ownership or affect the
meaning of the returned tree. Immutable child nodes may be shared, so the
reachable representation is a DAG, but cycles are rejected.

## Syntax queries

```crystal
manager = Facet::Compiler::SourceManager.new
file_id = manager.add("class Greeter; def hello(name : String); end; end", "greeter.cr")
queries = Facet::Compiler::QueryDb.new(manager)
tree = queries.syntax(file_id)

method = tree.nodes(Facet::Compiler::NodeKind::Def).first
puts method.name                         # hello
puts method.parameters.first.name       # name
puts method.parameters.first.declared_type.try(&.text) # String
puts tree.node_at(method.span.start)     # smallest node at the cursor
puts tree.position_at(method.span.start) # zero-based UTF-16 editor position
```

The compact arena remains Facet's native AST. `SyntaxTree` is an indexed query
facade over it, not a compatibility copy of Crystal's AST. It centralizes child
roles, parent/ancestor traversal, qualified names, name spans, contiguous doc
comments, control-flow conditions, byte-offset cursor lookup, and UTF-8/UTF-16
conversion so downstream tools do not depend on arena layout details. Call
queries expose `callee`, `call_name`, `receiver`, positional and named arguments;
they preserve the same roles through parenthesized and bare block calls.
Parameter queries expose internal/external names, exact name spans, declared
types, and default values across regular, splat, double-splat, and block params.

## Incremental queries

```crystal
manager = Facet::Compiler::SourceManager.new
file_id = manager.add("macro answer; 42; end", "macros.cr")
queries = Facet::Compiler::QueryDb.new(manager)

parsed = queries.parse(file_id)
expanded = queries.expand(file_id)

queries.update(file_id, "macro answer; 43; end")
updated = queries.expand(file_id)
```

`QueryDb` is the incremental frontend database. It caches parse, macro-index,
global-index, and expansion queries by monotonic source revisions. Updates made
through either `QueryDb` or `SourceManager` are observed automatically; manual
invalidation is only needed when a consumer deliberately wants to discard a
cached result without changing source bytes.

Expansion dependencies are footprint-based. Changing an unrelated file does
not re-expand a cached file, while changing a macro provider invalidates every
expansion that used that macro. `QueryDb#stats` exposes execution and cache-hit
counters for tests and editor telemetry. `upsert` keeps stable file IDs for
named documents, and `apply_edit` accepts the same byte spans used throughout
the lexer, parser, and AST. `pending_expansion_file_ids` exposes the exact files
whose cached expansion became stale so incremental semantic consumers can
reindex them without rebuilding the workspace.

Type-aware expansions additionally depend on the workspace declaration
revision. This conservatively invalidates only materialized consumers that used
type introspection when a declaration may have changed, while ordinary macro
consumers retain exact macro-name/required-file invalidation.

## Macro expansion scope

`MacroExpander` resolves indexed macro definitions across files and binds
positional, named, default, splat, and double-splat arguments. It evaluates
Crystal truthiness (only `false` and `nil` are falsey), `if`/`unless`, `for`,
`begin`, ordinary non-output `{% ... %}` expressions, assignments, ranges,
tuples, named tuples, arrays, hashes, indexing, and common collection/string
methods, including `id`, `stringify`, and `symbolize`. Loop variables have
iteration scope while other macro assignments remain visible to following
iterations and expressions. Arguments outside the evaluable subset are kept as
opaque source-backed AST values, so calls, generic types, and other syntax are
substituted by `{{arg}}` instead of being discarded.

Literal and computed macro strings, symbols, identifiers, and opaque syntax
retain separate source rendering and scalar values. Direct interpolation keeps
Crystal syntax (`"value"`, `:value`, or an unquoted `id`), while basic
`is_a?`, `responds_to?`, and `nil?` predicates can branch on supported macro
AST values without erasing their node role.

Collection expressions support lexical `map`, `map_with_index`, `select`,
`reject`, `any?`, `all?`, `each`, and `each_with_index` blocks over evaluated
arrays, tuples, hashes, named tuples, ranges, and strings. The evaluator
preserves the distinct Crystal macro AST roles of arrays, tuples, hashes, named
tuples, ranges, strings, chars, symbols, regexes, and typed numeric literals.
It supports exact rendering, indexing/slicing, mutation, collection transforms,
numeric operations and kind preservation, common string/regex operations, and
block reductions. Block parameters stay local while assignments to outer macro
variables propagate between iterations.

Both `{{ macro_call(...) }}` and ordinary receiverless Crystal macro calls are
expanded. Ordinary calls resolve through lexical type scopes, select overloads
by arity, and support bare zero-argument calls while respecting parameters and
previous local assignments that shadow the macro name. Calls with an explicit
receiver remain runtime calls. User macro blocks are source-backed values:
`{{yield}}`, `{{block.body}}`, and `block.args` preserve caller syntax, and the
block body and parameters participate in expansion cache keys.

The standard `getter`, `setter`, and `property` families (including class,
query, bang, typed, and block forms) plus `record` have Facet-native lowering.
They therefore work without loading the stdlib macro templates and expand
across the same cached multi-pass pipeline as user macros.

The first type-aware macro surface is backed by `ProgramIndex`, not Crystal's
AST. `@type`, `resolve`/`resolve?`, `methods`, `instance_vars`, `constants`,
`superclass`, and `ancestors` expose Facet-native macro values. Method metadata
includes names, arguments, return types, bodies, and source; instance-variable
metadata includes names, declared types, defaults, and default presence. Type
kind predicates and explicit superclass comparisons with `<` are supported.
Types, methods, instance variables, and arguments expose `annotation` and
`annotations`; annotation values support names, positional/named indexing,
`args`, and `named_args`. Collection results can be mapped, filtered, sorted,
reversed, deduplicated, compacted, and joined in the regular evaluator.

`%name` and `%name{key}` nodes produce stable hygienic identifiers within one
expansion and distinct identifiers across keys and invocations. Expansions
that use `%name` or `gensym` bypass the text cache so cached output cannot
reintroduce identifier collisions.

This remains a partial macro interpreter, not Crystal's complete compiler macro
engine. The self-contained upstream evaluator slice is at 371/371, but 602
contextual evaluator assertions and 133 semantic examples still require
injected compiler objects, error contracts, flags/environment dependencies,
generic/union type semantics, and broader name/type resolution. Unsupported
non-output control expressions produce an explicit expansion diagnostic.

## Architecture

- `Source` / `SourceManager`: source text, versions, fingerprints, and virtual origins.
- `Lexer` / `TokenStream`: tokenization and parser lookahead.
- `Parser`: tolerant syntax parsing and validation diagnostics.
- `AstArena` / `AstFile`: compact syntax storage and source spans.
- `SyntaxTree` / `SyntaxNode` / `LineIndex`: stable consumer queries and editor positions.
- `ProgramIndex`: indexes macros plus type/member metadata used by type-aware expansion.
- `MacroExpander` / `Hygiene`: partial compile-time expansion support.
- `QueryDb`: revisioned parse/syntax/index/expansion queries and footprint invalidation.

## cr-analyzer integration

[cr-analyzer](https://github.com/mikeoz32/cr-analyzer) uses Facet 0.1.5 for
its incremental syntax database, diagnostics, cursor queries, symbols, and the
primary editor semantic index. Crystal::Parser remains a measured fallback
while macro-generated declarations and the remaining unsupported inference
shapes move to Facet.

## Development

```bash
crystal spec
crystal spec spec/parser_spec.cr
crystal spec spec/parser/ast_contract_spec.cr
crystal run scripts/bench_lexer.cr
crystal run scripts/bench_incremental_queries.cr -- 1000
crystal run scripts/check_parser_compat.cr
crystal run scripts/check_upstream_ast_shape.cr
crystal run scripts/check_percent_literal_parity.cr
crystal run scripts/check_literal_value_parity.cr
crystal run scripts/check_number_literal_parity.cr
crystal run scripts/check_regex_literal_parity.cr
crystal run scripts/check_heredoc_literal_parity.cr
crystal run scripts/check_symbol_literal_parity.cr
crystal run scripts/check_operator_parity.cr
crystal run scripts/check_type_syntax_parity.cr
crystal run scripts/check_collection_literal_parity.cr
crystal run scripts/check_call_syntax_parity.cr
```

The percent-literal matrix adds 1,060 generated cases across every ASCII letter
prefix, five delimiter forms, and representative raw/interpolated bodies. It
currently has zero acceptance mismatches with Crystal::Parser and prevents
unsupported prefixes from being silently treated as Facet-only literals.
The literal-value oracle directly compares 21 decoded strings, chars, symbols,
regexes, and heredocs with `Crystal::Parser`, plus all 30 supported and rejected
character-escape forms. Both matrices currently report zero mismatches.
The numeric cross-product adds 968 cases covering binary/octal/hex/decimal
cores, underscores, fractions, exponents, all supported width suffixes, and
plausible invalid suffixes; it also currently reports zero acceptance
mismatches.
Regex-option and heredoc-opener matrices add another 120 and 16 generated
cases. They enforce Crystal's `i/m/x` regex options and its `<<-TAG` /
`<<-'TAG'` heredoc forms without accepting Ruby-only variants.
Symbol and operator-context matrices add 58 and 228 cases. They cover quoted
and unquoted symbol bodies plus every operator token in infix, prefix, postfix,
and parenthesized positions, including implicit-dot scoping and compound
assignment before local definition. Both currently report zero mismatches.
The type-syntax cross-product adds 354 cases across local declarations,
parameters, return types, aliases, `uninitialized`, and typed arrays. It gates
both acceptance and the common semantic AST projection and currently reports
zero acceptance, contract, shape, or unsupported-node mismatches.
The collection-literal cross-product adds 192 cases across arrays, tuples,
hashes, named tuples, typed collections, splats, assignments, call arguments,
and nesting. It also gates acceptance plus both AST contracts and currently has
zero mismatches.
The call-syntax cross-product adds 153 cases across explicit, command, member,
global, named, splat, double-splat, block-argument, and shorthand-block calls.
It gates acceptance plus both AST contracts and currently has zero mismatches.

The full suite includes lexer coverage against the installed Crystal stdlib and
ported parser compatibility cases. `check_parser_compat.cr` parses each source
file in an isolated subprocess so a parser crash cannot abort the corpus run;
diagnostic-free files must also pass the recursive AST contract and reachable
semantic-token checks. Pass files or directories after `--` to scan another
Crystal codebase, for example `crystal run scripts/check_parser_compat.cr --
src`.

### Upstream spec parity

The stdlib scan only proves that valid files produce no diagnostics. The
upstream parity checks additionally compare valid and invalid parser inputs,
validate AST spans and semantic-token ownership, and verify that the lexer did
not silently skip non-trivia bytes.

Capture the inputs from a disposable Crystal 1.21.0 checkout:

```bash
git clone --depth 1 --branch 1.21.0 https://github.com/crystal-lang/crystal.git /tmp/crystal-1.21-parity
git -C /tmp/crystal-1.21-parity apply "$PWD/scripts/upstream_input_trace.patch"

cd /tmp/crystal-1.21-parity
CRYSTAL_PARSER_INPUT_TRACE=/tmp/crystal-parser-inputs.b64 crystal spec spec/compiler/parser
CRYSTAL_LEXER_INPUT_TRACE=/tmp/crystal-lexer-inputs.b64 \
CRYSTAL_LEXER_ERROR_TRACE=/tmp/crystal-lexer-errors.b64 \
  crystal spec spec/compiler/lexer
cd -

crystal run scripts/check_upstream_parser_parity.cr -- /tmp/crystal-parser-inputs.b64
crystal run scripts/check_upstream_lexer_coverage.cr -- \
  /tmp/crystal-lexer-inputs.b64 /tmp/crystal-lexer-errors.b64

# Regenerate the committed native parser corpus (requires Crystal 1.21.0).
crystal run scripts/generate_upstream_parser_fixture.cr -- \
  /tmp/crystal-parser-inputs.b64 spec/fixtures/crystal_1_21_parser.jsonl \
  spec/compiler/parser

# Regenerate and verify the portable semantic AST projection oracle.
crystal run scripts/generate_upstream_ast_shape_fixture.cr
crystal run scripts/check_upstream_ast_shape.cr

# Report exact message/location parity against the committed error oracle.
crystal run scripts/report_upstream_parser_diagnostics.cr
```

Crystal and Facet intentionally expose different lexer token models, so the
lexer check verifies total consumption, spans, unknown tokens, crashes, and
non-trivia gaps rather than requiring identical token-array shapes. It also
compares whether each input raises a lexer diagnostic. Three source-only cases
are reported separately because the upstream result depends on mutable lexer
state (`slash_is_regex` for `/` and `/=`) or on consuming only the heredoc
opener instead of the whole input. Parser AST node classes also differ and are
not required to match. Instead, both trees are normalized into a common
semantic projection that compares construct shape, names, operators, child
ordering, and semantic flags without imposing Crystal's AST representation on
Facet. The resulting 3,437 expected projections are committed as a portable
fixture and checked by the regular native spec suite.

`spec/parser/ast_contract_spec.cr` separately snapshots Facet's own compact
contract: node kinds, child ordering, symbol/operator payloads, semantic flags,
raw macro segments, source-backed literal forms, and distinct heredoc content
spans. Eighteen focused golden inputs cover every node kind produced by
accepted syntax, including FFI globals; only the recovery-only `Error` kind and
currently unused `Const` kind are outside the accepted-tree goldens.

Current Crystal 1.21.0 parity baseline:

| Surface | Upstream suite | Facet replay result |
| --- | ---: | --- |
| Parser | 4,474 examples; 4,378 unique inputs | 4,378 acceptance decisions matched; 3,437/3,437 accepted inputs match the semantic AST projection; 941/941 rejected inputs match the exact first diagnostic message and line/column; 0 invariant failures; 0 uncovered significant tokens |
| Lexer | 708 examples; 690 unique inputs | 690 fully consumed; 0 structural failures; 0 diagnostic mismatches across 687 source-reproducible inputs; 3 state-dependent cases reported separately |
| Facet native suite | — | 7,370 examples passing; all 4,378 upstream parser inputs committed locally; all 3,437 accepted trees pass both the recursive native contract and semantic projection oracle |
| Crystal stdlib corpus | 1,625 source files | 1,625 clean; 0 diagnostics; 0 AST integrity errors; 0 crashes |

Raw example counts are not one-to-one coverage measures: Crystal helpers often
exercise multiple inputs inside one example, and Facet's compact token and AST
models intentionally differ. The replay results are the stronger parity signal
because they execute every unique upstream input and include invalid syntax,
diagnostic presence, span invariants, and anti-skip token retention checks.

Native input and first-diagnostic coverage are complete for the captured
Crystal 1.21 parser suite. For all 941 rejected inputs, Facet matches Crystal's
exact first diagnostic message and line/column. The committed fixture gates
acceptance/rejection, reachable AST graph and span integrity, node arity and
child roles, payload and flag validity, absence of error nodes in accepted
trees, semantic-token ownership, diagnostic span validity, and the complete
first-diagnostic oracle. Accepted inputs additionally gate the common semantic
projection. Significant identifiers and literals must be owned by an explicit
reachable Facet payload or literal node; unreachable arena nodes, containers,
and `Nop` cannot satisfy this check. Facet retains its own AST representation;
the projection compares semantics, not node-class identity. Later recovery
diagnostics after the first error are not yet part of the parity oracle.

## Contributing

Open an issue or pull request at <https://github.com/mikeoz32/facet>. Include a
focused parser/AST example and regression spec when changing syntax behavior.

## Contributors

- [Mike Oz](https://github.com/mikeoz32) - creator and maintainer
