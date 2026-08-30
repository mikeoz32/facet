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
- Multi-file macro indexing and partial macro expansion with origin tracking.
- `SourceManager` and `QueryDb` caching for parse, index, and expansion queries.
- Compatibility checks against the actual upstream Crystal 1.21 lexer/parser
  spec inputs. Facet currently matches Crystal::Parser acceptance/rejection on
  all 4,378 unique parser inputs, retains every significant token in a bounded
  AST node, and fully consumes all 690 unique lexer inputs without unknown
  tokens or non-trivia gaps. Lexer diagnostic presence also matches on all 687
  inputs whose upstream state can be reconstructed from source text alone.
- A committed Crystal 1.21 parser fixture makes all 4,378 upstream parser
  inputs permanent native Facet specs. It also retains the upstream AST inspect
  oracle for accepted inputs and the exact message/location oracle for rejected
  inputs so deeper parity can be tightened without another Crystal checkout.
- A clean parser baseline across all 1,625 files in the Crystal 1.21
  standard-library source tree.

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

## Incremental queries

```crystal
manager = Facet::Compiler::SourceManager.new
file_id = manager.add("macro answer; 42; end", "macros.cr")
queries = Facet::Compiler::QueryDb.new(manager)

parsed = queries.parse(file_id)
expanded = queries.expand(file_id)

manager.update(file_id, "macro answer; 43; end")
queries.invalidate(file_id)
```

`QueryDb` caches parse, macro-index, and expansion results. Call `invalidate`
after updating a source so dependent macro expansions are recalculated.

## Architecture

- `Source` / `SourceManager`: source text, versions, fingerprints, and virtual origins.
- `Lexer` / `TokenStream`: tokenization and parser lookahead.
- `Parser`: tolerant syntax parsing and validation diagnostics.
- `AstArena` / `AstFile`: compact syntax storage and source spans.
- `ProgramIndex`: currently indexes macro definitions across files.
- `MacroExpander` / `Hygiene`: partial compile-time expansion support.
- `QueryDb`: cached frontend queries and dependency invalidation.

## cr-analyzer integration

[cr-analyzer](https://github.com/mikeoz32/cr-analyzer) uses Facet 0.1.5 for
syntax diagnostics. Its semantic index still uses Crystal::Parser, providing a
safe environment for measuring Facet compatibility before a deeper migration.

## Development

```bash
crystal spec
crystal spec spec/parser_spec.cr
crystal run scripts/bench_lexer.cr
crystal run scripts/check_parser_compat.cr
```

The full suite includes lexer coverage against the installed Crystal stdlib and
ported parser compatibility cases. `check_parser_compat.cr` parses each source
file in an isolated subprocess so a parser crash cannot abort the corpus run.
Pass files or directories after `--` to scan another Crystal codebase, for
example `crystal run scripts/check_parser_compat.cr -- src`.

### Upstream spec parity

The stdlib scan only proves that valid files produce no diagnostics. The
upstream parity checks additionally compare valid and invalid parser inputs,
validate AST spans and semantic-token retention, and verify that the lexer did
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

# Report exact message/location parity against the committed error oracle.
crystal run scripts/report_upstream_parser_diagnostics.cr
```

Crystal and Facet intentionally expose different lexer token models, so the
lexer check verifies total consumption, spans, unknown tokens, crashes, and
non-trivia gaps rather than requiring identical token-array shapes. It also
compares whether each input raises a lexer diagnostic. Three source-only cases
are reported separately because the upstream result depends on mutable lexer
state (`slash_is_regex` for `/` and `/=`) or on consuming only the heredoc
opener instead of the whole input. Parser AST node classes also differ;
acceptance parity and token retention do not yet claim structural or semantic
AST equivalence with the compiler.

Current Crystal 1.21.0 parity baseline:

| Surface | Upstream suite | Facet replay result |
| --- | ---: | --- |
| Parser | 4,474 examples; 4,378 unique inputs | 4,378 acceptance decisions matched; 0 invariant failures; 0 uncovered significant tokens |
| Lexer | 708 examples; 690 unique inputs | 690 fully consumed; 0 structural failures; 0 diagnostic mismatches across 687 source-reproducible inputs; 3 state-dependent cases reported separately |
| Facet native parser suite | — | 7,173 examples passing; all 4,378 upstream inputs committed locally |
| Crystal stdlib corpus | 1,625 source files | 1,625 clean; 0 diagnostics; 0 crashes |

Raw example counts are not one-to-one coverage measures: Crystal helpers often
exercise multiple inputs inside one example, and Facet's compact token and AST
models intentionally differ. The replay results are the stronger parity signal
because they execute every unique upstream input and include invalid syntax,
diagnostic presence, span invariants, and anti-skip token retention checks.

Native input coverage is now complete, but exact output parity is a separate
metric. For the 941 rejected parser inputs, Facet currently matches Crystal's
exact first diagnostic message in 872 cases, its line/column in 858 cases, and
both in 837 cases. The committed fixture gates acceptance/rejection, AST span
integrity, absence of error nodes in accepted trees, semantic-token retention,
diagnostic span validity, and non-regression thresholds for the exact first
diagnostic oracle. It does not yet claim identical Crystal/Facet AST shapes or
identical diagnostic wording for every case.

## Contributing

Open an issue or pull request at <https://github.com/mikeoz32/facet>. Include a
focused parser/AST example and regression spec when changing syntax behavior.

## Contributors

- [Mike Oz](https://github.com/mikeoz32) - creator and maintainer
