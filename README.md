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
- A growing compatibility suite ported from the upstream Crystal parser specs.

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
```

The full suite includes lexer coverage against the installed Crystal stdlib and
ported parser compatibility cases.

## Contributing

Open an issue or pull request at <https://github.com/mikeoz32/facet>. Include a
focused parser/AST example and regression spec when changing syntax behavior.

## Contributors

- [Mike Oz](https://github.com/mikeoz32) - creator and maintainer
