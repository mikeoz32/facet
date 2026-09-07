# Crystal macro compatibility

Facet pins its macro contract corpus to Crystal 1.21.0 revision
`57cf7da5094db6c5d3c058c6d054a757b5ced19e`. The source suites are:

- `spec/compiler/macro/macro_expander_spec.cr`
- `spec/compiler/macro/macro_methods_spec.cr`
- `spec/compiler/semantic/macro_spec.cr`
- `spec/compiler/semantic/macro_overload_spec.cr`
- the complete `spec/compiler/semantic` suite

The first two source suites contain 731 declared examples, 973 syntactic
`assert_macro` calls, and 25 `assert_macro_error` calls. Compile-time loops
expand the successful assertions into 1,017 executions; together with the 25
error assertions they form 1,042 runtime contracts. The semantic suites add 133
examples which execute 147 distinct macro-expansion events: 69 user-macro calls
and 78 inline expansions. The full semantic suite executes 3,288 examples (nine
upstream pending) and emits 150,926 macro events while repeatedly constructing
programs and loading the standard library. After removing only the 1,832 events
whose invocation source is `src/primitives.cr`, exact duplicates are collapsed
by invocation, definition, scope, target flags, semantic snapshots, and oracle.
That leaves 2,736 distinct expansion contexts. The committed fixtures retain
both the static inventory and every distinct executed evaluator or semantic
input, so an unsupported contract cannot disappear from the denominator.

## Current result

Facet matches **1,042/1,042 (100%)** executed runtime contracts. This is the
primary no-regression gate. It checks exact expansion text for successful
evaluations, exact diagnostic text for all 29 expected failures (25
`assert_macro_error` calls plus four nested `parse_type` failures), and exact
output side effects for six print-family contracts. It includes every one of
the original 371 self-contained contracts plus argument-bearing,
compile-time-generated, environment/flag, captured-command, and structured
program-type-context cases from the executing Crystal 1.21 specs.

Facet also matches **147/147 (100%)** captured expansion events from all 133
official semantic macro examples. The gate runs every event, including all 131
successful expansions and 16 expansion errors, and never removes an unsupported
event from the denominator. It accepts exact text or equivalent Facet semantic
AST for successful output and exact diagnostic text for failures. Generic and
free-variable bindings, named-tuple key locations, type-member snapshots,
compile-time constants, resolved paths and path errors are explicit fixture
inputs that participate in the expansion-context fingerprint.

Across the complete official semantic suite, Facet matches **2,736/2,736
(100%)** distinct expansion contexts: all 1,077 user-macro call events and all
1,659 inline expansions. All 2,696 successful expansions and 40 expansion
errors remain in the corpus. This broader gate covers target-flag branches plus
stdlib and bootstrap macros which the dedicated macro examples never execute.
Successful results require equivalent semantic AST plus identical literal
payloads; failures require exact diagnostic text. It compares one Facet
`expand_once` compiler pass to one upstream macro expansion; the normal
`expand` API continues iterating to a fixed point.

The runtime corpus contains 1,042 contracts in total:

| Runtime slice | Count | Current status |
| --- | ---: | --- |
| Direct, source-replayable calls | 900 | 900 exact; full portable slice |
| Program-context calls | 142 | 142 exact through captured structured type state, expected errors, and output effects |
| Calls carrying AST arguments | 592 | Included in the totals above; their exact AST kind and source rendering are retained |

The earlier static extractor remains useful as a minimal hermetic layer. Its
371 self-contained evaluator contracts still match exactly: **371/371 (100%)**.

The earlier static extractor intentionally excludes 602 syntactic
`assert_macro` calls from its 371-case hermetic slice:

| Category | Count | Why static extraction excludes it |
| --- | ---: | --- |
| `requires_context` | 593 | Injected objects, program mutation, flags, blocks, or extra metadata require runtime capture. |
| `dynamic_expression` | 3 | The expected source or result is constructed dynamically by the spec. |
| `ambient_environment` | 2 | The assertion depends on a surrounding `with_env` setup. |
| `expected_exception` | 4 | The assertion is nested in `expect_raises`; runtime capture records its exact diagnostic. |

The static exclusions are not a second set of missing runtime contracts: the
runtime capture resolves dynamic bodies, compile-time loops, actual AST
arguments, and all 25 evaluator error assertions, then classifies the resulting
1,042 executions directly. Neither 1,042/1,042, 371/371, 147/147, nor
2,736/2,736 is by itself a claim of compatibility beyond the captured Crystal
1.21 behavior. The semantic
event corpora make both the dedicated macro-suite behavior and the broader
compiler/stdlib surface explicit and regression-tested without embedding
Crystal compiler objects in Facet.

Every portable direct AST-field, returned-collection, `env`/`flag?`,
`parse_type`, and backtick contract is exact. Ambient environment values,
compiler flags, and command outputs are explicit `MacroExpansionContext`
inputs and participate in expansion cache fingerprints. Facet never executes
an arbitrary shell command: a caller must provide its captured output, and an
unknown command remains source-backed.

The contextual slice snapshots 106 official `TypeNode` arguments with their
requested names, type variables, members, ancestry, visibility, predicates,
and relationships. Six print-family cases additionally compare captured stdout
with `MacroExpander#side_effect_output`. This keeps compiler state explicit and
cacheable instead of making Facet depend on Crystal compiler objects.

Output-fragment diagnostics are tracked separately from evaluator diagnostics.
An official macro result such as `1, 2, 3` is a valid splat fragment in its
caller even though it is not a valid standalone Crystal file; the harness still
requires exact output and zero evaluator diagnostics.

## Reproducing the corpus

Generate the fixture from a Crystal 1.21.0 checkout:

```bash
CRYSTAL_CACHE_DIR=/tmp/facet-macro-fixture-cache \
  crystal run scripts/generate_upstream_macro_fixture.cr -- \
  /path/to/crystal spec/fixtures/crystal_1_21_macro.jsonl
```

Run every eligible case and refresh the no-regression baseline:

```bash
CRYSTAL_CACHE_DIR=/tmp/facet-macro-parity-cache \
  crystal run scripts/check_upstream_macro_parity.cr -- \
  spec/fixtures/crystal_1_21_macro.jsonl \
  spec/fixtures/crystal_1_21_macro_supported.txt
```

Capture every contract actually executed by the official evaluator suites:

```bash
git -C /path/to/crystal apply /path/to/facet/scripts/upstream_macro_runtime_capture.patch
FACET_MACRO_CAPTURE=/tmp/crystal-macro-runtime.jsonl \
  /path/to/crystal/bin/crystal spec \
  /path/to/crystal/spec/compiler/macro/macro_expander_spec.cr \
  /path/to/crystal/spec/compiler/macro/macro_methods_spec.cr

CRYSTAL_CACHE_DIR=/tmp/facet-macro-fixture-cache \
  crystal run scripts/normalize_upstream_macro_capture.cr -- \
  /tmp/crystal-macro-runtime.jsonl /path/to/crystal \
  spec/fixtures/crystal_1_21_macro_runtime.jsonl
```

Run the complete executed runtime slice and refresh its no-regression baseline:

```bash
CRYSTAL_CACHE_DIR=/tmp/facet-macro-parity-cache \
  crystal run scripts/check_upstream_macro_runtime_parity.cr -- \
  spec/fixtures/crystal_1_21_macro_runtime.jsonl \
  spec/fixtures/crystal_1_21_macro_runtime_supported.txt
```

The regular test suite executes every baseline contract independently and also
reruns all eligible cases as an aggregate parity gate:

```bash
CRYSTAL_CACHE_DIR=/tmp/facet-spec-cache \
  crystal spec spec/upstream_macro_corpus_spec.cr \
    spec/upstream_macro_runtime_corpus_spec.cr
```

Capture every expansion event executed by the official semantic macro suites:

```bash
git -C /path/to/crystal apply \
  /path/to/facet/scripts/upstream_macro_semantic_capture.patch
FACET_SEMANTIC_MACRO_EVENT_CAPTURE=/tmp/crystal-macro-semantic-events.jsonl \
  /path/to/crystal/bin/crystal spec \
  /path/to/crystal/spec/compiler/semantic/macro_spec.cr \
  /path/to/crystal/spec/compiler/semantic/macro_overload_spec.cr

CRYSTAL_CACHE_DIR=/tmp/facet-semantic-fixture-cache \
  crystal run scripts/normalize_upstream_macro_semantic_capture.cr -- \
  /tmp/crystal-macro-semantic-events.jsonl /path/to/crystal \
  spec/fixtures/crystal_1_21_macro_semantic_events.jsonl focused
```

Run all 147 semantic events and refresh their no-regression baseline:

```bash
CRYSTAL_CACHE_DIR=/tmp/facet-semantic-parity-cache \
  crystal run scripts/check_upstream_macro_semantic_parity.cr -- \
  spec/fixtures/crystal_1_21_macro_semantic_events.jsonl \
  spec/fixtures/crystal_1_21_macro_semantic_events_supported.txt

CRYSTAL_CACHE_DIR=/tmp/facet-semantic-spec-cache \
  crystal spec spec/upstream_macro_semantic_corpus_spec.cr
```

Capture and deduplicate the broader full-semantic corpus:

```bash
FACET_SEMANTIC_MACRO_EVENT_CAPTURE=/tmp/crystal-full-semantic-events.jsonl \
  /path/to/crystal/bin/crystal spec \
  /path/to/crystal/spec/compiler/semantic

CRYSTAL_CACHE_DIR=/tmp/facet-semantic-fixture-cache \
  crystal run scripts/normalize_upstream_macro_semantic_capture.cr -- \
  /tmp/crystal-full-semantic-events.jsonl /path/to/crystal \
  spec/fixtures/crystal_1_21_macro_semantic_full_events.jsonl full

CRYSTAL_CACHE_DIR=/tmp/facet-semantic-parity-cache \
  crystal run scripts/check_upstream_macro_semantic_parity.cr -- \
  spec/fixtures/crystal_1_21_macro_semantic_full_events.jsonl \
  spec/fixtures/crystal_1_21_macro_semantic_full_events_supported.txt

CRYSTAL_CACHE_DIR=/tmp/facet-semantic-spec-cache \
  crystal spec spec/upstream_macro_semantic_full_corpus_spec.cr
```

## Next coverage layers

1. Extend live require-aware provider and type-state construction so production
   callers can supply the same explicit context without a Crystal runtime.
2. Add upstream captures for macro behavior not exercised by Crystal's semantic
   suite while preserving the 2,736-event full-suite gate.
