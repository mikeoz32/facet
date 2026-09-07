# Crystal macro compatibility

Facet pins its macro contract corpus to Crystal 1.21.0 revision
`57cf7da5094db6c5d3c058c6d054a757b5ced19e`. The source suites are:

- `spec/compiler/macro/macro_expander_spec.cr`
- `spec/compiler/macro/macro_methods_spec.cr`
- `spec/compiler/semantic/macro_spec.cr`
- `spec/compiler/semantic/macro_overload_spec.cr`

The first two source suites contain 731 declared examples and 973 syntactic
`assert_macro` calls. Compile-time loops expand those into 1,017 executed
contracts at runtime. The semantic suites add 133 examples. The committed
fixtures retain both the static inventory and every executed evaluator input,
so an unsupported contract cannot disappear from the denominator.

## Current result

Facet matches **1,017/1,017 (100%)** executed runtime contracts. This is the
primary no-regression gate. It checks exact expansion text for successful
evaluations, exact diagnostic text for four expected `parse_type` failures, and
exact output side effects for six print-family contracts. It includes every one
of the original 371 self-contained contracts plus argument-bearing,
compile-time-generated, environment/flag, captured-command, and structured
program-type-context cases from the executing Crystal 1.21 specs.

The runtime corpus contains 1,017 contracts in total:

| Runtime slice | Count | Current status |
| --- | ---: | --- |
| Direct, source-replayable calls | 900 | 900 exact; full portable slice |
| Program-context calls | 117 | 117 exact through captured structured type state and output effects |
| Calls carrying AST arguments | 578 | Included in the totals above; their exact AST kind and source rendering are retained |

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
runtime capture resolves dynamic bodies, compile-time loops, and actual AST
arguments, then classifies the resulting 1,017 executions directly. Neither
1,017/1,017 nor 371/371 is a claim of complete Crystal macro compatibility.
Broader diagnostic/error assertions and 133 semantic examples remain explicit
backlog.

Every portable direct AST-field, returned-collection, `env`/`flag?`,
`parse_type`, and backtick contract is exact. Ambient environment values,
compiler flags, and command outputs are explicit `MacroExpansionContext`
inputs and participate in expansion cache fingerprints. Facet never executes
an arbitrary shell command: a caller must provide its captured output, and an
unknown command remains source-backed.

The contextual slice snapshots 105 official `TypeNode` arguments with their
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

## Next coverage layers

1. Add expected diagnostic and exception parity for the broader
   `assert_macro_error` inventory. The four nested `parse_type`
   `expect_raises` contracts are already in the runtime gate.
2. Port the 133 semantic macro examples once name resolution and type semantics
   can express their contracts without the Crystal compiler runtime.
