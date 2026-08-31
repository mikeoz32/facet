# Crystal macro compatibility

Facet pins its macro contract corpus to Crystal 1.21.0 revision
`57cf7da5094db6c5d3c058c6d054a757b5ced19e`. The source suites are:

- `spec/compiler/macro/macro_expander_spec.cr`
- `spec/compiler/macro/macro_methods_spec.cr`
- `spec/compiler/semantic/macro_spec.cr`
- `spec/compiler/semantic/macro_overload_spec.cr`

The first two suites contain 731 declared examples and 973 `assert_macro`
calls. The semantic suites add 133 examples. The committed fixture records the
whole inventory, all assertion kinds, and every exclusion category so an
unsupported contract cannot disappear from the denominator.

## Current result

The 371 self-contained evaluator contracts all match Crystal's exact expansion
text: **371/371 (100%)**.

Another 602 `assert_macro` calls remain outside this first executable slice:

| Category | Count | Why it is not executed yet |
| --- | ---: | --- |
| `requires_context` | 593 | Injected Crystal AST/compiler objects, program mutation, flags, blocks, or extra expected metadata need a richer fixture schema. |
| `dynamic_expression` | 3 | The expected source or result is constructed dynamically by the spec. |
| `ambient_environment` | 2 | The assertion depends on a surrounding `with_env` setup. |
| `expected_exception` | 4 | The assertion is nested in `expect_raises` and belongs in the future error-parity runner. |

This is 100% parity for the reproducible, self-contained evaluator slice, not a
claim of complete Crystal macro compatibility. In particular, the 602
contextual assertions and 133 semantic examples remain explicit backlog.

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

The regular test suite executes every baseline contract independently and also
reruns all eligible cases as an aggregate parity gate:

```bash
CRYSTAL_CACHE_DIR=/tmp/facet-spec-cache \
  crystal spec spec/upstream_macro_corpus_spec.cr
```

## Next coverage layers

1. Encode injected upstream AST values in a Facet-native fixture schema.
2. Add expected diagnostic and exception parity for `assert_macro_error` and
   nested `expect_raises` contracts.
3. Model compiler inputs such as flags, environment reads, and program/type
   setup as explicit expansion dependencies.
4. Port the 133 semantic macro examples once name resolution and type semantics
   can express their contracts without the Crystal compiler runtime.
