# Semantic parity

Facet 0.2.0 starts a compiler-grade semantic layer over its native AST. The
first committed upstream slice comes from eight Crystal 1.21 compiler semantic
suites and contains 529 contracts executed by 397 examples (two upstream
pending). Every contract is classified: 273 currently pass Facet exactly and
256 are listed with a deferred reason. A passing type contract requires the
same inferred type. A passing diagnostic contract requires the same semantic
decision, Facet diagnostic code, and source line/column; Facet intentionally
owns the diagnostic wording.

The current exact baseline is 273 contracts. It includes bare zero-argument
method calls, call-site specialization of untyped and defaulted parameters,
structural explicit generic and union arguments, canonical union presentation,
typed overload selection, and positional-signature specificity under the
captured preview-overload target option. Compact numeric suffixes and nil-last
union ordering also follow the upstream type contract. Method-level `forall`
variables are retained in the semantic index and inferred from values,
metaclasses, default arguments, optional unions, tuples, and generic return
positions; method bodies use the actual call-site parameter types.
Block return restrictions, splat restrictions, generic include constraints,
and keyed named-tuple identities participate in the same binding model. Nested
union and metaclass restrictions are reconstructed from native AST structure,
so grouping punctuation cannot distort semantic type resolution.

The current slice covers class construction/allocation, simple method return
inference, generic receiver substitution, lexical assignments, unions,
inheritance/includes, and conservative undefined-method reporting. It is not a
claim of complete Crystal semantic compatibility. The committed denominator
and deferred manifest make unsupported behavior visible and monotonic.

## Run the gate

```bash
CRYSTAL_CACHE_DIR=/tmp/facet-semantic-parity-cache \
  crystal run scripts/check_upstream_semantic_parity.cr

CRYSTAL_CACHE_DIR=/tmp/facet-semantic-spec-cache \
  crystal spec spec/upstream_semantic_corpus_spec.cr
```

`spec/fixtures/crystal_1_21_semantic_supported.txt` is the no-regression
baseline. `spec/fixtures/crystal_1_21_semantic_deferred.tsv` classifies every
remaining index as parser, target-flags, injected-primitives, incomplete,
unsupported-error, type-mismatch, or diagnostic-mismatch work.

## Refresh from Crystal 1.21

Use the exact upstream revision recorded in the fixture header:

```bash
git clone --depth 1 --branch 1.21.0 \
  https://github.com/crystal-lang/crystal.git /tmp/crystal-1.21-semantic
git -C /tmp/crystal-1.21-semantic apply \
  "$PWD/scripts/upstream_semantic_capture.patch"

FACET_SEMANTIC_CAPTURE=/tmp/crystal-semantic-contracts.jsonl \
  /tmp/crystal-1.21-semantic/bin/crystal spec \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/class_spec.cr \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/def_spec.cr \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/def_overload_spec.cr \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/new_spec.cr \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/method_missing_spec.cr \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/union_spec.cr \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/var_spec.cr \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/if_spec.cr

crystal run scripts/normalize_upstream_semantic_capture.cr -- \
  /tmp/crystal-semantic-contracts.jsonl \
  spec/fixtures/crystal_1_21_semantic_contracts.jsonl

crystal run scripts/check_upstream_semantic_parity.cr -- \
  spec/fixtures/crystal_1_21_semantic_contracts.jsonl \
  spec/fixtures/crystal_1_21_semantic_supported.txt --write
```

Building the official compiler specs requires an LLVM development toolchain and
libffi headers. The published Crystal runtime image may not include them.
