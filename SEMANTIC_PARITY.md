# Semantic parity

Facet 0.2.0 starts a compiler-grade semantic layer over its native AST. The
committed upstream slice comes from nine Crystal 1.21 compiler semantic suites
and contains 582 contracts executed by 449 examples (two upstream pending).
Every contract is classified: 433 currently pass Facet exactly and 149 are
listed with a deferred reason. A passing type contract requires the
same inferred type. A passing diagnostic contract requires the same semantic
decision, Facet diagnostic code, and source line/column; Facet intentionally
owns the diagnostic wording. No-error contracts require a complete snapshot
without a semantic diagnostic; the 16 captured primitive-injection contracts
remain explicitly deferred because the portable replay does not inject the
Crystal prelude.

The current exact baseline is 433 contracts. It includes bare zero-argument
method calls, call-site specialization of untyped and defaulted parameters,
structural explicit generic and union arguments, canonical union presentation,
typed overload selection, and the complete captured preview-overload
partial-order contract across positional, named, splat, and double-splat
parameters. Conflicting strictness dimensions preserve declaration order.
Compact numeric suffixes and nil-last
union ordering also follow the upstream type contract. Method-level `forall`
variables are retained in the semantic index and inferred from values,
metaclasses, default arguments, optional unions, tuples, and generic return
positions; method bodies use the actual call-site parameter types.
Block return restrictions, splat restrictions, generic include constraints,
and keyed named-tuple identities participate in the same binding model. Nested
union and metaclass restrictions are reconstructed from native AST structure,
so grouping punctuation cannot distort semantic type resolution.
Overload selection additionally accounts for built-in numeric ancestry,
receiver-relative `self`, structural tuple/generic restrictions, block
presence, named-argument order, double splats, and later definitions of the
same signature. Union arguments dispatch per concrete member and merge the
selected return types instead of widening every candidate together.
Explicit class `new` methods take precedence over synthesized constructors.
Initializer defaults are evaluated in instance scope, and instance-variable
types propagate from declarations, defaults, and named call arguments into
reader methods. `Union(...)` expressions normalize to union metaclasses while
reopened `Union` class methods remain callable and can inspect their member
tuple through `T`.
Block return types can specialize generic constructors and flow through
`&@block` instance variables to native `Proc#call`. Instance-variable facts are
keyed by specialized receiver, so one generic instantiation cannot widen
another. Class type parameters used as expressions retain literal runtime types
or type metaclasses, and dynamic self dispatch prefers receiver overrides while
following the implicit `Object` superclass. The two captured constructor-block
examples now infer their exact upstream type with complete snapshots, but stay
classified as injected-primitives because the portable gate never promotes a
contract carrying that upstream capture flag.
Explicit instance-variable reads (`object.@value`) use the receiver's field
types, including inherited fields and unions of separately specialized generic
receivers. Missing field types remain unknown without undefined-method errors;
dedicated instance-variable error diagnostics remain deferred.
Library `fun` declarations participate in method lookup with their declared
parameter and return types, native-name aliases, and revision-safe bindings.
Relative type declaration paths stay inside their lexical namespace, creating
implicit parent modules as needed; leading `::` retains absolute declarations.
Method and constructor calls validate positional/named argument shape, declared
argument types across union dispatch, and required or unexpected blocks before
body inference. Diagnostics use stable codes (`facet.wrong_arity`,
`facet.argument_mismatch`, `facet.argument_type_mismatch`, `facet.block_required`,
and `facet.unexpected_block`). A subclass initializer set hides parent
initializers; synthesized `new` calls bind to the selected initializer.
Errors inside instantiated methods and initializers are reported at the calling
expression, including repeated calls to the same erroneous specialization.
Uncalled method bodies do not publish these errors. Signature edits invalidate
the results. Unknown arguments, unresolved restrictions, module-valued arguments,
missing providers, generated signatures without source handles, and unresolved
argument splats remain conservative rather than producing conclusive mismatches.
The captured diagnostic oracle now also classifies upstream arity, argument-type,
and block errors; exact diagnostic codes and source positions remain required.
Type aliases are indexed as `SemanticDefinitionKind::Alias`, retaining their
declaration handles while resolving to canonical target types. Forward chains,
lexical and absolute lookup, namespace aliases, open and specialized generic
targets, unions, nilables, tuple/named-tuple and proc types participate in method
restrictions, returns, local declarations, casts, construction and generic
arguments. Method `forall` variables can shadow unqualified aliases; absolute
alias references retain their meaning. Alias declarations, targets and method
signature uses have navigation bindings. Inheritance and includes resolve aliased
owners, including the snapshot method-candidate API.
Alias cycles and undefined targets produce `facet.alias_cycle` and
`facet.undefined_type`; missing providers keep unresolved targets conservative.
Required-file edits invalidate target types, signatures and stale handles.
Generated aliases refresh when their macro provider changes: expansion merges
into snapshot-local index copies instead of mutating cached source indexes.
This is frontend alias support, not full Crystal generic-argument validation or
ABI layout semantics for FFI `type` declarations.
Constants are indexed as first-class semantic definitions and lazily inferred
without leaking top-level local variables into their value environment. Lookup
covers nested and absolute paths, lexical method scope, superclass and included
module ancestry, and `forall` metaclass paths. Implicit constant namespaces are
represented as modules, enum members retain their enum type, and required-file
constant edits invalidate dependent snapshots.
Undefined constants, constant initializers that reference unavailable locals,
constant inference cycles, and constants used in type positions match the
captured semantic decision, coded diagnostic, and source location. Deferred
method-body diagnostics are relocated to the instantiating call site when that
is the official contract.

Conditional inference carries branch-specific truthiness and `is_a?` types
through negation and short-circuit `&&`/`||`. It merges assignments from
surviving branches, removes terminated return-guard branches from the following
environment, and combines explicit return values with fallthrough results.
This includes narrowing from concrete unions as well as from an ancestor type
to multiple possible descendants.

`while` and `until` compute loop-carried local types with a bounded fixed point,
then publish node facts and call diagnostics from the converged state. `break`
captures exit values and environments, `next` contributes back edges, and nested
loops, method calls and analyzed blocks keep their control-flow contexts separate.
Loop expressions return `Nil`, break-value unions, or `NoReturn` for literal
infinite loops without exits. `NoReturn` is the bottom of type unions and stops
sequential fallthrough, including through call arguments and receivers.
Exit environments combine normal condition narrowing with break exits; locals
introduced in possibly empty loops become nilable. As in the captured upstream
contract, assignments in a literal-false loop still contribute local type facts.
Structurally growing facts widen to unknown after 16 passes and mark the snapshot
incomplete. Speculative passes do not populate method return caches or publish
call diagnostics. Required-provider edits invalidate the resulting loop types.
The parser also accepts loop expressions in assignment positions.

Declaration validation detects incompatible class/struct/module reopenings,
changes to generic parameter names, arity or splat positions, superclass changes,
undefined or invalid superclasses, inheritance cycles and class-like declarations
inside dynamic control flow. Explicit declarations follow source/require order;
conflicts carry a related location for the previous declaration. Implicit
namespaces reuse already-declared types. The first declaration retains canonical
type facts for tolerant analysis, and missing providers suppress conclusive
undefined-superclass findings.
Abstractness is retained across reopenings and provider edits. Synthesized `new`
and `allocate` reject abstract types, including aliases, generic receivers and
the builtin abstract roots; argument validation still takes precedence over
abstract-instantiation diagnostics. Explicit user factories retain normal method
dispatch. These checks use `facet.type_kind_mismatch`,
`facet.generic_parameter_mismatch`, `facet.superclass_mismatch`,
`facet.invalid_superclass`, `facet.inheritance_cycle`,
`facet.dynamic_type_declaration` and `facet.abstract_instantiation` codes.

Recursive method returns are solved as a reachable call component, with separate
facts for argument, receiver and block-result specializations. Direct and mutual
cycles without an exit infer `NoReturn`; reachable base branches contribute their
return types until the component stabilizes. Provisional nested returns are not
published as method caches, and a final pass replays body errors at each real
call site. Analysis bounds both fixed-point iterations and polymorphic call
growth, exposing unresolved growth as incomplete rather than hanging the editor.
Per-pass memoization distinguishes suppressed probes from reporting passes and
invalidates after instance-variable facts widen. Provisional constant types and
initializer errors are not cached across recursive replay.
`NoReturn` also propagates through eager operators, range construction, call
arguments and indexing while still checking operand diagnostics. Collection
and tuple literals retain their element types, including `NoReturn`, matching
Crystal's type inference rather than collapsing the whole literal to bottom.
Primitive-injected recursion contracts remain explicitly deferred.

Explicit method return annotations are checked against instantiated bodies,
including explicit returns, branch unions, receiver type parameters and free
variables. Successful results retain their inferred narrower type; `Nil`/`Void`
annotations discard returning values without hiding `NoReturn`. Annotations
close recursive edges but do not exempt method bodies from validation. Omitted
defaults are inferred in parameter order and checked against their restrictions;
numeric literals use bounded numeric restriction, and supplied arguments do not
evaluate unused defaults. Errors are reported at each real method/constructor
call with `facet.return_type_mismatch` or `facet.default_type_mismatch`.
Inherited generic signatures and type expressions use the instantiated declaring
ancestor rather than the subclass's parameter positions. Numeric defaults also
support explicit suffixes and restriction against individual union members.

The current slice covers class construction/allocation, recursive method return
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
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/if_spec.cr \
  /tmp/crystal-1.21-semantic/spec/compiler/semantic/const_spec.cr

crystal run scripts/normalize_upstream_semantic_capture.cr -- \
  /tmp/crystal-semantic-contracts.jsonl \
  spec/fixtures/crystal_1_21_semantic_contracts.jsonl

crystal run scripts/check_upstream_semantic_parity.cr -- \
  spec/fixtures/crystal_1_21_semantic_contracts.jsonl \
  spec/fixtures/crystal_1_21_semantic_supported.txt --write
```

Building the official compiler specs requires an LLVM development toolchain and
libffi headers. The published Crystal runtime image may not include them.
