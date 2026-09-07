# Changelog

## Unreleased

- Added static and runtime-generated Crystal 1.21 macro specification corpora
  with complete assertion/context inventories and no-regression runners. Facet
  now matches exact output, expected diagnostic text, and output side effects
  for all 1,042/1,042 contracts executed by the official evaluator specs
  (including all 900 portable and 142 program-context contracts, plus all
  371/371 statically self-contained contracts). This covers all 25 official
  `assert_macro_error` calls and all four nested `parse_type` failures.
- Added a captured event corpus for all 133 official semantic macro examples:
  147 expansion events (69 user-macro calls and 78 inline expansions), including
  131 successful outputs and 16 errors. Facet matches 147/147 by exact text or
  equivalent Facet semantic AST with no skipped events. Generic/free-variable
  bindings, named-tuple key locations, type-member snapshots, compile-time
  constants, resolved paths, and exact path errors are explicit captured inputs.
- Added a deduplicated full-semantic macro corpus from all 3,288 official
  `spec/compiler/semantic` examples (nine upstream pending). It retains 2,731
  distinct invocation/definition/scope/target-flag/semantic-context contracts
  from 150,926 raw events and currently matches 2,376/2,731. All 1,077
  user-macro call events match; all 355 remaining inline mismatches stay in the
  denominator.
- Expanded semantic snapshots with lexical scope, abstractness, type methods,
  constants and values; added hygienic AST alpha-normalization, external named
  parameter binding, enum and union subtype predicates, tuple sizes, generated
  declaration parsing, and caller-context diagnostic comparison. Parenthesized
  builtin spans and macro-header `do ... end` balancing now preserve the exact
  generated Facet AST contract.
- Added target flags to semantic expansion snapshots and cache fingerprints,
  single-pass `MacroExpander#expand_once`, `elsif` execution, nil optional block
  binding, external/internal macro parameter binding, nested `skip_file`, and
  nil-like `Nop` behavior for absent defaults and optional `yield` bodies.
- Added structured `@caller`, yielded-argument binding, `skip_file`, exact macro
  `raise` and undefined-variable diagnostics, semantic `TypeNode` argument
  resolution, tuple splat binding, and macro-control multi-assignment/bare-yield
  parsing. Caller AST numeric literals now participate in numeric operations and
  equality without losing their captured AST identity.
- Added snapshot-backed semantic macro context for generic substitution,
  named-tuple keys and locations, type instance variables/class methods,
  compile-time hash constants, and exact unresolved-path diagnostics. Verbatim
  macro bodies now scan nested raw tags without parsing or discarding their
  deferred macro code.
- Added explicit macro expansion inputs for environment values, compiler flags,
  and captured command output, including cache fingerprinting. `env`, `flag?`,
  safe backtick replay, and `parse_type` validation close the final 23 portable
  runtime contracts. Facet does not execute arbitrary shell commands, and the
  four official `parse_type` failures require exact diagnostic parity.
- Captured 106 official contextual `TypeNode` arguments as structured state,
  including generic names, type variables, methods and variables, ancestry,
  visibility, predicates, and type relationships. Contextual print-family
  macros expose and verify exact side-effect output. This closes all 142
  program-context runtime contracts without retaining Crystal compiler objects
  inside Facet.
- Preserved captured AST start/end locations and documentation through
  source-independent macro template expansion, covering `filename`, line/column,
  `doc`, and `doc_comment` contracts from the official runtime corpus.
- Captured authoritative root-node names, including generic type parameters and
  their `generic_args: false` variants, and exposed them through Facet macro
  values. This adds 51 exact official `name` contracts without parsing names
  heuristically from rendered source.
- Added recursive captured-AST structure and Facet-native call argument views
  for `args`, `receiver`, `block`, `block_arg`, `named_args` (including nested
  names/values), and `global?`. This adds 13 exact official call-family
  contracts while keeping collection elements as typed macro AST values.
- Added recursive captured and Facet-native macro AST views for `Case`,
  `Select`, `When`, `ExceptionHandler`, and `Rescue`, including exhaustive-case
  flags, rescue names and union types, `else`, and `ensure`. This adds 28 exact
  official control-flow contracts and preserves nil rescue types distinctly
  from empty AST collections.
- Added recursive captured and Facet-native macro AST views for `Def`, `Macro`,
  `FunDef`, and their `Arg` values, covering bodies, arguments, splats, block
  arguments, return types, free variables, receivers, visibility, abstract and
  variadic flags, and external function names. This adds 49 exact official
  declaration contracts. AST-returned empty collections now retain Crystal's
  `[]` form independently from evaluator arrays typed as `[] of ::NoReturn`.
- Added recursive captured and Facet-native macro AST views for `ClassDef`,
  `ModuleDef`, `EnumDef`, `AnnotationDef`, `LibDef`, and `CStructOrUnionDef`,
  including declaration kind, body, superclass/base type, type variables,
  splat index, and abstract/struct/union flags. This adds 39 exact official
  declaration contracts; structured AST `is_a?` handling adds one more.
- Added captured and Facet-native macro AST views for `Asm` and `AsmOperand`,
  covering assembly text, input/output constraints and expressions, clobbers,
  and volatile/alignstack/intel/unwind flags. This adds all 20 official inline
  assembly contracts to the exact runtime gate.
- Added captured and Facet-native macro AST views for `TypeDeclaration`,
  `ProcNotation`, `Metaclass`, `Generic`, `Union`, and `Path`, including their
  fields, typed collections, mutation behavior, and `resolve`/`resolve?`
  semantics. The complete captured type-syntax slice is exact and adds 37 new
  runtime matches.
- Added captured and Facet-native macro AST views for `ProcLiteral`,
  `ProcPointer`, `Cast`, `NilableCast`, `If`, `Assign`, `MultiAssign`, and
  `RangeLiteral`. Fields, returned-container immutability, range iteration,
  `map(&.method)` shorthand, and `to_a` now match all 34 captured expression
  arguments, adding 33 exact runtime contracts.
- Preserved `StringLiteral`, `SymbolLiteral`, and `MacroId` result kinds across
  delegated string methods; aligned `id`, `stringify`, `symbolize`, `chars`, and
  cross-kind `MacroId` equality with Crystal. Added captured and Facet-native
  `And`/`Or` views with `left` and `right` fields. Together these close 22 more
  official runtime contracts.
- Captured authoritative `ArrayLiteral` and `HashLiteral` element and type
  metadata and exposed typed collection `of`, `of_key`, `of_value`, and custom
  literal `type` fields at the native Facet macro-argument boundary. This closes
  all five remaining collection-metadata contracts and raises exact portable
  parity to 815/900.
- Captured authoritative macro-control and miscellaneous AST views for
  `MacroExpression`, `MacroIf`, `MacroFor`, `MacroLiteral`, `MacroVar`,
  `UninitializedVar`, unary expressions, `OffsetOf`, `Alias`,
  `VisibilityModifier`, `IsA`, `RespondsTo`, and `Require`. Facet-native
  arguments expose the representable unary, predicate, and uninitialized forms,
  raising exact portable parity to 845/900.
- Captured authoritative `Block`, nested `Expressions`, `While`,
  `Break`/`Next`/`Return`, and `Yield` structure, including block arguments and
  splat positions, control operands, yield expressions, and yield scope.
  Facet-native call blocks and representable control nodes expose the same AST
  contract, raising exact portable parity to 864/900.
- Captured authoritative `ReadInstanceVar`, `Annotation`, `TypeDef`,
  `ExternalVar`, `StringInterpolation`, and standalone `When` structure.
  Annotation indexing preserves number/symbol/string/identifier keys, and
  returned AST collections remain immutable. This closes every remaining
  direct AST-field mismatch and raises exact portable parity to 877/900.
- Added Crystal-compatible macro hash iteration indices, union type arguments
  for `is_a?`, and target-layout-neutral number values for `sizeof`/`alignof`
  introspection.
- Expanded the Facet-native macro value model for typed numbers, chars, regexes,
  ranges, arrays, tuples, hashes, and named tuples. Added exact collection
  rendering and slicing/mutation, numeric kind preservation, regex/string
  operations, block transforms/reductions, splats, and contextual output
  fragment handling.
- Added type/method/instance-variable/argument annotation introspection with
  positional and named annotation values, and fixed signed-number member-access
  precedence such as `-128i8.kind`.

- Added Facet-native type-aware macro values backed by the cross-file program
  index. Macros can inspect `@type`, resolve indexed and builtin type syntax,
  enumerate methods, instance variables, and constants, inspect method/argument
  metadata, traverse explicit superclasses/ancestors, and test explicit subtype
  relationships. Type-aware expansion consumers now participate in conservative
  workspace-revision invalidation so declaration edits cannot leave stale
  generated ASTs.
- Added macro collection `sort`, `reverse`, `uniq`, and `compact` operations and
  end-to-end cr-analyzer coverage for declarations generated from Facet type
  introspection.
- Added a recursive contract validator for Facet's own arena-backed AST and
  applied it to all 3,437 accepted Crystal 1.21 fixture inputs. The gate checks
  reachable graph integrity, cycles, spans, node arity, child roles, payloads,
  semantic/storage flags, and significant-token ownership; unreachable parser
  scratch nodes can no longer hide missing syntax. The isolated stdlib scanner
  applies the same checks to every diagnostic-free source file, handles
  namespaced nilable-type ownership, and reliably self-spawns under
  `crystal run` on Linux.
- Added a generated, portable semantic AST projection oracle for all 3,437
  accepted Crystal 1.21 parser inputs. The regular native corpus spec now
  compares every accepted Facet tree against construct shape, names, operators,
  child ordering, and semantic flags without requiring Facet to copy Crystal's
  AST classes.
- Added payload-backed literal content spans and accessors, preserving distinct
  bodies for multiple heredocs declared on one header line even though their
  outer syntax spans overlap. Continued strings now retain their individual
  source-backed literal children instead of collapsing into an ambiguous span.
- Preserved previously discarded AST semantics: every `rescue` clause and its
  optional variable/type header, proc-literal return types, and FFI external
  variable real names. Proc `Block` nodes now store `[params, return_type,
  body]`, rescue clauses are explicitly flagged and retained as an ordered
  collection, and external `VarDecl` nodes retain their real-name child.
- Expanded the native golden AST contract to 18 focused syntax inputs covering
  every node kind emitted by accepted syntax, including globals, with malformed
  role, unreachable-token, source-backed literal, and multi-heredoc regression
  tests.
- Ported all 4,378 unique Crystal 1.21 parser-suite inputs into a committed
  native Facet fixture, including upstream AST and diagnostic oracles. The
  native parser suite now gates acceptance/rejection, AST integrity, semantic
  token retention, and diagnostic spans without requiring a Crystal checkout.
- Added a reproducible fixture generator and an exact parser diagnostic parity
  report. Parser and lexer recovery now match all 941/941 rejected inputs for
  the exact first diagnostic message and line/column (up from the initial
  365/422/276 message/location/combined baseline), with a 100% aggregate gate
  in the native suite. All 3,437 accepted inputs now also match the committed
  common semantic AST projection.
- Added reproducible upstream Crystal 1.21 spec-input audits: 4,378 unique
  parser inputs now have full acceptance/rejection parity and AST token
  retention, while 690 unique lexer inputs are fully consumed without unknown
  tokens or non-trivia gaps. Lexer diagnostic presence matches all 687 inputs
  whose upstream state is reproducible from source alone; three state-dependent
  inputs are identified explicitly by the audit.
- Added Crystal-compatible lexer validation for numeric suffixes and ranges,
  leading-zero decimals, Unicode codepoints, octal escapes, global match-data
  indices, heredoc labels, dot-prefixed numbers, and raw carriage returns.
- Fixed silent acceptance and AST-loss cases found by the parity audit,
  including FFI parameters, case/select patterns, asm operands, double splats,
  proc pointers, return type spacing, regex escapes, `%W` interpolation,
  heredoc error recovery, and retained `forall` variables.
- Reached a clean parser compatibility baseline across all 1,625 Crystal 1.21
  standard-library source files, with zero diagnostics, AST integrity errors,
  or crashes. The final gaps covered nested visible defs and exported funs in
  macros, keyword-named type declarations, `out` locals, rescue boundaries
  after zero-argument control expressions, and parenthesized multiline spans.
- Expanded lexer and parser support for contextual keywords, macro-generated
  declarations, nested/verbatim macro controls, FFI forms, rescue clauses,
  command splats, tuple types, and multiline statement boundaries.
- Replaced placeholder project documentation with frontend usage, architecture,
  integration, and scope guidance.
- Aligned `Facet::VERSION` with the shard version (0.1.5).
- Prevented `QueryDb#parse` from re-appending diagnostics already owned by `AstFile`.

## 0.1.5 - 2026-02-08

- Expanded Crystal parser compatibility coverage with a large upstream-spec port batch.
- Improved parser behavior for additional syntax paths and error-handling cases.
- Updated parser support specs/helpers and aligned lexer details with parser needs.
