# Changelog

## Unreleased

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
