# Changelog

## Unreleased

- Ported all 4,378 unique Crystal 1.21 parser-suite inputs into a committed
  native Facet fixture, including upstream AST and diagnostic oracles. The
  native parser suite now gates acceptance/rejection, AST integrity, semantic
  token retention, and diagnostic spans without requiring a Crystal checkout.
- Added a reproducible fixture generator and an exact parser diagnostic parity
  report. Parser and lexer recovery now match 872/941 exact first messages,
  858/941 exact locations, and 837/941 matching both (up from the initial
  365/422/276 baseline), with aggregate non-regression gates in the native
  suite. Output parity remains distinct from complete input and acceptance
  coverage.
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
  standard-library source files, with isolated crash containment and corpus
  reporting in `scripts/check_parser_compat.cr`.
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
