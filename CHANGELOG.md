# Changelog

## Unreleased

- Replaced placeholder project documentation with frontend usage, architecture,
  integration, and scope guidance.
- Aligned `Facet::VERSION` with the shard version (0.1.5).
- Prevented `QueryDb#parse` from re-appending diagnostics already owned by `AstFile`.

## 0.1.5 - 2026-02-08

- Expanded Crystal parser compatibility coverage with a large upstream-spec port batch.
- Improved parser behavior for additional syntax paths and error-handling cases.
- Updated parser support specs/helpers and aligned lexer details with parser needs.
