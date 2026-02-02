---
type: decision
name: DR-007-emacs-filesystem-delegation
date: 2026-01-21
status: accepted
change_type: additive
impacts:
  - features/file-system-tools/vocabulary.md
  - features/file-system-tools/contracts/*
---

# DR-007: Delegate Filesystem Access to Emacs

## Change Request

During Phase 2 implementation (directory_tree and search_files tools), we needed to decide how filesystem access should be performed: directly from the Common Lisp image or delegated to the Emacs environment.

## Context

Agent-Q tools run within a Common Lisp image connected to Emacs via SLY/SWANK. There are two approaches for filesystem operations:

1. **Direct CL Access**: Use Common Lisp's native filesystem functions (`uiop:directory-files`, `cl-fad`, etc.)
2. **Emacs Delegation**: Use `eval-in-emacs` to execute Elisp filesystem operations in the Emacs environment

## Decision

**We delegate ALL filesystem operations to Emacs via `eval-in-emacs`.**

All file-system-tools (list_directory, get_file_info, directory_tree, search_files, edit_file, etc.) use Emacs for filesystem access rather than native Common Lisp functions.

## Rationale

### 1. Security & Permission Isolation

**Problem**: The CL image may run with different permissions than the user's Emacs session.

**Benefit**: Emacs runs in the user's environment with their permissions. Delegating operations ensures filesystem access respects the user's security context.

### 2. Consistency with Existing Patterns

**Problem**: Mixing CL-native and Emacs-delegated approaches creates inconsistent behavior.

**Benefit**: Phase 1 tools (edit_file, list_directory) already used `eval-in-emacs`. Continuing this pattern maintains architectural consistency.

### 3. Robustness & Maturity

**Problem**: CL filesystem libraries vary across implementations (SBCL, CCL, etc.) with subtle differences.

**Benefit**: Emacs has mature, battle-tested file handling:
- Correct handling of symlinks, permissions, file attributes
- Cross-platform compatibility (Unix, macOS, Windows)
- Proper handling of edge cases (hidden files, special characters, etc.)

### 4. Integration with Emacs Features

**Problem**: Users expect tools to respect Emacs settings (file-name-coding-system, directory-local variables, etc.).

**Benefit**: Operations executed in Emacs automatically respect user's Emacs configuration.

### 5. Simplicity

**Problem**: Maintaining two parallel code paths increases complexity.

**Benefit**: Single, consistent approach simplifies implementation and testing.

## Alternatives Considered

### Alternative 1: Direct CL Access

**Approach**: Use `uiop:directory-files`, `probe-file`, `file-write-date`, etc.

**Pros**:
- No dependency on Emacs connection
- Could work in standalone REPL
- Potentially faster (no RPC overhead)

**Cons**:
- Implementation-specific quirks
- Different permission context
- Inconsistent with existing tools
- Miss Emacs configuration
- More code to maintain

**Rejected**: Cons outweigh pros. The "standalone REPL" use case is not a priority—Agent-Q is designed to run within Emacs.

### Alternative 2: Hybrid Approach

**Approach**: Use CL for simple operations (probe-file), Emacs for complex ones (directory traversal).

**Pros**:
- Could optimize performance for simple cases
- Reduces RPC calls

**Cons**:
- Inconsistent behavior across tools
- Complex decision logic ("when to use which?")
- Hard to test and debug
- Violates principle of least surprise

**Rejected**: Consistency is more valuable than micro-optimizations.

## Impact Analysis

### Direct Impacts

- **All file-system-tools contracts**: Require active SLY/SWANK connection
- **Testing**: Manual REPL testing must be done from within Emacs
- **Documentation**: Must document Emacs dependency clearly

### Cascading Impacts

- **Tool registration**: No changes needed (safety levels already defined)
- **Error handling**: Tools gracefully fail with "Not connected to Emacs" message
- **Performance**: RPC overhead negligible for typical file operations

### Migration

No migration needed (this decision was made before Phase 1 shipped).

## Implementation Notes

### Error Handling

When `eval-in-emacs` is unavailable:

```lisp
(handler-case
    (eval-in-emacs '(directory-files default-directory))
  (error (e)
    (format nil "Error: Not connected to Emacs (~A)" e)))
```

### Testing Strategy

**Unit Tests**: Mock `eval-in-emacs` responses for filesystem data.

**Integration Tests**: Require Emacs connection; gracefully skip when unavailable using FiveAM's `(skip "Requires Emacs connection")`.

**Manual Tests**: Must be run from `M-x sly` within Emacs.

## Success Metrics

- ✅ All Phase 1 tools use `eval-in-emacs` consistently
- ✅ All Phase 2 tools use `eval-in-emacs` consistently
- ✅ Clear error messages when Emacs unavailable
- ✅ Zero permission-related bugs in production

## Related Decisions

- **DR-008**: Glob Pattern Complexity Limits (complementary—both prioritize simplicity)

## References

- Implementation: `src/tools/filesystem.lisp`
- Tests: `tests/filesystem-tests.lisp`
- SLY Integration: `src/sly-interface.lisp` (eval-in-emacs implementation)
- Implementation Notes: `docs/implementation/file-system-tools/notes.md`

## Review

**Decision Status**: ✅ Accepted and implemented

**Confidence**: High (0.95) - Decision validated by successful Phase 1 + Phase 2 implementation with zero permission issues.

**Revisit Trigger**: If Agent-Q needs to support standalone (non-Emacs) deployment, this decision would need reevaluation.
