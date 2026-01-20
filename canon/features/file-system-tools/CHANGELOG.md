# Changelog: file-system-tools

All notable changes to the file-system-tools feature will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [1.1.0] - 2026-01-21

### Status
**Phase 2 Release** - Advanced Navigation (Phase 2 complete)

### Added
- **directory_tree** tool for recursive directory visualization
  - Parameters: path, exclude_patterns
  - Returns hierarchical tree structure
  - Supports exclusion via glob patterns
  - Safety level: :safe
  - Implementation: 3 helper functions (matches-exclusion-p, build-directory-tree, format-directory-tree)
  - Tests: 10 tests covering tree building, formatting, and exclusions
- **search_files** tool for glob-based file searching
  - Parameters: pattern, path, exclude_patterns
  - Supports `*` (wildcard), `**` (recursive), `?` (single char)
  - Returns list of matching files
  - Safety level: :safe
  - Implementation: 2 helper functions (glob-matches-p, search-files-recursively)
  - Tests: 12 tests covering all glob patterns and edge cases
- **Vocabulary**
  - Glob Pattern: Comprehensive definition with intentional limitations
  - Exclusion Pattern: Basename-only matching for filtering
  - eval-in-emacs: RPC mechanism for Emacs filesystem delegation
- **Decision Records**
  - DR-007: Emacs Filesystem Delegation (security, consistency, robustness)
  - DR-008: Glob Pattern Limits (simplicity, predictability, coverage)
- **Scenarios**
  - search-and-navigate: Find files by pattern, visualize project structure
  - refactor-across-files: Now covered (was planned)

### Implementation Notes
- **Test Coverage**: 37 new tests (64 total for feature)
- **Pass Rate**: 100% (unit tests)
- **Code Added**: ~220 LOC (helpers + tools)
- **Commits**: 8 commits across implementation and bug fixes

### Bug Fixes
- Fixed question mark wildcard logic error in glob matching
- Fixed recursive pattern handling for intermediate directories (`**/src/*.lisp`)

### Documentation
- Updated vocabulary with accurate glob pattern syntax
- Added comprehensive decision records documenting architectural choices
- Documented Emacs connection requirement and limitations

---

## [1.0.0] - 2026-01-20

### Status
**First Stable Release** - Partial implementation (Phase 1 + Phase 3 complete)

### Added
- **list_directory** tool for directory navigation
  - Parameters: path, show_hidden, sort_by
  - Formatted output with file counts and sizes
  - Safety level: :safe
- **get_file_info** tool for file metadata inspection
  - Returns size, timestamps, permissions, type
  - Safety level: :safe
- **get_project_root** tool for project boundary reporting
  - Shows detection method (Git, ASDF, etc.)
  - Safety level: :safe
- **edit_file** tool for targeted code editing
  - `str_replace` semantics with exact matching
  - Requires unique occurrence of old_str
  - Includes mini-diff generation
  - Safety level: :moderate
- **Project root security boundary**
  - `*project-root*` variable with auto-detection
  - `resolve-project-path` validation function
  - Path traversal attack prevention
  - Symlink resolution before boundary checking
- **Helper functions**
  - count-substring for unique match detection
  - format-file-size for human-readable sizes
  - read-file-content/write-file-content via Emacs

### Security
- **Path Safety Property** fully enforced
  - All tools validate paths against project root
  - Rejects `../` traversal attempts
  - Handles sibling directory attacks (e.g., /project vs /project-secrets)
  - 100% enforcement verified by tests

### Tests
- 27 comprehensive tests with 100% pass rate
- Security-critical path resolution tests
- Tool registration and parameter validation tests
- Helper function tests (count-substring whitespace handling)

### Documentation
- 4 stable contract documents (list-directory, edit-file, get-file-info, project-root)
- 1 stable property document (path-safety)
- Decision record DR-006 (partial feature stabilization)

### Implementation Notes
- **Phases Complete:** Phase 1 (core), Phase 3 (editing)
- **Phases Pending:** Phase 2 (advanced navigation), Phase 4 (lifecycle)
- **Contracts:** 4 implemented, 6 planned
- **Implementation:** `src/tools/filesystem.lisp` (306 lines)
- **Tests:** `tests/filesystem-tests.lisp` (282 lines)

---

## Roadmap (Planned)

### [1.1.0] - Phase 2: Advanced Navigation
**Planned tools:**
- directory_tree - Recursive directory listing
- search_files - Find files by glob pattern

### [1.2.0] - Phase 4: File Lifecycle
**Planned tools:**
- create_file - Create new files with approval
- move_file - Rename/move files with approval
- delete_file - Delete files with approval
- insert_at_line - Insert content at specific line numbers

---

## Migration Notes

### From Draft (0.1.0) to Stable (1.0.0)

**Breaking Changes:** None (first stable release)

**New Capabilities:**
- Project-scoped file operations now available
- Targeted editing without full-file replacement
- Security boundary prevents accidental external file access

**API Guarantees:**
- Stable tools will use semantic versioning
- Breaking changes require major version bump
- Planned tools may change before implementation

---

## See Also

- **Decision Record:** `core/decisions/0006-partial-feature-stabilization.md`
- **Impact Analysis:** `.impact-analysis-file-system-tools.md`
- **Research:** `canon/research/2026-01-20-file-system-tools.research.md`
