# Changelog: file-system-tools

All notable changes to the file-system-tools feature will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [1.2.0] - 2026-01-21

### Status
**Phase 4 Release** - File Lifecycle Complete (All 4 phases complete)

### Added
- **create_file** tool for creating new files
  - Parameters: path, content, allow_overwrite, create_parents, description
  - Prevents accidental overwrites by default
  - Automatically creates parent directories (configurable)
  - Safety level: :moderate
  - Tests: 7 tests (16 checks, 100% pass rate)
- **move_file** tool for moving/renaming files
  - Parameters: source, destination, allow_overwrite, create_parents, description
  - Dual path validation (source and destination)
  - Automatic Emacs buffer synchronization
  - Safety level: :moderate
  - Tests: 8 tests (16 checks, 100% pass rate)
- **delete_file** tool for permanent file deletion
  - Parameters: path, description
  - Rejects directory deletion (files only)
  - Automatic buffer cleanup on deletion
  - Safety level: :moderate
  - Tests: 8 tests (15 checks, 100% pass rate)
- **insert_at_line** tool for line-based content insertion
  - Parameters: path, content, line
  - Line numbering: 0=beginning, -1=end, 1-indexed otherwise
  - Safety level: :moderate
  - Tests: 7 tests
- **Canon Contracts**
  - create-file.md: Complete contract specification
  - move-file.md: Complete contract specification
  - delete-file.md: Complete contract specification
- **Scenarios**
  - create-new-file: Create files with content
  - file-lifecycle-management: Full create/move/delete workflow

### Changed
- **Feature Status**: partial → complete (all 10 tools implemented)
- **Test Coverage**: 64 → 111 tests (47 new tests added)
- **Implemented Contracts**: 6 → 10
- **Planned Contracts**: 4 → 0
- **Phase 4 Status**: planned → complete

### Implementation Notes
- **Test Coverage**: 47 new tests (111 total for feature)
- **Pass Rate**: 100% (all tests passing)
- **Code Added**: ~444 LOC (implementation + tests)
- **Safety Level**: All lifecycle tools use :moderate (not :dangerous)
  - Overwrite protection requires explicit flags
  - Project root boundary enforcement on all paths
  - Comprehensive validation before destructive operations

### Security
- **Path Validation**: All lifecycle tools validate paths against project root
- **Overwrite Protection**: create_file and move_file require explicit allow_overwrite flag
- **Directory Protection**: delete_file explicitly rejects directories
- **Buffer Synchronization**: All tools integrate with Emacs buffers for consistency

### Documentation
- Three complete Canon contract documents
- Updated feature.yaml with complete implementation status
- Code review completed with production-ready assessment

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
