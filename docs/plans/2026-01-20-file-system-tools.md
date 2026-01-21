# Implementation Plan: Comprehensive File System Tools

**Date:** 2026-01-20
**Feature:** file-system-tools
**Status:** Ready for Implementation
**Canon:** canon/features/file-system-tools/

---

## Overview

Extend Agent-Q's tool system with comprehensive file system capabilities, enabling autonomous file reading, writing, editing, and navigation with appropriate safety controls.

### Goals

1. Enable Agent-Q to understand project structure (directories, files)
2. Provide precise, targeted file editing (str_replace pattern)
3. Support file lifecycle operations (create, move, delete)
4. Enforce project root boundary for safety
5. Integrate with existing diff approval workflow for dangerous operations

### Non-Goals

- Git operations (separate feature)
- Remote filesystem access
- Binary file manipulation
- IDE-specific integrations (LSP, etc.)

---

## Research Summary

See: `canon/research/2026-01-20-file-system-tools.research.md`

### Key Findings

1. **Claude API pattern** (`str_replace_based_edit_tool`) is the gold standard for targeted edits
2. **MCP Filesystem Server** provides comprehensive tool coverage
3. **Project root boundary** is essential for safety
4. Agent-Q already has diff approval (`propose_file_edit`) - reuse for dangerous ops

### Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Edit mechanism | str_replace (exact) | Claude API pattern, prevents ambiguity |
| Path validation | Project root boundary | Security, user expectations |
| Write approval | Leverage diff-approval | Already implemented, tested |
| Implementation | Emacs bridge | Leverages Emacs file handling, undo |

---

## Implementation Phases

### Phase 1: Core Navigation (Priority: High)

**Goal:** Understand project structure

**Tools:**
1. `list_directory` - List files/dirs with metadata
2. `get_file_info` - File size, timestamps, permissions
3. `get_project_root` - Report current project root

**Configuration:**
- `*project-root*` variable with auto-detection
- `resolve-project-path` function with boundary checking

**Files to Create/Modify:**
```
src/tools/filesystem.lisp     # New file - all filesystem tools
src/tools/package.lisp        # Add exports
src/config.lisp               # Add *project-root* configuration
```

**Implementation Steps:**

1. Add `*project-root*` to config.lisp:
   ```lisp
   (defvar *project-root* nil
     "Root directory for file operations. Auto-detected if NIL.")
   ```

2. Create `resolve-project-path` in filesystem.lisp:
   ```lisp
   (defun resolve-project-path (path)
     "Resolve PATH within project boundary. Returns NIL if outside."
     ...)
   ```

3. Implement `list_directory` tool:
   - Validate path via `resolve-project-path`
   - Delegate to Emacs `directory-files-and-attributes`
   - Format output for LLM consumption

4. Implement `get_file_info` tool:
   - Return size, timestamps, permissions, type

5. Implement `get_project_root` tool:
   - Return current project root and detection method

**Tests:**
- Path validation (boundary checking)
- Hidden file filtering
- Sort order options
- Error cases (not found, not directory)

**Estimated Effort:** 2-3 hours

---

### Phase 2: Enhanced Reading (Priority: Medium)

**Goal:** Better file reading with line ranges

**Tools:**
1. `directory_tree` - Recursive tree with exclusions
2. `search_files` - Glob pattern matching

**Enhancements:**
- Enhanced `read_file` with `:start` and `:end` line parameters

**Implementation Steps:**

1. Implement `directory_tree`:
   ```lisp
   (define-tool "directory_tree"
     "Get recursive directory structure as hierarchical tree."
     '((:name "path" :type :string ...)
       (:name "exclude_patterns" :type :array ...))
     ...)
   ```

2. Implement `search_files`:
   - Glob pattern matching via Emacs `file-expand-wildcards`
   - Recursive search with `**` support

3. Enhance `read_file` (in buffer.lisp):
   - Add optional `:start_line` and `:end_line` parameters
   - Implement line-range extraction

**Tests:**
- Tree depth limits
- Exclusion patterns (`.git`, `node_modules`)
- Glob matching accuracy
- Line range edge cases

**Estimated Effort:** 2 hours

---

### Phase 3: Targeted Editing (Priority: High)

**Goal:** Precise file modifications

**Tools:**
1. `edit_file` - str_replace with exact matching
2. `insert_at_line` - Insert text at specific line

**Key Features:**
- Exact string matching (no regex)
- Single-match requirement
- Automatic mini-diff generation
- Buffer synchronization (update open Emacs buffers)

**Implementation Steps:**

1. Implement `edit_file`:
   ```lisp
   (define-tool "edit_file"
     "Make targeted edit using exact string replacement."
     '((:name "path" :type :string ...)
       (:name "old_str" :type :string ...)
       (:name "new_str" :type :string ...)
       (:name "description" :type :string ...))
     :required '("path" "old_str" "new_str")
     :safety-level :cautious
     ...)
   ```

2. Implement match counting:
   ```lisp
   (defun count-substring (needle haystack)
     "Count occurrences of NEEDLE in HAYSTACK."
     ...)
   ```

3. Implement buffer synchronization:
   ```lisp
   (defun write-file-content (path content)
     "Write CONTENT to PATH, syncing any open Emacs buffer."
     (eval-in-emacs
      `(let ((buf (find-buffer-visiting ,path)))
         (if buf
             (with-current-buffer buf
               (erase-buffer)
               (insert ,content)
               (save-buffer))
           (with-temp-file ,path
             (insert ,content))))))
   ```

4. Implement `insert_at_line`:
   - Insert text after specified line number
   - Line 0 = beginning of file
   - Line -1 = end of file

5. Add change logging:
   ```lisp
   (defun log-file-edit (path description old-str new-str)
     "Log a file edit for observability."
     ...)
   ```

**Tests:**
- Single match succeeds
- No match returns error
- Multiple matches returns error with count
- Whitespace sensitivity
- Buffer sync with open files

**Estimated Effort:** 3-4 hours

---

### Phase 4: File Lifecycle (Priority: Medium)

**Goal:** File creation, movement, deletion

**Tools:**
1. `create_file` - Create new file with content
2. `move_file` - Rename/relocate files
3. `delete_file` - Remove files

**Safety:**
- All tools are `:dangerous` safety level
- Route through approval handler
- Option to use `propose_file_edit` for create (shows full content as diff)

**Implementation Steps:**

1. Implement `create_file`:
   - Check file doesn't exist (or allow overwrite with approval)
   - Create parent directories if needed
   - Write content via Emacs

2. Implement `move_file`:
   - Validate both source and destination paths
   - Check destination doesn't exist
   - Use Emacs `rename-file`

3. Implement `delete_file`:
   - Extra confirmation in approval handler
   - Use Emacs `delete-file`
   - Handle directories (with recursive flag)

**Integration with diff-approval:**

For `create_file`, optionally show content as a "diff" (all additions):
```lisp
(if *show-create-as-diff*
    (propose-file-edit path "" content "Create new file")
    (create-file-directly path content))
```

**Tests:**
- Create in existing directory
- Create with parent creation
- Move within project
- Delete with confirmation
- Reject operations outside project root

**Estimated Effort:** 2-3 hours

---

## File Structure

After implementation:

```
src/tools/
├── package.lisp        # Updated with new exports
├── registry.lisp       # Unchanged
├── introspection.lisp  # Unchanged
├── execution.lisp      # Unchanged
├── buffer.lisp         # Enhanced read_file
├── diff.lisp           # Unchanged
└── filesystem.lisp     # NEW - all filesystem tools

src/
├── config.lisp         # Add *project-root*
└── ...
```

---

## API Summary

### Tier 1: Safe (Read-Only)

| Tool | Parameters | Returns |
|------|------------|---------|
| `list_directory` | path, show_hidden, sort_by | Formatted listing |
| `directory_tree` | path, exclude_patterns | JSON tree |
| `get_file_info` | path | Metadata object |
| `search_files` | path, pattern | List of matches |
| `get_project_root` | - | Path + detection method |

### Tier 2: Cautious (Logged)

| Tool | Parameters | Returns |
|------|------------|---------|
| `edit_file` | path, old_str, new_str, description | Success + mini-diff |
| `insert_at_line` | path, line, text, description | Success message |

### Tier 3: Dangerous (Approval Required)

| Tool | Parameters | Returns |
|------|------------|---------|
| `create_file` | path, content, description | Success or approval UI |
| `move_file` | source, destination | Success or approval UI |
| `delete_file` | path, recursive | Success or approval UI |

---

## Testing Strategy

### Unit Tests (Common Lisp)

```lisp
(test resolve-project-path
  "Path resolution respects project boundary"
  ...)

(test count-substring
  "Counts occurrences correctly"
  ...)

(test edit-file-single-match
  "Edit succeeds with single match"
  ...)
```

### Integration Tests (Emacs Lisp)

```elisp
(ert-deftest sly-agent-q-filesystem-list-directory ()
  "list_directory returns formatted output"
  ...)

(ert-deftest sly-agent-q-filesystem-edit-file ()
  "edit_file modifies file correctly"
  ...)
```

### Manual Testing Scenarios

1. **Navigation**: List project, navigate into src/, view tree
2. **Targeted Edit**: Find function, make single-character fix
3. **Create File**: Create new test file, verify content
4. **Boundary Check**: Attempt to access `/etc/passwd` (should fail)

---

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Path traversal attack | `resolve-project-path` validates all paths |
| Accidental overwrite | `:dangerous` level requires approval |
| Buffer desync | `write-file-content` syncs open buffers |
| Large file handling | `read_file` line ranges, truncation |
| Unicode paths | Delegate to Emacs (handles Unicode) |

---

## Success Criteria

1. ✅ All Tier 1 tools implemented and tested
2. ✅ `edit_file` works with exact matching
3. ✅ Project root boundary enforced for all tools
4. ✅ File lifecycle tools integrate with diff-approval
5. ✅ Existing tests still pass (161 Elisp tests)
6. ✅ Agent can complete "fix a bug" scenario end-to-end

---

## Dependencies

- **tool-system**: Uses `define-tool` macro and registry
- **diff-approval**: Used for dangerous operations
- **buffer.lisp**: Enhanced, not replaced
- **Emacs**: All filesystem operations via `eval-in-emacs`

---

## Timeline Estimate

| Phase | Effort | Cumulative |
|-------|--------|------------|
| Phase 1: Core Navigation | 2-3 hours | 2-3 hours |
| Phase 2: Enhanced Reading | 2 hours | 4-5 hours |
| Phase 3: Targeted Editing | 3-4 hours | 7-9 hours |
| Phase 4: File Lifecycle | 2-3 hours | 9-12 hours |
| Testing & Polish | 2 hours | 11-14 hours |

**Total:** ~2 working days

---

## References

- **Canon Specification:** `canon/features/file-system-tools/`
- **Research Document:** `canon/research/2026-01-20-file-system-tools.research.md`
- **Existing Tool System:** `canon/features/tool-system/`
- **Claude API Pattern:** https://platform.claude.com/docs/en/agents-and-tools/tool-use/text-editor-tool
- **MCP Filesystem:** https://github.com/modelcontextprotocol/servers/tree/main/src/filesystem

---

**Plan Status:** Ready for Implementation
**Last Updated:** 2026-01-20
