# Contract: move_file

**Feature:** file-system-tools
**Version:** 1.2.0
**Status:** Stable
**Confidence:** 0.95 (implemented and tested)

---

## Purpose

Move or rename files within the project. Handles both simple renames and moves to different directories. Updates open Emacs buffers automatically to track the new location.

---

## Tool Definition

```lisp
(define-tool
  "move_file"
  "Move or rename a file. By default, fails if destination exists.
   Use allow_overwrite to replace existing files.
   WARNING: This moves files. Changes are logged."
  '((:name "source" :type :string :description "Source file path (relative to project root)")
    (:name "destination" :type :string :description "Destination file path (relative to project root)")
    (:name "allow_overwrite" :type :boolean :description "Allow overwriting existing file (default: false)")
    (:name "create_parents" :type :boolean :description "Create parent directories if needed (default: true)")
    (:name "description" :type :string :description "Why this file is being moved (for logging)"))
  :required '("source" "destination")
  :safety-level :moderate
  :categories '(:filesystem :editing))
```

---

## Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `source` | string | Yes | - | Source file path relative to project root |
| `destination` | string | Yes | - | Destination file path relative to project root |
| `allow_overwrite` | boolean | No | false | Allow replacing existing destination file |
| `create_parents` | boolean | No | true | Create parent directories if needed |
| `description` | string | No | - | Human-readable reason for the move |

---

## Return Value

### Success

```
✓ Moved: src/utils/old-name.lisp → src/helpers/new-name.lisp

Reason: Reorganize utility functions into helpers directory
```

### Error Cases

| Error | Message | LLM Action |
|-------|---------|------------|
| Source doesn't exist | "Error: Source file 'foo.lisp' does not exist" | Check path, use list_directory |
| Destination exists | "Error: Destination file 'bar.lisp' already exists. Use allow_overwrite: true" | Set allow_overwrite or choose different path |
| Source outside root | "Error: Source path outside project root" | Use correct relative path |
| Dest outside root | "Error: Destination path outside project root" | Use correct relative path |
| Permission denied | "Error: Cannot move file" | Report to user |

---

## Behavior

### Simple Rename

Rename a file in the same directory:

```json
{
  "tool": "move_file",
  "arguments": {
    "source": "src/agent.lisp",
    "destination": "src/agent-core.lisp",
    "description": "Rename to clarify it's the core module"
  }
}
```

### Move to Different Directory

Move file to a different directory:

```json
{
  "tool": "move_file",
  "arguments": {
    "source": "utils.lisp",
    "destination": "src/utils/helpers.lisp",
    "description": "Move utilities into organized structure"
  }
}
```

### Overwrite Existing File

Replace an existing file:

```json
{
  "tool": "move_file",
  "arguments": {
    "source": "config-new.lisp",
    "destination": "config.lisp",
    "allow_overwrite": true,
    "description": "Replace old config with new version"
  }
}
```

### Buffer Synchronization

If the source file is open in Emacs, the buffer is automatically updated to track the new location. The buffer's visited file name is changed, and it's marked as not modified.

---

## Execution Flow

```
1. Validate required parameters (source, destination)
2. Resolve both paths relative to project root
3. Verify both paths within project boundary
4. Check if source file exists:
   - Doesn't exist → Error
   - Exists → Continue
5. Check if destination exists:
   - Exists + !allow_overwrite → Error
   - Exists + allow_overwrite → Continue
   - Doesn't exist → Continue
6. If create_parents=true:
   - Create parent directories if needed
7. Perform move/rename (via Emacs rename-file)
8. If source was open in buffer:
   - Update buffer's visited file name
   - Mark buffer as not modified
9. Return success message
```

---

## Invariants

### INV-001: No Accidental Overwrites

Cannot overwrite existing file unless explicitly allowed.

**Enforcement:** Check file-exists-p before move
**Confidence:** 1.00

### INV-002: Dual Project Boundary

Both source and destination must be within project root.

**Enforcement:** Both paths validated via `resolve-project-path`
**Confidence:** 1.00

### INV-003: Source Must Exist

Cannot move a file that doesn't exist.

**Enforcement:** Check source file-exists-p before move
**Confidence:** 1.00

### INV-004: Buffer Synchronization

Open buffers track file to new location.

**Enforcement:** Emacs rename-file + set-visited-file-name
**Confidence:** 1.00

---

## Usage Examples

### Example 1: Reorganize Module

**Tool Call:**
```json
{
  "tool": "move_file",
  "arguments": {
    "source": "src/agent.lisp",
    "destination": "src/core/agent.lisp",
    "description": "Move agent into core subdirectory"
  }
}
```

### Example 2: Rename for Clarity

**Tool Call:**
```json
{
  "tool": "move_file",
  "arguments": {
    "source": "src/helpers.lisp",
    "destination": "src/string-helpers.lisp",
    "description": "Rename to clarify contents are string utilities"
  }
}
```

### Example 3: Consolidate Files

**Tool Call:**
```json
{
  "tool": "move_file",
  "arguments": {
    "source": "src/new-utils.lisp",
    "destination": "src/utils.lisp",
    "allow_overwrite": true,
    "description": "Replace old utils with refactored version"
  }
}
```

---

## Relationship to Other Tools

| Tool | When to Use | vs move_file |
|------|-------------|--------------|
| **create_file** | Create new file | Doesn't move existing files |
| **delete_file** | Remove file | Deletes instead of moving |
| **edit_file** | Change contents | Keeps same location |

**Refactoring Workflow**: Move → Edit → Test
1. `move_file` - Reorganize file structure
2. `edit_file` - Update references to moved files
3. Run tests to verify nothing broke

---

## Related Contracts

- **create_file** - Create new files
- **delete_file** - Remove files
- **edit_file** - Modify file contents
- **get_file_info** - Check if file exists

---

## Test Coverage

| Test | Behavior | Status |
|------|----------|--------|
| move-file-tool-exists | Tool registered in registry | ✅ Passing |
| move-file-is-moderate | Safety level is :moderate | ✅ Passing |
| move-file-has-required-parameters | Requires source and destination | ✅ Passing |
| move-file-has-optional-parameters | Has allow_overwrite, create_parents, description | ✅ Passing |
| move-file-has-description | Description mentions move/rename | ✅ Passing |
| move-file-source-path-validation | Rejects source paths outside project root | ✅ Passing |
| move-file-dest-path-validation | Rejects dest paths outside project root | ✅ Passing |
| move-file-categories | In :filesystem and :editing categories | ✅ Passing |

**Test File:** `tests/filesystem-tests.lisp:830-908`
**Test Count:** 18 checks (100% pass rate)

---

## Security Considerations

### Dual Path Validation

Both source and destination paths are validated:

```lisp
;; These are REJECTED:
source: "../../etc/passwd"        ; Source outside root
destination: "/tmp/evil.txt"      ; Destination outside root
source: "src/../../../etc/passwd" ; Traversal in source
```

### Overwrite Protection

Default behavior prevents accidental data loss:

```
Destination exists → Error (must use allow_overwrite)
Destination doesn't exist → Move
```

### Buffer Consistency

Emacs buffers automatically track file moves, preventing confusion about which file is being edited.

---

**Contract Status:** Stable (production ready)
**Last Updated:** 2026-01-21
**Implementation:** `src/tools/filesystem.lisp:669-754`
