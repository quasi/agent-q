# Contract: create_file

**Feature:** file-system-tools
**Version:** 1.2.0
**Status:** Stable
**Confidence:** 0.95 (implemented and tested)

---

## Purpose

Create new files with specified content. Prevents accidental overwrites by default and supports creating parent directories. This is the **primary file creation mechanism** for Agent-Q.

---

## Tool Definition

```lisp
(define-tool
  "create_file"
  "Create a new file with the specified content. By default, fails if file exists.
   Use allow_overwrite to replace existing files.
   WARNING: This creates files. Changes are logged."
  '((:name "path" :type :string :description "File path (relative to project root)")
    (:name "content" :type :string :description "File content to write")
    (:name "allow_overwrite" :type :boolean :description "Allow overwriting existing file (default: false)")
    (:name "create_parents" :type :boolean :description "Create parent directories if needed (default: true)")
    (:name "description" :type :string :description "What this file is for (for logging)"))
  :required '("path" "content")
  :safety-level :moderate
  :categories '(:filesystem :editing))
```

---

## Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `path` | string | Yes | - | File path relative to project root |
| `content` | string | Yes | - | File content to write |
| `allow_overwrite` | boolean | No | false | Allow replacing existing file |
| `create_parents` | boolean | No | true | Create parent directories if needed |
| `description` | string | No | - | Human-readable purpose of the file |

---

## Return Value

### Success

```
✓ Created file: src/utils/helper.lisp

Purpose: Helper utilities for string manipulation

Content size: 2.3 KB
Lines: 87
```

### Error Cases

| Error | Message | LLM Action |
|-------|---------|------------|
| File exists | "Error: File 'foo.lisp' already exists. Use allow_overwrite: true" | Set allow_overwrite or choose different path |
| Path outside root | "Error: Path outside project root" | Use correct relative path |
| Permission denied | "Error: Cannot create file" | Report to user |
| Parent dir missing | (auto-created if create_parents=true) | - |

---

## Behavior

### Default Behavior (Safe)

By default, create_file **prevents overwriting** existing files:

```lisp
;; This will fail if src/new.lisp exists:
{
  "tool": "create_file",
  "arguments": {
    "path": "src/new.lisp",
    "content": "(in-package :myapp)\n..."
  }
}
```

### Overwrite Mode

To replace an existing file, explicitly set `allow_overwrite`:

```lisp
{
  "tool": "create_file",
  "arguments": {
    "path": "src/config.lisp",
    "content": "(defparameter *version* \"2.0\")",
    "allow_overwrite": true
  }
}
```

### Parent Directory Creation

Parent directories are created automatically by default:

```lisp
;; Creates src/tools/ if it doesn't exist:
{
  "tool": "create_file",
  "arguments": {
    "path": "src/tools/analyzer.lisp",
    "content": "..."
  }
}
```

To require parent directories to exist:

```lisp
{
  "tool": "create_file",
  "arguments": {
    "path": "src/tools/analyzer.lisp",
    "content": "...",
    "create_parents": false
  }
}
```

---

## Execution Flow

```
1. Validate required parameters (path, content)
2. Resolve path relative to project root
3. Verify path within project boundary
4. Check if file exists:
   - Exists + !allow_overwrite → Error
   - Exists + allow_overwrite → Continue
   - Doesn't exist → Continue
5. If create_parents=true:
   - Create parent directories if needed
6. Write content to file (via Emacs)
7. Return success message with file stats
```

---

## Invariants

### INV-001: No Accidental Overwrites

Cannot overwrite existing file unless explicitly allowed.

**Enforcement:** Check file-exists-p before write
**Confidence:** 1.00

### INV-002: Project Boundary

Cannot create files outside project root.

**Enforcement:** `resolve-project-path` returns NIL for external paths
**Confidence:** 1.00

### INV-003: Buffer Synchronization

If file is created in an open Emacs session, it's accessible.

**Enforcement:** Emacs handles file creation atomically
**Confidence:** 1.00

### INV-004: Parent Directory Guarantee

If create_parents=true, parent directories exist before write.

**Enforcement:** `make-directory` with parents=t flag
**Confidence:** 0.95

---

## Usage Examples

### Example 1: Create New Module

**Tool Call:**
```json
{
  "tool": "create_file",
  "arguments": {
    "path": "src/modules/analytics.lisp",
    "content": ";;; -*- Mode: LISP; -*-\n\n(in-package :agent-q.analytics)\n\n(defun track-event (event)\n  \"Track an analytics event.\"\n  (log-info \"Event: ~A\" event))",
    "description": "Analytics tracking module"
  }
}
```

### Example 2: Create Configuration File

**Tool Call:**
```json
{
  "tool": "create_file",
  "arguments": {
    "path": "config/settings.json",
    "content": "{\n  \"api_key\": \"...\",\n  \"timeout\": 30\n}",
    "description": "Application configuration"
  }
}
```

### Example 3: Replace Existing File

**Tool Call:**
```json
{
  "tool": "create_file",
  "arguments": {
    "path": "VERSION",
    "content": "2.0.0",
    "allow_overwrite": true,
    "description": "Update version number"
  }
}
```

### Example 4: Create Without Parent Directory

**Tool Call:**
```json
{
  "tool": "create_file",
  "arguments": {
    "path": "src/new-file.lisp",
    "content": "(in-package :agent-q)",
    "create_parents": false
  }
}
```

---

## Relationship to Other Tools

| Tool | When to Use | vs create_file |
|------|-------------|----------------|
| **write_file** | (deprecated) | Use create_file instead |
| **edit_file** | Modify existing file | Only modifies, doesn't create |
| **insert_at_line** | Add to existing file | Requires existing file |

**Workflow**: Create → Edit → Refine
1. `create_file` - Create new file with initial content
2. `edit_file` - Make targeted changes
3. `insert_at_line` - Add content at specific locations

---

## Related Contracts

- **edit_file** - Targeted string replacement in files
- **move_file** - Rename or move files
- **delete_file** - Remove files
- **get_file_info** - Check if file exists

---

## Test Coverage

| Test | Behavior | Status |
|------|----------|--------|
| create-file-tool-exists | Tool registered in registry | ✅ Passing |
| create-file-is-moderate | Safety level is :moderate | ✅ Passing |
| create-file-has-required-parameters | Requires path and content | ✅ Passing |
| create-file-has-optional-parameters | Has allow_overwrite, create_parents, description | ✅ Passing |
| create-file-has-description | Description mentions creation | ✅ Passing |
| create-file-path-validation | Rejects paths outside project root | ✅ Passing |
| create-file-categories | In :filesystem and :editing categories | ✅ Passing |

**Test File:** `tests/filesystem-tests.lisp:762-829`
**Test Count:** 16 checks (100% pass rate)

---

## Security Considerations

### Path Traversal Prevention

All paths validated against project root boundary:

```lisp
;; These are REJECTED:
"../../etc/passwd"           ; Escapes project root
"/etc/passwd"                ; Absolute path
"src/../../outside/file.txt" ; Traversal attempt
```

### Overwrite Protection

Default behavior prevents accidental data loss:

```
File exists → Error (must use allow_overwrite)
File doesn't exist → Create
```

### Parent Directory Control

The `create_parents` flag controls directory creation:

- `true` (default): Creates missing parent directories
- `false`: Requires parent directories to exist

---

**Contract Status:** Stable (production ready)
**Last Updated:** 2026-01-21
**Implementation:** `src/tools/filesystem.lisp:600-667`
