# Contract: delete_file

**Feature:** file-system-tools
**Version:** 1.2.0
**Status:** Stable
**Confidence:** 0.95 (implemented and tested)

---

## Purpose

Permanently delete files from the project. This is a **destructive operation** that cannot be undone. Automatically closes associated Emacs buffers when deleting open files.

---

## Tool Definition

```lisp
(define-tool
  "delete_file"
  "Delete a file from the project. This operation cannot be undone.
   WARNING: This permanently deletes files. Use with caution."
  '((:name "path" :type :string :description "File path to delete (relative to project root)")
    (:name "description" :type :string :description "Why this file is being deleted (for logging)"))
  :required '("path")
  :safety-level :moderate
  :categories '(:filesystem :editing))
```

---

## Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `path` | string | Yes | File path relative to project root |
| `description` | string | No | Human-readable reason for deletion |

---

## Return Value

### Success

```
✓ Deleted: src/deprecated/old-module.lisp

Reason: Removed deprecated module no longer in use

Size freed: 12.3 KB
```

### Error Cases

| Error | Message | LLM Action |
|-------|---------|------------|
| File doesn't exist | "Error: File 'foo.lisp' does not exist" | Check path, use get_file_info |
| Path is directory | "Error: 'src/' is a directory. Use a directory deletion tool instead." | Use correct tool for directories |
| Path outside root | "Error: Path outside project root" | Use correct relative path |
| Permission denied | "Error: Cannot delete file" | Report to user |

---

## Behavior

### Permanent Deletion

Files are permanently deleted - this operation **cannot be undone**:

```json
{
  "tool": "delete_file",
  "arguments": {
    "path": "src/temp-file.lisp",
    "description": "Remove temporary testing file"
  }
}
```

### Directory Protection

Attempting to delete a directory returns an error:

```json
{
  "tool": "delete_file",
  "arguments": {
    "path": "src/utils/"
  }
}
// Returns: "Error: 'src/utils/' is a directory. Use a directory deletion tool instead."
```

### Buffer Cleanup

If the deleted file is open in Emacs, the associated buffer is automatically closed (killed) after deletion.

---

## Execution Flow

```
1. Validate required parameter (path)
2. Resolve path relative to project root
3. Verify path within project boundary
4. Check if file exists:
   - Doesn't exist → Error
   - Exists → Continue
5. Check if path is a directory:
   - Is directory → Error
   - Is file → Continue
6. Get file size (for logging)
7. Delete file (via Emacs delete-file)
8. If file was open in buffer:
   - Kill the buffer
9. Return success message with freed space
```

---

## Invariants

### INV-001: No Directory Deletion

Cannot delete directories - only files.

**Enforcement:** Check file-directory-p before deletion
**Confidence:** 1.00

### INV-002: Project Boundary

Cannot delete files outside project root.

**Enforcement:** `resolve-project-path` returns NIL for external paths
**Confidence:** 1.00

### INV-003: File Must Exist

Cannot delete a file that doesn't exist.

**Enforcement:** Check file-exists-p before deletion
**Confidence:** 1.00

### INV-004: Buffer Cleanup

Open buffers are automatically killed when file is deleted.

**Enforcement:** Emacs kill-buffer after delete-file
**Confidence:** 1.00

---

## Usage Examples

### Example 1: Remove Deprecated Code

**Tool Call:**
```json
{
  "tool": "delete_file",
  "arguments": {
    "path": "src/deprecated/legacy-parser.lisp",
    "description": "Remove deprecated parser replaced by new implementation"
  }
}
```

### Example 2: Clean Up Test Files

**Tool Call:**
```json
{
  "tool": "delete_file",
  "arguments": {
    "path": "tests/temp-test.lisp",
    "description": "Remove temporary test file created for debugging"
  }
}
```

### Example 3: Remove Generated File

**Tool Call:**
```json
{
  "tool": "delete_file",
  "arguments": {
    "path": "build/output.fasl",
    "description": "Clean compiled output before rebuild"
  }
}
```

---

## Safety Considerations

### Permanent Operation

**This operation cannot be undone.** The file is permanently removed from the filesystem. LLMs should:

1. Only delete files when explicitly requested by user
2. Never delete files speculatively or "to be safe"
3. Prefer moving to a "trash" or "deprecated" directory over deletion
4. Ask for confirmation before deleting important-looking files

### When NOT to Use

Prefer alternatives to deletion when possible:

| Instead of Deleting | Consider |
|---------------------|----------|
| Temporary removal | `move_file` to a "deprecated" directory |
| Replace with new version | `create_file` with `allow_overwrite: true` |
| Clean up | `move_file` to ".trash" or ".old" directory |

### Directory Protection

The tool explicitly rejects directories to prevent:
- Accidentally deleting entire directory trees
- Confusion about whether contents are also deleted
- Need for recursive deletion semantics

For directory deletion, a separate tool with explicit recursive semantics should be used.

---

## Relationship to Other Tools

| Tool | When to Use | vs delete_file |
|------|-------------|----------------|
| **move_file** | Relocate or archive | Non-destructive alternative |
| **create_file** | Replace content | Use with allow_overwrite instead of delete+create |
| **edit_file** | Modify content | Change existing file instead of deleting |

**Safer Workflow**: Move → Verify → Delete
1. `move_file` to ".trash" directory
2. Verify system works without the file
3. `delete_file` from trash if confirmed safe

---

## Related Contracts

- **move_file** - Non-destructive alternative to deletion
- **create_file** - Creating new files
- **get_file_info** - Check file exists before deletion

---

## Test Coverage

| Test | Behavior | Status |
|------|----------|--------|
| delete-file-tool-exists | Tool registered in registry | ✅ Passing |
| delete-file-is-moderate | Safety level is :moderate | ✅ Passing |
| delete-file-has-required-parameters | Requires path parameter | ✅ Passing |
| delete-file-has-optional-parameters | Has optional description | ✅ Passing |
| delete-file-has-description | Description mentions deletion | ✅ Passing |
| delete-file-has-warning | Description warns it's dangerous | ✅ Passing |
| delete-file-path-validation | Rejects paths outside project root | ✅ Passing |
| delete-file-categories | In :filesystem and :editing categories | ✅ Passing |

**Test File:** `tests/filesystem-tests.lisp:909-979`
**Test Count:** 15 checks (100% pass rate)

---

## Audit Trail

Every deletion is logged with:
- File path
- Reason (if provided via `description` parameter)
- File size freed
- Timestamp (implicit in logging system)

This creates an audit trail for tracking what was deleted and why, which is critical for:
- Debugging accidental deletions
- Understanding project cleanup over time
- Compliance and accountability

---

**Contract Status:** Stable (production ready)
**Last Updated:** 2026-01-21
**Implementation:** `src/tools/filesystem.lisp:756-824`
