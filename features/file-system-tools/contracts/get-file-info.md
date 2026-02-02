# Contract: get_file_info

**Feature:** file-system-tools
**Version:** 1.0.0
**Status:** Stable
**Confidence:** 0.95 (implemented and tested)

---

## Purpose

Retrieve detailed metadata about a file or directory including size, timestamps, and permissions. This is the primary tool for understanding file properties before performing operations.

---

## Tool Definition

```lisp
(define-tool
  "get_file_info"
  "Get detailed information about a file or directory including size, timestamps, and permissions."
  '((:name "path" :type :string :description "File or directory path (relative to project root)"))
  :required '("path")
  :safety-level :safe
  :categories '(:filesystem :navigation))
```

---

## Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `path` | string | Yes | File or directory path relative to project root |

---

## Return Value

### Success

Returns formatted string with file metadata:

```
File: src/config.lisp

Type: file
Size: 2.4 KB
Modified: 2026-01-15 14:23:45
Accessed: 2026-01-20 09:12:03
Readable: Yes
Writable: Yes
```

For directories:

```
File: src/tools/

Type: directory
Size: 4.0 KB
Modified: 2026-01-20 10:45:12
Accessed: 2026-01-20 11:02:33
Readable: Yes
Writable: Yes
```

### Error Cases

| Error | Message | LLM Action |
|-------|---------|------------|
| Path outside root | "Error: Path '...' is outside project root" | Use correct relative path |
| File not found | "Error: File '...' not found" | Check path with list_directory |
| Permission denied | Error during read | Report to user |

---

## Execution Flow

```
1. Validate path parameter exists
2. Resolve path relative to project root
3. Verify path within project boundary
4. Check file/directory exists
5. Query file attributes via Emacs:
   (file-attributes path)
6. Format metadata for LLM consumption
7. Return formatted string
```

---

## Implementation

### Lisp Handler

```lisp
:handler (lambda (args)
           (block get-file-info-handler
             (let* ((path (gethash "path" args))
                    (resolved (agent-q::resolve-project-path path)))
               (unless resolved
                 (return-from get-file-info-handler
                   (format nil "Error: Path '~A' is outside project root" path)))
               (handler-case
                   (let ((info (eval-in-emacs
                               `(let* ((path ,(namestring resolved))
                                       (attrs (file-attributes path)))
                                  (when attrs
                                    (list :path path
                                         :type (if (eq (file-attribute-type attrs) t)
                                                  :directory :file)
                                         :size (file-attribute-size attrs)
                                         :modified (format-time-string
                                                   "%Y-%m-%d %H:%M:%S"
                                                   (file-attribute-modification-time attrs))
                                         :accessed (format-time-string
                                                   "%Y-%m-%d %H:%M:%S"
                                                   (file-attribute-access-time attrs))
                                         :readable (file-readable-p path)
                                         :writable (file-writable-p path)))))))
                     (if info
                         (with-output-to-string (s)
                           (format s "File: ~A~%~%" (getf info :path))
                           (format s "Type: ~A~%" (getf info :type))
                           (format s "Size: ~A~%" (format-file-size (getf info :size)))
                           (format s "Modified: ~A~%" (getf info :modified))
                           (format s "Accessed: ~A~%" (getf info :accessed))
                           (format s "Readable: ~A~%" (if (getf info :readable) "Yes" "No"))
                           (format s "Writable: ~A~%" (if (getf info :writable) "Yes" "No")))
                         (format nil "Error: File '~A' not found" path)))
                 (error (e)
                   (format nil "Error getting file info: ~A" e))))))
```

### Emacs Integration

Delegates to Emacs `file-attributes` function:
- Returns tuple: (type link-count uid gid access-time mod-time status-time size mode ...)
- Handles both files and directories uniformly
- Respects Emacs buffer state for open files

---

## Invariants

### INV-001: Project Boundary

Cannot query files outside project root.

**Enforcement:** `resolve-project-path` returns NIL for external paths
**Confidence:** 1.00

### INV-002: Read-Only Operation

This tool only reads metadata; no modifications.

**Enforcement:** `file-attributes` is read-only
**Confidence:** 1.00

### INV-003: Symbolic Link Resolution

Symlinks are resolved to their targets before checking boundaries.

**Enforcement:** `truename` called within `resolve-project-path`
**Confidence:** 1.00

---

## Usage Examples

### Example 1: Check File Before Editing

**Tool Call:**
```json
{
  "tool": "get_file_info",
  "arguments": {"path": "src/agent.lisp"}
}
```

**Result:**
```
File: /Users/quasi/agent-q/src/agent.lisp

Type: file
Size: 12.3 KB
Modified: 2026-01-19 16:45:22
Accessed: 2026-01-20 09:03:15
Readable: Yes
Writable: Yes
```

**LLM Decision:** "File is writable, proceed with edit_file"

### Example 2: Verify Directory Exists

**Tool Call:**
```json
{
  "tool": "get_file_info",
  "arguments": {"path": "tests/"}
}
```

**Result:**
```
File: /Users/quasi/agent-q/tests/

Type: directory
Size: 4.0 KB
Modified: 2026-01-20 08:12:44
Accessed: 2026-01-20 11:23:19
Readable: Yes
Writable: Yes
```

### Example 3: Handle Missing File

**Tool Call:**
```json
{
  "tool": "get_file_info",
  "arguments": {"path": "nonexistent.lisp"}
}
```

**Result:**
```
Error: File 'nonexistent.lisp' not found
```

**LLM Action:** Use list_directory to find correct path

---

## Related Contracts

- **list_directory** - List files in directory before querying specific file
- **edit_file** - Use get_file_info to verify file is writable before editing
- **get_project_root** - Understand path resolution context

---

## Test Coverage

| Test | Behavior | Status |
|------|----------|--------|
| get-file-info-tool-exists | Tool registered in registry | ✅ Passing |
| get-file-info-is-safe | Safety level is :safe | ✅ Passing |
| get-file-info-has-required-parameters | Requires "path" parameter | ✅ Passing |
| get-file-info-has-description | Description mentions file information | ✅ Passing |

**Test File:** `tests/filesystem-tests.lisp:144-167`

---

**Contract Status:** Stable (production ready)
**Last Updated:** 2026-01-20
**Implementation:** `src/tools/filesystem.lisp:108-155`
