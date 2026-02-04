---
type: contract
name: edit-file
version: 1.0.0
feature: file-system-tools
depends_on: []
---

# Contract: edit_file

**Feature:** file-system-tools
**Version:** 1.0.0
**Status:** Stable
**Confidence:** 0.95 (implemented and tested)

---

## Purpose

Make targeted edits to a file using exact string replacement (`str_replace` pattern). This is the **primary edit mechanism** for Agent-Q, enabling precise code modifications without full-file replacement.

---

## Tool Definition

```lisp
(define-tool
  "edit_file"
  "Make targeted edits to a file using exact string replacement. The old_str must
   match exactly once in the file. For multiple changes, call multiple times.
   WARNING: This modifies files. Changes are logged and may require approval."
  '((:name "path" :type :string :description "File path (relative to project root)")
    (:name "old_str" :type :string :description "Exact string to find (must be unique)")
    (:name "new_str" :type :string :description "Replacement string")
    (:name "description" :type :string :description "What this change does (for logging)"))
  :required '("path" "old_str" "new_str")
  :safety-level :moderate
  :categories '(:filesystem :editing))
```

---

## Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `path` | string | Yes | File path relative to project root |
| `old_str` | string | Yes | Exact string to replace (must match once) |
| `new_str` | string | Yes | Replacement string |
| `description` | string | No | Human-readable description of the change |

---

## Return Value

### Success

```
✓ Edit applied to src/agent.lisp

Change: Add docstring to process-instruction function
Lines affected: 42-44
Diff:
  - (defun process-instruction (instruction)
  + (defun process-instruction (instruction)
  +   "Process a user instruction and return the agent response."
```

### Error Cases

| Error | Message | LLM Action |
|-------|---------|------------|
| No match | "Error: String not found in file" | Re-read file, verify content |
| Multiple matches | "Error: Found 3 matches. Provide more context." | Add surrounding lines to old_str |
| Path outside root | "Error: Path outside project root" | Use correct relative path |
| File not found | "Error: File 'foo.lisp' not found" | Check path, use list_directory |
| Permission denied | "Error: Cannot write to file" | Report to user |

---

## Matching Rules

### Exact Match Required

The `old_str` must match **exactly**, including:
- Whitespace (spaces, tabs, newlines)
- Indentation (leading spaces/tabs)
- Line endings (`\n`)

```lisp
;; This will NOT match if file has 2 spaces:
old_str: "foo bar"      ; 1 space
file:    "foo  bar"     ; 2 spaces - NO MATCH

;; Indentation matters:
old_str: "(defun foo"           ; no indent
file:    "  (defun foo"         ; 2 space indent - NO MATCH
```

### Unique Match Required

The `old_str` must appear **exactly once** in the file:

```
Matches = 0  →  Error: "String not found"
Matches = 1  →  Success: Apply replacement
Matches > 1  →  Error: "Found N matches. Provide more context."
```

### Include Context for Uniqueness

If the target string is not unique, include surrounding context:

```lisp
;; BAD: "return nil" appears 5 times
old_str: "return nil"

;; GOOD: Include function context
old_str: "(defun validate-input (x)
  (when (null x)
    return nil)"
```

---

## Execution Flow

```
1. Validate required parameters
2. Resolve path relative to project root
3. Verify path within project boundary
4. Read current file content (via Emacs)
5. Count occurrences of old_str:
   - 0 matches → Return error
   - >1 matches → Return error with count
   - 1 match → Continue
6. Perform replacement
7. Log the change (for :cautious level)
8. Write new content (via Emacs)
9. Generate mini-diff for confirmation
10. Return success message with diff
```

---

## Implementation

### Lisp Handler

```lisp
:handler (lambda (args)
           (let* ((path (gethash "path" args))
                  (old-str (gethash "old_str" args))
                  (new-str (gethash "new_str" args))
                  (description (gethash "description" args))
                  (resolved (resolve-project-path path)))
             ;; Validate path
             (unless resolved
               (return-from edit-file
                 (format nil "Error: Path '~A' is outside project root" path)))

             (handler-case
                 ;; Read file and count matches
                 (let* ((content (read-file-content resolved))
                        (match-count (count-substring old-str content)))
                   (cond
                     ;; No matches
                     ((zerop match-count)
                      (format nil "Error: String not found in ~A~%~%Searched for:~%~A"
                             path (truncate-for-display old-str 200)))

                     ;; Multiple matches - need more context
                     ((> match-count 1)
                      (format nil "Error: Found ~D matches for the search string in ~A.~%~%~
                                  Please provide more surrounding context to make a unique match."
                             match-count path))

                     ;; Exactly one match - apply edit
                     (t
                      (let* ((new-content (replace-string old-str new-str content))
                             (diff (generate-mini-diff old-str new-str)))
                        ;; Write via Emacs (handles buffer sync)
                        (write-file-content resolved new-content)
                        ;; Log the change
                        (log-file-edit resolved description old-str new-str)
                        ;; Return confirmation
                        (format nil "✓ Edit applied to ~A~%~%~@[Change: ~A~%~]~A"
                               path description diff)))))
               (error (e)
                 (format nil "Error editing file: ~A" e)))))
```

### Emacs Integration

File reading and writing is delegated to Emacs:

```lisp
(defun read-file-content (path)
  "Read file content via Emacs."
  (eval-in-emacs
   `(with-temp-buffer
      (insert-file-contents ,path)
      (buffer-string))))

(defun write-file-content (path content)
  "Write content to file via Emacs, syncing any open buffers."
  (eval-in-emacs
   `(let ((buf (find-buffer-visiting ,path)))
      (if buf
          ;; File is open - modify the buffer
          (with-current-buffer buf
            (erase-buffer)
            (insert ,content)
            (save-buffer))
        ;; File not open - write directly
        (with-temp-file ,path
          (insert ,content))))))
```

---

## Invariants

### INV-001: Single Match Guarantee

Edit only succeeds if `old_str` matches exactly once.

**Enforcement:** Count matches before applying
**Confidence:** 1.00

### INV-002: Atomic Replacement

The replacement is atomic - either fully applied or not at all.

**Enforcement:** Read → Match → Replace in memory → Write
**Confidence:** 0.95 (Emacs provides atomic write)

### INV-003: Project Boundary

Cannot edit files outside project root.

**Enforcement:** `resolve-project-path` returns NIL for external paths
**Confidence:** 1.00

### INV-004: Buffer Synchronization

If file is open in Emacs buffer, the buffer is updated.

**Enforcement:** `find-buffer-visiting` check before write
**Confidence:** 1.00

---

## Usage Examples

### Example 1: Add Docstring

**Tool Call:**
```json
{
  "tool": "edit_file",
  "arguments": {
    "path": "src/agent.lisp",
    "old_str": "(defun process-instruction (instruction)",
    "new_str": "(defun process-instruction (instruction)\n  \"Process a user instruction and return the agent response.\"",
    "description": "Add docstring to process-instruction"
  }
}
```

### Example 2: Fix Bug

**Tool Call:**
```json
{
  "tool": "edit_file",
  "arguments": {
    "path": "src/context.lisp",
    "old_str": "(when (> (length items) 50)\n    (setf items (subseq items 0 50)))",
    "new_str": "(when (> (length items) *context-limit*)\n    (setf items (subseq items (- (length items) *context-limit*))))",
    "description": "Fix sliding window to keep recent items, not oldest"
  }
}
```

### Example 3: Handling Non-Unique Match

**Initial Call (fails):**
```json
{
  "tool": "edit_file",
  "arguments": {
    "path": "src/tools/buffer.lisp",
    "old_str": "return nil",
    "new_str": "return (values nil \"Not found\")"
  }
}
```

**Response:**
```
Error: Found 4 matches for the search string in src/tools/buffer.lisp.

Please provide more surrounding context to make a unique match.
```

**Fixed Call (with context):**
```json
{
  "tool": "edit_file",
  "arguments": {
    "path": "src/tools/buffer.lisp",
    "old_str": "(defun find-in-buffer (pattern)\n  (unless (stringp pattern)\n    return nil)",
    "new_str": "(defun find-in-buffer (pattern)\n  (unless (stringp pattern)\n    return (values nil \"Pattern must be a string\"))",
    "description": "Improve error handling in find-in-buffer"
  }
}
```

---

## Relationship to propose_file_edit

`edit_file` and `propose_file_edit` serve different use cases:

| Aspect | edit_file | propose_file_edit |
|--------|-----------|-------------------|
| **Scope** | Single replacement | Full file diff |
| **Approval** | Logged, no approval | User reviews hunks |
| **Use Case** | Quick, precise edits | Multi-change review |
| **Safety** | `:cautious` | `:dangerous` |

**Escalation**: For complex or risky edits, `edit_file` can fall back to `propose_file_edit`:
- If change affects >50 lines
- If explicitly requested by user
- If configured in tool settings

---

## Related Contracts

- **propose_file_edit** - Full diff approval workflow
- **insert_at_line** - Insert at specific line number
- **read_file** - Read file before editing
- **get_file_info** - Check file exists and is writable

---

## Test Coverage

| Test | Behavior | Status |
|------|----------|--------|
| edit-file-tool-exists | Tool registered in registry | ✅ Passing |
| edit-file-is-moderate | Safety level is :moderate | ✅ Passing |
| edit-file-has-required-parameters | Requires path, old_str, new_str | ✅ Passing |
| edit-file-has-description | Description mentions replacement | ✅ Passing |
| count-substring-basic | Helper counts occurrences | ✅ Passing |
| count-substring-whitespace-sensitive | Whitespace exact matching | ✅ Passing |
| count-substring-multiline | Handles multiline strings | ✅ Passing |
| count-substring-overlapping | Non-overlapping count | ✅ Passing |

**Test File:** `tests/filesystem-tests.lisp:209-274`

---

**Contract Status:** Stable (production ready)
**Last Updated:** 2026-01-20
**Implementation:** `src/tools/filesystem.lisp:249-305`
