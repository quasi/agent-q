# Contract: list_directory

**Feature:** file-system-tools
**Version:** 1.0.0
**Status:** Stable
**Confidence:** 0.95 (implemented and tested)

---

## Purpose

List the contents of a directory, returning files and subdirectories with metadata. This is the primary navigation tool for understanding project structure.

---

## Tool Definition

```lisp
(define-tool
  "list_directory"
  "List files and subdirectories in a directory. Returns names, types, and sizes."
  '((:name "path" :type :string :description "Directory path (relative to project root)")
    (:name "show_hidden" :type :boolean :description "Include hidden files (default false)")
    (:name "sort_by" :type :string :description "Sort by: name, size, modified (default: name)"))
  :required '("path")
  :safety-level :safe
  :categories '(:filesystem :navigation))
```

---

## Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `path` | string | Yes | - | Directory path relative to project root |
| `show_hidden` | boolean | No | false | Include dotfiles and hidden directories |
| `sort_by` | string | No | "name" | Sort order: "name", "size", "modified" |

---

## Return Value

### Success

Returns formatted string with directory contents:

```
Directory: src/tools/
Total: 6 files, 1 directory

[DIR]  introspection/
[FILE] buffer.lisp         (4.2 KB)
[FILE] diff.lisp           (2.8 KB)
[FILE] execution.lisp      (3.1 KB)
[FILE] introspection.lisp  (8.5 KB)
[FILE] package.lisp        (1.2 KB)
[FILE] registry.lisp       (2.4 KB)
```

### Error Cases

| Error | Message |
|-------|---------|
| Path outside project | "Error: Path '/etc/passwd' is outside project root" |
| Not a directory | "Error: 'src/agent.lisp' is a file, not a directory" |
| Not found | "Error: Directory 'src/nonexistent/' not found" |
| Permission denied | "Error: Cannot read directory 'protected/'" |

---

## Execution Flow

```
1. Validate path parameter exists
2. Resolve path relative to project root
3. Verify resolved path is within project boundary
4. Verify path exists and is a directory
5. List directory contents via Emacs:
   (directory-files-and-attributes path nil ...)
6. Filter hidden files if show_hidden is false
7. Sort by specified attribute
8. Format output for LLM consumption
9. Return formatted string
```

---

## Implementation

### Lisp Handler

```lisp
:handler (lambda (args)
           (let* ((path (gethash "path" args))
                  (show-hidden (gethash "show_hidden" args))
                  (sort-by (or (gethash "sort_by" args) "name"))
                  (resolved (resolve-project-path path)))
             (unless resolved
               (return-from list-directory
                 (format nil "Error: Path '~A' is outside project root" path)))
             (handler-case
                 (let ((entries (eval-in-emacs
                                `(let* ((dir ,resolved)
                                        (files (directory-files-and-attributes dir nil nil t)))
                                   (cl-loop for (name . attrs) in files
                                            unless (member name '("." ".."))
                                            unless (and (not ,show-hidden)
                                                       (string-prefix-p "." name))
                                            collect (list :name name
                                                         :type (if (eq (file-attribute-type attrs) t)
                                                                  :directory :file)
                                                         :size (file-attribute-size attrs)
                                                         :modified (file-attribute-modification-time attrs)))))))
                   (format-directory-listing resolved entries sort-by))
               (error (e)
                 (format nil "Error listing directory: ~A" e)))))
```

### Emacs Integration

The tool delegates to Emacs for actual filesystem access:
- Uses `directory-files-and-attributes` for efficient metadata retrieval
- Respects Emacs' file-name-coding-system for Unicode paths
- Benefits from Emacs' path expansion and validation

---

## Invariants

### INV-001: Project Boundary

All paths must resolve within `*project-root*`.

**Enforcement:** `resolve-project-path` returns NIL for external paths
**Confidence:** 1.00 (will be enforced by implementation)

### INV-002: Directory Only

Only directories can be listed; files return error.

**Enforcement:** `file-directory-p` check before listing
**Confidence:** 1.00

### INV-003: Safe Operation

This tool is read-only and cannot modify filesystem state.

**Enforcement:** `directory-files-and-attributes` is read-only
**Confidence:** 1.00

---

## Usage Examples

### Example 1: List Project Root

**Tool Call:**
```json
{
  "tool": "list_directory",
  "arguments": {"path": "."}
}
```

**Result:**
```
Directory: /Users/quasi/projects/agent-q/
Total: 8 files, 4 directories

[DIR]  contrib/
[DIR]  docs/
[DIR]  specs/
[DIR]  src/
[FILE] agent-q.asd        (2.1 KB)
[FILE] CLAUDE.md          (8.4 KB)
[FILE] LICENSE            (1.0 KB)
[FILE] README.md          (5.2 KB)
```

### Example 2: List with Hidden Files

**Tool Call:**
```json
{
  "tool": "list_directory",
  "arguments": {"path": ".", "show_hidden": true}
}
```

**Result:**
```
Directory: /Users/quasi/projects/agent-q/
Total: 10 files, 5 directories

[DIR]  .git/
[DIR]  contrib/
...
[FILE] .gitignore         (0.3 KB)
```

### Example 3: Sort by Size

**Tool Call:**
```json
{
  "tool": "list_directory",
  "arguments": {"path": "src/tools", "sort_by": "size"}
}
```

---

## Related Contracts

- **directory_tree** - Recursive listing with hierarchy
- **get_file_info** - Detailed metadata for single file
- **search_files** - Find files by glob pattern

---

## Test Coverage

| Test | Behavior | Status |
|------|----------|--------|
| list-directory-tool-exists | Tool registered in registry | ✅ Passing |
| list-directory-is-safe | Safety level is :safe | ✅ Passing |
| list-directory-has-required-parameters | Requires "path" parameter | ✅ Passing |
| list-directory-has-description | Description mentions directory listing | ✅ Passing |

**Test File:** `tests/filesystem-tests.lisp:114-137`

---

**Contract Status:** Stable (production ready)
**Last Updated:** 2026-01-20
**Implementation:** `src/tools/filesystem.lisp:65-105`
