# Property: Path Safety

**Feature:** file-system-tools
**Version:** 1.0.0
**Status:** Stable
**Confidence:** 1.00 (fully enforced and tested)

---

## Statement

All file system tools MUST validate paths against the project root boundary before performing any operation. No tool may access, read, write, or modify files outside the configured project root.

---

## Formal Definition

```
∀ tool ∈ file-system-tools:
  ∀ operation ∈ tool.operations:
    ∀ path ∈ operation.paths:
      LET resolved = resolve-project-path(path)
      THEN resolved ≠ NIL
      AND starts-with(resolved, *project-root*)
```

---

## Enforcement Points

### 1. Path Resolution

Every tool handler MUST call `resolve-project-path`:

```lisp
(let ((resolved (resolve-project-path user-provided-path)))
  (unless resolved
    (return-from tool-handler "Error: Path outside project root"))
  ;; Continue with resolved path...
  )
```

### 2. Symlink Resolution

Symlinks are resolved BEFORE boundary checking:

```lisp
(truename path)  ; Resolves symlinks to actual path
```

### 3. Traversal Sequences

Paths with `..` are canonicalized and checked:

```lisp
;; "src/../../../etc/passwd" → "/etc/passwd"
;; "/etc/passwd" doesn't start with project-root → NIL
```

---

## Test Cases

| Input | Project Root | Expected |
|-------|--------------|----------|
| `"src/foo.lisp"` | `/project/` | `/project/src/foo.lisp` |
| `"../outside"` | `/project/` | NIL (reject) |
| `"/etc/passwd"` | `/project/` | NIL (reject) |
| `"src/../../outside"` | `/project/` | NIL (reject) |
| `"src/link"` → `/etc/` | `/project/` | NIL (reject, symlink) |
| `""` (empty) | `/project/` | `/project/` (project root) |
| `"."` | `/project/` | `/project/` |

---

## Rationale

### Security

Without path validation, an attacker (or buggy LLM) could:
- Read sensitive files (`/etc/passwd`, `~/.ssh/id_rsa`)
- Overwrite system files (`/etc/hosts`)
- Access other user's projects
- Exfiltrate data from unexpected locations

### User Expectations

Users expect Agent-Q to operate on "their project", not their entire filesystem. Project root provides this mental model.

### LLM Safety

LLMs may hallucinate paths or try creative solutions that access unexpected locations. Path validation prevents this from causing harm.

---

## Violation Impact

If this property is violated:

1. **Confidentiality**: Sensitive files could be read
2. **Integrity**: Files outside project could be modified
3. **Trust**: User loses confidence in the tool
4. **Scope Creep**: Operations affect unintended areas

---

## Relationship to Safety Levels

Path validation is **independent of safety levels**:

| Safety Level | Path Check | Example |
|--------------|------------|---------|
| `:safe` | ✓ Required | `list_directory` still validates |
| `:cautious` | ✓ Required | `edit_file` validates before edit |
| `:dangerous` | ✓ Required | `delete_file` validates before delete |

Even read-only safe operations must validate paths.

---

## Verification

### Automated Tests

```lisp
(test path-safety-property
  "All paths must be within project root"
  (let ((*project-root* #P"/test/project/"))
    ;; Valid paths
    (is (resolve-project-path "src/file.lisp"))
    (is (resolve-project-path "."))
    (is (resolve-project-path ""))

    ;; Invalid paths - must return NIL
    (is (null (resolve-project-path "../outside")))
    (is (null (resolve-project-path "/etc/passwd")))
    (is (null (resolve-project-path "src/../../outside")))))
```

### Manual Verification

1. Set project root to a test directory
2. Attempt operations with traversal paths
3. Verify all return errors, no file access occurs

---

## Verification Results

### Automated Tests (Passing)

All path safety tests pass:

```lisp
✅ resolve-project-path-relative              ; Valid paths work
✅ resolve-project-path-rejects-traversal     ; ../../../etc/passwd blocked
✅ resolve-project-path-allows-current-dir    ; . and "" work
✅ resolve-project-path-rejects-sibling       ; /project-secrets blocked
```

**Test File:** `tests/filesystem-tests.lisp:70-106`

### Implementation Verification

All 4 filesystem tools call `resolve-project-path`:

| Tool | Line | Check |
|------|------|-------|
| list_directory | filesystem.lisp:80 | ✅ Verified |
| get_file_info | filesystem.lisp:121 | ✅ Verified |
| get_project_root | filesystem.lisp:173 | ✅ N/A (reports root) |
| edit_file | filesystem.lisp:273 | ✅ Verified |

**Property Enforcement:** 100%

---

**Property Status:** Stable (fully enforced)
**Last Updated:** 2026-01-20
**Enforcement:** `src/config.lisp:resolve-project-path`
