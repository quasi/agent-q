# Contract: Project Root Configuration

**Feature:** file-system-tools
**Version:** 1.0.0
**Status:** Stable
**Confidence:** 0.95 (implemented and tested)

---

## Purpose

Define the project root boundary that constrains all file system operations. This is a **security-critical** configuration that prevents accidental or malicious access to files outside the intended project scope.

---

## Configuration

### Lisp Variable

```lisp
(defvar *project-root* nil
  "The root directory for file system operations.
   All paths are resolved relative to this directory.
   Paths outside this directory are rejected.")
```

### Setting Project Root

```lisp
;; Explicit setting (highest priority)
(setf agent-q:*project-root* #P"/Users/quasi/projects/agent-q/")

;; Via initialization
(agent-q:configure :project-root "/path/to/project")

;; Reset to auto-detect
(setf agent-q:*project-root* nil)
```

---

## JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Project Root Configuration",
  "description": "Configuration for project root boundary (security-critical)",
  "properties": {
    "project_root": {
      "type": ["string", "null"],
      "description": "Absolute path to project root directory, or null for auto-detection",
      "format": "uri-reference",
      "examples": ["/Users/quasi/projects/agent-q/", null]
    },
    "auto_detect": {
      "type": "boolean",
      "description": "Whether to auto-detect project root when not explicitly set",
      "default": true
    },
    "detection_methods": {
      "type": "array",
      "description": "Ordered list of detection methods to try",
      "items": {
        "type": "string",
        "enum": ["git-root", "asdf-system", "sly-directory", "default-pathname"]
      },
      "default": ["git-root", "asdf-system", "sly-directory", "default-pathname"]
    }
  },
  "required": ["project_root"]
}
```

---

## Auto-Detection Algorithm

When `*project-root*` is NIL, it is auto-detected using this precedence:

```lisp
(defun detect-project-root ()
  "Auto-detect project root using available heuristics."
  (or
   ;; 1. Git repository root
   (find-git-root (slynk:default-directory))

   ;; 2. ASDF system root (if agent-q loaded as system)
   (asdf:system-source-directory :agent-q)

   ;; 3. SLY default directory
   (slynk:default-directory)

   ;; 4. CL default pathname
   *default-pathname-defaults*))

(defun find-git-root (start-dir)
  "Walk up from START-DIR looking for .git directory."
  (let ((dir (pathname-directory (truename start-dir))))
    (loop for i from (length dir) downto 1
          for parent = (make-pathname :directory (subseq dir 0 i))
          when (probe-file (merge-pathnames ".git/" parent))
          return parent)))
```

### Detection Priority

| Priority | Method | Rationale |
|----------|--------|-----------|
| 1 | Explicit `*project-root*` | User knows best |
| 2 | Git root (`.git` directory) | Most reliable for versioned projects |
| 3 | ASDF system root | Good for Lisp projects |
| 4 | SLY default directory | Current working context |
| 5 | `*default-pathname-defaults*` | Last resort |

---

## Path Resolution Function

```lisp
(defun resolve-project-path (path)
  "Resolve PATH relative to project root.
   Returns canonical absolute path if within boundary.
   Returns NIL if path is outside project root."
  (let* ((root (ensure-project-root))
         (root-str (namestring (truename root)))
         (resolved (if (uiop:absolute-pathname-p path)
                      (truename path)
                      (truename (merge-pathnames path root))))
         (resolved-str (namestring resolved)))
    ;; Security check: must start with root
    (if (and resolved
             (>= (length resolved-str) (length root-str))
             (string= root-str (subseq resolved-str 0 (length root-str))))
        resolved
        nil)))  ; Outside boundary - return NIL
```

---

## Security Properties

### PROP-001: Containment

**Statement**: All resolved paths MUST have `*project-root*` as a prefix.

**Formal**:
```
∀ path ∈ resolved-paths:
  (starts-with (namestring path) (namestring *project-root*))
```

**Enforcement**: `resolve-project-path` returns NIL for violating paths

### PROP-002: Traversal Prevention

**Statement**: Path traversal sequences (`..`) cannot escape the project root.

**Test Cases**:
```lisp
;; All should return NIL:
(resolve-project-path "../../../etc/passwd")
(resolve-project-path "/absolute/outside/path")
(resolve-project-path "src/../../outside")
```

### PROP-003: Symlink Resolution

**Statement**: Symlinks are resolved before boundary checking.

**Rationale**: A symlink inside the project could point outside.

```lisp
;; If src/link points to /etc/:
(resolve-project-path "src/link/passwd")  ; → NIL
```

---

## Tool Integration

### Reading Project Root

```lisp
(define-tool
  "get_project_root"
  "Get the current project root directory."
  '()
  :required '()
  :safety-level :safe
  :categories '(:filesystem :configuration)
  :handler (lambda (args)
             (declare (ignore args))
             (format nil "Project root: ~A~%~%Detection method: ~A"
                    (namestring (ensure-project-root))
                    (project-root-detection-method))))
```

### Changing Project Root (RPC)

```lisp
;; SLY RPC endpoint for Emacs
(defslyfun agent-q-set-project-root (path)
  "Set project root from Emacs.
   PATH should be an absolute directory path."
  (let ((resolved (truename path)))
    (if (and resolved (cl-fad:directory-exists-p resolved))
        (progn
          (setf *project-root* resolved)
          (format nil "Project root set to: ~A" (namestring resolved)))
        (format nil "Error: Directory not found: ~A" path))))
```

---

## Emacs Integration

### Elisp Configuration

```elisp
(defun sly-agent-q-set-project-root (dir)
  "Set Agent-Q project root to DIR."
  (interactive "DProject root: ")
  (sly-eval `(agent-q::agent-q-set-project-root ,(expand-file-name dir))))

(defun sly-agent-q-project-root ()
  "Get current Agent-Q project root."
  (interactive)
  (message "Project root: %s"
           (sly-eval '(namestring (agent-q::ensure-project-root)))))
```

### Integration with Projectile/project.el

```elisp
(defun sly-agent-q-use-emacs-project-root ()
  "Set Agent-Q project root to current Emacs project root."
  (interactive)
  (when-let ((root (project-root (project-current))))
    (sly-agent-q-set-project-root root)))
```

---

## Error Handling

### Project Root Not Set

When no project root can be determined:

```lisp
(define-condition no-project-root-error (error)
  ()
  (:report "No project root configured and auto-detection failed.
           Set *project-root* or navigate to a project directory."))
```

### Path Outside Boundary

When a tool receives a path outside project root:

```lisp
;; Don't signal an error - return informative message
(format nil "Error: Path '~A' is outside project root (~A).
            Use paths relative to the project root."
       path (namestring *project-root*))
```

---

## Invariants

### INV-001: Always Absolute

`*project-root*` when set is always an absolute, canonical pathname.

**Enforcement**: `ensure-project-root` canonicalizes on access

### INV-002: Always Directory

`*project-root*` always ends with directory separator.

**Enforcement**: `ensure-trailing-slash` in `ensure-project-root`

### INV-003: Existence Optional

`*project-root*` may point to non-existent directory (for new projects).

**Rationale**: Allow `create_file` to create files in new directories

---

## Related Contracts

- **list_directory** - Uses project root for path resolution
- **edit_file** - Uses project root for path validation
- **all file tools** - All respect project root boundary

---

## Test Coverage

| Test | Behavior | Status |
|------|----------|--------|
| project-root-variable-exists | Variable exists and configurable | ✅ Passing |
| project-root-defaults-to-nil | Defaults to nil (auto-detect) | ✅ Passing |
| find-git-root-detects-git-directory | Finds .git directory | ✅ Passing |
| find-git-root-returns-nil-for-non-git | Returns nil when no .git | ✅ Passing |
| find-git-root-handles-nil-input | Handles nil input gracefully | ✅ Passing |
| detect-project-root-uses-git | Prefers git root | ✅ Passing |
| detect-project-root-returns-valid-directory | Returns existing directory | ✅ Passing |
| resolve-project-path-relative | Resolves relative paths | ✅ Passing |
| resolve-project-path-rejects-traversal | Rejects path traversal attacks | ✅ Passing |
| resolve-project-path-allows-current-dir | Allows . and empty paths | ✅ Passing |
| resolve-project-path-rejects-sibling-directories | Rejects sibling dirs with prefix | ✅ Passing |
| get-project-root-tool-exists | Tool registered | ✅ Passing |
| get-project-root-is-safe | Safety level is :safe | ✅ Passing |
| get-project-root-has-no-required-parameters | No required parameters | ✅ Passing |
| get-project-root-returns-path | Returns project root | ✅ Passing |
| get-project-root-includes-detection-method | Includes detection method | ✅ Passing |

**Test File:** `tests/filesystem-tests.lisp:17-206`

---

**Contract Status:** Stable (production ready)
**Last Updated:** 2026-01-20
**Implementation:** `src/config.lisp` (variables and functions), `src/tools/filesystem.lisp:163-192` (tool)
