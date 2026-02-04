# File System Tools Vocabulary

Feature-specific terms for the file-system-tools feature.

> Core domain terms (tool, safety-level, etc.) are defined in `core/foundation/vocabulary.md`.

---

## Project Root

The **project root** is the top-level directory that bounds all file system operations. Tools cannot access paths outside this boundary.

### Determination

Project root is determined in order of precedence:
1. Explicit configuration via `(setf agent-q:*project-root* "/path")`
2. Git repository root (presence of `.git` directory)
3. ASDF system root (if loaded as a system)
4. SLY default directory `(slynk:default-directory)`
5. Current working directory `*default-pathname-defaults*`

### Invariants

1. **Containment**: All file operations must resolve to paths within project root
2. **Traversal Prevention**: `..` sequences that escape root are rejected
3. **Symlink Safety**: Symlinks are resolved before boundary checking

### Semantic Boundaries

**Project Root is NOT**:
- **Working Directory**: The CWD may differ; project root is the boundary
- **Home Directory**: Never defaults to `~` unless explicitly configured
- **System Root**: Never `/` - always scoped to a project

---

## Path Resolution

**Path resolution** is the process of converting a tool-provided path (potentially relative) into an absolute, canonical path within the project root.

### Algorithm

```
1. If path is absolute:
   a. Verify path starts with project-root
   b. If yes: canonicalize and return
   c. If no: reject with "path outside project root" error

2. If path is relative:
   a. Join with project-root
   b. Canonicalize (resolve . and .. and symlinks)
   c. Verify result within project-root
   d. Return canonical path
```

### Canonicalization

- Removes redundant separators (`//` → `/`)
- Resolves `.` (current directory)
- Resolves `..` (parent directory)
- Resolves symlinks to their targets
- Normalizes to absolute path

---

## str_replace Edit

A **str_replace edit** is a targeted file modification that replaces an exact string with a new string. This is the primary edit mechanism, following the Claude API pattern.

### Structure

```lisp
(:old_str "exact string to find"
 :new_str "replacement string")
```

### Matching Rules

1. **Exact Match**: `old_str` must match exactly (including whitespace, indentation)
2. **Unique Match**: Must match exactly once in the file
3. **Multiple Matches**: Error - user must provide more context
4. **No Matches**: Error - string not found

### Semantic Boundaries

**str_replace is NOT**:
- **Regex Replace**: No pattern matching, literal string only
- **Multi-Replace**: Only handles one occurrence; for multiple, call repeatedly
- **Fuzzy Match**: Whitespace matters; `"foo  bar"` ≠ `"foo bar"`

---

## Line-Based Operations

**Line-based operations** use 1-indexed line numbers for positioning within files.

### Line Numbering

- Line 1 is the first line of the file
- Line 0 is used for "beginning of file" insertion
- Line -1 means "end of file"
- Lines are separated by `\n` (newline character)

### View Range

A **view range** specifies a portion of a file to read:

```lisp
(:start 10 :end 50)  ; Lines 10-50 inclusive
(:start 1 :end -1)   ; Entire file
(:start 100)         ; Line 100 to end
```

---

## Directory Entry

A **directory entry** represents a single file or subdirectory within a directory listing.

### Structure

```lisp
(:name "filename.lisp"
 :type :file          ; or :directory
 :size 1234           ; bytes, for files only
 :modified #<timestamp>)
```

### Types

| Type | Description |
|------|-------------|
| `:file` | Regular file |
| `:directory` | Subdirectory |
| `:symlink` | Symbolic link (resolved for operations) |

---

## Directory Tree

A **directory tree** is a hierarchical representation of a directory's contents, recursively including subdirectories.

### Structure

```lisp
(:name "src"
 :type :directory
 :children ((:name "package.lisp" :type :file :size 500)
            (:name "tools"
             :type :directory
             :children (...))))
```

### Exclusion Patterns

Trees can be filtered using glob patterns:

```lisp
:exclude ("*.fasl" ".git" "node_modules")
```

---

## File Info

**File info** provides metadata about a file or directory.

### Structure

```lisp
(:path "/project/src/agent.lisp"
 :type :file
 :size 15234
 :created #<timestamp>
 :modified #<timestamp>
 :accessed #<timestamp>
 :permissions "rw-r--r--"
 :readable t
 :writable t)
```

---

## Safety Tiers

File system tools are organized into **safety tiers** based on their potential impact:

| Tier | Safety Level | Operations | Approval |
|------|--------------|------------|----------|
| 1 | `:safe` | Read-only: list, tree, info, search | Auto-execute |
| 2 | `:cautious` | Modify existing: str_replace, insert | Logged, may diff |
| 3 | `:dangerous` | Destructive: create, move, delete | User approval |

### Tier Behavior

**Tier 1 (Safe)**: Execute immediately, no side effects
**Tier 2 (Cautious)**: Execute and log; may generate diff for review
**Tier 3 (Dangerous)**: Always route through approval handler

---

## Glob Pattern

A **glob pattern** is a wildcard pattern for matching file paths. Agent-Q supports a practical subset of glob syntax optimized for common use cases.

### Supported Syntax

| Pattern | Matches | Example |
|---------|---------|---------|
| `*` | Any characters within a path segment | `*.lisp` matches `agent.lisp` |
| `**` | Any characters across path segments (recursive) | `**/*.md` matches `docs/api/README.md` |
| `?` | Single character | `test-?.el` matches `test-1.el`, `test-a.el` |

### Examples

```
*.lisp         → package.lisp, agent.lisp (same directory)
**/*.lisp      → src/package.lisp, src/tools/buffer.lisp (recursive)
test-?.el      → test-1.el, test-a.el (single char wildcard)
**/test/*.lisp → project/test/foo.lisp (intermediate paths)
```

### Intentional Limitations

**Not Supported**:
- Character classes: `[abc]`, `[!abc]`, `[a-z]`
- Brace expansion: `{a,b,c}`
- Extended glob: `@(pattern)`, `+(pattern)`
- Full regex: `/regex/`

**Rationale**: The implemented subset covers ~95% of use cases while maintaining predictable behavior and manageable implementation complexity. See DR-008 for detailed rationale.

### Semantic Boundaries

**Glob Pattern is NOT**:
- **Regular Expression**: No regex metacharacters (`^`, `$`, `[]`, etc.)
- **Shell Expansion**: No brace expansion or extended globs
- **Path Specification**: Must be matched, not path-joined (use `**` for recursion)

---

## Exclusion Pattern

An **exclusion pattern** is a glob pattern used to filter out unwanted files or directories from operations like directory trees or file searches.

### Behavior

Exclusion patterns are matched against **basename only** (not full path):

```
Pattern: "*.fasl"
Matches:  build/tools.fasl → EXCLUDED (basename: tools.fasl)
Matches:  src/package.fasl → EXCLUDED (basename: package.fasl)
```

### Multiple Exclusions

To exclude both a directory and file patterns, use multiple patterns:

```lisp
:exclude ("*.fasl" ".git" "node_modules")
```

This excludes:
- All `.fasl` files (anywhere)
- Any file/directory named `.git`
- Any file/directory named `node_modules`

### Wildcard Behavior

Empty pattern list means no exclusions. Single `*` pattern excludes everything.

### Semantic Boundaries

**Exclusion Pattern is NOT**:
- **Full Path Match**: Matches basename only, not `"build/*.fasl"`
- **Negation Pattern**: No `!` prefix for inclusion (only exclusions supported)
- **Case-Insensitive**: Matching follows filesystem (Unix: sensitive, macOS HFS+: insensitive)

---

## eval-in-emacs

**eval-in-emacs** is an RPC mechanism for delegating operations to the Emacs environment from the Common Lisp image.

### Purpose

File system tools use `eval-in-emacs` to:
1. Access the filesystem in the user's environment (not Lisp's)
2. Leverage Emacs' mature file handling (attributes, permissions, symlinks)
3. Maintain separation between Lisp image and filesystem side effects
4. Provide security isolation (Emacs runs with user permissions)

### Architecture

```
Agent-Q (CL)              SLY/SWANK              Emacs
    │                         │                     │
    │  (eval-in-emacs expr)  │                     │
    ├────────────────────────►│                     │
    │                         │  Forward elisp expr │
    │                         ├─────────────────────►│
    │                         │                     │
    │                         │  Execute & return   │
    │                         │◄─────────────────────┤
    │  Return result          │                     │
    │◄────────────────────────┤                     │
```

### Dependency

**Critical**: Tools using `eval-in-emacs` require an active SLY or SWANK connection. They will fail with "Not connected to Emacs" error when run outside an Emacs session.

**Implication**: Manual testing via standalone REPL will not work. Must test from within Emacs using `M-x sly`.

### Example Usage

```lisp
;; Get directory listing from Emacs
(eval-in-emacs
  `(directory-files-and-attributes ,path nil nil t))
```

See DR-007 for architectural rationale.

### Semantic Boundaries

**eval-in-emacs is NOT**:
- **Optional**: Not a convenience—filesystem access requires it
- **Fallback-Capable**: No CL-only implementation available
- **Synchronous Guaranteed**: Can block if Emacs is busy
