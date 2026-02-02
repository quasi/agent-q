# Scenario: Targeted Code Edit

**Feature:** file-system-tools
**Version:** 0.1.0
**Status:** Draft

---

## Overview

A developer asks Agent-Q to fix a bug in a specific function. Agent-Q uses the file system tools to locate, understand, and precisely edit the code.

---

## Preconditions

- Agent-Q is connected to a running Lisp image via SLY
- Project root is set to `/Users/quasi/projects/my-app/`
- File `src/validator.lisp` exists with a bug in `validate-email` function

---

## Trigger

User: "The email validation function is incorrectly rejecting emails with plus signs like user+tag@example.com. Can you fix it?"

---

## Flow

### Step 1: Locate the File

Agent-Q uses `list_directory` to understand the project structure:

```json
{"tool": "list_directory", "arguments": {"path": "src"}}
```

**Response:**
```
Directory: /Users/quasi/projects/my-app/src/
Total: 5 files, 1 directory

[DIR]  utils/
[FILE] main.lisp       (2.1 KB)
[FILE] routes.lisp     (4.3 KB)
[FILE] validator.lisp  (1.8 KB)
[FILE] models.lisp     (3.2 KB)
```

### Step 2: Read the File

Agent-Q uses existing `read_file` tool:

```json
{"tool": "read_file", "arguments": {"path": "src/validator.lisp"}}
```

**Response:**
```lisp
;;;; validator.lisp - Input validation utilities

(defun validate-email (email)
  "Validate that EMAIL is a valid email address."
  (and (stringp email)
       (scan "^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" email)))

(defun validate-username (username)
  ...)
```

### Step 3: Analyze the Bug

Agent-Q recognizes the issue: the regex `[a-zA-Z0-9._-]` doesn't include `+`.

### Step 4: Make Targeted Edit

Agent-Q uses `edit_file` to fix the regex:

```json
{
  "tool": "edit_file",
  "arguments": {
    "path": "src/validator.lisp",
    "old_str": "(scan \"^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}$\" email)",
    "new_str": "(scan \"^[a-zA-Z0-9._+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}$\" email)",
    "description": "Add + to allowed characters before @ in email regex"
  }
}
```

**Response:**
```
✓ Edit applied to src/validator.lisp

Change: Add + to allowed characters before @ in email regex
Lines affected: 5
Diff:
  - (scan "^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" email)
  + (scan "^[a-zA-Z0-9._+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" email)
```

### Step 5: Verify the Fix

Agent-Q uses `eval_form` to test:

```json
{
  "tool": "eval_form",
  "arguments": {
    "form": "(validate-email \"user+tag@example.com\")",
    "package": "my-app"
  }
}
```

**Response:**
```
Result: T
```

---

## Postconditions

- File `src/validator.lisp` is modified with the fix
- The regex now includes `+` in the character class
- The fix is minimal (single character addition)
- If the file was open in an Emacs buffer, the buffer is updated

---

## Alternative Flows

### A1: Multiple Matches

If the regex appeared multiple times:

```
Error: Found 3 matches for the search string in src/validator.lisp.

Please provide more surrounding context to make a unique match.
```

Agent-Q would then include more of the function:

```json
{
  "tool": "edit_file",
  "arguments": {
    "path": "src/validator.lisp",
    "old_str": "(defun validate-email (email)\n  \"Validate that EMAIL is a valid email address.\"\n  (and (stringp email)\n       (scan \"^[a-zA-Z0-9._-]+@...",
    "new_str": "..."
  }
}
```

### A2: Complex Change Requiring Approval

For changes spanning many lines or affecting critical code, Agent-Q might use `propose_file_edit` instead to get explicit user approval:

```json
{
  "tool": "propose_file_edit",
  "arguments": {
    "path": "src/validator.lisp",
    "original": "... full original content ...",
    "modified": "... full modified content ...",
    "description": "Refactor validate-email to support plus addressing"
  }
}
```

User reviews diff in `*Agent-Q Diff*` buffer and approves.

---

## Error Cases

### E1: File Not Found

```
Error: File 'src/validater.lisp' not found

Similar files in src/:
- validator.lisp
```

### E2: Path Outside Project

```
Error: Path '/etc/passwd' is outside project root (/Users/quasi/projects/my-app/)

Use paths relative to the project root.
```

### E3: No Match

```
Error: String not found in src/validator.lisp

Searched for:
  (scan "^[a-zA-Z0-9._-]+@...

The file may have been modified. Try reading it again with read_file.
```

---

## Key Properties Demonstrated

1. **Minimal Change**: Only the regex character class is modified
2. **Exact Matching**: The edit targets a specific, unique string
3. **Verification**: Agent tests the fix using `eval_form`
4. **Buffer Sync**: Emacs buffers updated if file was open
5. **Path Safety**: All operations within project root

---

**Scenario Status:** Draft
**Last Updated:** 2026-01-20
