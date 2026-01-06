# Diff Review Guide: Per-Hunk Approval

## Overview

Agent-Q's diff review system lets you accept or reject individual changes (hunks) instead of all-or-nothing. This gives you granular control over what the agent modifies.

## What is a Hunk?

A **hunk** is a contiguous block of changes in a diff. Example:

```diff
@@ -10,3 +10,4 @@
 (defun calculate-total (items)
+  "Sum the prices of ITEMS."
   (reduce #'+ items :key #'price))
```

This hunk adds a docstring. You can accept this specific change even if you reject other hunks in the same diff.

## Basic Workflow

1. **Agent proposes changes** - Diff buffer opens
2. **Navigate hunks** - `n` (next) / `p` (previous)
3. **Review each hunk** - Press `a` (accept) or `r` (reject)
4. **Finish review** - Press `q` to close

## Keybinding Reference

### Per-Hunk Commands

| Key     | Command                  | Description                              |
|---------|--------------------------|------------------------------------------|
| `a`     | Accept hunk              | Apply this hunk immediately              |
| `r`     | Reject hunk              | Skip this hunk (don't apply)             |
| `SPC`   | Toggle                   | Toggle between accepted/rejected         |
| `n`     | Next hunk                | Move to next hunk                        |
| `p`     | Previous hunk            | Move to previous hunk                    |
| `RET`   | Preview                  | Jump to source location                  |
| `q`     | Finish                   | Close review (prompts if hunks pending)  |
| `?`     | Help                     | Show keybinding help                     |

### All-or-Nothing Commands (Legacy)

| Key       | Command       | Description                    |
|-----------|---------------|--------------------------------|
| `C-c C-c` | Accept all    | Apply all hunks and close      |
| `C-c C-k` | Reject all    | Discard all changes and close  |

## Visual Feedback

The diff buffer provides clear visual indicators:

- **Green background** with `[APPLIED]` prefix - Hunk was accepted and applied
- **Red background** with `[REJECTED]` prefix - Hunk was rejected
- **Normal display** - Hunk is pending review

### Progress Header

The header line shows current progress:

```
Progress: 2/5 applied, 1 rejected, 2 pending  |  [a]ccept [r]eject [n]ext [p]rev [q]uit
```

## Example Scenarios

### Scenario 1: Selective Documentation

Agent proposes to add docstrings to three functions. You want only two:

```
Agent-Q proposes changes to: utils.lisp
Description: Add documentation to utility functions

Progress: 0/3 applied, 0 rejected, 3 pending

@@ -5,2 +5,3 @@
 (defun parse-input (str)
+  "Parse STR and return structured data."    ← Press 'a' to accept
   (let ((data (read-from-string str)))

@@ -15,2 +16,3 @@
 (defun format-output (data)
+  "Format DATA for display."                  ← Press 'a' to accept
   (format nil "~A" data))

@@ -25,2 +26,3 @@
 (defun internal-helper (x)
+  "Internal helper function."                 ← Press 'r' (private fn, skip)
   (* x 2))
```

**Result:** 2/3 hunks applied. File is modified. Save when ready.

### Scenario 2: Partial Refactoring

Agent proposes to rename variables and add type declarations. You want only the renames:

```
@@ -1,3 +1,3 @@
-(defvar items nil)
+(defvar *items* nil)                          ← Press 'a' (better naming)

@@ -10,2 +10,3 @@
+(declaim (type list *items*))                 ← Press 'r' (skip type decl)

@@ -15,3 +16,3 @@
-(push item items)
+(push item *items*)                           ← Press 'a' (consistent naming)
```

### Scenario 3: Reviewing Fixes

Agent proposes a bug fix with multiple changes:

```
@@ -20,3 +20,4 @@
 (defun process (data)
+  (check-type data list)                      ← Press 'a' (good validation)
   (dolist (item data)

@@ -25,2 +26,2 @@
-    (when (valid-p item)
+    (when (and item (valid-p item))           ← Press 'a' (nil check needed)

@@ -30,1 +31,3 @@
-      (handle item))))
+      (handler-case
+          (handle item)
+        (error (e) (log-error e))))))         ← Press 'r' (too defensive)
```

## Tips

### Navigating Large Diffs

- Use `n`/`p` to move between hunks quickly
- Use `RET` to see where changes apply in the source file
- The progress header shows how many hunks remain

### When Accepting Applied Hunks

Once you press `a` on a hunk:
- The change is immediately applied to the source buffer
- The hunk is marked `[APPLIED]` with green background
- You **cannot undo** individual hunks (use regular Emacs undo on the source file)

### Toggle for Quick Review

Use `SPC` to toggle a hunk's state:
- On pending hunks: accepts the hunk
- On rejected hunks: accepts the hunk
- On applied hunks: marks as pending (but doesn't unapply - changes stay in source)

### Finishing Review

When you press `q`:
- If all hunks are reviewed: closes immediately
- If hunks are pending: prompts "X unreviewed hunk(s) remaining. Finish anyway?"
- The result (accepted/rejected) is sent back to the agent

### Agent Behavior on Rejection

When you reject changes:
- The agent is told the changes were rejected
- The agent will **not** automatically retry with alternatives
- If you want alternatives, explicitly ask the agent

## Integration with Agent Loop

The diff review is synchronous - the agent waits for your decision:

1. Agent calls `propose_file_edit` tool
2. Emacs displays diff buffer
3. You review and accept/reject hunks
4. You press `q` to finish
5. Agent receives feedback: "N hunks applied, M rejected"
6. Agent can continue with next steps

## Troubleshooting

### Hunk Won't Apply

If pressing `a` shows an error:
- The source file may have changed since the diff was generated
- The hunk context may not match the current file state
- Try `C-c C-k` to reject all and ask agent to regenerate

### Can't Find Hunk in Source

If `RET` doesn't jump to the right location:
- The diff may reference outdated line numbers
- Navigate manually and compare context lines

### Buffer Disappeared

If the diff buffer closes unexpectedly:
- Check `*Messages*` for errors
- The review defaults to "rejected" if interrupted
- Ask the agent to propose changes again
