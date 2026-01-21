# How-to: Review and Apply Code Changes

Learn to review Agent-Q's proposed changes hunk-by-hunk and selectively apply them.

## Problem

Agent-Q proposes changes to your code, but you want granular control—accepting good changes while rejecting others.

## Solution

Agent-Q shows changes as unified diffs with **hunk-by-hunk review**. Navigate between hunks, accept or reject each one individually, and finish when done. Applied changes modify your file immediately.

---

## Quickstart (1 minute)

### 1. Agent Proposes Changes

When Agent-Q wants to modify a file, you'll see:

```
Agent-Q proposes changes to: src/utils.lisp
Description: Add error handling and docstring

[Diff buffer appears]
```

### 2. Review the Diff

```diff
--- a/src/utils.lisp
+++ b/src/utils.lisp
@@ -10,5 +10,8 @@
 (defun process-items (items)
-  (mapcar #'1+ items))
+  "Increment each numeric item in the list."
+  (mapcar (lambda (x)
+            (when (numberp x)
+              (1+ x)))
+          items))
```

### 3. Navigate and Decide

```
n         Next hunk
p         Previous hunk
a         Accept this hunk (applies to file immediately)
r         Reject this hunk (skip it)
q         Done reviewing
```

### 4. Press 'a' to Accept

The hunk turns **green with [APPLIED]** and your file is modified.

### 5. Press 'q' When Done

The diff buffer closes. Your file now has the accepted changes.

**Important:** Changes are saved to the buffer but not the file. Press `C-x C-s` when ready to save.

---

## Understanding Diffs

### The Diff Format

```diff
--- a/src/utils.lisp        ← Original file
+++ b/src/utils.lisp        ← Modified file
@@ -10,5 +10,8 @@           ← Hunk header (line 10, context)
 (defun process-items (items)  ← Context (unchanged)
-  (mapcar #'1+ items))      ← Removed line (red -)
+  "Increment each item."    ← Added line (green +)
+  (mapcar (lambda (x)       ← Added line (green +)
```

### Hunks

A **hunk** is one contiguous block of changes. Large diffs have multiple hunks:

```
Hunk 1: Add docstring (lines 10-12)
Hunk 2: Refactor function name (lines 45-48)
Hunk 3: Fix typo (line 102)
```

You review each hunk separately.

### The Progress Line

```
Progress: 1/3 applied, 0 rejected, 2 pending  |  [a]ccept [r]eject [n]ext [p]rev [q]uit
```

Shows:
- **applied**: How many hunks you've accepted
- **rejected**: How many hunks you've explicitly rejected
- **pending**: How many hunks you haven't decided on yet

---

## Reviewing Changes

### Navigate Between Hunks

```
n         Next hunk
p         Previous hunk
```

The cursor jumps to each hunk's `@@` header.

### Accept a Hunk

```
a         Accept (apply immediately)
```

**What happens:**
1. The hunk applies to your file **right now** (not at the end)
2. The hunk turns **green** with **[APPLIED]** marker
3. Progress updates: `applied` count increases
4. Your file buffer is modified

**Important:** This action is immediate and permanent. The file change happens now.

### Reject a Hunk

```
r         Reject (skip this change)
```

**What happens:**
1. The hunk turns **red** with **[REJECTED]** marker
2. Progress updates: `rejected` count increases
3. **No file change** - this hunk is skipped

### Toggle State

```
SPC       Toggle hunk state
```

**Behavior:**
- **Pending → Rejected**: Mark as rejected
- **Rejected → Applied**: Apply the hunk
- **Applied → Pending**: Change visual state back to pending

**⚠️ Warning:** Toggling from **Applied → Pending** does **NOT** undo the file change! The file remains modified. Only the visual state changes.

### Preview Source

```
RET       Jump to source location
```

Opens the target file and positions cursor at the hunk's location. Useful for seeing surrounding code.

### Finish Review

```
q         Done (close diff buffer)
```

Closes the diff buffer and returns to the chat.

**Decision sent to Agent-Q:**
- **"accepted"** - If at least one hunk was applied
- **"rejected"** - If zero hunks were applied

---

## Common Workflows

### Accept All Changes

If you trust all changes:

```
a a a q
```

Or use the legacy shortcut:

```
C-c C-c   Accept all hunks at once
```

### Reject All Changes

If you don't want any changes:

```
C-c C-k   Reject all hunks
```

Or quit immediately:

```
q   (with no hunks applied = rejected)
```

### Selective Review (Recommended)

Review each hunk carefully:

```
[Hunk 1] Add docstring
  → Press 'a' ✅ Applied

[Hunk 2] Rename variable to 'new-name'
  → Press 'r' ❌ Rejected (I like the old name)

[Hunk 3] Fix null check
  → Press 'a' ✅ Applied

Press 'q' → 2/3 hunks applied
```

### Preview Before Deciding

For complex changes:

```
1. Press 'n' to see hunk
2. Press 'RET' to jump to source
3. Read surrounding code
4. Press 'C-x o' to return to diff buffer
5. Press 'a' or 'r'
```

---

## Visual Feedback

### Hunk States

**Pending (white background):**
```diff
@@ -10,5 +10,8 @@
 (defun process-items (items)
-  (mapcar #'1+ items))
```

No decision yet. Default state.

**Applied (green background):**
```diff
[APPLIED]
@@ -10,5 +10,8 @@
 (defun process-items (items)
-  (mapcar #'1+ items))
```

Hunk was accepted and applied to file.

**Rejected (red background):**
```diff
[REJECTED]
@@ -10,5 +10,8 @@
 (defun process-items (items)
-  (mapcar #'1+ items))
```

Hunk was explicitly rejected (not applied).

### Progress Bar

The progress line updates in real-time:

```
Initial:  Progress: 0/3 applied, 0 rejected, 3 pending
After 'a': Progress: 1/3 applied, 0 rejected, 2 pending
After 'r': Progress: 1/3 applied, 1 rejected, 1 pending
After 'a': Progress: 2/3 applied, 1 rejected, 0 pending
```

---

## Understanding File Application

### When Changes Happen

**Applied hunks modify the file immediately.** Not when you press `q`, but when you press `a`.

```
Press 'a' → File modified NOW
Press 'q' → Closes diff (file already modified)
```

### Saving Changes

Agent-Q modifies your **buffer**, not the **file on disk**.

**To save:**
```
C-x C-s   Save buffer to file
```

**Why?** Gives you a chance to review the final result before committing to disk.

### Undoing Changes

If you applied a hunk by mistake:

1. **Switch to the source buffer** (`C-x b filename`)
2. **Undo** (`C-x u` or `C-/`)
3. **Return to diff** (`C-x o`)

The diff buffer won't know you undid, but your file is reverted.

### Irreversibility

**⚠️ Important:** Once you press `a`, the hunk is applied. The diff UI has no "undo applied hunk" button.

**Options if you made a mistake:**
1. Manually undo in the source buffer
2. Reject remaining hunks and ask Agent-Q to try again
3. Quit and restart with clarified instructions

---

## Advanced Features

### Help Overlay

```
?         Show help overlay
```

Displays all keybindings in a popup.

### Multiple Files

If Agent-Q proposes changes to multiple files, each file gets its own diff buffer reviewed separately.

### Diff Context

By default, diffs show 3 lines of context around changes. This helps you understand what's being modified.

```diff
@@ -10,5 +10,8 @@        ← Line 10, 5 lines before, 8 lines after
 (defun process-items (items)    ← Context line
-  (mapcar #'1+ items))          ← Change
+  "Process items."              ← Change
+  (mapcar #'1+ items))          ← Change
                                 ← Context line
```

### Verification Mismatch

If the diff can't be applied (e.g., the file changed since Agent-Q read it):

```
Error: Diff verification failed
Expected: <original content>
Found: <actual content>
```

**Fix:**
1. Press `q` to quit
2. Ask Agent-Q to regenerate the diff with the current file state

---

## Keyboard Reference

### Navigation

| Key | Action |
|-----|--------|
| `n` | Next hunk |
| `p` | Previous hunk |
| `RET` | Preview source location |

### Review

| Key | Action |
|-----|--------|
| `a` | Accept hunk (apply immediately) |
| `r` | Reject hunk |
| `SPC` | Toggle hunk state |

### Finish

| Key | Action |
|-----|--------|
| `q` | Done reviewing (close buffer) |
| `?` | Show help |

### Legacy (All-or-Nothing)

| Key | Action |
|-----|--------|
| `C-c C-c` | Accept all hunks |
| `C-c C-k` | Reject all hunks |

---

## Tips

### Review Strategy

1. **Read the description** at the top of the diff
2. **Scan all hunks first** (press `n` repeatedly)
3. **Review each hunk** with context (`RET` to see source)
4. **Accept or reject** based on correctness and intent
5. **Test after applying** - run your code to verify

### When to Accept

✅ **Accept when:**
- The change is correct
- The change matches your intent
- You understand what it does
- The code looks good

### When to Reject

❌ **Reject when:**
- The change is wrong
- You want a different approach
- The code style doesn't match
- You're unsure (can always ask Agent-Q to explain)

### Partial Acceptance Workflow

```
1. Accept obviously good changes (docstrings, fixes)
2. Reject uncertain changes (refactors, renames)
3. Ask Agent-Q to explain rejected hunks
4. Regenerate diff with clarified instructions
```

---

## Troubleshooting

### "Diff won't apply"

**Error:**
```
Error: Hunk failed to apply
```

**Causes:**
1. File changed since Agent-Q read it
2. Previous hunk modified nearby lines
3. Concurrent edit by another process

**Fix:**
1. Press `q` to quit
2. Save your buffer: `C-x C-s`
3. Ask Agent-Q to regenerate: "Try again with the current file"

### "Can't see the whole hunk"

Large hunks may scroll off screen.

**Fix:**
```
C-v        Scroll down (page down)
M-v        Scroll up (page up)
```

Or adjust window size: `C-x ^` (enlarge window vertically)

### "Pressed 'a' by accident"

**Fix:**
1. Switch to source buffer: `C-x b filename`
2. Undo: `C-x u`
3. Return to diff: `C-x o`
4. Press `r` on remaining hunks if desired

### "Diff buffer disappeared"

If you accidentally closed the diff buffer, you can't get it back. The review is canceled.

**Fix:**
Ask Agent-Q to regenerate: "Please show me the diff again"

### "Don't understand the change"

**Fix:**
1. Press `q` to quit without applying
2. Ask Agent-Q: "Explain why you made these changes"
3. Review the explanation
4. Ask for a new diff if needed

---

## What Happens Behind the Scenes

### The propose_file_edit Tool

Agent-Q uses the `propose_file_edit` tool:

```lisp
(propose-file-edit
  :path "src/utils.lisp"
  :old-content "..."
  :new-content "..."
  :description "Add error handling")
```

**Flow:**
1. Tool generates unified diff
2. Diff buffer opens (blocks Agent-Q)
3. You review and decide
4. Tool returns decision: "accepted" or "rejected"
5. Agent-Q continues based on decision

### Blocking Behavior

**Important:** While the diff buffer is open, **Agent-Q is waiting**.

- The Lisp process is blocked (recursive-edit)
- Agent-Q won't do anything until you finish
- This prevents the agent from continuing before you review

### Decision Protocol

**"accepted"** = At least one hunk applied
```
Agent-Q sees: "Applied 2 of 3 hunks to src/utils.lisp"
```

**"rejected"** = Zero hunks applied
```
Agent-Q sees: "User rejected proposed changes to src/utils.lisp"
```

Agent-Q uses this feedback to decide what to do next.

---

## Next Steps

- **Learn Context**: See `context-completion.md` for adding files to context before asking for changes
- **Tool System**: See `tool-system.md` to understand other tools Agent-Q uses
- **Chat Interface**: See `chat-interface.md` for the full conversation workflow
- **File System Tools**: See `file-system-tools.md` for how Agent-Q proposes changes via edit_file
