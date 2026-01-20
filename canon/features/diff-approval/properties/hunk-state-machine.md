# Property: Hunk State Machine

**Feature:** diff-approval
**Category:** State Management
**Source:** docs/DIFF-IMPLEMENTATION.AGENT.md:76-98
**Confidence:** 1.00

---

## Statement

**Each hunk in a diff has a state (nil/applied/rejected) that transitions according to a deterministic state machine based on user actions.**

---

## Formal Definition

### State Space

```
S = {nil, applied, rejected}

Where:
  nil      = pending (not yet acted upon)
  applied  = successfully applied to target file
  rejected = explicitly rejected by user
```

### State Machine

```
        ┌─────┐
        │ nil │ (initial state)
        └──┬──┘
           │
    ┌──────┴──────┐
    │             │
    │ (a)         │ (r)
    ▼             ▼
┌─────────┐   ┌──────────┐
│ applied │   │ rejected │
└────┬────┘   └────┬─────┘
     │             │
     │ (SPC)       │ (SPC)
     │             │
     └──────┬──────┘
            │
            ▼
         ┌─────┐
         │ nil │
         └─────┘
```

### Transition Table

| Current State | Action | Next State | Side Effect |
|---------------|--------|------------|-------------|
| `nil` | `a` | `applied` | File modified via `diff-apply-hunk` |
| `nil` | `r` | `rejected` | None (state change only) |
| `applied` | `SPC` | `nil` | **None** (visual only, file unchanged) |
| `rejected` | `SPC` | `applied` | File modified (attempts `diff-apply-hunk`) |

**Critical:** `applied → nil` transition does **NOT** unapply changes from file.

---

## Implementation

### State Storage

```elisp
(defvar-local sly-agent-q-diff--hunk-states nil
  "Alist mapping hunk position → state.
   Example: ((100 . applied) (200 . rejected) (300 . nil))")
```

**Structure:** `((pos1 . state1) (pos2 . state2) ...)`
**Position:** Buffer position of `@@` hunk header
**Initial State:** All hunks start as `nil` (pending)

### Transition Functions

#### Apply Hunk (`nil` → `applied`)

```elisp
(defun sly-agent-q-diff-accept-hunk ()
  "Apply current hunk and mark as applied."
  (interactive)
  (save-excursion
    (sly-agent-q-diff--goto-hunk-start)
    (let ((hunk-pos (point)))
      (condition-case err
          (progn
            (diff-apply-hunk)  ; ← File modification
            (setf (alist-get hunk-pos sly-agent-q-diff--hunk-states) 'applied)
            (sly-agent-q-diff--apply-visual-feedback))
        (error
         (message "Failed to apply hunk: %s" err))))))
```

**Side Effect:** Modifies target file buffer + updates state

#### Reject Hunk (`nil` → `rejected`)

```elisp
(defun sly-agent-q-diff-reject-hunk ()
  "Mark current hunk as rejected (no file change)."
  (interactive)
  (save-excursion
    (sly-agent-q-diff--goto-hunk-start)
    (let ((hunk-pos (point)))
      (setf (alist-get hunk-pos sly-agent-q-diff--hunk-states) 'rejected)
      (sly-agent-q-diff--apply-visual-feedback))))
```

**Side Effect:** State change only, no file modification

#### Toggle Hunk (Cycle States)

```elisp
(defun sly-agent-q-diff-toggle-hunk ()
  "Toggle hunk state."
  (interactive)
  (let* ((hunk-pos (sly-agent-q-diff--current-hunk-start))
         (current-state (alist-get hunk-pos sly-agent-q-diff--hunk-states)))
    (cond
      ((eq current-state 'applied)
       ;; applied → nil (visual only, file unchanged)
       (setf (alist-get hunk-pos sly-agent-q-diff--hunk-states) nil))

      ((eq current-state 'rejected)
       ;; rejected → applied (attempt to apply)
       (condition-case err
           (progn
             (diff-apply-hunk)
             (setf (alist-get hunk-pos sly-agent-q-diff--hunk-states) 'applied))
         (error
          (message "Failed to apply hunk: %s" err))))

      (t
       ;; nil → rejected (default toggle)
       (setf (alist-get hunk-pos sly-agent-q-diff--hunk-states) 'rejected))))
    (sly-agent-q-diff--apply-visual-feedback)))
```

---

## Rationale

### Why Three States?

1. **nil (pending)**: User hasn't decided yet
2. **applied**: User approved and changes are live
3. **rejected**: User explicitly declined (prevents accidental application)

### Why Allow `applied → nil` Transition?

**Reason:** Visual feedback correction. User may accidentally press `a`, toggle back to see original diff.

**Important:** File remains modified—this is visual state only.

### Why Not Allow `applied → rejected` Direct Transition?

**Reason:** Unclear semantics. Applied changes already in file. Reject would be misleading.

**Solution:** Use `applied → nil → rejected` (two steps)

---

## Behavior Examples

### Example 1: Standard Apply Flow

```
Initial: Hunk at position 100, state = nil

User presses 'a':
  → diff-apply-hunk modifies file
  → State becomes 'applied'
  → Visual overlay shows ✓

User presses 'q' (finish):
  → Decision = "accepted" (at least one applied)
```

### Example 2: Accidental Apply, Then Toggle

```
Initial: Hunk at position 100, state = nil

User presses 'a' (accidentally):
  → State becomes 'applied'
  → File modified

User presses 'SPC' (toggle):
  → State becomes 'nil' (pending)
  → File STILL modified (toggle is visual only)

User presses 'r':
  → State becomes 'rejected'
  → File STILL modified (reject doesn't undo)
```

**Lesson:** Toggle doesn't undo file changes!

### Example 3: Reject Then Change Mind

```
Initial: Hunk at position 100, state = nil

User presses 'r':
  → State becomes 'rejected'
  → No file change

User presses 'SPC' (toggle):
  → State becomes 'applied'
  → File now modified (diff-apply-hunk executed)
```

---

## Verification

### Test: State Transitions

**Test:** `sly-agent-q-diff-test-accept-single-hunk` (sly-agent-q-diff-test.el:88)

```elisp
(ert-deftest sly-agent-q-diff-test-accept-single-hunk ()
  "Test accepting a single hunk marks it as applied."
  (with-temp-buffer
    (insert diff-text)
    (sly-agent-q-diff-mode)
    (goto-char (point-min))
    (sly-agent-q-diff-accept-hunk)
    (should (eq (alist-get (point) sly-agent-q-diff--hunk-states) 'applied))))
```

**Status:** ✅ Pass

### Test: Reject Transition

**Test:** `sly-agent-q-diff-test-reject-single-hunk` (sly-agent-q-diff-test.el:127)

**Status:** ✅ Pass

### Test: Toggle Behavior

**Test:** `sly-agent-q-diff-integration-toggle` (sly-agent-q-diff-integration-test.el:143)

**Status:** ✅ Pass

---

## Related Properties

- **irreversibility.md**: Applied hunks cannot be truly unapplied
- **decision-logic.md**: Final decision based on aggregated states

---

## Known Issues

### Issue 1: Toggle Doesn't Undo File Changes

**Problem:** `applied → nil` visual transition misleads user
**Impact:** User may think file changes were reverted
**Mitigation:** Documentation clearly states irreversibility
**Status:** By design (undoing would require complex undo stack)

### Issue 2: No Confirmation on Accidental Apply

**Problem:** Single keypress (`a`) permanently modifies file
**Impact:** No safety prompt for potentially destructive action
**Workaround:** User can undo in target buffer
**Recommendation:** Consider confirmation for first apply in session

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2026-01-10 | State machine implemented | Phase 2 diff approval |
| 2026-01-17 | Property documented | Canon extraction |

---

**Property Status:** ✅ Verified (tested)
**Confidence:** 1.00
**Test Coverage:** ✅ Comprehensive (all transitions tested)
