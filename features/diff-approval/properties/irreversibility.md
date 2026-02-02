# Property: Irreversibility

**Feature:** diff-approval
**Category:** Data Integrity
**Source:** docs/DIFF-IMPLEMENTATION.AGENT.md:98
**Confidence:** 1.00

---

## Statement

**Applied hunks modify the target file immediately and irreversibly. The diff approval UI cannot unapply changes—only Emacs undo can reverse them.**

---

## Formal Definition

### Irreversibility Invariant

```
∀ hunk h, ∀ time t1 < t2:
  If state(h, t1) = applied
  Then file-content(target, t1) ≠ file-content(target, t0)
  And  ∃ no UI action in diff-mode to restore file-content(target, t0)
```

**In words:** Once a hunk is applied at time t1, the file is modified. No action in the diff UI can restore the file to its pre-application state at t0.

### Recovery Path

```
Applied hunk → File modified
                    ↓
            Only recovery: Emacs undo in target buffer
                (NOT via diff UI)
```

---

## Implementation

### File Modification

**Function:** `diff-apply-hunk` (from Emacs built-in `diff-mode.el`)

```elisp
(defun sly-agent-q-diff-accept-hunk ()
  "Apply current hunk to target file."
  (interactive)
  (save-excursion
    (diff-apply-hunk)  ; ← Modifies target buffer directly
    ...))
```

**Behavior:**
1. Finds target file buffer (opens with `find-file-noselect` if needed)
2. Applies hunk changes to buffer text
3. Marks buffer as modified (enables auto-save)

**Result:** Target file buffer immediately contains changes

### No Undo in Diff UI

**State Toggle:** `applied → nil` transition exists but is **visual only**

```elisp
(defun sly-agent-q-diff-toggle-hunk ()
  (cond
    ((eq current-state 'applied)
     ;; Visual state changes to nil
     (setf (alist-get hunk-pos sly-agent-q-diff--hunk-states) nil)
     ;; BUT: File buffer remains modified (no unapply))
  ...))
```

**Critical:** Visual state ≠ file state

---

## Rationale

### Why Irreversible?

1. **Simplicity**: No need to track reverse patches
2. **Correctness**: Undoing is complex (hunks may conflict after partial application)
3. **Trust Emacs undo**: Built-in undo system is robust and well-tested
4. **User intent**: Applied = user approved change, shouldn't be accidental

### Why Not Implement Undo?

**Challenge:** Reverse patch application is non-trivial

**Example:**
```
Original file:
1: (defun foo ()
2:   42)

Apply hunk: Add line 2
1: (defun foo ()
2:   "Docstring"
3:   42)

User then edits: Change line 3
1: (defun foo ()
2:   "Docstring"
3:   43)  ← Edited

Attempt to unapply original hunk?
→ Context no longer matches
→ Cannot safely remove line 2
```

**Conclusion:** Reverse patches fail when file edited after application.

### Emacs Undo is Superior

**Built-in undo advantages:**
- Tracks all edits (not just hunk applications)
- Handles conflicts correctly
- User familiar with `C-/` workflow
- No need to reimplement

---

## Behavior Examples

### Example 1: Apply, Then Realize Mistake

```
Initial state:
- File: (defun foo () 42)
- Diff proposes: Add docstring

User presses 'a' (apply hunk):
- File becomes: (defun foo () "Doc" 42)
- State: applied

User realizes mistake, presses SPC (toggle):
- State: nil (visual change)
- File: STILL (defun foo () "Doc" 42)  ← Unchanged!

User wants to undo:
- Switch to file buffer
- Press C-/ (undo)
- File reverts: (defun foo () 42)  ✓
```

**Lesson:** Undo must happen in target buffer, not diff UI.

### Example 2: Partial Application, Then File Edit

```
Diff has 2 hunks:
- Hunk 1: Add docstring to foo
- Hunk 2: Add docstring to bar

User applies Hunk 1:
- File modified

User switches to file buffer, manually edits foo:
- foo now has different docstring than proposed

User returns to diff, tries to "unapply" Hunk 1:
- No UI mechanism exists
- Emacs undo would work, but user already made other edits
- File state is now mix of applied hunk + manual edits

Result: Cannot cleanly unapply
```

**Lesson:** Once applied + file edited, unapply is ambiguous.

---

## Consequences

### Positive

✅ **Simple mental model**: Apply = permanent (like git commit)
✅ **No state tracking**: Don't need to maintain reverse patches
✅ **Delegates to Emacs undo**: Leverages robust built-in system

### Negative

⚠️ **Accidental apply is costly**: Single keypress can't be undone in UI
⚠️ **Visual state misleading**: `applied → nil` toggle looks like undo but isn't
⚠️ **Requires buffer switch**: Must leave diff UI to undo

---

## User Experience Implications

### User Expectation vs Reality

**User might expect:**
- Toggle `applied → nil` undoes file changes
- "Reject" after "apply" reverts file

**Reality:**
- Toggle is visual only
- File remains modified

**Mitigation:**
- Documentation explicitly states irreversibility
- Visual feedback (overlays) clearly distinguish states
- Header shows "N applied" count (helps user track)

### Recommended User Workflow

**Before applying:**
1. Carefully review each hunk
2. Use `RET` to preview in source file
3. Only press `a` when certain

**If mistake:**
1. Press `q` to exit diff UI
2. Switch to target buffer
3. Use Emacs undo (`C-/`)

---

## Verification

### Test: File Modified After Apply

**Test:** `sly-agent-q-diff-test-apply-single-hunk` (sly-agent-q-diff-test.el:16)

```elisp
(ert-deftest sly-agent-q-diff-test-apply-single-hunk ()
  "Test that applying a hunk modifies the target file."
  (let ((file (make-temp-file "test" nil ".lisp")))
    (with-temp-file file
      (insert "(defun foo () 42)"))
    (let ((original "(defun foo () 42)")
          (modified "(defun foo ()\n  \"Doc\"\n  42)"))
      ;; Generate diff, apply hunk
      (sly-agent-q-show-diff-and-wait file original modified "Add doc")
      ;; Simulate user pressing 'a'
      (sly-agent-q-diff-accept-hunk)
      ;; Verify file buffer changed
      (with-current-buffer (find-file-noselect file)
        (should (string-match "Doc" (buffer-string)))))))
```

**Status:** ✅ Pass (file modified after apply)

### Test: Toggle Doesn't Unapply

**Manual verification** (no explicit test for this property):

```elisp
;; Verify toggle doesn't modify file
(let ((file "test.lisp"))
  ;; Apply hunk
  (sly-agent-q-diff-accept-hunk)
  (let ((content-after-apply (buffer-string (find-buffer file))))

    ;; Toggle to nil
    (sly-agent-q-diff-toggle-hunk)
    (let ((content-after-toggle (buffer-string (find-buffer file))))

      ;; Content should be UNCHANGED
      (assert (string= content-after-apply content-after-toggle)))))
```

**Status:** ⚠️ Not in test suite (coverage gap)

---

## Related Properties

- **hunk-state-machine.md**: State transitions (apply is one-way)
- **decision-logic.md**: "accepted" decision persists even if user toggles visual state

---

## Known Limitations

### L-001: No Unapply Mechanism

**Problem:** Cannot undo applied hunks from diff UI
**Workaround:** Use Emacs undo in target buffer
**Status:** By design

### L-002: Visual State Misleading

**Problem:** `applied → nil` looks like undo but isn't
**Impact:** User confusion
**Mitigation:** Documentation + visual indicators
**Recommendation:** Consider renaming state to `nil (pending)` → `pending` to clarify

### L-003: No Confirmation Prompt

**Problem:** Single `a` keypress permanently modifies file
**Impact:** Easy to apply accidentally
**Mitigation:** Users learn to be careful
**Recommendation:** Add `(yes-or-no-p "Apply this hunk?")` for first application

---

## Future Enhancements

### Enhancement 1: Explicit "Cannot Unapply" Message

When user toggles `applied → nil`, display message:
```elisp
(message "Hunk marked as pending (visual only). File remains modified. Use C-/ in target buffer to undo.")
```

### Enhancement 2: Undo Stack Integration

Integrate with Emacs undo system:
```elisp
;; After applying hunk, record in undo list
(with-current-buffer target-buffer
  (undo-boundary)  ; Create undo checkpoint
  (diff-apply-hunk)
  (undo-boundary))
```

**Benefit:** User can undo multiple hunks as single operation

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2026-01-10 | Irreversibility enforced | Phase 2 implementation |
| 2026-01-17 | Property documented | Canon extraction |

---

**Property Status:** ✅ Verified (code inspection + tests)
**Confidence:** 1.00
**Test Coverage:** ⚠️ Partial (apply tested, but toggle-doesn't-unapply not explicitly tested)
