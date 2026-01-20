# Property: Decision Logic

**Feature:** diff-approval
**Category:** Business Logic
**Source:** docs/DIFF-IMPLEMENTATION.AGENT.md:165-171
**Confidence:** 1.00

---

## Statement

**The final decision returned to the LLM is "accepted" if at least one hunk was applied, otherwise "rejected". This is determined when the user exits the diff UI.**

---

## Formal Definition

### Decision Function

```
decision(hunk-states) =
  if count({h | state(h) = applied, h ∈ hunks}) > 0
  then "accepted"
  else "rejected"
```

**In words:** Decision is "accepted" if any hunk has state `applied`, otherwise "rejected".

### Binary Decision Space

```
D = {"accepted", "rejected"}
```

**No partial states:** LLM receives binary outcome, not granular details.

---

## Implementation

### Function: `sly-agent-q-diff-finish`

```elisp
(defun sly-agent-q-diff-finish ()
  "Exit diff review and return decision based on applied hunks."
  (interactive)
  (let ((applied-count
         (seq-count (lambda (pair) (eq (cdr pair) 'applied))
                    sly-agent-q-diff--hunk-states)))
    (setq sly-agent-q-diff--decision
          (if (> applied-count 0)
              "accepted"
            "rejected")))
  (exit-recursive-edit))
```

**Algorithm:**
1. Count hunks with `state = applied`
2. If count > 0 → "accepted"
3. If count = 0 → "rejected"
4. Exit recursive-edit (unblocks Lisp)

### Alternative Exit Points

| Command | Function | Decision Logic |
|---------|----------|----------------|
| `q` | `sly-agent-q-diff-finish` | Count applied hunks |
| `C-c C-c` | `sly-agent-q-diff-accept-all` | Always "accepted" |
| `C-c C-k` | `sly-agent-q-diff-reject-all` | Always "rejected" |

---

## Rationale

### Why "Any Applied" = Accepted?

**Reason:** User took action to modify file

**Example:**
- 5 hunks proposed
- User applies 2, rejects 3
- File is modified (2 hunks applied)
- LLM should know edits were made

**Alternative considered:** Require all hunks applied
**Rejected:** Too strict. Partial acceptance is common and valid.

### Why Binary Decision?

**Reason:** Simplifies LLM tool protocol

**Benefits:**
- LLM doesn't need to parse granular results
- Clear success/failure signal
- Matches tool execution model (tools succeed or fail)

**Trade-off:** Loses granular information (which hunks applied)

**Acceptable:** LLM can infer from file content on next read

---

## Behavior Examples

### Example 1: All Hunks Applied

```
Diff with 3 hunks:
- Hunk 1: applied
- Hunk 2: applied
- Hunk 3: applied

User presses 'q':
→ applied-count = 3
→ decision = "accepted"
```

### Example 2: Partial Application

```
Diff with 5 hunks:
- Hunk 1: applied
- Hunk 2: rejected
- Hunk 3: applied
- Hunk 4: nil (pending)
- Hunk 5: rejected

User presses 'q':
→ applied-count = 2
→ decision = "accepted"  ← Still accepted!
```

**Important:** Pending hunks (`nil`) don't count as applied.

### Example 3: Zero Hunks Applied

```
Diff with 3 hunks:
- Hunk 1: rejected
- Hunk 2: nil (pending)
- Hunk 3: rejected

User presses 'q':
→ applied-count = 0
→ decision = "rejected"
```

### Example 4: Toggle After Apply

```
Diff with 1 hunk:
- Hunk 1: applied (file modified)

User presses 'SPC' (toggle):
- Hunk 1: nil (visual state)
- File: STILL modified

User presses 'q':
→ applied-count = 0 (counts current visual state)
→ decision = "rejected"  ← Despite file being modified!
```

**Critical:** Decision based on **visual state**, not actual file state.

**Implication:** User can apply hunk, toggle to `nil`, and get "rejected" decision even though file was modified.

**Status:** Known quirk, unlikely in practice (user would need to toggle applied hunks before quitting).

---

## Verification

### Test: Accepted When Any Applied

**Test:** `sly-agent-q-diff-integration-multi-hunk` (sly-agent-q-diff-integration-test.el:18)

```elisp
(ert-deftest sly-agent-q-diff-integration-multi-hunk ()
  "Test that partial hunk application results in accepted decision."
  (let ((original "line1\nline2\nline3")
        (modified "line1\nLINE2\nline3"))  ; 1 hunk
    (with-temp-buffer
      (insert-diff original modified)
      (sly-agent-q-diff-mode)
      ;; Apply hunk
      (goto-hunk 1)
      (sly-agent-q-diff-accept-hunk)
      ;; Finish
      (sly-agent-q-diff-finish)
      ;; Verify decision
      (should (string= sly-agent-q-diff--decision "accepted")))))
```

**Status:** ✅ Pass

### Test: Rejected When None Applied

**Test:** Manual verification (no explicit test)

```elisp
(let ((original "a\nb\nc")
      (modified "a\nB\nc"))
  (with-temp-buffer
    (insert-diff original modified)
    (sly-agent-q-diff-mode)
    ;; Don't apply anything, just finish
    (sly-agent-q-diff-finish)
    ;; Should be rejected
    (assert (string= sly-agent-q-diff--decision "rejected"))))
```

**Status:** ⚠️ Not in test suite (coverage gap)

---

## Decision Message Formatting

### Lisp Side: Message Construction

```lisp
(cond
  ((string= decision "accepted")
   (format nil "✓ Changes accepted and applied to ~A~%~%~
                The buffer has been updated..."
          path))

  ((string= decision "rejected")
   (format nil "✗ REJECTED: User explicitly declined changes to ~A~%~%~
                STOP: Do NOT retry or propose alternative changes..."
          path)))
```

### "Rejected" Includes STOP Directive

**Critical:** Rejection message instructs LLM to **STOP**

**Text:**
```
STOP: Do NOT retry or propose alternative changes. The user has made a
deliberate decision to reject this edit. Report to the user that the
changes were not applied and ask if they would like to proceed differently.
```

**Purpose:** Prevent LLM retry loops

**Example Without STOP:**
```
LLM: propose_file_edit(...) → rejected
LLM: (tries slight variation) propose_file_edit(...) → rejected
LLM: (tries another variation) propose_file_edit(...) → rejected
User: frustrated, closes Emacs
```

**Example With STOP:**
```
LLM: propose_file_edit(...) → rejected + STOP directive
LLM: "The changes were rejected. Would you like me to try a different approach?"
User: "Yes, just add a comment instead"
LLM: (proceeds with different strategy)
```

---

## Consequences

### Positive

✅ **Simple binary outcome**: Easy for LLM to understand
✅ **Partial success supported**: User can accept some hunks, reject others
✅ **Clear success signal**: File modified = "accepted"
✅ **STOP prevents loops**: Rejection halts retry attempts

### Negative

⚠️ **Loses granular info**: LLM doesn't know which hunks applied
⚠️ **Visual state quirk**: Toggle after apply can cause "rejected" despite file modification
⚠️ **No hunk count**: Success message doesn't say "2/5 hunks applied"

---

## Related Properties

- **hunk-state-machine.md**: Individual hunk states aggregated here
- **irreversibility.md**: Applied hunks remain in file even if decision = "rejected" (via toggle quirk)

---

## Known Limitations

### L-001: Decision Based on Visual State

**Problem:** Decision uses current visual state, not actual file state
**Impact:** User can apply hunk, toggle to nil, get "rejected" decision
**Example:**
```
1. Apply hunk → file modified
2. Toggle to nil → visual state = pending
3. Press 'q' → decision = "rejected" (no applied hunks)
4. File remains modified despite "rejected" decision
```

**Mitigation:** Unlikely in practice (why toggle before finishing?)
**Status:** Accepted quirk

### L-002: No Granular Feedback

**Problem:** LLM doesn't know which hunks applied
**Impact:** On "accepted", LLM must re-read file to see changes
**Workaround:** LLM reads file after successful edit
**Recommendation:** Include hunk count: "✓ 3/5 hunks applied to..."

### L-003: Pending Hunks Ignored

**Problem:** Pending hunks (nil) don't affect decision
**Impact:** User could forget to decide on some hunks
**Mitigation:** Header shows "N pending" count
**Status:** Acceptable (user responsibility to review all hunks)

---

## Future Enhancements

### Enhancement 1: Granular Feedback

Include counts in success message:
```lisp
(format nil "✓ ~D/~D hunks applied to ~A"
        applied-count total-hunks path)
```

### Enhancement 2: Warn on Pending Hunks

Before finishing with pending hunks:
```elisp
(when (> pending-count 0)
  (unless (yes-or-no-p (format "%d hunks still pending. Finish anyway?" pending-count))
    (error "Cancelled")))
```

### Enhancement 3: Decision Based on File State

Track actual file modifications instead of visual state:
```elisp
(defvar-local sly-agent-q-diff--file-modified-p nil
  "T if any hunk was applied to file (doesn't reset on toggle).")

(defun sly-agent-q-diff-finish ()
  (setq sly-agent-q-diff--decision
        (if sly-agent-q-diff--file-modified-p
            "accepted"
          "rejected")))
```

**Benefit:** Eliminates toggle quirk

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2026-01-10 | Decision logic implemented | Phase 2 diff approval |
| 2026-01-17 | Property documented | Canon extraction |

---

**Property Status:** ✅ Verified (tested)
**Confidence:** 1.00
**Test Coverage:** ⚠️ Partial (accepted case tested, rejected case not explicitly tested)
