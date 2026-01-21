# Enhancement 1: Leverage Emacs Diff Infrastructure with Per-Hunk Approval

**Date**: 2025-01-31
**Status**: Research
**Category**: Diff Application / UX Improvement

## Problem Statement

Current Agent-Q diff implementation (Phase 2) uses a custom string-matching approach to apply diffs:

```elisp
;; Current approach: Manual string replacement
(let ((start-pos (string-match (regexp-quote original) current-content)))
  (delete-region (1+ start-pos) (1+ end-pos))
  (insert modified))
```

**Issues:**
- Fragile: requires exact string match, breaks on whitespace changes
- No fuzzy matching for context shifts
- Manual position calculation (off-by-one error risks)
- Reinvents 30+ years of proven infrastructure
- All-or-nothing: user must accept/reject entire diff
- Doesn't handle multiple disjoint changes well

## Research Summary

### Existing Emacs Infrastructure

Emacs has battle-tested diff application via `diff-mode.el`:

**Key Functions:**
- `diff-apply-hunk` - Apply single hunk at point (bound to `C-c C-a`)
- `diff-apply-buffer` - Apply all hunks in buffer
- `diff-hunk-next` / `diff-hunk-prev` - Navigate between hunks
- `diff-refine-hunk` - Show fine-grained character-level changes
- `diff-goto-source` - Jump to source location

**Advantages:**
- Handles whitespace variations
- Fuzzy context matching when nearby code has changed
- Automatic line number adjustment during manual edits
- Multiple hunk support
- 30+ years of edge case handling

**Sources:**
- [GNU Emacs Diff Mode Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Diff-Mode.html)
- [EmacsWiki: Applying Patches](https://www.emacswiki.org/emacs/ApplyingPatches)
- [diff-mode.el source](http://web.mit.edu/Emacs/source/emacs/lisp/diff-mode.el)

### How Other AI Assistants Handle Diffs

**gptel** (most popular Emacs LLM package):
- Uses VC conflict markers: `<<<<<<` / `======` / `>>>>>>`
- Leverages `M-x vc-resolve-conflicts` (Ediff)
- Philosophy: "combining the magic of AI with the magic of emacs"
- Source: [gptel GitHub](https://github.com/karthink/gptel)

**gptel-rewrite with inline-diff addon**:
- Shows changes inline with diff highlighting
- Move cursor into diff and press `M-a` (accept) or `M-k` (reject)
- **Per-hunk approval** - granular control
- Source: [gptel-rewrite addons](https://github.com/karthink/gptel/wiki/gptel%E2%80%90rewrite-addons)

**macher** (multi-file LLM editing, built on gptel):
- Generates standard unified diffs
- Users "apply changes using **standard diff-mode commands**"
- Explicitly delegates to proven infrastructure
- Source: [macher GitHub](https://github.com/kmontag/macher)

**aider** (terminal-based, not Emacs, but relevant):
- Uses unified diffs with fuzzy matching
- Found unified diffs make GPT-4 Turbo **3X less lazy**
- Raised benchmark scores from 20% → 61%
- Advanced strategy: content-based matching, no strict line numbers
- Sources:
  - [Aider unified diffs](https://aider.chat/docs/unified-diffs.html)
  - [Code Surgery: AI File Edits](https://fabianhertwig.com/blog/coding-assistants-file-edits/)

### Industry Best Practices

From research on AI coding assistants:

1. **Unified diffs preferred** over other formats (SEARCH/REPLACE blocks, line numbers)
2. **Fuzzy matching essential** - code changes between proposal and application
3. **Granular approval UX wins** - per-hunk beats all-or-nothing
4. **Leverage existing infrastructure** - don't reinvent diff application

## Proposed Enhancement: Per-Hunk Approval

### High-Level Design

Instead of accept/reject for entire diff, allow user to:

1. View diff with multiple hunks
2. Navigate between hunks (`n` / `p`)
3. Preview each hunk's effect
4. **Accept/reject individual hunks** (`a` / `r`)
5. Apply accepted hunks using `diff-apply-hunk`
6. Skip rejected hunks
7. Close when done

### UX Flow

```
Agent proposes changes (e.g., "Add docstring + fix typo")
  ↓
Diff buffer opens with 2 hunks:
  [Hunk 1/2] Add docstring to function FOO
  [Hunk 2/2] Fix typo in comment
  ↓
User on Hunk 1, presses 'a' (accept)
  → Hunk 1 applied, marked as [APPLIED]
  → Cursor moves to Hunk 2
  ↓
User on Hunk 2, presses 'r' (reject)
  → Hunk 2 marked as [REJECTED]
  ↓
User presses 'q' or auto-close when all hunks reviewed
  ↓
Summary: "Applied 1/2 hunks. File modified, save when ready."
```

### Keybindings

Extend current `sly-agent-q-diff-mode-map`:

```elisp
(defvar sly-agent-q-diff-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Current (all-or-nothing)
    (define-key map (kbd "C-c C-c") #'sly-agent-q-diff-accept-all)  ; Accept all
    (define-key map (kbd "C-c C-k") #'sly-agent-q-diff-reject-all)  ; Reject all

    ;; NEW: Per-hunk approval
    (define-key map (kbd "a") #'sly-agent-q-diff-accept-hunk)       ; Accept current hunk
    (define-key map (kbd "r") #'sly-agent-q-diff-reject-hunk)       ; Reject current hunk
    (define-key map (kbd "n") #'diff-hunk-next)                     ; Next hunk (built-in)
    (define-key map (kbd "p") #'diff-hunk-prev)                     ; Previous hunk (built-in)
    (define-key map (kbd "SPC") #'sly-agent-q-diff-toggle-hunk)     ; Toggle accept/reject
    (define-key map (kbd "RET") #'sly-agent-q-diff-preview-hunk)    ; Show where this applies
    (define-key map (kbd "q") #'sly-agent-q-diff-finish)            ; Done reviewing
    map))
```

### Implementation Approach

#### Track Hunk State

```elisp
(defvar-local sly-agent-q-diff--hunk-states nil
  "Alist mapping hunk positions to states.
   Each entry: (HUNK-START . STATE)
   STATE: 'pending | 'accepted | 'rejected | 'applied")
```

#### Core Functions

**1. Accept Single Hunk**

```elisp
(defun sly-agent-q-diff-accept-hunk ()
  "Accept and apply the current hunk."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (let ((hunk-start (save-excursion
                        (diff-beginning-of-hunk)
                        (point))))

      ;; Apply using built-in diff-mode function
      (condition-case err
          (progn
            (diff-apply-hunk)
            ;; Mark as applied
            (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
                  'applied)
            ;; Visual feedback: overlay/face change
            (sly-agent-q-diff--mark-hunk-applied hunk-start)
            (message "✓ Hunk applied"))
        (error
         (message "✗ Failed to apply hunk: %s" (error-message-string err))
         (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
               'error)))))

  ;; Move to next hunk
  (diff-hunk-next))
```

**2. Reject Single Hunk**

```elisp
(defun sly-agent-q-diff-reject-hunk ()
  "Reject the current hunk (mark as skipped)."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (let ((hunk-start (save-excursion
                        (diff-beginning-of-hunk)
                        (point))))
      (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
            'rejected)
      (sly-agent-q-diff--mark-hunk-rejected hunk-start)
      (message "✗ Hunk rejected")))

  ;; Move to next hunk
  (diff-hunk-next))
```

**3. Finish Review**

```elisp
(defun sly-agent-q-diff-finish ()
  "Finish reviewing hunks and close diff buffer."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  ;; Check if there are unreviewed hunks
  (let* ((all-hunks (sly-agent-q-diff--count-hunks))
         (reviewed-hunks (length sly-agent-q-diff--hunk-states))
         (applied (cl-count 'applied sly-agent-q-diff--hunk-states :key #'cdr))
         (rejected (cl-count 'rejected sly-agent-q-diff--hunk-states :key #'cdr)))

    (when (< reviewed-hunks all-hunks)
      (unless (yes-or-no-p
               (format "%d unreviewed hunk(s) remaining. Finish anyway? "
                       (- all-hunks reviewed-hunks)))
        (user-error "Continue reviewing")))

    ;; Set decision for Lisp side
    (setq sly-agent-q-diff--decision
          (if (> applied 0) 'accepted 'rejected))

    (message "Review complete: %d applied, %d rejected" applied rejected)

    ;; Exit recursive edit and close buffer
    (let ((diff-buf (current-buffer)))
      (exit-recursive-edit)
      (when (buffer-live-p diff-buf)
        (kill-buffer diff-buf)))))
```

**4. Visual Feedback**

```elisp
(defface sly-agent-q-diff-applied-face
  '((t :background "#2d4f2d" :foreground "#a0ffa0"))
  "Face for applied hunks."
  :group 'sly-agent-q-diff)

(defface sly-agent-q-diff-rejected-face
  '((t :background "#4f2d2d" :foreground "#ffa0a0"))
  "Face for rejected hunks."
  :group 'sly-agent-q-diff)

(defun sly-agent-q-diff--mark-hunk-applied (hunk-start)
  "Add overlay marking HUNK-START as applied."
  (save-excursion
    (goto-char hunk-start)
    (let* ((hunk-end (save-excursion (diff-end-of-hunk) (point)))
           (ov (make-overlay hunk-start hunk-end)))
      (overlay-put ov 'face 'sly-agent-q-diff-applied-face)
      (overlay-put ov 'sly-agent-q-hunk-state 'applied)
      (overlay-put ov 'before-string
                   (propertize "[✓ APPLIED] " 'face 'success)))))

(defun sly-agent-q-diff--mark-hunk-rejected (hunk-start)
  "Add overlay marking HUNK-START as rejected."
  (save-excursion
    (goto-char hunk-start)
    (let* ((hunk-end (save-excursion (diff-end-of-hunk) (point)))
           (ov (make-overlay hunk-start hunk-end)))
      (overlay-put ov 'face 'sly-agent-q-diff-rejected-face)
      (overlay-put ov 'sly-agent-q-hunk-state 'rejected)
      (overlay-put ov 'before-string
                   (propertize "[✗ REJECTED] " 'face 'error)))))
```

**5. Helper: Count Hunks**

```elisp
(defun sly-agent-q-diff--count-hunks ()
  "Return the number of hunks in current diff buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (ignore-errors (diff-hunk-next) t)
        (cl-incf count))
      count)))
```

#### Update Diff Header

Show hunk progress in header:

```elisp
(defun sly-agent-q-diff--update-header ()
  "Update diff header with hunk progress."
  (let* ((total (sly-agent-q-diff--count-hunks))
         (applied (cl-count 'applied sly-agent-q-diff--hunk-states :key #'cdr))
         (rejected (cl-count 'rejected sly-agent-q-diff--hunk-states :key #'cdr))
         (pending (- total applied rejected)))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "C-c C-c = ACCEPT" nil t)
        (let ((inhibit-read-only t))
          (beginning-of-line)
          (kill-line)
          (insert (propertize
                   (format "Progress: %d/%d applied, %d rejected, %d pending  [a]ccept [r]eject [n]ext [q]uit\n"
                           applied total rejected pending)
                   'face 'sly-agent-q-diff-keybinding)))))))
```

### Compatibility: Keep All-or-Nothing Fallback

For simple single-hunk diffs, the current `C-c C-c` / `C-c C-k` workflow still works:

```elisp
(defun sly-agent-q-diff-accept-all ()
  "Accept all hunks at once (original behavior)."
  (interactive)
  (goto-char (point-min))
  (condition-case err
      (progn
        (diff-apply-buffer)  ; Built-in: apply all hunks
        (message "✓ All hunks applied")
        (setq sly-agent-q-diff--decision 'accepted)
        (exit-recursive-edit)
        (kill-buffer))
    (error
     (message "✗ Error applying diff: %s" (error-message-string err))
     (setq sly-agent-q-diff--decision 'rejected))))
```

## Benefits

### 1. **Granular Control**
User can accept good changes, reject questionable ones from same LLM response.

**Example**: Agent proposes:
- Hunk 1: Add docstring ✓ (accept)
- Hunk 2: Rename variable to `new-impl` ✗ (reject - bad name)
- Hunk 3: Add type declaration ✓ (accept)

Result: 2/3 changes applied, user avoids bad naming while keeping good changes.

### 2. **Robust Infrastructure**
Leverage 30+ years of `diff-mode.el` edge case handling:
- Whitespace variations
- Nearby code changes
- Fuzzy context matching
- Line number adjustments

### 3. **Standard Emacs UX**
Familiar keybindings (`n`/`p` for navigation) and concepts (hunks, overlays, diff-mode).

### 4. **Better Feedback Loop**
User can reject specific changes → inform agent → agent refines just those hunks.

### 5. **Code Reduction**
Replace ~40 lines of fragile custom string-matching with ~10 lines calling `diff-apply-hunk`.

## Tradeoffs

### Pros
- ✅ Robust (fuzzy matching, whitespace handling)
- ✅ Granular control (per-hunk approval)
- ✅ Standard Emacs UX
- ✅ Less code to maintain
- ✅ Proven infrastructure (30+ years)
- ✅ Handles multi-hunk diffs naturally

### Cons
- ❌ Slightly more complex UX (more keybindings to learn)
- ❌ Requires understanding hunk concept
- ❌ Can't easily "edit" a hunk before applying (would need manual diff editing)
- ❌ Initial implementation effort (state tracking, overlays)

### Mitigation
- Provide clear on-screen instructions (`[a]ccept [r]eject [n]ext [q]uit`)
- Keep `C-c C-c` for "accept all" as quick option
- Add tutorial in QUICKSTART.md
- Consider `?` key for help overlay

## Implementation Phases

### Phase 1: Foundation (Use Built-in Functions)
- Replace manual string matching with `diff-apply-hunk`
- Keep all-or-nothing UX initially
- Validate that built-in functions work for our use cases
- **Estimated effort**: 2-3 hours

### Phase 2: Per-Hunk Approval
- Add hunk state tracking
- Implement `accept-hunk` / `reject-hunk` / `finish`
- Add keybindings
- **Estimated effort**: 4-6 hours

### Phase 3: Visual Feedback
- Overlays for applied/rejected hunks
- Header progress display
- Highlight current hunk
- **Estimated effort**: 2-3 hours

### Phase 4: Polish
- Help overlay (`?` key)
- Preview functionality (show where hunk applies)
- Undo last hunk action
- Statistics in status line
- **Estimated effort**: 3-4 hours

**Total estimated effort**: 11-16 hours

## Open Questions

1. **Multi-file diffs**: How to handle when agent proposes changes to multiple files in one diff?
   - Option A: One diff buffer per file
   - Option B: Multi-file diff buffer, show filename headers
   - Recommendation: Start with A (simpler), evaluate B later

2. **Conflict handling**: What if `diff-apply-hunk` fails (e.g., context changed)?
   - Show error message
   - Mark hunk as 'error state
   - Allow user to manually edit diff and retry
   - Provide "show conflict" command

3. **Partial hunk application**: Can user accept only part of a hunk?
   - Not in initial version (complex to implement)
   - Users can reject hunk → ask agent to split into smaller changes
   - Future: investigate `diff-split-hunk` for manual editing

4. **Integration with LLM feedback**: Should agent see which hunks were rejected?
   - Yes! Send back to LLM: "Hunks 1,3 accepted, hunk 2 rejected (reason: X)"
   - Agent can refine just the rejected changes
   - Requires conversation history enhancement

5. **Diff format**: Continue using unified diff? Consider other formats?
   - Stick with unified diff (industry standard, what LLMs know)
   - Aider research shows unified diffs reduce LLM "laziness"
   - Compatible with all built-in Emacs tooling

## Testing Strategy

### Unit Tests (ERT)
```elisp
(ert-deftest sly-agent-q-diff-test-single-hunk ()
  "Test applying a single-hunk diff."
  (with-temp-buffer
    (insert original-content)
    (let ((diff-buffer (sly-agent-q--generate-diff-buffer
                        original modified "Test change")))
      (with-current-buffer diff-buffer
        (goto-char (point-min))
        (diff-hunk-next)
        (sly-agent-q-diff-accept-hunk)
        (should (eq (alist-get ... sly-agent-q-diff--hunk-states) 'applied))))))
```

### Integration Tests
1. Generate diff with 3 hunks
2. Accept hunk 1
3. Reject hunk 2
4. Accept hunk 3
5. Verify: hunks 1 and 3 applied to target file, hunk 2 not applied

### Manual Testing Scenarios
1. **Simple function docstring**: 1 hunk, accept
2. **Refactoring with concerns**: 3 hunks, accept 2, reject 1
3. **Whitespace mismatch**: Apply diff when target has different indentation
4. **Context changed**: Apply diff after user made nearby edits
5. **Multi-hunk ordering**: Accept hunks out of order (3, 1, 2)

## Documentation Updates

### QUICKSTART.md
Add section:
```markdown
### Reviewing Diffs Per-Hunk

When Agent-Q proposes changes, you can review each change individually:

1. Diff buffer opens with all proposed changes
2. Press `n` to go to next hunk, `p` for previous
3. Press `a` to accept current hunk (applies it immediately)
4. Press `r` to reject current hunk (skip it)
5. Press `q` when done reviewing

Quick accept all: `C-c C-c` (same as before)
Quick reject all: `C-c C-k`

Hunks are color-coded:
- Green background = applied ✓
- Red background = rejected ✗
- Normal = pending review
```

### User Guide
Document:
- Hunk concept explanation
- Keybinding reference table
- Example workflows
- Troubleshooting (what if apply fails?)

## References

- [GNU Emacs Diff Mode Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Diff-Mode.html)
- [EmacsWiki: Applying Patches](https://www.emacswiki.org/emacs/ApplyingPatches)
- [diff-mode.el source](http://web.mit.edu/Emacs/source/emacs/lisp/diff-mode.el)
- [gptel: LLM client for Emacs](https://github.com/karthink/gptel)
- [gptel-rewrite addons (inline-diff)](https://github.com/karthink/gptel/wiki/gptel%E2%80%90rewrite-addons)
- [macher: multi-file LLM editing](https://github.com/kmontag/macher)
- [Aider: unified diffs make GPT-4 3X less lazy](https://aider.chat/docs/unified-diffs.html)
- [Code Surgery: How AI Assistants Edit Files](https://fabianhertwig.com/blog/coding-assistants-file-edits/)

## Next Steps

1. Create additional enhancement proposals for research
2. Review all enhancements together
3. Prioritize based on:
   - User value
   - Implementation effort
   - Risk
   - Dependencies
4. Create implementation plan
5. Execute in phases

---

**End of Enhancement 1**
