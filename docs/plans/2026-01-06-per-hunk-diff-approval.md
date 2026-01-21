# Per-Hunk Diff Approval Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace custom string-matching diff application with Emacs's `diff-mode.el` infrastructure and add per-hunk approval for granular control.

**Architecture:** Leverage built-in `diff-apply-hunk` and `diff-apply-buffer` functions from `diff-mode.el` instead of manual string replacement. Track hunk state (pending/accepted/rejected/applied) with buffer-local alist. Use overlays for visual feedback. Maintain backward compatibility with `C-c C-c` all-or-nothing workflow.

**Tech Stack:** Emacs Lisp 27.1+, diff-mode.el (built-in), ERT (Emacs Regression Testing)

---

## Task 1: Setup ERT Test Infrastructure

**Files:**
- Create: `contrib/sly-agent-q/test/test-helper.el`
- Create: `contrib/sly-agent-q/test/sly-agent-q-diff-test.el`

**Step 1: Create test helper file**

Create `contrib/sly-agent-q/test/test-helper.el`:

```elisp
;;; test-helper.el --- Test helper for sly-agent-q -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for testing sly-agent-q

;;; Code:

(require 'ert)

(defvar sly-agent-q-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing tests.")

(defvar sly-agent-q-root-dir
  (file-name-directory (directory-file-name sly-agent-q-test-dir))
  "Root directory of sly-agent-q.")

;; Add parent directory to load path
(add-to-list 'load-path sly-agent-q-root-dir)

(require 'sly-agent-q-diff)

(provide 'test-helper)
;;; test-helper.el ends here
```

**Step 2: Create initial test file**

Create `contrib/sly-agent-q/test/sly-agent-q-diff-test.el`:

```elisp
;;; sly-agent-q-diff-test.el --- Tests for diff functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for sly-agent-q-diff.el

;;; Code:

(require 'test-helper)

(ert-deftest sly-agent-q-diff-test-mode-derived ()
  "sly-agent-q-diff-mode should derive from diff-mode."
  (with-temp-buffer
    (sly-agent-q-diff-mode)
    (should (derived-mode-p 'diff-mode))))

(provide 'sly-agent-q-diff-test)
;;; sly-agent-q-diff-test.el ends here
```

**Step 3: Run tests to verify setup**

Run from command line:
```bash
cd /Users/quasi/quasilabs/projects/agent-q/contrib/sly-agent-q
emacs -batch -l test/test-helper.el -l test/sly-agent-q-diff-test.el -f ert-run-tests-batch-and-exit
```

Expected output:
```
Running 1 tests...
   passed  1/1  sly-agent-q-diff-test-mode-derived

Ran 1 tests, 1 results as expected (2026-01-06...)
```

**Step 4: Commit test infrastructure**

```bash
git add contrib/sly-agent-q/test/
git commit -m "test: add ERT test infrastructure for diff mode"
```

---

## Task 2: Replace String Matching with diff-apply-hunk (Phase 1 Foundation)

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el:173-224`
- Test: `contrib/sly-agent-q/test/sly-agent-q-diff-test.el`

**Step 1: Write failing test for diff-apply-hunk acceptance**

Add to `contrib/sly-agent-q/test/sly-agent-q-diff-test.el`:

```elisp
(ert-deftest sly-agent-q-diff-test-apply-single-hunk ()
  "Accepting diff should use diff-apply-hunk to apply changes."
  (let* ((test-file (make-temp-file "agent-q-test-" nil ".lisp"))
         (original ";;;; test.lisp\n(defun foo ()\n  42)")
         (modified ";;;; test.lisp\n(defun foo ()\n  \"Returns 42.\"\n  42)")
         (diff-text (concat "--- a/test.lisp\n"
                           "+++ b/test.lisp\n"
                           "@@ -1,2 +1,3 @@\n"
                           " ;;;; test.lisp\n"
                           " (defun foo ()\n"
                           "+  \"Returns 42.\"\n"
                           "   42)\n")))
    (unwind-protect
        (progn
          ;; Write original to file
          (with-temp-file test-file
            (insert original))

          ;; Create diff buffer
          (with-temp-buffer
            (insert diff-text)
            (sly-agent-q-diff-mode)
            (setq sly-agent-q-diff--path test-file
                  sly-agent-q-diff--original original
                  sly-agent-q-diff--modified modified)
            (goto-char (point-min))

            ;; Find and apply hunk
            (re-search-forward "^@@")
            (beginning-of-line)
            (diff-apply-hunk)

            ;; Verify file was modified
            (with-temp-buffer
              (insert-file-contents test-file)
              (should (string-match-p "Returns 42" (buffer-string))))))

      ;; Cleanup
      (when (file-exists-p test-file)
        (delete-file test-file)))))
```

**Step 2: Run test to verify it fails**

```bash
cd /Users/quasi/quasilabs/projects/agent-q/contrib/sly-agent-q
emacs -batch -l test/test-helper.el -l test/sly-agent-q-diff-test.el -f ert-run-tests-batch-and-exit
```

Expected: FAIL (diff-apply-hunk not yet integrated)

**Step 3: Refactor sly-agent-q-diff-accept to use diff-apply-hunk**

Replace `sly-agent-q-diff-accept` function in `sly-agent-q-diff.el` (lines 173-223):

```elisp
(defun sly-agent-q-diff-accept ()
  "Accept the proposed changes and apply them using diff-mode infrastructure."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (let ((path sly-agent-q-diff--path))
    (unless path
      (user-error "Missing file path - cannot apply changes"))

    ;; Ensure target file exists and is visited
    (unless (file-exists-p path)
      (user-error "File does not exist: %s" path))

    (let ((file-buffer (find-file-noselect path)))
      ;; Apply all hunks in diff buffer using built-in diff-apply-buffer
      (condition-case err
          (progn
            (diff-apply-buffer)  ; Built-in function from diff-mode.el
            (message "✓ All changes applied to %s. Save when ready." path))
        (error
         (message "✗ Error applying diff: %s" (error-message-string err))
         (user-error "Failed to apply changes: %s" (error-message-string err))))))

  ;; Set decision, exit recursive edit, and close the diff buffer
  (setq sly-agent-q-diff--decision 'accepted)
  (let ((diff-buf (current-buffer)))
    (exit-recursive-edit)
    (when (buffer-live-p diff-buf)
      (kill-buffer diff-buf))))
```

**Step 4: Run test to verify it passes**

```bash
cd /Users/quasi/quasilabs/projects/agent-q/contrib/sly-agent-q
emacs -batch -l test/test-helper.el -l test/sly-agent-q-diff-test.el -f ert-run-tests-batch-and-exit
```

Expected: All tests PASS

**Step 5: Commit refactored implementation**

```bash
git add contrib/sly-agent-q/sly-agent-q-diff.el contrib/sly-agent-q/test/sly-agent-q-diff-test.el
git commit -m "refactor: replace string matching with diff-apply-buffer

Replace fragile manual string matching with built-in diff-mode
infrastructure. Maintains all-or-nothing UX while fixing edge
cases with whitespace and context changes."
```

---

## Task 3: Add Hunk State Tracking Infrastructure (Phase 2 Foundation)

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el:44-60`
- Test: `contrib/sly-agent-q/test/sly-agent-q-diff-test.el`

**Step 1: Write test for hunk counting**

Add to `contrib/sly-agent-q/test/sly-agent-q-diff-test.el`:

```elisp
(ert-deftest sly-agent-q-diff-test-count-hunks ()
  "Should correctly count hunks in multi-hunk diff."
  (with-temp-buffer
    (insert "--- a/test.lisp\n"
            "+++ b/test.lisp\n"
            "@@ -1,2 +1,3 @@\n"
            " line1\n"
            "+line2\n"
            " line3\n"
            "@@ -10,1 +11,2 @@\n"
            " line10\n"
            "+line11\n"
            "@@ -20,1 +22,1 @@\n"
            "-old\n"
            "+new\n")
    (sly-agent-q-diff-mode)
    (should (= 3 (sly-agent-q-diff--count-hunks)))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL (function not defined)

**Step 3: Add hunk state tracking variables**

Add after line 59 in `sly-agent-q-diff.el`:

```elisp
(defvar-local sly-agent-q-diff--hunk-states nil
  "Alist mapping hunk positions to states.
Each entry: (HUNK-START . STATE)
STATE: 'pending | 'accepted | 'rejected | 'applied")
```

**Step 4: Implement hunk counting function**

Add after the state variables in `sly-agent-q-diff.el`:

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

**Step 5: Run test to verify it passes**

Expected: All tests PASS

**Step 6: Write test for hunk state initialization**

Add to test file:

```elisp
(ert-deftest sly-agent-q-diff-test-hunk-state-init ()
  "Hunk states should initialize as empty alist."
  (with-temp-buffer
    (sly-agent-q-diff-mode)
    (should (null sly-agent-q-diff--hunk-states))))
```

**Step 7: Run tests**

Expected: All tests PASS (variable already defined)

**Step 8: Commit hunk state tracking**

```bash
git add contrib/sly-agent-q/sly-agent-q-diff.el contrib/sly-agent-q/test/sly-agent-q-diff-test.el
git commit -m "feat: add hunk state tracking infrastructure

Add buffer-local variable to track per-hunk approval state
and utility function to count hunks in diff buffer."
```

---

## Task 4: Implement Accept/Reject Single Hunk Commands (Phase 2 Core)

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el:171+`
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el:64-69` (keymap)
- Test: `contrib/sly-agent-q/test/sly-agent-q-diff-test.el`

**Step 1: Write test for accepting single hunk**

Add to test file:

```elisp
(ert-deftest sly-agent-q-diff-test-accept-single-hunk ()
  "Accepting single hunk should apply it and mark as 'applied."
  (let* ((test-file (make-temp-file "agent-q-test-" nil ".lisp"))
         (diff-text (concat "--- a/test.lisp\n"
                           "+++ b/test.lisp\n"
                           "@@ -1,1 +1,2 @@\n"
                           " line1\n"
                           "+line2\n")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "line1\n"))

          (with-temp-buffer
            (insert diff-text)
            (sly-agent-q-diff-mode)
            (setq sly-agent-q-diff--path test-file)
            (goto-char (point-min))
            (re-search-forward "^@@")
            (beginning-of-line)

            (let ((hunk-start (point)))
              (sly-agent-q-diff-accept-hunk)

              ;; Check state was recorded
              (should (eq 'applied (alist-get hunk-start sly-agent-q-diff--hunk-states)))

              ;; Check file was modified
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string-match-p "line2" (buffer-string)))))))

      (when (file-exists-p test-file)
        (delete-file test-file)))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL (function not defined)

**Step 3: Implement sly-agent-q-diff-accept-hunk**

Add after `sly-agent-q-diff-reject` in `sly-agent-q-diff.el`:

```elisp
(defun sly-agent-q-diff-accept-hunk ()
  "Accept and apply the current hunk."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (diff-beginning-of-hunk)
    (let ((hunk-start (point)))
      ;; Apply using built-in diff-mode function
      (condition-case err
          (progn
            (diff-apply-hunk)
            ;; Mark as applied
            (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
                  'applied)
            (message "✓ Hunk applied"))
        (error
         (message "✗ Failed to apply hunk: %s" (error-message-string err))
         (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
               'error)))))

  ;; Move to next hunk
  (condition-case nil
      (diff-hunk-next)
    (error nil)))  ; No more hunks
```

**Step 4: Run test to verify it passes**

Expected: All tests PASS

**Step 5: Write test for rejecting single hunk**

Add to test file:

```elisp
(ert-deftest sly-agent-q-diff-test-reject-single-hunk ()
  "Rejecting single hunk should mark it as 'rejected without applying."
  (with-temp-buffer
    (insert "--- a/test.lisp\n"
            "+++ b/test.lisp\n"
            "@@ -1,1 +1,2 @@\n"
            " line1\n"
            "+line2\n")
    (sly-agent-q-diff-mode)
    (goto-char (point-min))
    (re-search-forward "^@@")
    (beginning-of-line)

    (let ((hunk-start (point)))
      (sly-agent-q-diff-reject-hunk)

      ;; Check state was recorded
      (should (eq 'rejected (alist-get hunk-start sly-agent-q-diff--hunk-states))))))
```

**Step 6: Run test to verify it fails**

Expected: FAIL (function not defined)

**Step 7: Implement sly-agent-q-diff-reject-hunk**

Add after `sly-agent-q-diff-accept-hunk`:

```elisp
(defun sly-agent-q-diff-reject-hunk ()
  "Reject the current hunk (mark as skipped)."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (diff-beginning-of-hunk)
    (let ((hunk-start (point)))
      (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
            'rejected)
      (message "✗ Hunk rejected")))

  ;; Move to next hunk
  (condition-case nil
      (diff-hunk-next)
    (error nil)))
```

**Step 8: Run tests to verify they pass**

Expected: All tests PASS

**Step 9: Add keybindings**

Modify `sly-agent-q-diff-mode-map` (around line 64-69):

```elisp
(defvar sly-agent-q-diff-mode-map
  (let ((map (make-sparse-keymap)))
    ;; All-or-nothing (backward compatible)
    (define-key map (kbd "C-c C-c") #'sly-agent-q-diff-accept)
    (define-key map (kbd "C-c C-k") #'sly-agent-q-diff-reject)

    ;; Per-hunk approval (new)
    (define-key map (kbd "a") #'sly-agent-q-diff-accept-hunk)
    (define-key map (kbd "r") #'sly-agent-q-diff-reject-hunk)
    (define-key map (kbd "n") #'diff-hunk-next)
    (define-key map (kbd "p") #'diff-hunk-prev)
    (define-key map (kbd "q") #'sly-agent-q-diff-finish)
    map)
  "Keymap for `sly-agent-q-diff-mode'.")
```

**Step 10: Commit per-hunk commands**

```bash
git add contrib/sly-agent-q/sly-agent-q-diff.el contrib/sly-agent-q/test/sly-agent-q-diff-test.el
git commit -m "feat: add per-hunk accept/reject commands

Implement sly-agent-q-diff-accept-hunk and reject-hunk
with state tracking. Add keybindings: a (accept), r (reject),
n (next), p (previous)."
```

---

## Task 5: Implement Finish Review Command (Phase 2 Core)

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el`
- Test: `contrib/sly-agent-q/test/sly-agent-q-diff-test.el`

**Step 1: Write test for finish command**

Add to test file:

```elisp
(ert-deftest sly-agent-q-diff-test-finish-review ()
  "Finishing review should check for unreviewed hunks."
  (with-temp-buffer
    (insert "--- a/test.lisp\n"
            "+++ b/test.lisp\n"
            "@@ -1,1 +1,2 @@\n"
            " line1\n"
            "+line2\n"
            "@@ -5,1 +6,2 @@\n"
            " line5\n"
            "+line6\n")
    (sly-agent-q-diff-mode)

    ;; Accept first hunk only
    (goto-char (point-min))
    (re-search-forward "^@@")
    (beginning-of-line)
    (let ((hunk-start (point)))
      (setf (alist-get hunk-start sly-agent-q-diff--hunk-states) 'applied))

    ;; Should detect 1 unreviewed hunk
    (let ((all-hunks (sly-agent-q-diff--count-hunks))
          (reviewed (length sly-agent-q-diff--hunk-states)))
      (should (= 2 all-hunks))
      (should (= 1 reviewed)))))
```

**Step 2: Run test to verify it passes**

Expected: PASS (just verifying logic)

**Step 3: Implement sly-agent-q-diff-finish**

Add after `sly-agent-q-diff-reject-hunk`:

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

**Step 4: Add require for cl-lib at top of file**

Add after `(require 'diff-mode)` in `sly-agent-q-diff.el`:

```elisp
(require 'cl-lib)
```

**Step 5: Test manually (interactive test)**

Manual test:
1. Open Emacs
2. Eval buffer `sly-agent-q-diff.el`
3. Create test diff with 2 hunks
4. Accept 1 hunk with `a`
5. Press `q` to finish
6. Should see summary: "Review complete: 1 applied, 0 rejected"

**Step 6: Commit finish command**

```bash
git add contrib/sly-agent-q/sly-agent-q-diff.el contrib/sly-agent-q/test/sly-agent-q-diff-test.el
git commit -m "feat: add finish review command

Implement sly-agent-q-diff-finish to complete review session
with summary of applied/rejected hunks. Warns about unreviewed
hunks before closing."
```

---

## Task 6: Add Visual Feedback with Overlays (Phase 3)

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el:34-42` (faces)
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el` (overlay functions)

**Step 1: Add custom faces for hunk states**

Add after existing faces in `sly-agent-q-diff.el`:

```elisp
(defface sly-agent-q-diff-applied-face
  '((t :background "#2d4f2d" :foreground "#a0ffa0"))
  "Face for applied hunks."
  :group 'sly-agent-q-diff)

(defface sly-agent-q-diff-rejected-face
  '((t :background "#4f2d2d" :foreground "#ffa0a0"))
  "Face for rejected hunks."
  :group 'sly-agent-q-diff)

(defface sly-agent-q-diff-pending-face
  '((t :inherit diff-hunk-header))
  "Face for pending hunks (not yet reviewed)."
  :group 'sly-agent-q-diff)
```

**Step 2: Implement overlay marking functions**

Add after `sly-agent-q-diff--count-hunks`:

```elisp
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

(defun sly-agent-q-diff--clear-hunk-overlays ()
  "Remove all hunk state overlays from buffer."
  (remove-overlays (point-min) (point-max) 'sly-agent-q-hunk-state))
```

**Step 3: Update accept-hunk to add overlay**

Modify `sly-agent-q-diff-accept-hunk` to call overlay function:

```elisp
(defun sly-agent-q-diff-accept-hunk ()
  "Accept and apply the current hunk."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (diff-beginning-of-hunk)
    (let ((hunk-start (point)))
      ;; Apply using built-in diff-mode function
      (condition-case err
          (progn
            (diff-apply-hunk)
            ;; Mark as applied
            (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
                  'applied)
            ;; Visual feedback
            (sly-agent-q-diff--mark-hunk-applied hunk-start)
            (message "✓ Hunk applied"))
        (error
         (message "✗ Failed to apply hunk: %s" (error-message-string err))
         (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
               'error)))))

  ;; Move to next hunk
  (condition-case nil
      (diff-hunk-next)
    (error nil)))
```

**Step 4: Update reject-hunk to add overlay**

Modify `sly-agent-q-diff-reject-hunk`:

```elisp
(defun sly-agent-q-diff-reject-hunk ()
  "Reject the current hunk (mark as skipped)."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (diff-beginning-of-hunk)
    (let ((hunk-start (point)))
      (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
            'rejected)
      ;; Visual feedback
      (sly-agent-q-diff--mark-hunk-rejected hunk-start)
      (message "✗ Hunk rejected")))

  ;; Move to next hunk
  (condition-case nil
      (diff-hunk-next)
    (error nil)))
```

**Step 5: Clear overlays on buffer kill**

Add cleanup hook to mode definition:

```elisp
(define-derived-mode sly-agent-q-diff-mode diff-mode "Agent-Q-Diff"
  "Major mode for reviewing Agent-Q proposed file changes.

\\{sly-agent-q-diff-mode-map}"
  :group 'sly-agent-q-diff
  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (add-hook 'kill-buffer-hook #'sly-agent-q-diff--clear-hunk-overlays nil t))
```

**Step 6: Manual test**

Manual test:
1. Reload `sly-agent-q-diff.el`
2. Create multi-hunk diff
3. Accept one hunk - should see green background and ✓ marker
4. Reject one hunk - should see red background and ✗ marker

**Step 7: Commit visual feedback**

```bash
git add contrib/sly-agent-q/sly-agent-q-diff.el
git commit -m "feat: add visual feedback with overlays

Add color-coded overlays and status markers for accepted
(green ✓) and rejected (red ✗) hunks. Clear overlays on
buffer cleanup."
```

---

## Task 7: Add Header Progress Display (Phase 3)

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el`

**Step 1: Implement header update function**

Add after overlay functions:

```elisp
(defun sly-agent-q-diff--update-header ()
  "Update diff header with hunk progress."
  (let* ((total (sly-agent-q-diff--count-hunks))
         (applied (cl-count 'applied sly-agent-q-diff--hunk-states :key #'cdr))
         (rejected (cl-count 'rejected sly-agent-q-diff--hunk-states :key #'cdr))
         (pending (- total applied rejected)))
    (save-excursion
      (goto-char (point-min))
      ;; Find the keybinding line
      (when (re-search-forward "^C-c C-c = ACCEPT" nil t)
        (let ((inhibit-read-only t))
          (beginning-of-line)
          (delete-region (point) (line-end-position))
          (insert (propertize
                   (format "Progress: %d/%d applied, %d rejected, %d pending  |  [a]ccept [r]eject [n]ext [p]rev [q]uit"
                           applied total rejected pending)
                   'face 'sly-agent-q-diff-keybinding)))))))
```

**Step 2: Call header update after hunk actions**

Modify `sly-agent-q-diff-accept-hunk` to update header:

```elisp
(defun sly-agent-q-diff-accept-hunk ()
  "Accept and apply the current hunk."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (diff-beginning-of-hunk)
    (let ((hunk-start (point)))
      (condition-case err
          (progn
            (diff-apply-hunk)
            (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
                  'applied)
            (sly-agent-q-diff--mark-hunk-applied hunk-start)
            (sly-agent-q-diff--update-header)  ; NEW
            (message "✓ Hunk applied"))
        (error
         (message "✗ Failed to apply hunk: %s" (error-message-string err))
         (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
               'error)))))

  (condition-case nil
      (diff-hunk-next)
    (error nil)))
```

**Step 3: Call header update in reject-hunk**

Modify `sly-agent-q-diff-reject-hunk`:

```elisp
(defun sly-agent-q-diff-reject-hunk ()
  "Reject the current hunk (mark as skipped)."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (diff-beginning-of-hunk)
    (let ((hunk-start (point)))
      (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
            'rejected)
      (sly-agent-q-diff--mark-hunk-rejected hunk-start)
      (sly-agent-q-diff--update-header)  ; NEW
      (message "✗ Hunk rejected")))

  (condition-case nil
      (diff-hunk-next)
    (error nil)))
```

**Step 4: Initialize header on diff display**

Modify `sly-agent-q-show-diff-and-wait` to show initial progress:

Find the line that inserts keybinding help (around line 107) and modify:

```elisp
        ;; Insert header with initial progress
        (let ((total-hunks (save-excursion
                            (goto-char (point-min))
                            (sly-agent-q-diff--count-hunks))))
          (insert (propertize
                   (format "Progress: 0/%d applied, 0 rejected, %d pending  |  [a]ccept [r]eject [n]ext [p]rev [q]uit\n\n"
                           total-hunks total-hunks)
                   'face 'sly-agent-q-diff-keybinding)))
```

**Step 5: Manual test**

Manual test - verify header updates as you accept/reject hunks

**Step 6: Commit header progress**

```bash
git add contrib/sly-agent-q/sly-agent-q-diff.el
git commit -m "feat: add header progress display

Show real-time progress (applied/rejected/pending) in diff
header. Updates after each hunk action."
```

---

## Task 8: Add Toggle and Preview Commands (Phase 4)

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el`
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el:64-69` (keymap)

**Step 1: Implement toggle command**

Add after `sly-agent-q-diff-finish`:

```elisp
(defun sly-agent-q-diff-toggle-hunk ()
  "Toggle current hunk between accepted and rejected."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (diff-beginning-of-hunk)
    (let* ((hunk-start (point))
           (current-state (alist-get hunk-start sly-agent-q-diff--hunk-states)))
      (cond
       ((eq current-state 'applied)
        ;; Can't unapply, so just mark as pending
        (setf (alist-get hunk-start sly-agent-q-diff--hunk-states) nil)
        (sly-agent-q-diff--clear-hunk-overlays)
        (message "Hunk marked as pending (cannot unapply)"))

       ((eq current-state 'rejected)
        ;; Try to apply
        (sly-agent-q-diff-accept-hunk))

       (t
        ;; Pending - accept by default
        (sly-agent-q-diff-accept-hunk))))))
```

**Step 2: Implement preview command**

Add after toggle:

```elisp
(defun sly-agent-q-diff-preview-hunk ()
  "Jump to source location where current hunk would apply."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (let ((path sly-agent-q-diff--path))
    (unless path
      (user-error "No file path set"))

    (save-excursion
      (diff-beginning-of-hunk)
      ;; Use built-in diff-goto-source to jump to location
      (diff-goto-source))))
```

**Step 3: Add keybindings**

Update keymap:

```elisp
(defvar sly-agent-q-diff-mode-map
  (let ((map (make-sparse-keymap)))
    ;; All-or-nothing (backward compatible)
    (define-key map (kbd "C-c C-c") #'sly-agent-q-diff-accept)
    (define-key map (kbd "C-c C-k") #'sly-agent-q-diff-reject)

    ;; Per-hunk approval
    (define-key map (kbd "a") #'sly-agent-q-diff-accept-hunk)
    (define-key map (kbd "r") #'sly-agent-q-diff-reject-hunk)
    (define-key map (kbd "n") #'diff-hunk-next)
    (define-key map (kbd "p") #'diff-hunk-prev)
    (define-key map (kbd "q") #'sly-agent-q-diff-finish)
    (define-key map (kbd "SPC") #'sly-agent-q-diff-toggle-hunk)
    (define-key map (kbd "RET") #'sly-agent-q-diff-preview-hunk)
    map)
  "Keymap for `sly-agent-q-diff-mode'.")
```

**Step 4: Manual test**

Manual test:
1. Open diff with multiple hunks
2. Press SPC on a hunk - should toggle state
3. Press RET - should jump to source location

**Step 5: Commit toggle and preview**

```bash
git add contrib/sly-agent-q/sly-agent-q-diff.el
git commit -m "feat: add toggle and preview commands

Add SPC to toggle hunk state and RET to preview source
location. Enhances interactive review workflow."
```

---

## Task 9: Add Help Overlay (Phase 4 Polish)

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el`

**Step 1: Implement help display function**

Add after preview function:

```elisp
(defun sly-agent-q-diff-show-help ()
  "Display help overlay with keybindings."
  (interactive)
  (let ((help-text
         (concat
          (propertize "Agent-Q Diff Mode Help\n\n" 'face 'bold)
          (propertize "Per-Hunk Commands:\n" 'face 'underline)
          "  a         - Accept current hunk (apply immediately)\n"
          "  r         - Reject current hunk (skip)\n"
          "  SPC       - Toggle hunk state\n"
          "  n         - Next hunk\n"
          "  p         - Previous hunk\n"
          "  RET       - Preview source location\n"
          "  q         - Finish review and close\n\n"
          (propertize "All-or-Nothing (Legacy):\n" 'face 'underline)
          "  C-c C-c   - Accept all hunks\n"
          "  C-c C-k   - Reject all hunks\n\n"
          (propertize "Help:\n" 'face 'underline)
          "  ?         - Toggle this help\n\n"
          (propertize "Press any key to dismiss" 'face 'italic))))

    (let ((buf (get-buffer-create "*Agent-Q Diff Help*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert help-text)
          (goto-char (point-min))
          (view-mode)
          (local-set-key (kbd "?") #'quit-window)
          (local-set-key (kbd "q") #'quit-window)))
      (pop-to-buffer buf)
      (fit-window-to-buffer)
      (message "Press ? or q to close help"))))
```

**Step 2: Add keybinding**

Update keymap:

```elisp
(defvar sly-agent-q-diff-mode-map
  (let ((map (make-sparse-keymap)))
    ;; All-or-nothing (backward compatible)
    (define-key map (kbd "C-c C-c") #'sly-agent-q-diff-accept)
    (define-key map (kbd "C-c C-k") #'sly-agent-q-diff-reject)

    ;; Per-hunk approval
    (define-key map (kbd "a") #'sly-agent-q-diff-accept-hunk)
    (define-key map (kbd "r") #'sly-agent-q-diff-reject-hunk)
    (define-key map (kbd "n") #'diff-hunk-next)
    (define-key map (kbd "p") #'diff-hunk-prev)
    (define-key map (kbd "q") #'sly-agent-q-diff-finish)
    (define-key map (kbd "SPC") #'sly-agent-q-diff-toggle-hunk)
    (define-key map (kbd "RET") #'sly-agent-q-diff-preview-hunk)
    (define-key map (kbd "?") #'sly-agent-q-diff-show-help)
    map)
  "Keymap for `sly-agent-q-diff-mode'.")
```

**Step 3: Manual test**

Press `?` in diff buffer - help should display

**Step 4: Commit help system**

```bash
git add contrib/sly-agent-q/sly-agent-q-diff.el
git commit -m "feat: add interactive help overlay

Press ? to display keybinding reference. Improves
discoverability of per-hunk approval features."
```

---

## Task 10: Update Documentation

**Files:**
- Modify: `QUICKSTART.md`
- Create: `docs/DIFF-REVIEW-GUIDE.md`

**Step 1: Update QUICKSTART.md**

Add section to QUICKSTART.md after existing diff section:

```markdown
### Reviewing Diffs Per-Hunk (New in v0.3)

When Agent-Q proposes changes, you can review each change individually:

**Quick Start:**
1. Diff buffer opens with all proposed changes
2. Press `n` to go to next hunk, `p` for previous
3. Press `a` to accept current hunk (applies immediately)
4. Press `r` to reject current hunk (skip it)
5. Press `q` when done reviewing

**Quick accept all:** `C-c C-c` (same as before)
**Quick reject all:** `C-c C-k`

**Visual Feedback:**
- Green background with ✓ = applied
- Red background with ✗ = rejected
- Normal = pending review

**Additional Commands:**
- `SPC` - Toggle hunk between accept/reject
- `RET` - Jump to source location
- `?` - Show help with all keybindings

**Example Workflow:**

```
Agent proposes 3 changes to defun FOO:
  [Hunk 1] Add docstring          ← Press 'a' to accept ✓
  [Hunk 2] Rename to foo-impl     ← Press 'r' to reject ✗
  [Hunk 3] Add type declaration   ← Press 'a' to accept ✓

Result: 2/3 hunks applied, file modified, save when ready
```

See `docs/DIFF-REVIEW-GUIDE.md` for detailed examples.
```

**Step 2: Create comprehensive guide**

Create `docs/DIFF-REVIEW-GUIDE.md`:

```markdown
# Diff Review Guide: Per-Hunk Approval

## Overview

Agent-Q's diff review system lets you accept or reject individual changes (hunks) instead of all-or-nothing. This gives you granular control over what the agent modifies.

## What is a Hunk?

A **hunk** is a contiguous block of changes in a diff. Example:

\`\`\`diff
@@ -10,3 +10,4 @@
 (defun calculate-total (items)
+  "Sum the prices of ITEMS."
   (reduce #'+ items :key #'price))
\`\`\`

This hunk adds a docstring. You can accept this specific change even if you reject other hunks in the same diff.

## Basic Workflow

1. **Agent proposes changes** → Diff buffer opens
2. **Navigate hunks** → `n` (next) / `p` (previous)
3. **Review each hunk** → Press `a` (accept) or `r` (reject)
4. **Finish review** → Press `q` to close

## Keybindings Reference

| Key       | Action                          |
|-----------|---------------------------------|
| `a`       | Accept current hunk (apply)     |
| `r`       | Reject current hunk (skip)      |
| `n`       | Next hunk                       |
| `p`       | Previous hunk                   |
| `SPC`     | Toggle accept/reject            |
| `RET`     | Jump to source location         |
| `q`       | Finish review and close         |
| `?`       | Show help                       |
| `C-c C-c` | Accept all (legacy)             |
| `C-c C-k` | Reject all (legacy)             |

## Visual Indicators

- **Green background + ✓** = Hunk applied
- **Red background + ✗** = Hunk rejected
- **Normal diff colors** = Pending review

Header shows progress:
```
Progress: 2/5 applied, 1 rejected, 2 pending
```

## Common Scenarios

### Scenario 1: Accept Most, Reject One Bad Change

Agent proposes:
- Hunk 1: Add docstring ✓
- Hunk 2: Rename to `new-impl` ✗ (bad name)
- Hunk 3: Add type declaration ✓

**Workflow:**
1. Press `a` on hunk 1 → Applied ✓
2. Press `n` to move to hunk 2
3. Press `r` on hunk 2 → Rejected ✗
4. Press `n` to move to hunk 3
5. Press `a` on hunk 3 → Applied ✓
6. Press `q` to finish

Result: 2/3 applied, file updated with good changes only.

### Scenario 2: Preview Before Accepting

Not sure what a hunk does?

1. Press `RET` on the hunk → Jumps to source location
2. Review context in source file
3. Press `C-x o` to return to diff buffer
4. Press `a` (accept) or `r` (reject)

### Scenario 3: Quick Accept All (Legacy)

Single hunk or trust all changes?

1. Press `C-c C-c` → All hunks applied immediately

### Scenario 4: Accidentally Accepted Wrong Hunk

You can't un-apply a hunk, but you can:

1. Reject remaining hunks (`r` on each)
2. Press `q` to finish
3. Use `C-x u` (undo) in the source file
4. Ask agent to re-propose just the good changes

## Advanced Usage

### Toggling State with SPC

- On pending hunk → Press `SPC` → Accepts it
- On rejected hunk → Press `SPC` → Accepts it
- On applied hunk → Press `SPC` → Marks as pending (doesn't undo)

### Handling Apply Failures

If `diff-apply-hunk` fails (e.g., context changed):

1. You'll see: "✗ Failed to apply hunk: <reason>"
2. Press `RET` to jump to source
3. Check what changed
4. Options:
   - Manually edit the file
   - Ask agent to regenerate the diff
   - Reject the hunk and move on

### Reviewing Multi-File Diffs

Currently, Agent-Q shows one file's diff at a time. If the agent proposes changes to multiple files, you'll review them sequentially.

## Tips

1. **Read the description** at the top of the diff buffer - it explains what the agent changed and why
2. **Use `n`/`p` liberally** - scan all hunks before deciding
3. **Preview with `RET`** when unsure - see the change in context
4. **Press `?` anytime** for help
5. **You can save later** - accepted hunks modify the buffer but don't auto-save

## Comparison with Legacy Mode

| Feature                  | Legacy (`C-c C-c`) | Per-Hunk (`a`/`r`) |
|--------------------------|--------------------|--------------------|
| Granularity              | All or nothing     | Per-hunk           |
| Visual feedback          | None               | Color-coded        |
| Preview location         | No                 | Yes (`RET`)        |
| Progress tracking        | No                 | Yes (header)       |
| Undo single change       | No                 | No*                |

*Both modes apply changes immediately. Use file undo (`C-x u`) to revert.

## Troubleshooting

**Q: Hunk won't apply (error message)**
A: The file changed since the diff was generated. Ask the agent to regenerate the diff or manually apply the change.

**Q: Accepted wrong hunk, how to undo?**
A: Use `C-x u` in the source buffer to undo. Can't undo from diff buffer.

**Q: What happens to unreviewed hunks when I press `q`?**
A: You'll be warned: "2 unreviewed hunk(s) remaining. Finish anyway?" They won't be applied unless you confirm.

**Q: Can I edit a hunk before applying?**
A: Not directly. Reject it and ask the agent to refine the change, or manually edit the source file.

## See Also

- `QUICKSTART.md` - Basic Agent-Q usage
- `CLAUDE.md` - Architecture and design
- Enhancement 1 Spec: `specs/enhancement-1.md`
```

**Step 3: Commit documentation**

```bash
git add QUICKSTART.md docs/DIFF-REVIEW-GUIDE.md
git commit -m "docs: add per-hunk diff review documentation

Add comprehensive guide to per-hunk approval workflow with
examples, keybinding reference, and troubleshooting."
```

---

## Task 11: Integration Testing and Validation

**Files:**
- Create: `contrib/sly-agent-q/test/sly-agent-q-diff-integration-test.el`

**Step 1: Create end-to-end test file**

Create `contrib/sly-agent-q/test/sly-agent-q-diff-integration-test.el`:

```elisp
;;; sly-agent-q-diff-integration-test.el --- Integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; End-to-end tests for diff workflow

;;; Code:

(require 'test-helper)

(ert-deftest sly-agent-q-diff-integration-multi-hunk ()
  "Integration test: Apply some hunks, reject others."
  (let* ((test-file (make-temp-file "agent-q-test-" nil ".lisp"))
         (original (concat ";;;; test.lisp\n"
                          "(defun foo ()\n"
                          "  42)\n\n"
                          "(defun bar ()\n"
                          "  99)\n"))
         (modified (concat ";;;; test.lisp\n"
                          "(defun foo ()\n"
                          "  \"Returns 42.\"\n"
                          "  42)\n\n"
                          "(defun bar ()\n"
                          "  \"Returns 99.\"\n"
                          "  99)\n")))
    (unwind-protect
        (progn
          ;; Write original to file
          (with-temp-file test-file
            (insert original))

          ;; Generate diff
          (let ((diff-text (sly-agent-q-diff--generate-unified-diff
                           original modified "test.lisp")))
            (with-temp-buffer
              (insert diff-text)
              (sly-agent-q-diff-mode)
              (setq sly-agent-q-diff--path test-file
                    sly-agent-q-diff--original original
                    sly-agent-q-diff--modified modified)

              ;; Navigate to first hunk and accept
              (goto-char (point-min))
              (re-search-forward "^@@")
              (beginning-of-line)
              (sly-agent-q-diff-accept-hunk)

              ;; Navigate to second hunk and reject
              (re-search-forward "^@@")
              (beginning-of-line)
              (sly-agent-q-diff-reject-hunk)

              ;; Verify state
              (should (= 1 (cl-count 'applied sly-agent-q-diff--hunk-states
                                    :key #'cdr)))
              (should (= 1 (cl-count 'rejected sly-agent-q-diff--hunk-states
                                    :key #'cdr)))

              ;; Verify file has first change but not second
              (with-temp-buffer
                (insert-file-contents test-file)
                (let ((content (buffer-string)))
                  (should (string-match-p "Returns 42" content))
                  (should-not (string-match-p "Returns 99" content)))))))

      ;; Cleanup
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest sly-agent-q-diff-integration-all-accept ()
  "Integration test: Accept all hunks at once (legacy mode)."
  (let* ((test-file (make-temp-file "agent-q-test-" nil ".lisp"))
         (original "line1\nline2\n")
         (modified "line1\nline1.5\nline2\nline2.5\n"))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert original))

          (let ((diff-text (sly-agent-q-diff--generate-unified-diff
                           original modified "test.lisp")))
            (with-temp-buffer
              (insert diff-text)
              (sly-agent-q-diff-mode)
              (setq sly-agent-q-diff--path test-file
                    sly-agent-q-diff--original original
                    sly-agent-q-diff--modified modified)

              ;; Simulate C-c C-c (accept all)
              (goto-char (point-min))
              (diff-apply-buffer)

              ;; Verify all changes applied
              (with-temp-buffer
                (insert-file-contents test-file)
                (should (string-match-p "line1.5" (buffer-string)))
                (should (string-match-p "line2.5" (buffer-string)))))))

      (when (file-exists-p test-file)
        (delete-file test-file)))))

(provide 'sly-agent-q-diff-integration-test)
;;; sly-agent-q-diff-integration-test.el ends here
```

**Step 2: Run integration tests**

```bash
cd /Users/quasi/quasilabs/projects/agent-q/contrib/sly-agent-q
emacs -batch -l test/test-helper.el \
  -l test/sly-agent-q-diff-integration-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: All tests PASS

**Step 3: Commit integration tests**

```bash
git add contrib/sly-agent-q/test/sly-agent-q-diff-integration-test.el
git commit -m "test: add integration tests for diff workflow

End-to-end tests for multi-hunk approval and legacy
all-or-nothing mode."
```

---

## Task 12: Manual Testing Checklist and Final Validation

**Files:**
- Create: `contrib/sly-agent-q/TESTING-CHECKLIST.md`

**Step 1: Create manual testing checklist**

Create `contrib/sly-agent-q/TESTING-CHECKLIST.md`:

```markdown
# Manual Testing Checklist: Per-Hunk Diff Approval

## Prerequisites

- [ ] Agent-Q loaded in Emacs with SLY connection
- [ ] Test project with multiple Lisp files available
- [ ] `sly-agent-q-diff.el` loaded and compiled

## Test Cases

### TC1: Single Hunk Diff

**Setup:**
1. Create simple file: `(defun foo () 42)`
2. Ask agent to add docstring

**Expected:**
- [ ] Diff buffer opens with 1 hunk
- [ ] Header shows "Progress: 0/1 applied, 0 rejected, 1 pending"
- [ ] Hunk shows docstring addition
- [ ] Press `a` → Green overlay, "✓ Hunk applied"
- [ ] Header updates to "1/1 applied, 0 rejected, 0 pending"
- [ ] Press `q` → Buffer closes, file modified
- [ ] Source file contains docstring

### TC2: Multi-Hunk Diff - Accept Some, Reject Others

**Setup:**
1. File with 3 functions
2. Ask agent to add docstrings to all 3

**Expected:**
- [ ] Diff opens with 3 hunks
- [ ] Navigate with `n` between hunks
- [ ] Accept hunk 1 with `a` → Applied ✓
- [ ] Accept hunk 3 with `a` → Applied ✓
- [ ] Reject hunk 2 with `r` → Rejected ✗
- [ ] Press `q` → Summary: "2 applied, 1 rejected"
- [ ] Source file has docstrings for functions 1 and 3, not 2

### TC3: Preview Source Location

**Setup:**
1. Multi-hunk diff

**Expected:**
- [ ] Position on hunk 2
- [ ] Press `RET` → Jumps to source file at hunk location
- [ ] Press `C-x o` → Returns to diff buffer
- [ ] Still on same hunk

### TC4: Toggle Hunk State

**Setup:**
1. Multi-hunk diff

**Expected:**
- [ ] Position on pending hunk
- [ ] Press `SPC` → Hunk accepted ✓
- [ ] Press `n` to next hunk
- [ ] Reject with `r` → Rejected ✗
- [ ] Press `p` to go back
- [ ] Press `SPC` on rejected hunk → Attempts to apply

### TC5: Help Display

**Expected:**
- [ ] Press `?` → Help buffer opens
- [ ] Help shows all keybindings
- [ ] Press `q` → Help closes

### TC6: Unreviewed Hunks Warning

**Setup:**
1. 3-hunk diff

**Expected:**
- [ ] Accept hunk 1 only
- [ ] Press `q` → Warning: "2 unreviewed hunk(s) remaining"
- [ ] Answer `no` → Returns to diff
- [ ] Review remaining hunks
- [ ] Press `q` → Closes without warning

### TC7: Legacy All-or-Nothing Mode

**Expected:**
- [ ] Multi-hunk diff opens
- [ ] Press `C-c C-c` → All hunks applied
- [ ] Buffer closes immediately
- [ ] File has all changes

### TC8: Apply Failure Handling

**Setup:**
1. Create diff
2. Manually edit source file to change context
3. Try to apply hunk

**Expected:**
- [ ] Press `a` → Error: "Failed to apply hunk: <reason>"
- [ ] Hunk marked with error state (not applied)
- [ ] Can continue reviewing other hunks

### TC9: Multi-File Diff Sequence

**Setup:**
1. Ask agent to modify 2 different files

**Expected:**
- [ ] First file's diff opens
- [ ] Review and accept/reject
- [ ] Press `q` → First diff closes
- [ ] Second file's diff opens automatically
- [ ] Review second file
- [ ] Both files modified as reviewed

### TC10: Visual Feedback

**Expected:**
- [ ] Accepted hunks: green background, ✓ marker
- [ ] Rejected hunks: red background, ✗ marker
- [ ] Pending hunks: normal diff colors
- [ ] Header shows real-time progress

## Performance Tests

- [ ] 10-hunk diff: Smooth navigation with `n`/`p`
- [ ] 50-hunk diff: No lag when accepting/rejecting
- [ ] Large file diff (1000+ lines): diff-apply-hunk completes < 1s

## Regression Tests

- [ ] Single-hunk diff still works with `C-c C-c`
- [ ] Reject all with `C-c C-k` still works
- [ ] Diff generation still produces valid unified diffs
- [ ] Buffer read-only except during modifications

## Edge Cases

- [ ] Empty file → single hunk adding content
- [ ] Delete entire file content → single hunk removing all
- [ ] Whitespace-only changes → diff-apply-hunk handles gracefully
- [ ] Overlapping hunks → apply in order, each updates context
- [ ] Non-existent file → graceful error
- [ ] File deleted during review → graceful error

## Compatibility

- [ ] Works with Emacs 27.1
- [ ] Works with Emacs 28.x
- [ ] Works with Emacs 29.x
- [ ] Works with SLY current version

## Sign-Off

Tester: ________________
Date: ________________
Version: ________________
Result: PASS / FAIL
Notes:
```

**Step 2: Commit testing checklist**

```bash
git add contrib/sly-agent-q/TESTING-CHECKLIST.md
git commit -m "docs: add manual testing checklist

Comprehensive checklist for validating per-hunk diff
approval functionality."
```

---

## Task 13: Update CHANGELOG and Version

**Files:**
- Modify: `CHANGELOG.md`
- Modify: `contrib/sly-agent-q/sly-agent-q-diff.el:4`

**Step 1: Update CHANGELOG.md**

Add to `CHANGELOG.md`:

```markdown
## [0.3.0] - 2026-01-06

### Added

- **Per-hunk diff approval**: Review and apply changes granularly instead of all-or-nothing
  - `a` key to accept individual hunk
  - `r` key to reject individual hunk
  - `n`/`p` to navigate between hunks
  - `SPC` to toggle hunk state
  - `RET` to preview source location
  - `?` to show help overlay
  - Real-time progress display in header
  - Visual feedback with color-coded overlays (green=applied, red=rejected)
- Comprehensive ERT test suite for diff functionality
- Integration tests for multi-hunk workflows
- Manual testing checklist
- Detailed diff review guide documentation

### Changed

- Replaced fragile string-matching diff application with `diff-mode.el` infrastructure
- Accept/reject commands now use built-in `diff-apply-hunk` and `diff-apply-buffer`
- Improved robustness for whitespace variations and context changes

### Deprecated

- None (backward compatible - `C-c C-c` / `C-c C-k` still work)

### Fixed

- Edge cases with whitespace mismatches in diffs
- Off-by-one errors in manual position calculation
- Context shift handling when nearby code changes
```

**Step 2: Update version number**

Modify line 4 in `contrib/sly-agent-q/sly-agent-q-diff.el`:

```elisp
;; Version: 0.3.0
```

**Step 3: Commit changelog and version bump**

```bash
git add CHANGELOG.md contrib/sly-agent-q/sly-agent-q-diff.el
git commit -m "chore: bump version to 0.3.0

Release per-hunk diff approval feature with comprehensive
testing and documentation."
```

---

## Task 14: Create Release Tag

**Files:**
- None (git operations)

**Step 1: Verify all tests pass**

```bash
cd /Users/quasi/quasilabs/projects/agent-q/contrib/sly-agent-q
emacs -batch -l test/test-helper.el \
  -l test/sly-agent-q-diff-test.el \
  -l test/sly-agent-q-diff-integration-test.el \
  -f ert-run-tests-batch-and-exit
```

Expected: All tests PASS

**Step 2: Verify Lisp tests still pass**

```bash
cd /Users/quasi/quasilabs/projects/agent-q
sbcl --non-interactive \
     --eval "(ql:quickload :agent-q/tests)" \
     --eval "(agent-q-tests:run-phase-2-tests)" \
     --quit
```

Expected: All Phase 2 tests PASS

**Step 3: Create annotated tag**

```bash
git tag -a v0.3.0 -m "Release v0.3.0: Per-Hunk Diff Approval

Features:
- Per-hunk accept/reject with granular control
- Visual feedback with overlays
- Real-time progress tracking
- Built-in diff-mode integration
- Comprehensive test suite
- Backward compatible with legacy mode"
```

**Step 4: Push tag to remote**

```bash
git push origin v0.3.0
```

---

## Summary

This plan implements Enhancement 1: Per-Hunk Diff Approval in 14 tasks following TDD principles:

**Phases Covered:**
1. ✅ Phase 1 (Foundation): Replace string matching with `diff-apply-hunk`
2. ✅ Phase 2 (Core): Per-hunk approval with state tracking
3. ✅ Phase 3 (Visual): Overlays and progress display
4. ✅ Phase 4 (Polish): Toggle, preview, help system

**Testing:**
- Unit tests with ERT
- Integration tests for workflows
- Manual testing checklist

**Documentation:**
- QUICKSTART.md updates
- Comprehensive diff review guide
- Testing checklist

**Total Estimated Time:** 11-16 hours (per spec)

**Deliverables:**
- Production-ready per-hunk diff approval
- Backward-compatible with legacy mode
- Comprehensive test coverage
- User-facing documentation
- Version 0.3.0 release

---

**Plan complete and saved to `docs/plans/2026-01-06-per-hunk-diff-approval.md`.**

Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach would you prefer?
