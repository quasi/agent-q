;;; sly-agent-q-diff.el --- Diff view for Agent-Q file edits -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (sly "1.0.0"))
;; Keywords: lisp, ai, tools
;; URL: https://github.com/quasilabs/agent-q

;;; Commentary:

;; This file implements the diff view UI for Agent-Q Phase 2.
;; When the agent proposes file edits via the propose_file_edit tool,
;; this displays a unified diff and allows the user to accept or reject.
;;
;; Workflow:
;;   1. Agent calls propose_file_edit tool
;;   2. Lisp calls sly-agent-q-show-diff-and-wait
;;   3. This displays diff in *Agent-Q Diff* buffer
;;   4. User presses C-c C-c (accept) or C-c C-k (reject)
;;   5. On accept: buffer is updated and saved
;;   6. Function returns 'accepted or 'rejected to Lisp

;;; Code:

(require 'diff-mode)
(require 'cl-lib)

;;; Customization

(defgroup sly-agent-q-diff nil
  "Diff view for Agent-Q file edits."
  :group 'sly
  :prefix "sly-agent-q-diff-")

(defface sly-agent-q-diff-header
  '((t :inherit bold :foreground "cyan"))
  "Face for diff header information."
  :group 'sly-agent-q-diff)

(defface sly-agent-q-diff-keybinding
  '((t :inherit warning :weight bold))
  "Face for keybinding hints."
  :group 'sly-agent-q-diff)

(defface sly-agent-q-diff-applied-face
  '((t :background "#2d4f2d" :extend t))
  "Face for applied hunks."
  :group 'sly-agent-q-diff)

(defface sly-agent-q-diff-rejected-face
  '((t :background "#4f2d2d" :extend t))
  "Face for rejected hunks."
  :group 'sly-agent-q-diff)

;;; Buffer-local state

(defvar-local sly-agent-q-diff--path nil
  "File path for the current diff.")

(defvar-local sly-agent-q-diff--original nil
  "Original file content.")

(defvar-local sly-agent-q-diff--modified nil
  "Modified file content (proposed changes).")

(defvar-local sly-agent-q-diff--description nil
  "Description of what changed and why.")

(defvar-local sly-agent-q-diff--decision nil
  "User's decision: 'accepted or 'rejected.")

(defvar-local sly-agent-q-diff--hunk-states nil
  "Alist mapping hunk positions to states.
Each entry: (HUNK-START . STATE)
STATE: 'pending | 'accepted | 'rejected | 'applied")

;;; Hunk utilities

(defun sly-agent-q-diff--count-hunks ()
  "Return the number of hunks in current diff buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      ;; Count @@ lines which mark hunk headers
      (while (re-search-forward "^@@" nil t)
        (cl-incf count))
      count)))

(defun sly-agent-q-diff--hunk-end ()
  "Return end position of current hunk."
  (save-excursion
    (diff-end-of-hunk)
    (point)))

(defun sly-agent-q-diff--mark-hunk-applied (hunk-start)
  "Add overlay marking HUNK-START as applied."
  (let* ((hunk-end (save-excursion
                    (goto-char hunk-start)
                    (sly-agent-q-diff--hunk-end)))
         (ov (make-overlay hunk-start hunk-end)))
    (overlay-put ov 'face 'sly-agent-q-diff-applied-face)
    (overlay-put ov 'sly-agent-q-hunk-state 'applied)
    (overlay-put ov 'before-string
                 (propertize "[APPLIED] " 'face '(:foreground "#00ff00" :weight bold)))))

(defun sly-agent-q-diff--mark-hunk-rejected (hunk-start)
  "Add overlay marking HUNK-START as rejected."
  (let* ((hunk-end (save-excursion
                    (goto-char hunk-start)
                    (sly-agent-q-diff--hunk-end)))
         (ov (make-overlay hunk-start hunk-end)))
    (overlay-put ov 'face 'sly-agent-q-diff-rejected-face)
    (overlay-put ov 'sly-agent-q-hunk-state 'rejected)
    (overlay-put ov 'before-string
                 (propertize "[REJECTED] " 'face '(:foreground "#ff6666" :weight bold)))))

(defun sly-agent-q-diff--clear-hunk-overlays ()
  "Remove all hunk state overlays from buffer."
  (remove-overlays (point-min) (point-max) 'sly-agent-q-hunk-state))

(defun sly-agent-q-diff--update-header ()
  "Update diff header with hunk progress.
Shows counts for applied, rejected, and pending hunks along with keybinding hints."
  (let* ((total (sly-agent-q-diff--count-hunks))
         (applied (cl-count 'applied sly-agent-q-diff--hunk-states :key #'cdr))
         (rejected (cl-count 'rejected sly-agent-q-diff--hunk-states :key #'cdr))
         (pending (- total applied rejected)))
    (save-excursion
      (goto-char (point-min))
      ;; Find the progress/keybinding line (starts with "Progress:" or "C-c C-c")
      (when (re-search-forward "^\\(Progress:\\|C-c C-c =\\)" nil t)
        (let ((inhibit-read-only t))
          (beginning-of-line)
          (delete-region (point) (line-end-position))
          (insert (propertize
                   (format "Progress: %d/%d applied, %d rejected, %d pending  |  [a]ccept [r]eject [n]ext [p]rev [q]uit"
                           applied total rejected pending)
                   'face 'sly-agent-q-diff-keybinding)))))))

;;; Major mode

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

(define-derived-mode sly-agent-q-diff-mode diff-mode "Agent-Q-Diff"
  "Major mode for reviewing Agent-Q proposed file changes.

\\{sly-agent-q-diff-mode-map}"
  :group 'sly-agent-q-diff
  ;; Don't set buffer-read-only - diff-mode needs to modify buffer properties
  (setq truncate-lines nil))

;;; Core functionality

(defun sly-agent-q-show-diff-and-wait (path original modified description)
  "Show diff for PATH and block until user accepts or rejects.

ORIGINAL is the expected original content.
MODIFIED is the proposed new content.
DESCRIPTION explains what changed and why.

Returns 'accepted if user accepts, 'rejected otherwise."
  ;; Expand path to absolute (LLM may provide relative path)
  (let* ((abs-path (expand-file-name path))
         (diff-buffer (get-buffer-create "*Agent-Q Diff*")))
    (with-current-buffer diff-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sly-agent-q-diff-mode)

        ;; Store state (use absolute path)
        (setq sly-agent-q-diff--path abs-path
              sly-agent-q-diff--original original
              sly-agent-q-diff--modified modified
              sly-agent-q-diff--description description
              sly-agent-q-diff--decision nil)

        ;; Insert header
        (insert (propertize (format "Agent-Q proposes changes to: %s\n" path)
                           'face 'sly-agent-q-diff-header))
        (insert (propertize (format "Description: %s\n\n" description)
                           'face 'font-lock-comment-face))
        ;; Placeholder for progress line (updated by sly-agent-q-diff--update-header)
        (insert (propertize "C-c C-c = ACCEPT  |  C-c C-k = REJECT  |  q = REJECT & QUIT\n\n"
                           'face 'sly-agent-q-diff-keybinding))

        ;; Generate and insert diff
        (condition-case err
            (let ((diff-text (sly-agent-q-diff--generate-unified-diff original modified path)))
              (insert diff-text))
          (error
           (insert (propertize (format "Error generating diff: %s\n" (error-message-string err))
                              'face 'error))
           (insert "\n--- Original ---\n")
           (insert original)
           (insert "\n\n--- Modified ---\n")
           (insert modified)))

        ;; Update header with initial hunk count
        (sly-agent-q-diff--update-header)))

    ;; Display buffer and switch to it
    (pop-to-buffer diff-buffer)

    ;; Use recursive-edit to wait for decision without blocking Emacs
    ;; User presses C-c C-c or C-c C-k which will call exit-recursive-edit
    (setq sly-agent-q-diff--decision nil)
    (message "Press C-c C-c to ACCEPT or C-c C-k to REJECT")

    ;; Enter recursive edit - this allows Emacs to remain responsive
    (recursive-edit)

    ;; Return decision as string (SLY can't serialize symbols)
    ;; Buffer may have been killed by accept/reject, so check first
    (let ((decision (if (buffer-live-p diff-buffer)
                        (with-current-buffer diff-buffer
                          sly-agent-q-diff--decision)
                      ;; Buffer was killed, default to rejected
                      'rejected)))
      ;; Clean up the buffer if still alive
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (if (eq decision 'accepted)
          "accepted"
        "rejected"))))

(defun sly-agent-q-diff--unescape-string (str)
  "Unescape common escape sequences in STR.
Converts \\t to tab, \\n to newline, \\\\ to backslash."
  (let ((result str))
    ;; Order matters: unescape backslash-backslash last
    (setq result (replace-regexp-in-string "\\\\t" "\t" result))
    (setq result (replace-regexp-in-string "\\\\n" "\n" result))
    (setq result (replace-regexp-in-string "\\\\\\\\" "\\" result))
    result))

(defun sly-agent-q-diff--generate-unified-diff (original modified path)
  "Generate unified diff between ORIGINAL and MODIFIED for PATH.

Returns the diff as a string."
  ;; Unescape content in case SLY serialization escaped special chars
  (let* ((orig-unescaped (sly-agent-q-diff--unescape-string original))
         (mod-unescaped (sly-agent-q-diff--unescape-string modified))
         (orig-file (make-temp-file "agent-q-orig-"))
         (mod-file (make-temp-file "agent-q-mod-")))
    (unwind-protect
        (progn
          ;; Write content to temp files
          (with-temp-file orig-file
            (insert orig-unescaped))
          (with-temp-file mod-file
            (insert mod-unescaped))

          ;; Generate diff
          (with-temp-buffer
            (let ((exit-code (call-process "diff" nil t nil
                                          "-u"
                                          "--label" (format "a/%s" path)
                                          "--label" (format "b/%s" path)
                                          orig-file mod-file)))
              ;; diff returns 1 when files differ, which is expected
              (when (and exit-code (> exit-code 1))
                (error "diff command failed with exit code %d" exit-code))
              (buffer-string))))

      ;; Cleanup temp files
      (when (file-exists-p orig-file)
        (delete-file orig-file))
      (when (file-exists-p mod-file)
        (delete-file mod-file)))))

;;; Interactive commands

(defun sly-agent-q-diff-accept ()
  "Accept the proposed changes and apply them using diff-mode infrastructure."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (let ((path sly-agent-q-diff--path)
        (diff-buf (current-buffer)))
    (unless path
      (user-error "Missing file path - cannot apply changes"))

    ;; Ensure target file exists and is visited
    (unless (file-exists-p path)
      (user-error "File does not exist: %s" path))

    (let ((file-buffer (find-file-noselect path)))
      ;; Apply all hunks in diff buffer using built-in diff-apply-hunk
      ;; We apply hunks one by one to avoid auto-save behavior of diff-apply-buffer
      (condition-case err
          (progn
            (goto-char (point-min))
            (let ((applied 0))
              (while (re-search-forward "^@@" nil t)
                (beginning-of-line)
                (diff-apply-hunk)
                (cl-incf applied))
              (message "✓ %d hunk(s) applied to %s. Save when ready." applied path)))
        (error
         (message "✗ Error applying diff: %s" (error-message-string err))
         (user-error "Failed to apply changes: %s" (error-message-string err)))))

    ;; Set decision and exit recursive edit
    ;; Don't kill buffer here - sly-agent-q-show-diff-and-wait needs to read the decision
    (setq sly-agent-q-diff--decision 'accepted)
    (exit-recursive-edit)))

(defun sly-agent-q-diff-reject ()
  "Reject the proposed changes."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (setq sly-agent-q-diff--decision 'rejected)
  (message "✗ Changes rejected")
  ;; Don't kill buffer here - sly-agent-q-show-diff-and-wait needs to read the decision
  (exit-recursive-edit))

;;; Per-hunk commands

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
            (sly-agent-q-diff--update-header)
            (message "✓ Hunk applied"))
        (error
         (message "✗ Failed to apply hunk: %s" (error-message-string err))
         (setf (alist-get hunk-start sly-agent-q-diff--hunk-states)
               'error)))))

  ;; Move to next hunk
  (condition-case nil
      (diff-hunk-next)
    (error nil)))  ; No more hunks

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
      (sly-agent-q-diff--update-header)
      (message "✗ Hunk rejected")))

  ;; Move to next hunk
  (condition-case nil
      (diff-hunk-next)
    (error nil)))

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

    ;; Exit recursive edit (caller will clean up buffer)
    (exit-recursive-edit)))

(defun sly-agent-q-diff-toggle-hunk ()
  "Toggle current hunk between accepted and rejected states.
If the hunk is already applied, mark as pending (cannot unapply).
If rejected or pending, attempt to apply the hunk."
  (interactive)
  (unless (derived-mode-p 'sly-agent-q-diff-mode)
    (user-error "Not in Agent-Q diff buffer"))

  (save-excursion
    (diff-beginning-of-hunk)
    (let* ((hunk-start (point))
           (current-state (alist-get hunk-start sly-agent-q-diff--hunk-states)))
      (pcase current-state
        ('applied
         ;; Can't unapply, so just mark as pending
         (setf (alist-get hunk-start sly-agent-q-diff--hunk-states) nil)
         (sly-agent-q-diff--clear-hunk-overlays)
         (sly-agent-q-diff--update-header)
         (message "Hunk marked as pending (cannot unapply)"))

        ('rejected
         ;; Try to apply
         (sly-agent-q-diff-accept-hunk))

        (_
         ;; Pending - accept by default
         (sly-agent-q-diff-accept-hunk))))))

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

(defun sly-agent-q-diff-show-help ()
  "Display help buffer with keybinding reference."
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
          (propertize "Press ? or q to close" 'face 'italic))))

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

(provide 'sly-agent-q-diff)
;;; sly-agent-q-diff.el ends here
