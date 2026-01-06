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

;;; Major mode

(defvar sly-agent-q-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'sly-agent-q-diff-accept)
    (define-key map (kbd "C-c C-k") #'sly-agent-q-diff-reject)
    (define-key map (kbd "q") #'sly-agent-q-diff-reject)
    map)
  "Keymap for `sly-agent-q-diff-mode'.")

(define-derived-mode sly-agent-q-diff-mode diff-mode "Agent-Q-Diff"
  "Major mode for reviewing Agent-Q proposed file changes.

\\{sly-agent-q-diff-mode-map}"
  :group 'sly-agent-q-diff
  (setq buffer-read-only t)
  (setq truncate-lines nil))

;;; Core functionality

(defun sly-agent-q-show-diff-and-wait (path original modified description)
  "Show diff for PATH and block until user accepts or rejects.

ORIGINAL is the expected original content.
MODIFIED is the proposed new content.
DESCRIPTION explains what changed and why.

Returns 'accepted if user accepts, 'rejected otherwise."
  (let ((diff-buffer (get-buffer-create "*Agent-Q Diff*")))
    (with-current-buffer diff-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sly-agent-q-diff-mode)

        ;; Store state
        (setq sly-agent-q-diff--path path
              sly-agent-q-diff--original original
              sly-agent-q-diff--modified modified
              sly-agent-q-diff--description description
              sly-agent-q-diff--decision nil)

        ;; Insert header
        (insert (propertize (format "Agent-Q proposes changes to: %s\n" path)
                           'face 'sly-agent-q-diff-header))
        (insert (propertize (format "Description: %s\n\n" description)
                           'face 'font-lock-comment-face))
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
           (insert modified)))))

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

(provide 'sly-agent-q-diff)
;;; sly-agent-q-diff.el ends here
