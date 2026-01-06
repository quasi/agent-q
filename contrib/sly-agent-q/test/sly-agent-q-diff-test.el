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

(ert-deftest sly-agent-q-diff-test-apply-single-hunk ()
  "Accepting diff should use diff-apply-hunk to apply changes."
  (let* ((test-file (make-temp-file "agent-q-test-" nil ".lisp"))
         (original ";;;; test.lisp\n(defun foo ()\n  42)")
         (modified ";;;; test.lisp\n(defun foo ()\n  \"Returns 42.\"\n  42)"))
    (unwind-protect
        (progn
          ;; Write original to file
          (with-temp-file test-file
            (insert original))

          ;; Generate diff with actual file path
          (let ((diff-text (concat "--- " test-file "\n"
                                   "+++ " test-file "\n"
                                   "@@ -1,3 +1,4 @@\n"
                                   " ;;;; test.lisp\n"
                                   " (defun foo ()\n"
                                   "+  \"Returns 42.\"\n"
                                   "   42)")))
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

              ;; Verify the buffer was modified (diff-apply-hunk modifies buffer, not file)
              (let ((file-buffer (find-buffer-visiting test-file)))
                (should file-buffer)
                (with-current-buffer file-buffer
                  (should (string-match-p "Returns 42" (buffer-string)))
                  ;; Clean up buffer
                  (set-buffer-modified-p nil))))))

      ;; Cleanup
      (when (file-exists-p test-file)
        (delete-file test-file))
      ;; Kill any visiting buffers
      (let ((buf (find-buffer-visiting test-file)))
        (when buf (kill-buffer buf))))))

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

(ert-deftest sly-agent-q-diff-test-hunk-state-init ()
  "Hunk states should initialize as empty alist."
  (with-temp-buffer
    (sly-agent-q-diff-mode)
    (should (null sly-agent-q-diff--hunk-states))))

(provide 'sly-agent-q-diff-test)
;;; sly-agent-q-diff-test.el ends here
