;;; sly-agent-q-diff-integration-test.el --- Integration tests -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: lisp, ai, tools, tests

;;; Commentary:

;; End-to-end integration tests for sly-agent-q-diff.el
;; These tests verify complete workflows with real temp files.

;;; Code:

(require 'test-helper)
(require 'cl-lib)

(ert-deftest sly-agent-q-diff-integration-multi-hunk ()
  "Integration test: Apply first hunk only, verify selective application.
Uses content with enough spacing to generate separate hunks."
  (let* ((test-file (make-temp-file "agent-q-test-" nil ".lisp"))
         ;; Use content with many lines between changes to force separate hunks
         (original (concat ";;;; test.lisp\n"
                           "\n"
                           "(defun foo ()\n"
                           "  42)\n"
                           "\n"
                           ";; some\n"
                           ";; filler\n"
                           ";; lines\n"
                           ";; to\n"
                           ";; separate\n"
                           ";; the\n"
                           ";; hunks\n"
                           ";; apart\n"
                           "\n"
                           "(defun bar ()\n"
                           "  99)\n"))
         (modified (concat ";;;; test.lisp\n"
                           "\n"
                           "(defun foo ()\n"
                           "  \"Returns 42.\"\n"
                           "  42)\n"
                           "\n"
                           ";; some\n"
                           ";; filler\n"
                           ";; lines\n"
                           ";; to\n"
                           ";; separate\n"
                           ";; the\n"
                           ";; hunks\n"
                           ";; apart\n"
                           "\n"
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
                            original modified test-file)))
            ;; Verify we have multiple hunks
            (should (> (with-temp-buffer
                         (insert diff-text)
                         (goto-char (point-min))
                         (let ((count 0))
                           (while (re-search-forward "^@@" nil t)
                             (cl-incf count))
                           count))
                       1))

            (with-temp-buffer
              (insert diff-text)
              (sly-agent-q-diff-mode)
              (setq sly-agent-q-diff--path test-file
                    sly-agent-q-diff--original original
                    sly-agent-q-diff--modified modified)

              ;; Navigate to first hunk and accept it only
              (goto-char (point-min))
              (re-search-forward "^@@")
              (beginning-of-line)
              (diff-apply-hunk)  ; Apply first hunk directly

              ;; Verify file has first change but not second
              (let ((file-buffer (find-buffer-visiting test-file)))
                (should file-buffer)
                (with-current-buffer file-buffer
                  (let ((content (buffer-string)))
                    ;; First docstring should be applied
                    (should (string-match-p "Returns 42" content))
                    ;; Second docstring should NOT be applied
                    (should-not (string-match-p "Returns 99" content)))
                  (set-buffer-modified-p nil))))))

      ;; Cleanup
      (let ((buf (find-buffer-visiting test-file)))
        (when buf (kill-buffer buf)))
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
                            original modified test-file)))
            (with-temp-buffer
              (insert diff-text)
              (sly-agent-q-diff-mode)
              (setq sly-agent-q-diff--path test-file
                    sly-agent-q-diff--original original
                    sly-agent-q-diff--modified modified)

              ;; Apply all hunks using diff-apply-buffer
              (goto-char (point-min))
              (diff-apply-buffer)

              ;; Verify all changes applied in the buffer
              (let ((file-buffer (find-buffer-visiting test-file)))
                (should file-buffer)
                (with-current-buffer file-buffer
                  (should (string-match-p "line1.5" (buffer-string)))
                  (should (string-match-p "line2.5" (buffer-string)))
                  (set-buffer-modified-p nil))))))

      ;; Cleanup
      (let ((buf (find-buffer-visiting test-file)))
        (when buf (kill-buffer buf)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest sly-agent-q-diff-integration-toggle ()
  "Integration test: Toggle hunk state."
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
      ;; Initially pending (nil state)
      (should (null (alist-get hunk-start sly-agent-q-diff--hunk-states)))

      ;; Simulate rejecting without a file (just mark state)
      (setf (alist-get hunk-start sly-agent-q-diff--hunk-states) 'rejected)
      (should (eq 'rejected (alist-get hunk-start sly-agent-q-diff--hunk-states))))))

(ert-deftest sly-agent-q-diff-integration-header-update ()
  "Integration test: Header updates reflect hunk states."
  (let* ((test-file (make-temp-file "agent-q-test-" nil ".lisp"))
         (original "line1\nline2\nline3\n")
         (modified "lineA\nline2\nlineC\n"))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert original))

          (let ((diff-text (sly-agent-q-diff--generate-unified-diff
                            original modified test-file)))
            (with-temp-buffer
              ;; Insert header like the real function does
              (insert "Agent-Q proposes changes to: test.lisp\n")
              (insert "Description: Test changes\n\n")
              (insert "C-c C-c = ACCEPT  |  C-c C-k = REJECT\n\n")
              (insert diff-text)
              (sly-agent-q-diff-mode)
              (setq sly-agent-q-diff--path test-file)

              ;; Update header (should now show progress)
              (sly-agent-q-diff--update-header)

              ;; Verify header contains "Progress:"
              (goto-char (point-min))
              (should (re-search-forward "Progress:" nil t)))))

      ;; Cleanup
      (let ((buf (find-buffer-visiting test-file)))
        (when buf (kill-buffer buf)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(provide 'sly-agent-q-diff-integration-test)
;;; sly-agent-q-diff-integration-test.el ends here
