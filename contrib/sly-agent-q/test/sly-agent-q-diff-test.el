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
