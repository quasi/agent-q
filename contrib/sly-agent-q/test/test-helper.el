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

;; Mock SLY functions for testing without a connection
(unless (featurep 'sly)
  (message "SLY not available - loading mocks for testing")

  ;; Minimal SLY mocks
  (defun sly-connected-p () nil)
  (defun sly-eval-async (form callback)
    "Mock sly-eval-async for testing."
    (funcall callback nil))

  ;; Provide fake sly feature
  (provide 'sly))

;; Load all modules being tested
(require 'sly-agent-q-diff)
(require 'sly-agent-q-chat)
(require 'sly-agent-q-sessions)

(provide 'test-helper)
;;; test-helper.el ends here
