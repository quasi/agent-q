;;; run.el --- Batch test runner for sly-agent-q -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>

;;; Commentary:

;; Batch test runner for all sly-agent-q test suites.
;; Run with: emacs -Q --batch -l test/run.el

;;; Code:

(let ((dir (file-name-directory load-file-name)))
  ;; Add parent directory (package root) to load path
  (add-to-list 'load-path (expand-file-name ".." dir))
  ;; Add test directory to load path
  (add-to-list 'load-path dir))

(require 'ert)

;; Load test helper
(require 'test-helper)

;; Load all test files
(message "Loading test suites...")
(require 'sly-agent-q-chat-test)
(require 'sly-agent-q-sessions-test)
(require 'sly-agent-q-diff-test)

(message "\n========================================")
(message "Running sly-agent-q test suite")
(message "========================================\n")

;; Run all tests
(ert-run-tests-batch-and-exit)

;;; run.el ends here
