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
