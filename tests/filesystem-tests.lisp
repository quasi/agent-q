;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q-TESTS; Base: 10 -*-

(in-package :agent-q-tests)

;;; ============================================================================
;;; Filesystem Tools Tests
;;; ============================================================================
;;; ABOUTME: Tests for file system tools configuration and security boundaries.
;;; The *project-root* variable is the security boundary - all filesystem tools
;;; validate paths against this root to prevent access outside the project.

(def-suite filesystem-tests
  :description "Tests for file system tools")

(in-suite filesystem-tests)

(test project-root-variable-exists
  "Project root variable should exist and be configurable"
  (is (boundp 'agent-q:*project-root*))
  ;; Can set it
  (let ((agent-q:*project-root* #P"/tmp/test-project/"))
    (is (pathnamep agent-q:*project-root*))))

(test project-root-defaults-to-nil
  "Project root should default to nil (auto-detect mode)"
  (is (null (symbol-value 'agent-q:*project-root*))))

;;; ============================================================================
;;; Project Root Auto-Detection Tests
;;; ============================================================================

(test find-git-root-detects-git-directory
  "find-git-root should find .git directory"
  ;; Use the actual agent-q directory which has .git
  (let ((root (agent-q::find-git-root (asdf:system-source-directory :agent-q))))
    (is (not (null root)))
    (is (probe-file (merge-pathnames ".git/" root)))))

(test find-git-root-returns-nil-for-non-git
  "find-git-root should return nil when no .git found"
  ;; /tmp is unlikely to be a git repo
  (let ((root (agent-q::find-git-root #P"/tmp/")))
    (is (null root))))

(test find-git-root-handles-nil-input
  "find-git-root should handle nil input gracefully"
  (is (null (agent-q::find-git-root nil))))

(test detect-project-root-uses-git
  "detect-project-root should prefer git root"
  (let ((detected (agent-q::detect-project-root)))
    (is (not (null detected)))
    (is (pathnamep detected))
    ;; Verify git root was actually selected (priority 1)
    (is (probe-file (merge-pathnames ".git/" detected))
        "Should return git root when available")))

(test detect-project-root-returns-valid-directory
  "detect-project-root should return a directory that exists"
  (let ((detected (agent-q::detect-project-root)))
    (is (probe-file detected))))
