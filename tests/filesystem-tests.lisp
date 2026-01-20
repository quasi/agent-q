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

;;; ============================================================================
;;; Path Resolution Tests (Security Critical)
;;; ============================================================================
;;; ABOUTME: Tests for resolve-project-path security boundary checking.
;;; This function is the core security mechanism - all filesystem tools
;;; call it to validate paths stay within the project root.

(test resolve-project-path-relative
  "resolve-project-path should resolve relative paths within project"
  (let ((agent-q:*project-root* (asdf:system-source-directory :agent-q)))
    (let ((resolved (agent-q::resolve-project-path "src/config.lisp")))
      (is (not (null resolved)))
      (is (pathnamep resolved))
      ;; Should be within project root
      (is (uiop:subpathp resolved agent-q:*project-root*)))))

(test resolve-project-path-rejects-traversal
  "resolve-project-path should reject path traversal attacks"
  (let ((agent-q:*project-root* (asdf:system-source-directory :agent-q)))
    (is (null (agent-q::resolve-project-path "../../../etc/passwd")))
    (is (null (agent-q::resolve-project-path "/etc/passwd")))
    (is (null (agent-q::resolve-project-path "src/../../outside")))))

(test resolve-project-path-allows-current-dir
  "resolve-project-path should allow . and empty paths"
  (let ((agent-q:*project-root* (asdf:system-source-directory :agent-q)))
    (is (not (null (agent-q::resolve-project-path "."))))
    (is (not (null (agent-q::resolve-project-path ""))))))

(test resolve-project-path-rejects-sibling-directories
  "resolve-project-path should reject sibling directories with similar names.
   This tests for a critical vulnerability: /project vs /project-secrets.
   A naive string prefix check would allow /project-secrets when root is /project."
  (let* ((agent-q:*project-root* (asdf:system-source-directory :agent-q))
         ;; Construct a sibling path that starts with the same prefix
         (parent (uiop:pathname-parent-directory-pathname agent-q:*project-root*))
         (root-dir-name (car (last (pathname-directory agent-q:*project-root*))))
         (sibling-name (concatenate 'string root-dir-name "-secrets"))
         (sibling (merge-pathnames (make-pathname :directory (list :relative sibling-name))
                                   parent)))
    ;; Even if the sibling doesn't exist, we can test the string logic
    ;; by checking that our root + "-secrets" suffix would be rejected
    (is (null (agent-q::resolve-project-path (namestring sibling)))
        "Should reject sibling directories that share prefix with project root")))

;;; ============================================================================
;;; list_directory Tool Tests
;;; ============================================================================
;;; ABOUTME: Tests for the list_directory tool which lists files and directories
;;; within the project root boundary. Uses resolve-project-path for security.

(test list-directory-tool-exists
  "list_directory tool should be registered"
  (let ((tool (find-tool-definition "list_directory")))
    (is (not (null tool)))
    (is (equal (tool-name tool) "list_directory"))))

(test list-directory-is-safe
  "list_directory should have :safe safety level"
  ;; Should appear in safe tools list
  (let ((safe-tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    (is (find "list_directory" safe-tools :test #'equal :key #'tool-name))))

(test list-directory-has-required-parameters
  "list_directory should require the path parameter"
  (let ((tool (find-tool-definition "list_directory")))
    (is (not (null tool)))
    (let ((required (tool-required tool)))
      (is (member "path" required :test #'equal)))))

(test list-directory-has-description
  "list_directory should have a description mentioning directory listing"
  (let ((tool (find-tool-definition "list_directory")))
    (is (not (null tool)))
    (is (search "directory" (tool-description tool) :test #'char-equal))))

;;; ============================================================================
;;; get_file_info Tool Tests
;;; ============================================================================
;;; ABOUTME: Tests for the get_file_info tool which retrieves file metadata
;;; (size, timestamps, permissions) within the project root boundary.

(test get-file-info-tool-exists
  "get_file_info tool should be registered"
  (let ((tool (find-tool-definition "get_file_info")))
    (is (not (null tool)))
    (is (equal (tool-name tool) "get_file_info"))))

(test get-file-info-is-safe
  "get_file_info should have :safe safety level"
  (let ((safe-tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    (is (find "get_file_info" safe-tools :test #'equal :key #'tool-name))))

(test get-file-info-has-required-parameters
  "get_file_info should require the path parameter"
  (let ((tool (find-tool-definition "get_file_info")))
    (is (not (null tool)))
    (let ((required (tool-required tool)))
      (is (member "path" required :test #'equal)))))

(test get-file-info-has-description
  "get_file_info should have a description mentioning file information"
  (let ((tool (find-tool-definition "get_file_info")))
    (is (not (null tool)))
    (is (search "information" (tool-description tool) :test #'char-equal))))

;;; ============================================================================
;;; get_project_root Tool Tests
;;; ============================================================================
;;; ABOUTME: Tests for the get_project_root tool which reports the current
;;; project root directory and how it was detected.

(test get-project-root-tool-exists
  "get_project_root tool should be registered"
  (let ((tool (find-tool-definition "get_project_root")))
    (is (not (null tool)))
    (is (equal (tool-name tool) "get_project_root"))))

(test get-project-root-is-safe
  "get_project_root should have :safe safety level"
  (let ((safe-tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    (is (find "get_project_root" safe-tools :test #'equal :key #'tool-name))))

(test get-project-root-has-no-required-parameters
  "get_project_root should not require any parameters"
  (let ((tool (find-tool-definition "get_project_root")))
    (is (not (null tool)))
    (let ((required (tool-required tool)))
      (is (or (null required) (= 0 (length required)))))))

(test get-project-root-returns-path
  "get_project_root should return current project root"
  (let ((handler (find-tool-handler "get_project_root")))
    (is (not (null handler)))
    (let ((result (funcall handler (make-hash-table :test 'equal))))
      (is (stringp result))
      (is (search "Project root" result)))))

(test get-project-root-includes-detection-method
  "get_project_root should include how the root was determined"
  (let ((handler (find-tool-handler "get_project_root")))
    (let ((result (funcall handler (make-hash-table :test 'equal))))
      (is (stringp result))
      (is (search "Detection method" result)))))

;;; ============================================================================
;;; Edit Helper Function Tests
;;; ============================================================================
;;; ABOUTME: Tests for count-substring helper used by edit_file tool.
;;; The count-substring function counts non-overlapping occurrences of a
;;; substring, used to determine if old_str is unique for replacement.

(test count-substring-basic
  "count-substring should count occurrences"
  (is (= 0 (agent-q.tools::count-substring "foo" "bar baz")))
  (is (= 1 (agent-q.tools::count-substring "foo" "foo bar")))
  (is (= 3 (agent-q.tools::count-substring "foo" "foo foo foo"))))

(test count-substring-whitespace-sensitive
  "count-substring should be whitespace sensitive"
  (is (= 0 (agent-q.tools::count-substring "foo bar" "foo  bar")))  ; 2 spaces vs 1
  (is (= 1 (agent-q.tools::count-substring "foo  bar" "foo  bar")))) ; exact match

(test count-substring-multiline
  "count-substring should handle multiline strings"
  (is (= 2 (agent-q.tools::count-substring "def" "def foo
def bar"))))

(test count-substring-overlapping
  "count-substring should count non-overlapping occurrences"
  ;; "aa" in "aaa" should be 1, not 2 (non-overlapping)
  (is (= 1 (agent-q.tools::count-substring "aa" "aaa"))))

;;; ============================================================================
;;; edit_file Tool Tests
;;; ============================================================================
;;; ABOUTME: Tests for the edit_file tool which performs targeted string
;;; replacement in files. Uses str_replace semantics requiring unique matches.

(test edit-file-tool-exists
  "edit_file tool should be registered"
  ;; edit_file is :moderate, need to query with that safety level
  (let* ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :moderate))
         (tool (find "edit_file" tools :test #'equal :key #'tool-name)))
    (is (not (null tool)))
    (is (equal (tool-name tool) "edit_file"))))

(test edit-file-is-moderate
  "edit_file should have :moderate safety level (not :safe)"
  ;; Should appear with :moderate level but not :safe
  (let ((moderate-tools (agent-q.tools:get-agent-q-tools :max-safety-level :moderate))
        (safe-tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    (is (find "edit_file" moderate-tools :test #'equal :key #'tool-name))
    (is (not (find "edit_file" safe-tools :test #'equal :key #'tool-name)))))

(test edit-file-has-required-parameters
  "edit_file should require path, old_str, and new_str"
  (let* ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :moderate))
         (tool (find "edit_file" tools :test #'equal :key #'tool-name)))
    (is (not (null tool)))
    (let ((required (tool-required tool)))
      (is (member "path" required :test #'equal))
      (is (member "old_str" required :test #'equal))
      (is (member "new_str" required :test #'equal)))))

(test edit-file-has-description
  "edit_file should have a description mentioning string replacement"
  (let* ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :moderate))
         (tool (find "edit_file" tools :test #'equal :key #'tool-name)))
    (is (not (null tool)))
    (is (search "replacement" (tool-description tool) :test #'char-equal))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-filesystem-tests ()
  "Run all filesystem tests."
  (run! 'filesystem-tests))
