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
;;; Directory Tree Helper Tests
;;; ============================================================================
;;; ABOUTME: Tests for helper functions used by directory_tree tool.
;;; These include pattern matching, tree building, and formatting.

(defmacro with-temp-directory ((var) &body body)
  "Execute BODY with VAR bound to a temporary directory path.
   Directory is created before and cleaned up after execution."
  `(let ((,var (merge-pathnames
                (make-pathname :directory (list :relative
                                               (format nil "agent-q-test-~A"
                                                      (get-universal-time))))
                (uiop:temporary-directory))))
     (ensure-directories-exist ,var)
     (unwind-protect
          (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,var :validate t)))))

(test matches-exclusion-p-exact-match
  "matches-exclusion-p should match exact filenames"
  (is (agent-q.tools::matches-exclusion-p ".git" '(".git" "node_modules")))
  (is (not (agent-q.tools::matches-exclusion-p ".github" '(".git" "node_modules")))))

(test matches-exclusion-p-suffix-match
  "matches-exclusion-p should match suffix patterns like *.fasl"
  (is (agent-q.tools::matches-exclusion-p "foo.fasl" '("*.fasl")))
  (is (agent-q.tools::matches-exclusion-p "bar.fasl" '("*.fasl")))
  (is (not (agent-q.tools::matches-exclusion-p "foo.lisp" '("*.fasl")))))

(test matches-exclusion-p-prefix-match
  "matches-exclusion-p should match prefix patterns like TODO*"
  (is (agent-q.tools::matches-exclusion-p "TODO.txt" '("TODO*")))
  (is (agent-q.tools::matches-exclusion-p "TODO-notes" '("TODO*")))
  (is (not (agent-q.tools::matches-exclusion-p "FIXME" '("TODO*")))))

(test matches-exclusion-p-empty-list
  "matches-exclusion-p should return nil for empty exclusion list"
  (is (not (agent-q.tools::matches-exclusion-p "anything" nil)))
  (is (not (agent-q.tools::matches-exclusion-p "anything" '()))))

(test build-directory-tree-single-level
  "Test building directory tree for a single level"
  (skip "Requires Emacs connection - integration test only")
  (with-temp-directory (tmpdir)
    ;; Create structure: tmpdir/a.txt, tmpdir/subdir/
    (let ((file1 (merge-pathnames "a.txt" tmpdir))
          (subdir (merge-pathnames "subdir/" tmpdir)))
      (with-open-file (s file1 :direction :output)
        (write-string "test" s))
      (ensure-directories-exist subdir)

      (let ((tree (agent-q.tools::build-directory-tree (namestring tmpdir) nil)))
        (is (equal :directory (getf tree :type)))
        (is (listp (getf tree :children)))
        (is (= 2 (length (getf tree :children))))
        ;; Check file entry exists
        (is (find "a.txt" (getf tree :children) :key (lambda (e) (getf e :name)) :test #'string=))
        ;; Check directory entry exists
        (is (find "subdir" (getf tree :children) :key (lambda (e) (getf e :name)) :test #'string=))))))

(test build-directory-tree-recursive
  "Test building directory tree recursively"
  (skip "Requires Emacs connection - integration test only")
  (with-temp-directory (tmpdir)
    ;; Create nested structure
    (let ((subdir (merge-pathnames "subdir/" tmpdir))
          (nested (merge-pathnames "subdir/nested/" tmpdir))
          (file1 (merge-pathnames "subdir/b.txt" tmpdir)))
      (ensure-directories-exist nested)
      (with-open-file (s file1 :direction :output)
        (write-string "test" s))

      (let ((tree (agent-q.tools::build-directory-tree (namestring tmpdir) nil)))
        ;; Check top level has subdir
        (let ((subdir-entry (find "subdir" (getf tree :children)
                                  :key (lambda (e) (getf e :name)) :test #'string=)))
          (is (not (null subdir-entry)))
          (is (equal :directory (getf subdir-entry :type)))
          ;; Check subdir has children
          (is (= 2 (length (getf subdir-entry :children)))))))))

(test build-directory-tree-with-exclusions
  "Test excluding patterns from tree"
  (skip "Requires Emacs connection - integration test only")
  (with-temp-directory (tmpdir)
    (let ((file1 (merge-pathnames "keep.lisp" tmpdir))
          (file2 (merge-pathnames "ignore.fasl" tmpdir))
          (gitdir (merge-pathnames ".git/" tmpdir)))
      (with-open-file (s file1 :direction :output)
        (write-string "test" s))
      (with-open-file (s file2 :direction :output)
        (write-string "test" s))
      (ensure-directories-exist gitdir)

      (let ((tree (agent-q.tools::build-directory-tree
                   (namestring tmpdir)
                   '("*.fasl" ".git"))))
        ;; Should only have keep.lisp
        (is (= 1 (length (getf tree :children))))
        (is (string= "keep.lisp" (getf (first (getf tree :children)) :name)))))))

(test format-directory-tree-simple
  "Test formatting a simple directory tree"
  (let ((tree (list :name "root"
                    :type :directory
                    :children (list (list :name "file.txt" :type :file :size 100)
                                   (list :name "subdir" :type :directory :children nil)))))
    (let ((output (agent-q.tools::format-directory-tree tree)))
      (is (search "[DIR]  root" output))
      (is (search "[FILE] file.txt" output))
      (is (search "[DIR]  subdir" output)))))

(test format-directory-tree-nested
  "Test formatting nested directory structure"
  (let ((tree (list :name "root"
                    :type :directory
                    :children (list (list :name "subdir"
                                         :type :directory
                                         :children (list (list :name "nested.txt" :type :file :size 50)))))))
    (let ((output (agent-q.tools::format-directory-tree tree)))
      (is (search "[DIR]  root" output))
      (is (search "[DIR]  subdir" output))
      (is (search "[FILE] nested.txt" output))
      ;; Check indentation - nested file should be indented more
      (is (> (search "[FILE] nested.txt" output)
             (search "[DIR]  subdir" output))))))

(test format-directory-tree-with-error
  "Test formatting tree with error"
  (let ((tree (list :error "Access denied")))
    (let ((output (agent-q.tools::format-directory-tree tree)))
      (is (search "Error: Access denied" output)))))

;;; ============================================================================
;;; search_files Helper Function Tests
;;; ============================================================================
;;; ABOUTME: Tests for glob matching and recursive file search helpers.
;;; These functions support the search_files tool with glob patterns and exclusions.

(test glob-match-simple-wildcard
  "Test glob matching with * wildcard"
  (is (agent-q.tools::glob-matches-p "*.lisp" "test.lisp"))
  (is (agent-q.tools::glob-matches-p "*.lisp" "package.lisp"))
  (is (not (agent-q.tools::glob-matches-p "*.lisp" "test.el")))
  (is (not (agent-q.tools::glob-matches-p "*.lisp" "test.lisp.bak"))))

(test glob-match-recursive-wildcard
  "Test glob matching with ** recursive wildcard"
  (is (agent-q.tools::glob-matches-p "**/*.lisp" "src/package.lisp"))
  (is (agent-q.tools::glob-matches-p "**/*.lisp" "src/tools/buffer.lisp"))
  (is (agent-q.tools::glob-matches-p "**/*.lisp" "test.lisp"))
  (is (not (agent-q.tools::glob-matches-p "**/*.lisp" "src/test.el"))))

(test glob-match-prefix
  "Test glob matching with prefix"
  (is (agent-q.tools::glob-matches-p "test-*" "test-main"))
  (is (agent-q.tools::glob-matches-p "test-*" "test-helper"))
  (is (not (agent-q.tools::glob-matches-p "test-*" "main-test"))))

(test glob-match-question-mark
  "Test ? wildcard matches single character (including special chars)"
  (is (agent-q.tools::glob-matches-p "file?.txt" "fileX.txt"))
  (is (agent-q.tools::glob-matches-p "file?.txt" "file1.txt"))
  (is (agent-q.tools::glob-matches-p "file?.txt" "file*.txt"))  ; ? DOES match literal * (glob semantics)
  (is (not (agent-q.tools::glob-matches-p "file?.txt" "fileXY.txt")))  ; ? matches exactly 1 char
  (is (not (agent-q.tools::glob-matches-p "file?.txt" "file.txt"))))  ; ? must match something

(test glob-match-recursive-complex
  "Test ** with complex suffix patterns"
  (is (agent-q.tools::glob-matches-p "**/*.lisp" "project/src/test.lisp"))
  (is (agent-q.tools::glob-matches-p "**/*.lisp" "src/tools/buffer.lisp"))
  (is (agent-q.tools::glob-matches-p "**/*.lisp" "deep/nested/path/file.lisp")))

(test search-files-recursively-basic
  "Test recursive file search"
  (skip "Requires Emacs connection - integration test only")
  (with-temp-directory (tmpdir)
    ;; Create structure
    (let ((file1 (merge-pathnames "test.lisp" tmpdir))
          (subdir (merge-pathnames "src/" tmpdir))
          (file2 (merge-pathnames "src/main.lisp" tmpdir))
          (file3 (merge-pathnames "src/test.el" tmpdir)))
      (with-open-file (s file1 :direction :output) (write-string "test" s))
      (ensure-directories-exist subdir)
      (with-open-file (s file2 :direction :output) (write-string "test" s))
      (with-open-file (s file3 :direction :output) (write-string "test" s))

      (let ((results (agent-q.tools::search-files-recursively
                      (namestring tmpdir) "*.lisp" nil)))
        (is (= 2 (length results)))
        (is (find "test.lisp" results :test #'search))
        (is (find "main.lisp" results :test #'search))))))

(test search-files-with-exclusions
  "Test search respects exclusions"
  (skip "Requires Emacs connection - integration test only")
  (with-temp-directory (tmpdir)
    (let ((gitdir (merge-pathnames ".git/" tmpdir))
          (file1 (merge-pathnames ".git/config" tmpdir))
          (file2 (merge-pathnames "real.txt" tmpdir)))
      (ensure-directories-exist gitdir)
      (with-open-file (s file1 :direction :output) (write-string "test" s))
      (with-open-file (s file2 :direction :output) (write-string "test" s))

      (let ((results (agent-q.tools::search-files-recursively
                      (namestring tmpdir) "*" '(".git"))))
        (is (= 1 (length results)))
        (is (search "real.txt" (first results)))))))

;;; ============================================================================
;;; directory_tree Tool Tests
;;; ============================================================================
;;; ABOUTME: Tests for the directory_tree tool which shows recursive directory
;;; structure. Uses resolve-project-path for security and supports exclusions.

(test directory-tree-tool-exists
  "directory_tree tool should be registered"
  (let ((tool (find-tool-definition "directory_tree")))
    (is (not (null tool)))
    (is (equal (tool-name tool) "directory_tree"))))

(test directory-tree-is-safe
  "directory_tree should have :safe safety level"
  (let ((safe-tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    (is (find "directory_tree" safe-tools :test #'equal :key #'tool-name))))

(test directory-tree-has-no-required-parameters
  "directory_tree should not require any parameters"
  (let ((tool (find-tool-definition "directory_tree")))
    (is (not (null tool)))
    (let ((required (tool-required-params tool)))
      (is (or (null required) (= 0 (length required)))))))

(test directory-tree-has-description
  "directory_tree should have a description mentioning tree or structure"
  (let ((tool (find-tool-definition "directory_tree")))
    (is (not (null tool)))
    (is (or (search "tree" (tool-description tool) :test #'char-equal)
            (search "structure" (tool-description tool) :test #'char-equal)))))

(test directory-tree-tool-path-validation
  "Test directory_tree rejects paths outside project root"
  (let ((agent-q:*project-root* (uiop:temporary-directory)))
    (let* ((tool (find-tool-definition "directory_tree"))
           (args (make-hash-table :test 'equal)))
      (setf (gethash "path" args) "../../etc")
      (let ((result (funcall (tool-handler tool) args)))
        (is (search "Error" result))
        (is (search "outside project root" result))))))

(test directory-tree-tool-basic
  "Test directory_tree tool returns formatted tree"
  (skip "Requires Emacs connection - integration test only")
  (let ((agent-q:*project-root* (merge-pathnames "test-project/" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; Setup test structure
           (ensure-directories-exist (merge-pathnames "src/" agent-q:*project-root*))
           (ensure-directories-exist (merge-pathnames "tests/" agent-q:*project-root*))
           (with-open-file (s (merge-pathnames "src/main.lisp" agent-q:*project-root*)
                              :direction :output)
             (write-string "(defun test ())" s))

           (let* ((tool (agent-q.tools::find-tool-definition "directory_tree"))
                  (args (make-hash-table :test 'equal)))
             (setf (gethash "path" args) ".")
             (let ((result (funcall (tool-handler tool) args)))
               (is (stringp result))
               (is (search "[DIR]" result))
               (is (search "src" result))
               (is (search "tests" result)))))
      ;; Cleanup
      (uiop:delete-directory-tree agent-q:*project-root* :validate t :if-does-not-exist :ignore))))

(test directory-tree-tool-with-exclusions
  "Test directory_tree excludes patterns"
  (skip "Requires Emacs connection - integration test only")
  (let ((agent-q:*project-root* (merge-pathnames "test-project/" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; Setup with .git directory
           (ensure-directories-exist (merge-pathnames ".git/" agent-q:*project-root*))
           (ensure-directories-exist (merge-pathnames "src/" agent-q:*project-root*))
           (with-open-file (s (merge-pathnames "code.fasl" agent-q:*project-root*)
                              :direction :output)
             (write-string "binary" s))

           (let* ((tool (agent-q.tools::find-tool-definition "directory_tree"))
                  (args (make-hash-table :test 'equal)))
             (setf (gethash "path" args) ".")
             (setf (gethash "exclude_patterns" args) (list ".git" "*.fasl"))
             (let ((result (funcall (tool-handler tool) args)))
               (is (not (search ".git" result)))
               (is (not (search "code.fasl" result)))
               (is (search "src" result)))))
      ;; Cleanup
      (uiop:delete-directory-tree agent-q:*project-root* :validate t :if-does-not-exist :ignore))))

;;; ============================================================================
;;; search_files Tool Tests
;;; ============================================================================
;;; ABOUTME: Tests for the search_files tool which exposes file searching to the LLM.
;;; Uses glob patterns with support for * and ** wildcards, respects exclusions.

(test search-files-tool-exists
  "search_files tool should be registered"
  (let ((tool (find-tool-definition "search_files")))
    (is (not (null tool)))
    (is (equal (tool-name tool) "search_files"))))

(test search-files-is-safe
  "search_files should have :safe safety level"
  (let ((safe-tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    (is (find "search_files" safe-tools :test #'equal :key #'tool-name))))

(test search-files-has-required-parameters
  "search_files should require the pattern parameter"
  (let ((tool (find-tool-definition "search_files")))
    (is (not (null tool)))
    (let ((required (tool-required-params tool)))
      (is (member "pattern" required :test #'equal)))))

(test search-files-has-description
  "search_files should have a description mentioning search and glob"
  (let ((tool (find-tool-definition "search_files")))
    (is (not (null tool)))
    (is (or (search "search" (tool-description tool) :test #'char-equal)
            (search "glob" (tool-description tool) :test #'char-equal)))))

(test search-files-tool-basic
  "Test search_files tool finds matching files"
  (skip "Requires Emacs connection - integration test only")
  (let ((agent-q:*project-root* (merge-pathnames "test-project/" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; Setup test structure
           (ensure-directories-exist (merge-pathnames "src/" agent-q:*project-root*))
           (with-open-file (s (merge-pathnames "main.lisp" agent-q:*project-root*)
                              :direction :output)
             (write-string "test" s))
           (with-open-file (s (merge-pathnames "src/package.lisp" agent-q:*project-root*)
                              :direction :output)
             (write-string "test" s))
           (with-open-file (s (merge-pathnames "readme.md" agent-q:*project-root*)
                              :direction :output)
             (write-string "test" s))

           (let* ((tool (find-tool-definition "search_files"))
                  (args (make-hash-table :test 'equal)))
             (setf (gethash "pattern" args) "*.lisp")
             (setf (gethash "path" args) ".")
             (let ((result (funcall (tool-handler tool) args)))
               (is (stringp result))
               (is (search "main.lisp" result))
               (is (search "package.lisp" result))
               (is (not (search "readme.md" result))))))
      ;; Cleanup
      (uiop:delete-directory-tree agent-q:*project-root* :validate t :if-does-not-exist :ignore))))

(test search-files-tool-recursive
  "Test search_files with recursive pattern"
  (skip "Requires Emacs connection - integration test only")
  (let ((agent-q:*project-root* (merge-pathnames "test-project/" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; Deep nesting
           (ensure-directories-exist (merge-pathnames "src/tools/" agent-q:*project-root*))
           (with-open-file (s (merge-pathnames "src/tools/buffer.lisp" agent-q:*project-root*)
                              :direction :output)
             (write-string "test" s))

           (let* ((tool (find-tool-definition "search_files"))
                  (args (make-hash-table :test 'equal)))
             (setf (gethash "pattern" args) "**/*.lisp")
             (setf (gethash "path" args) ".")
             (let ((result (funcall (tool-handler tool) args)))
               (is (search "buffer.lisp" result)))))
      ;; Cleanup
      (uiop:delete-directory-tree agent-q:*project-root* :validate t :if-does-not-exist :ignore))))

(test search-files-tool-no-matches
  "Test search_files when no files match"
  (skip "Requires Emacs connection - integration test only")
  (let ((agent-q:*project-root* (uiop:temporary-directory)))
    (let* ((tool (find-tool-definition "search_files"))
           (args (make-hash-table :test 'equal)))
      (setf (gethash "pattern" args) "*.nonexistent")
      (setf (gethash "path" args) ".")
      (let ((result (funcall (tool-handler tool) args)))
        (is (search "No files found" result))))))

(test search-files-tool-path-validation
  "Test search_files validates project root"
  (let ((agent-q:*project-root* (uiop:temporary-directory)))
    (let* ((tool (find-tool-definition "search_files"))
           (args (make-hash-table :test 'equal)))
      (setf (gethash "pattern" args) "*")
      (setf (gethash "path" args) "../../etc")
      (let ((result (funcall (tool-handler tool) args)))
        (is (search "Error" result))
        (is (search "outside project root" result))))))

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
    (let ((required (tool-required-params tool)))
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
    (let ((required (tool-required-params tool)))
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
    (let ((required (tool-required-params tool)))
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
    (let ((required (tool-required-params tool)))
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
