# File System Tools Phase 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement `directory_tree` and `search_files` tools to enable recursive directory navigation and glob-based file searching within the project boundary.

**Architecture:** Two `:safe` tools following existing filesystem.lisp patterns. Both use Emacs integration via `eval-in-emacs`, validate paths against project root, and return human-readable formatted output for LLM consumption.

**Tech Stack:** Common Lisp, Emacs Lisp (for filesystem operations), existing Agent-Q tool system

---

## Task 1: Add directory_tree Helper Functions

**Files:**
- Modify: `src/tools/filesystem.lisp` (after line 32, in helpers section)
- Test: `tests/filesystem-tests.lisp` (will add test in next task)

**Step 1: Write test for recursive directory tree building**

Add to `tests/filesystem-tests.lisp` after the existing `count-substring` tests:

```lisp
(test build-directory-tree-single-level
  "Test building directory tree for a single level"
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
```

**Step 2: Run tests to verify they fail**

```bash
cd /Users/quasi/quasilabs/projects/agent-q
sbcl --load tests/run-tests.lisp
```

Expected: FAIL with "undefined function BUILD-DIRECTORY-TREE"

**Step 3: Implement build-directory-tree helper**

Add to `src/tools/filesystem.lisp` after the `format-directory-listing` function (around line 59):

```lisp
(defun matches-exclusion-p (name exclusions)
  "Check if NAME matches any glob pattern in EXCLUSIONS."
  (when exclusions
    (some (lambda (pattern)
            (cond
              ;; Exact match
              ((not (find #\* pattern))
               (string= name pattern))
              ;; Suffix match (*.fasl)
              ((and (char= (char pattern 0) #\*)
                    (not (find #\* pattern :start 1)))
               (let ((suffix (subseq pattern 1)))
                 (and (>= (length name) (length suffix))
                      (string= suffix name :start2 (- (length name) (length suffix))))))
              ;; Prefix match (TODO*)
              ((and (char= (char pattern (1- (length pattern))) #\*)
                    (not (find #\* pattern :end (1- (length pattern)))))
               (let ((prefix (subseq pattern 0 (1- (length pattern)))))
                 (and (>= (length name) (length prefix))
                      (string= prefix name :end2 (length prefix)))))
              ;; Default: no match for complex patterns
              (t nil)))
          exclusions)))

(defun build-directory-tree (path exclusions)
  "Build a recursive directory tree structure.
   EXCLUSIONS is a list of glob patterns to skip."
  (handler-case
      (let* ((entries (eval-in-emacs
                      `(let* ((dir ,path)
                              (files (directory-files-and-attributes dir nil nil t)))
                         (cl-loop for (name . attrs) in files
                                  unless (member name '("." "..") :test #'string=)
                                  collect (list :name name
                                               :type (if (eq (file-attribute-type attrs) t)
                                                        :directory :file)
                                               :size (file-attribute-size attrs))))))
             (filtered (remove-if (lambda (entry)
                                   (matches-exclusion-p (getf entry :name) exclusions))
                                 entries)))
        (list :name (file-namestring (pathname path))
              :type :directory
              :children (mapcar (lambda (entry)
                                 (if (eq (getf entry :type) :directory)
                                     ;; Recurse into subdirectories
                                     (build-directory-tree
                                      (merge-pathnames
                                       (concatenate 'string (getf entry :name) "/")
                                       path)
                                      exclusions)
                                     ;; Files are leaf nodes
                                     entry))
                               filtered)))
    (error (e)
      (list :error (format nil "~A" e)))))

(defun format-directory-tree (tree &optional (indent 0))
  "Format directory tree as indented text for LLM consumption."
  (with-output-to-string (s)
    (when (getf tree :error)
      (format s "Error: ~A" (getf tree :error))
      (return-from format-directory-tree))

    (let ((spaces (make-string indent :initial-element #\Space))
          (name (getf tree :name))
          (type (getf tree :type))
          (children (getf tree :children)))

      ;; Print current node
      (format s "~A~A~A~%"
              spaces
              (if (eq type :directory) "[DIR]  " "[FILE] ")
              name)

      ;; Recurse for children
      (when children
        (dolist (child children)
          (if (eq (getf child :type) :directory)
              (format s "~A" (format-directory-tree child (+ indent 2)))
              (let ((child-name (getf child :name))
                    (child-size (getf child :size)))
                (format s "~A[FILE] ~A~@[ (~A)~]~%"
                        (make-string (+ indent 2) :initial-element #\Space)
                        child-name
                        (when child-size (format-file-size child-size))))))))))
```

**Step 4: Run tests to verify they pass**

```bash
sbcl --load tests/run-tests.lisp
```

Expected: 3 new tests PASS (build-directory-tree-single-level, build-directory-tree-recursive, build-directory-tree-with-exclusions)

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add build-directory-tree helper with exclusions"
```

---

## Task 2: Implement directory_tree Tool

**Files:**
- Modify: `src/tools/filesystem.lisp` (add after get_project_root tool, around line 192)
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write test for directory_tree tool**

Add to `tests/filesystem-tests.lisp`:

```lisp
(test directory-tree-tool-basic
  "Test directory_tree tool returns formatted tree"
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
             (let ((result (funcall (agent-q.tools::tool-handler tool) args)))
               (is (stringp result))
               (is (search "[DIR]" result))
               (is (search "src" result))
               (is (search "tests" result)))))
      ;; Cleanup
      (uiop:delete-directory-tree agent-q:*project-root* :validate t :if-does-not-exist :ignore))))

(test directory-tree-tool-with-exclusions
  "Test directory_tree excludes patterns"
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
             (let ((result (funcall (agent-q.tools::tool-handler tool) args)))
               (is (not (search ".git" result)))
               (is (not (search "code.fasl" result)))
               (is (search "src" result)))))
      ;; Cleanup
      (uiop:delete-directory-tree agent-q:*project-root* :validate t :if-does-not-exist :ignore))))

(test directory-tree-tool-path-validation
  "Test directory_tree rejects paths outside project root"
  (let ((agent-q:*project-root* (uiop:temporary-directory)))
    (let* ((tool (agent-q.tools::find-tool-definition "directory_tree"))
           (args (make-hash-table :test 'equal)))
      (setf (gethash "path" args) "../../etc")
      (let ((result (funcall (agent-q.tools::tool-handler tool) args)))
        (is (search "Error" result))
        (is (search "outside project root" result))))))
```

**Step 2: Run tests to verify they fail**

```bash
sbcl --load tests/run-tests.lisp
```

Expected: FAIL with "tool directory_tree not found"

**Step 3: Implement directory_tree tool**

Add to `src/tools/filesystem.lisp` after the `get_project_root` tool (around line 192):

```lisp
;;; ============================================================================
;;; directory_tree Tool
;;; ============================================================================

(let ((tool (define-tool
              "directory_tree"
              "Get a recursive directory tree showing the hierarchical structure.
               Useful for understanding project layout. Can exclude patterns like .git, *.fasl."
              '((:name "path" :type :string :description "Directory path (relative to project root, default: '.')")
                (:name "exclude_patterns" :type :array :description "List of glob patterns to exclude (e.g., ['.git', '*.fasl'])"))
              :required '()
              :safety-level :safe
              :categories '(:filesystem :navigation)
              :handler (lambda (args)
                         (block directory-tree-handler
                           (let* ((path (or (gethash "path" args) "."))
                                  (exclusions (gethash "exclude_patterns" args))
                                  (resolved (agent-q::resolve-project-path path)))
                             (unless resolved
                               (return-from directory-tree-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (let* ((tree (build-directory-tree (namestring resolved) exclusions))
                                        (formatted (format-directory-tree tree)))
                                   (with-output-to-string (s)
                                     (format s "Directory tree: ~A~%~%" (namestring resolved))
                                     (when exclusions
                                       (format s "Excluding: ~{~A~^, ~}~%~%" exclusions))
                                     (format s "~A" formatted)))
                               (error (e)
                                 (format nil "Error building directory tree: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run tests to verify they pass**

```bash
sbcl --load tests/run-tests.lisp
```

Expected: 3 new tests PASS

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add directory_tree tool for recursive navigation"
```

---

## Task 3: Add search_files Helper Functions

**Files:**
- Modify: `src/tools/filesystem.lisp` (add after directory tree helpers)
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write tests for glob matching**

Add to `tests/filesystem-tests.lisp`:

```lisp
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

(test search-files-recursively-basic
  "Test recursive file search"
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
```

**Step 2: Run tests to verify they fail**

```bash
sbcl --load tests/run-tests.lisp
```

Expected: FAIL with "undefined function GLOB-MATCHES-P"

**Step 3: Implement glob matching and search helpers**

Add to `src/tools/filesystem.lisp` after the `format-directory-tree` function:

```lisp
(defun glob-matches-p (pattern filename)
  "Check if FILENAME matches glob PATTERN.
   Supports: * (any chars), ** (recursive), ? (single char)."
  (cond
    ;; Exact match (no wildcards)
    ((not (or (find #\* pattern) (find #\? pattern)))
     (string= pattern filename))

    ;; Recursive wildcard **/*.ext
    ((search "**/" pattern)
     (let* ((parts (uiop:split-string pattern :separator "**/"))
            (suffix-pattern (second parts)))
       (and suffix-pattern
            (glob-matches-p suffix-pattern (file-namestring filename)))))

    ;; Simple suffix match *.ext
    ((and (char= (char pattern 0) #\*)
          (not (find #\* pattern :start 1))
          (not (find #\? pattern)))
     (let ((suffix (subseq pattern 1)))
       (and (>= (length filename) (length suffix))
            (string= suffix filename :start2 (- (length filename) (length suffix))))))

    ;; Simple prefix match prefix*
    ((and (char= (char pattern (1- (length pattern))) #\*)
          (not (find #\* pattern :end (1- (length pattern))))
          (not (find #\? pattern)))
     (let ((prefix (subseq pattern 0 (1- (length pattern)))))
       (and (>= (length filename) (length prefix))
            (string= prefix filename :end2 (length prefix)))))

    ;; Question mark (single char)
    ((find #\? pattern)
     ;; Simple implementation: convert ? to . for basic matching
     ;; Full regex implementation would be more complex
     (and (= (length pattern) (length filename))
          (every (lambda (pc fc)
                  (or (char= pc #\?)
                      (char= pc #\*)
                      (char= pc fc)))
                pattern filename)))

    ;; Default: no match
    (t nil)))

(defun search-files-recursively (path pattern exclusions)
  "Recursively search for files matching PATTERN under PATH.
   Returns list of relative paths."
  (let ((results '()))
    (labels ((walk (dir prefix)
               (handler-case
                   (let ((entries (eval-in-emacs
                                  `(let* ((dir ,dir)
                                          (files (directory-files-and-attributes dir nil nil t)))
                                     (cl-loop for (name . attrs) in files
                                              unless (member name '("." "..") :test #'string=)
                                              collect (list :name name
                                                           :type (if (eq (file-attribute-type attrs) t)
                                                                    :directory :file)))))))
                     (dolist (entry entries)
                       (let* ((name (getf entry :name))
                              (type (getf entry :type))
                              (rel-path (if prefix
                                           (concatenate 'string prefix "/" name)
                                           name)))
                         ;; Skip if matches exclusion
                         (unless (matches-exclusion-p name exclusions)
                           (if (eq type :directory)
                               ;; Recurse into subdirectory
                               (walk (merge-pathnames
                                     (concatenate 'string name "/")
                                     dir)
                                    rel-path)
                               ;; Check if file matches pattern
                               (when (glob-matches-p pattern rel-path)
                                 (push rel-path results)))))))
                 (error (e)
                   ;; Silently skip directories we can't read
                   nil))))
      (walk path nil))
    (nreverse results)))
```

**Step 4: Run tests to verify they pass**

```bash
sbcl --load tests/run-tests.lisp
```

Expected: 7 new tests PASS

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add glob matching and recursive file search"
```

---

## Task 4: Implement search_files Tool

**Files:**
- Modify: `src/tools/filesystem.lisp` (add after directory_tree tool)
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write tests for search_files tool**

Add to `tests/filesystem-tests.lisp`:

```lisp
(test search-files-tool-basic
  "Test search_files tool finds matching files"
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

           (let* ((tool (agent-q.tools::find-tool-definition "search_files"))
                  (args (make-hash-table :test 'equal)))
             (setf (gethash "pattern" args) "*.lisp")
             (setf (gethash "path" args) ".")
             (let ((result (funcall (agent-q.tools::tool-handler tool) args)))
               (is (stringp result))
               (is (search "main.lisp" result))
               (is (search "package.lisp" result))
               (is (not (search "readme.md" result))))))
      ;; Cleanup
      (uiop:delete-directory-tree agent-q:*project-root* :validate t :if-does-not-exist :ignore))))

(test search-files-tool-recursive
  "Test search_files with recursive pattern"
  (let ((agent-q:*project-root* (merge-pathnames "test-project/" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; Deep nesting
           (ensure-directories-exist (merge-pathnames "src/tools/" agent-q:*project-root*))
           (with-open-file (s (merge-pathnames "src/tools/buffer.lisp" agent-q:*project-root*)
                              :direction :output)
             (write-string "test" s))

           (let* ((tool (agent-q.tools::find-tool-definition "search_files"))
                  (args (make-hash-table :test 'equal)))
             (setf (gethash "pattern" args) "**/*.lisp")
             (setf (gethash "path" args) ".")
             (let ((result (funcall (agent-q.tools::tool-handler tool) args)))
               (is (search "buffer.lisp" result)))))
      ;; Cleanup
      (uiop:delete-directory-tree agent-q:*project-root* :validate t :if-does-not-exist :ignore))))

(test search-files-tool-no-matches
  "Test search_files when no files match"
  (let ((agent-q:*project-root* (uiop:temporary-directory)))
    (let* ((tool (agent-q.tools::find-tool-definition "search_files"))
           (args (make-hash-table :test 'equal)))
      (setf (gethash "pattern" args) "*.nonexistent")
      (setf (gethash "path" args) ".")
      (let ((result (funcall (agent-q.tools::tool-handler tool) args)))
        (is (search "No files found" result))))))

(test search-files-tool-path-validation
  "Test search_files validates project root"
  (let ((agent-q:*project-root* (uiop:temporary-directory)))
    (let* ((tool (agent-q.tools::find-tool-definition "search_files"))
           (args (make-hash-table :test 'equal)))
      (setf (gethash "pattern" args) "*")
      (setf (gethash "path" args) "../../etc")
      (let ((result (funcall (agent-q.tools::tool-handler tool) args)))
        (is (search "Error" result))
        (is (search "outside project root" result))))))
```

**Step 2: Run tests to verify they fail**

```bash
sbcl --load tests/run-tests.lisp
```

Expected: FAIL with "tool search_files not found"

**Step 3: Implement search_files tool**

Add to `src/tools/filesystem.lisp` after the `directory_tree` tool:

```lisp
;;; ============================================================================
;;; search_files Tool
;;; ============================================================================

(let ((tool (define-tool
              "search_files"
              "Search for files matching a glob pattern. Supports * (wildcard) and ** (recursive).
               Examples: '*.lisp' finds all Lisp files, '**/*.md' finds markdown files recursively."
              '((:name "pattern" :type :string :description "Glob pattern (e.g., '*.lisp', '**/*.md')")
                (:name "path" :type :string :description "Starting directory (relative to project root, default: '.')")
                (:name "exclude_patterns" :type :array :description "Patterns to exclude (e.g., ['.git', '*.fasl'])"))
              :required '("pattern")
              :safety-level :safe
              :categories '(:filesystem :search)
              :handler (lambda (args)
                         (block search-files-handler
                           (let* ((pattern (gethash "pattern" args))
                                  (path (or (gethash "path" args) "."))
                                  (exclusions (gethash "exclude_patterns" args))
                                  (resolved (agent-q::resolve-project-path path)))
                             (unless resolved
                               (return-from search-files-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (let ((results (search-files-recursively
                                                (namestring resolved) pattern exclusions)))
                                   (if results
                                       (with-output-to-string (s)
                                         (format s "Found ~D file~:P matching '~A':~%~%"
                                                (length results) pattern)
                                         (dolist (file results)
                                           (format s "  ~A~%" file)))
                                       (format nil "No files found matching pattern '~A'" pattern)))
                               (error (e)
                                 (format nil "Error searching files: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run tests to verify they pass**

```bash
sbcl --load tests/run-tests.lisp
```

Expected: 4 new tests PASS

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add search_files tool with glob support"
```

---

## Task 5: Update Feature Metadata and Test Suite Integration

**Files:**
- Modify: `tests/core-tests.lisp`
- Read: `canon/features/file-system-tools/feature.yaml` (for reference)

**Step 1: Add new tests to test runner**

Verify `tests/core-tests.lisp` includes filesystem tests (should already be there from Phase 1):

```lisp
;; Should already exist from Phase 1
(format t "~%Running filesystem tests...~%")
(run-filesystem-tests)
```

**Step 2: Run full test suite**

```bash
sbcl --load tests/run-tests.lisp
```

Expected: All tests PASS (27 from Phase 1 + 17 new from Phase 2 = 44 total)

**Step 3: Verify tools are registered**

Test in REPL:

```lisp
(ql:quickload "agent-q")
(agent-q.tools::find-tool-definition "directory_tree")
(agent-q.tools::find-tool-definition "search_files")
```

Expected: Both return tool objects

**Step 4: Document test count**

The test count will be updated when canon-evolve processes the implementation notes.

**Step 5: Commit**

```bash
git add tests/core-tests.lisp
git commit -m "test(filesystem): verify Phase 2 integration with test suite"
```

---

## Task 6: Manual Integration Testing

**Files:**
- None (interactive REPL testing)

**Step 1: Start Agent-Q and test directory_tree**

```lisp
(ql:quickload "agent-q")
(in-package :agent-q)

;; Set project root to agent-q itself
(setf *project-root* (asdf:system-source-directory :agent-q))

;; Test directory_tree
(let* ((tool (agent-q.tools::find-tool-definition "directory_tree"))
       (args (make-hash-table :test 'equal)))
  (setf (gethash "path" args) "src/tools")
  (setf (gethash "exclude_patterns" args) '("*.fasl"))
  (princ (funcall (agent-q.tools::tool-handler tool) args)))
```

Expected: Formatted tree showing src/tools/ structure

**Step 2: Test search_files with various patterns**

```lisp
;; Find all Lisp files
(let* ((tool (agent-q.tools::find-tool-definition "search_files"))
       (args (make-hash-table :test 'equal)))
  (setf (gethash "pattern" args) "**/*.lisp")
  (setf (gethash "path" args) "src")
  (princ (funcall (agent-q.tools::tool-handler tool) args)))
```

Expected: List of all .lisp files under src/

**Step 3: Test with exclusions**

```lisp
;; Find markdown files, excluding canon/
(let* ((tool (agent-q.tools::find-tool-definition "search_files"))
       (args (make-hash-table :test 'equal)))
  (setf (gethash "pattern" args) "**/*.md")
  (setf (gethash "path" args) ".")
  (setf (gethash "exclude_patterns" args) '("canon" ".git"))
  (princ (funcall (agent-q.tools::tool-handler tool) args)))
```

Expected: Markdown files excluding canon/ and .git/

**Step 4: Test path security**

```lisp
;; Try to escape project root
(let* ((tool (agent-q.tools::find-tool-definition "search_files"))
       (args (make-hash-table :test 'equal)))
  (setf (gethash "pattern" args) "*")
  (setf (gethash "path" args) "../../")
  (princ (funcall (agent-q.tools::tool-handler tool) args)))
```

Expected: Error message about path outside project root

**Step 5: Document findings**

Create `docs/implementation/file-system-tools/notes.md` with any discoveries from testing.

---

## Testing Strategy

### Unit Tests (FiveAM)
- Helper functions: `glob-matches-p`, `build-directory-tree`, `search-files-recursively`
- Tool handlers: Both tools with various input combinations
- Edge cases: Empty directories, no matches, path validation
- **Total new tests: 17** (3 tree building + 3 tree tool + 7 glob/search helpers + 4 search tool)

### Integration Tests
- Tool registration verification
- End-to-end with real project structure
- Interaction with existing Phase 1 tools

### Security Tests
- Path traversal attempts
- Symlink boundary checking
- Exclusion pattern validation

---

## Success Criteria

- [ ] `directory_tree` tool returns formatted hierarchical directory listing
- [ ] `search_files` tool finds files matching glob patterns
- [ ] Both tools respect project root boundary (path-safety property)
- [ ] Exclusion patterns work correctly for both tools
- [ ] All 17 new tests pass (44 total for file-system-tools)
- [ ] Tools registered in `*agent-q-registry*`
- [ ] No regressions in Phase 1 tools
- [ ] Manual testing confirms real-world usage works

---

## Notes for Implementation

### Key Design Decisions
1. **Emacs Integration**: Use `eval-in-emacs` for directory traversal (consistent with Phase 1)
2. **Glob Simplicity**: Support common patterns (`*`, `**`, `?`) but not full regex
3. **Exclusions**: Use same pattern matching for both tree building and search
4. **Output Format**: Human-readable text optimized for LLM consumption
5. **Error Handling**: Return formatted strings (not signals) for graceful LLM handling

### Patterns from Existing Code
- Tool definition: `(let ((tool (define-tool ...))) (register-tool ...))`
- Path validation: Always call `resolve-project-path` first
- Emacs RPC: Use `eval-in-emacs` with quoted expressions
- Error format: `"Error: <message>"` strings

### Common Pitfalls to Avoid
- Don't use CL filesystem functions directly (use Emacs)
- Don't forget to validate ALL paths against project root
- Don't assume directories are readable (handle errors gracefully)
- Don't implement full regex (keep glob matching simple)

### References
- Canon spec: `canon/features/file-system-tools/feature.yaml`
- Research: `canon/research/2026-01-20-file-system-tools.research.md`
- Vocabulary: `canon/features/file-system-tools/vocabulary.md`
- Existing implementation: `src/tools/filesystem.lisp` (Phase 1)
- Test patterns: `tests/filesystem-tests.lisp` (Phase 1 examples)
