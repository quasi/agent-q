# File System Tools TDD Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement comprehensive file system tools with project root boundary safety

**Architecture:** Tools in `src/tools/filesystem.lisp` using `define-tool` macro, delegating to Emacs via `eval-in-emacs`. All paths validated against `*project-root*` before any operation.

**Tech Stack:** Common Lisp + FiveAM tests + Emacs integration via SLY

---

## Phase 1: Core Navigation

### Task 1: Project Root Configuration

**Files:**
- Modify: `src/config.lisp`
- Test: `tests/filesystem-tests.lisp` (create)

**Step 1: Write the failing test**

Add to `tests/filesystem-tests.lisp`:

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q-TESTS; Base: 10 -*-

(in-package :agent-q-tests)

;;; ============================================================================
;;; Filesystem Tools Tests
;;; ============================================================================

(def-suite filesystem-tests
  :description "Tests for file system tools")

(in-suite filesystem-tests)

(test project-root-variable-exists
  "Project root variable should exist and be configurable"
  (is (boundp 'agent-q:*project-root*))
  ;; Can set it
  (let ((agent-q:*project-root* #P"/tmp/test-project/"))
    (is (pathnamep agent-q:*project-root*))))
```

**Step 2: Run test to verify it fails**

Run: `(asdf:test-system :agent-q)` or in REPL:
```lisp
(ql:quickload :agent-q/tests)
(5am:run! 'agent-q-tests::filesystem-tests)
```
Expected: FAIL with "AGENT-Q:*PROJECT-ROOT* not bound"

**Step 3: Write minimal implementation**

Add to `src/config.lisp`:

```lisp
(defvar *project-root* nil
  "Root directory for file operations. Auto-detected if NIL.
   All file system tools validate paths against this boundary.")
```

**Step 4: Export from package.lisp**

In `src/package.lisp`, add to exports:

```lisp
#:*project-root*
```

**Step 5: Run test to verify it passes**

Run: `(5am:run! 'agent-q-tests::filesystem-tests)`
Expected: PASS

**Step 6: Commit**

```bash
git add src/config.lisp src/package.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add *project-root* configuration variable"
```

---

### Task 2: Project Root Auto-Detection

**Files:**
- Modify: `src/config.lisp`
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write the failing test**

```lisp
(test find-git-root-detects-git-directory
  "find-git-root should find .git directory"
  ;; Use the actual agent-q directory which has .git
  (let ((root (agent-q::find-git-root (asdf:system-source-directory :agent-q))))
    (is (not (null root)))
    (is (probe-file (merge-pathnames ".git/" root)))))

(test detect-project-root-uses-git
  "detect-project-root should prefer git root"
  (let ((detected (agent-q::detect-project-root)))
    (is (not (null detected)))
    (is (pathnamep detected))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL with "AGENT-Q::FIND-GIT-ROOT undefined"

**Step 3: Write minimal implementation**

Add to `src/config.lisp`:

```lisp
(defun find-git-root (start-dir)
  "Walk up from START-DIR looking for .git directory."
  (when start-dir
    (let* ((truename (ignore-errors (truename start-dir)))
           (dir (when truename (pathname-directory truename))))
      (when dir
        (loop for i from (length dir) downto 1
              for parent = (make-pathname :directory (subseq dir 0 i)
                                         :defaults truename)
              when (probe-file (merge-pathnames ".git/" parent))
              return parent)))))

(defun detect-project-root ()
  "Auto-detect project root using available heuristics.
   Priority: 1. Git root, 2. ASDF system, 3. SLYNK default, 4. CWD"
  (or
   ;; 1. Git repository root
   (find-git-root *default-pathname-defaults*)
   ;; 2. ASDF system root (if agent-q loaded)
   (ignore-errors (asdf:system-source-directory :agent-q))
   ;; 3. SLY/SLYNK default directory
   (let ((slynk (find-package :slynk)))
     (when slynk
       (let ((default-dir (find-symbol "DEFAULT-DIRECTORY" slynk)))
         (when (and default-dir (fboundp default-dir))
           (ignore-errors (funcall default-dir))))))
   ;; 4. Current working directory
   *default-pathname-defaults*))
```

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Commit**

```bash
git add src/config.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add project root auto-detection"
```

---

### Task 3: Path Resolution with Boundary Checking

**Files:**
- Modify: `src/config.lisp`
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write the failing tests**

```lisp
(test resolve-project-path-relative
  "resolve-project-path should resolve relative paths within project"
  (let ((agent-q:*project-root* #P"/tmp/test-project/"))
    (let ((resolved (agent-q::resolve-project-path "src/foo.lisp")))
      (is (not (null resolved)))
      (is (uiop:subpathp resolved agent-q:*project-root*)))))

(test resolve-project-path-rejects-traversal
  "resolve-project-path should reject path traversal attacks"
  (let ((agent-q:*project-root* #P"/tmp/test-project/"))
    (is (null (agent-q::resolve-project-path "../../../etc/passwd")))
    (is (null (agent-q::resolve-project-path "/etc/passwd")))
    (is (null (agent-q::resolve-project-path "src/../../outside")))))

(test resolve-project-path-allows-current-dir
  "resolve-project-path should allow . and empty paths"
  (let ((agent-q:*project-root* #P"/tmp/test-project/"))
    (is (not (null (agent-q::resolve-project-path "."))))
    (is (not (null (agent-q::resolve-project-path ""))))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL with "AGENT-Q::RESOLVE-PROJECT-PATH undefined"

**Step 3: Write minimal implementation**

Add to `src/config.lisp`:

```lisp
(defun ensure-project-root ()
  "Ensure *project-root* is set, auto-detecting if needed.
   Returns the project root pathname."
  (or *project-root*
      (setf *project-root* (detect-project-root))
      (error "No project root configured and auto-detection failed.")))

(defun resolve-project-path (path)
  "Resolve PATH relative to project root.
   Returns canonical absolute path if within boundary.
   Returns NIL if path is outside project root (SECURITY)."
  (handler-case
      (let* ((root (ensure-project-root))
             (root-truename (truename root))
             (root-str (namestring root-truename))
             ;; Merge relative paths with root
             (merged (if (uiop:absolute-pathname-p path)
                        (pathname path)
                        (merge-pathnames path root-truename)))
             ;; Resolve symlinks and canonicalize
             (resolved (ignore-errors (truename merged))))
        (when resolved
          (let ((resolved-str (namestring resolved)))
            ;; Security check: must start with root
            (when (and (>= (length resolved-str) (length root-str))
                      (string= root-str (subseq resolved-str 0 (length root-str))))
              resolved))))
    (error () nil)))
```

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Commit**

```bash
git add src/config.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add path resolution with boundary checking"
```

---

### Task 4: Create filesystem.lisp and list_directory Tool

**Files:**
- Create: `src/tools/filesystem.lisp`
- Modify: `agent-q.asd`
- Modify: `src/tools/package.lisp`
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write the failing test**

```lisp
(def-suite filesystem-tools-tests
  :description "Tests for filesystem tools")

(in-suite filesystem-tools-tests)

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
```

**Step 2: Run test to verify it fails**

Expected: FAIL with "list_directory" not found in tools

**Step 3: Create filesystem.lisp**

Create `src/tools/filesystem.lisp`:

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q.TOOLS; Base: 10 -*-

(in-package :agent-q.tools)

;;; Filesystem Tools
;;;
;;; Tools for file system navigation and manipulation.
;;; All paths are validated against *project-root* for safety.

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun format-file-size (bytes)
  "Format BYTES as human-readable size."
  (cond
    ((null bytes) "?")
    ((< bytes 1024) (format nil "~D B" bytes))
    ((< bytes (* 1024 1024)) (format nil "~,1F KB" (/ bytes 1024.0)))
    ((< bytes (* 1024 1024 1024)) (format nil "~,1F MB" (/ bytes (* 1024.0 1024))))
    (t (format nil "~,1F GB" (/ bytes (* 1024.0 1024 1024))))))

(defun format-directory-listing (path entries sort-by)
  "Format directory listing for LLM consumption."
  (let ((sorted (sort (copy-list entries)
                      (case (intern (string-upcase sort-by) :keyword)
                        (:size (lambda (a b) (> (or (getf a :size) 0)
                                                (or (getf b :size) 0))))
                        (:modified (lambda (a b)
                                    (string> (princ-to-string (getf a :modified))
                                            (princ-to-string (getf b :modified)))))
                        (otherwise (lambda (a b) (string< (getf a :name)
                                                         (getf b :name))))))))
    (with-output-to-string (s)
      (format s "Directory: ~A~%" path)
      (let ((files (count :file sorted :key (lambda (e) (getf e :type))))
            (dirs (count :directory sorted :key (lambda (e) (getf e :type)))))
        (format s "Total: ~D file~:P, ~D director~:@P~%~%" files dirs))
      (dolist (entry sorted)
        (let ((type (getf entry :type))
              (name (getf entry :name))
              (size (getf entry :size)))
          (format s "~A ~A~A~%"
                  (if (eq type :directory) "[DIR] " "[FILE]")
                  name
                  (if (and (eq type :file) size)
                      (format nil "~40T(~A)" (format-file-size size))
                      "")))))))

;;; ============================================================================
;;; list_directory Tool
;;; ============================================================================

(let ((tool (define-tool
              "list_directory"
              "List files and subdirectories in a directory. Returns names, types, and sizes.
               All paths are validated against the project root boundary."
              '((:name "path" :type :string :description "Directory path (relative to project root)")
                (:name "show_hidden" :type :boolean :description "Include hidden files (default false)")
                (:name "sort_by" :type :string :description "Sort by: name, size, modified (default: name)"))
              :required '("path")
              :safety-level :safe
              :categories '(:filesystem :navigation)
              :handler (lambda (args)
                         (let* ((path (gethash "path" args))
                                (show-hidden (gethash "show_hidden" args))
                                (sort-by (or (gethash "sort_by" args) "name"))
                                (resolved (agent-q::resolve-project-path path)))
                           (unless resolved
                             (return-from list-directory-handler
                               (format nil "Error: Path '~A' is outside project root (~A)"
                                      path (namestring (agent-q::ensure-project-root)))))
                           (handler-case
                               (let ((entries (eval-in-emacs
                                              `(let* ((dir ,(namestring resolved))
                                                      (files (directory-files-and-attributes dir nil nil t)))
                                                 (cl-loop for (name . attrs) in files
                                                          unless (member name '("." "..") :test #'string=)
                                                          unless (and (not ,show-hidden)
                                                                     (string-prefix-p "." name))
                                                          collect (list :name name
                                                                       :type (if (eq (file-attribute-type attrs) t)
                                                                                :directory :file)
                                                                       :size (file-attribute-size attrs)
                                                                       :modified (format-time-string
                                                                                 "%Y-%m-%d %H:%M"
                                                                                 (file-attribute-modification-time attrs))))))))
                                 (if (and (stringp entries) (search "Error" entries))
                                     entries
                                     (format-directory-listing (namestring resolved) entries sort-by)))
                             (error (e)
                               (format nil "Error listing directory: ~A" e))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Update agent-q.asd**

Add `(:file "filesystem")` after `(:file "diff")` in the tools module.

**Step 5: Run test to verify it passes**

Expected: PASS

**Step 6: Commit**

```bash
git add src/tools/filesystem.lisp agent-q.asd tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add list_directory tool"
```

---

### Task 5: get_file_info Tool

**Files:**
- Modify: `src/tools/filesystem.lisp`
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write the failing test**

```lisp
(test get-file-info-tool-exists
  "get_file_info tool should be registered"
  (let ((tool (find-tool-definition "get_file_info")))
    (is (not (null tool)))
    (is (equal (tool-name tool) "get_file_info"))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL

**Step 3: Add get_file_info to filesystem.lisp**

```lisp
;;; ============================================================================
;;; get_file_info Tool
;;; ============================================================================

(let ((tool (define-tool
              "get_file_info"
              "Get detailed information about a file or directory including size, timestamps, and permissions."
              '((:name "path" :type :string :description "File or directory path (relative to project root)"))
              :required '("path")
              :safety-level :safe
              :categories '(:filesystem :navigation)
              :handler (lambda (args)
                         (let* ((path (gethash "path" args))
                                (resolved (agent-q::resolve-project-path path)))
                           (unless resolved
                             (return-from get-file-info-handler
                               (format nil "Error: Path '~A' is outside project root" path)))
                           (handler-case
                               (let ((info (eval-in-emacs
                                           `(let* ((path ,(namestring resolved))
                                                   (attrs (file-attributes path)))
                                              (when attrs
                                                (list :path path
                                                     :type (if (eq (file-attribute-type attrs) t)
                                                              :directory :file)
                                                     :size (file-attribute-size attrs)
                                                     :modified (format-time-string
                                                               "%Y-%m-%d %H:%M:%S"
                                                               (file-attribute-modification-time attrs))
                                                     :accessed (format-time-string
                                                               "%Y-%m-%d %H:%M:%S"
                                                               (file-attribute-access-time attrs))
                                                     :permissions (file-attribute-modes attrs)
                                                     :readable (file-readable-p path)
                                                     :writable (file-writable-p path)))))))
                                 (if info
                                     (with-output-to-string (s)
                                       (format s "File: ~A~%~%" (getf info :path))
                                       (format s "Type: ~A~%" (getf info :type))
                                       (format s "Size: ~A~%" (format-file-size (getf info :size)))
                                       (format s "Modified: ~A~%" (getf info :modified))
                                       (format s "Accessed: ~A~%" (getf info :accessed))
                                       (format s "Readable: ~A~%" (if (getf info :readable) "Yes" "No"))
                                       (format s "Writable: ~A~%" (if (getf info :writable) "Yes" "No")))
                                     (format nil "Error: File '~A' not found" path)))
                             (error (e)
                               (format nil "Error getting file info: ~A" e))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add get_file_info tool"
```

---

### Task 6: get_project_root Tool

**Files:**
- Modify: `src/tools/filesystem.lisp`
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write the failing test**

```lisp
(test get-project-root-tool-exists
  "get_project_root tool should be registered"
  (let ((tool (find-tool-definition "get_project_root")))
    (is (not (null tool)))))

(test get-project-root-returns-path
  "get_project_root should return current project root"
  (let ((handler (find-tool-handler "get_project_root")))
    (let ((result (funcall handler (make-hash-table :test 'equal))))
      (is (stringp result))
      (is (search "Project root" result)))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL

**Step 3: Add get_project_root to filesystem.lisp**

```lisp
;;; ============================================================================
;;; get_project_root Tool
;;; ============================================================================

(let ((tool (define-tool
              "get_project_root"
              "Get the current project root directory and how it was determined."
              '()
              :required '()
              :safety-level :safe
              :categories '(:filesystem :configuration)
              :handler (lambda (args)
                         (declare (ignore args))
                         (let* ((root (agent-q::ensure-project-root))
                                (method (cond
                                         (agent-q:*project-root* "Explicitly configured")
                                         ((agent-q::find-git-root *default-pathname-defaults*)
                                          "Git repository root (.git directory)")
                                         ((ignore-errors (asdf:system-source-directory :agent-q))
                                          "ASDF system directory")
                                         (t "Default directory"))))
                           (format nil "Project root: ~A~%~%Detection method: ~A"
                                  (namestring root) method))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add get_project_root tool"
```

---

## Phase 2: Targeted Editing (edit_file)

### Task 7: String Matching Helper

**Files:**
- Modify: `src/tools/filesystem.lisp`
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write the failing test**

```lisp
(test count-substring-basic
  "count-substring should count occurrences"
  (is (= 0 (agent-q.tools::count-substring "foo" "bar baz")))
  (is (= 1 (agent-q.tools::count-substring "foo" "foo bar")))
  (is (= 3 (agent-q.tools::count-substring "foo" "foo foo foo"))))

(test count-substring-whitespace-sensitive
  "count-substring should be whitespace sensitive"
  (is (= 0 (agent-q.tools::count-substring "foo bar" "foo  bar")))  ; 2 spaces
  (is (= 1 (agent-q.tools::count-substring "foo  bar" "foo  bar"))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL

**Step 3: Write minimal implementation**

Add to filesystem.lisp:

```lisp
(defun count-substring (needle haystack)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK.
   Matching is exact (whitespace-sensitive)."
  (let ((count 0)
        (pos 0)
        (needle-len (length needle)))
    (loop
      (let ((found (search needle haystack :start2 pos)))
        (unless found (return count))
        (incf count)
        (setf pos (+ found needle-len))))))
```

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add count-substring helper for edit matching"
```

---

### Task 8: edit_file Tool

**Files:**
- Modify: `src/tools/filesystem.lisp`
- Test: `tests/filesystem-tests.lisp`

**Step 1: Write the failing test**

```lisp
(test edit-file-tool-exists
  "edit_file tool should be registered"
  (let ((tool (find-tool-definition "edit_file")))
    (is (not (null tool)))
    (is (equal (tool-name tool) "edit_file"))))

(test edit-file-is-cautious
  "edit_file should have :cautious safety level"
  ;; Should appear with :cautious level
  (let ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :cautious)))
    (is (find "edit_file" tools :test #'equal :key #'tool-name))))
```

**Step 2: Run test to verify it fails**

Expected: FAIL

**Step 3: Add edit_file to filesystem.lisp**

```lisp
;;; ============================================================================
;;; File Content Helpers
;;; ============================================================================

(defun read-file-content (path)
  "Read file content via Emacs."
  (eval-in-emacs
   `(with-temp-buffer
      (insert-file-contents ,path)
      (buffer-string))))

(defun write-file-content (path content)
  "Write content to file via Emacs, syncing any open buffers."
  (eval-in-emacs
   `(let ((buf (find-buffer-visiting ,path)))
      (if buf
          ;; File is open - modify the buffer
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert ,content)
              (save-buffer)))
        ;; File not open - write directly
        (with-temp-file ,path
          (insert ,content)))
      t)))

(defun generate-mini-diff (old-str new-str)
  "Generate a minimal diff display for the change."
  (with-output-to-string (s)
    (format s "~%Diff:~%")
    (dolist (line (uiop:split-string old-str :separator '(#\Newline)))
      (format s "  - ~A~%" line))
    (dolist (line (uiop:split-string new-str :separator '(#\Newline)))
      (format s "  + ~A~%" line))))

(defun truncate-for-display (str max-len)
  "Truncate string for display if too long."
  (if (> (length str) max-len)
      (concatenate 'string (subseq str 0 (- max-len 3)) "...")
      str))

;;; ============================================================================
;;; edit_file Tool
;;; ============================================================================

(let ((tool (define-tool
              "edit_file"
              "Make targeted edits to a file using exact string replacement.
               The old_str must match EXACTLY once in the file.
               For multiple changes, call multiple times.
               WARNING: This modifies files. Changes are logged."
              '((:name "path" :type :string :description "File path (relative to project root)")
                (:name "old_str" :type :string :description "Exact string to find (must be unique)")
                (:name "new_str" :type :string :description "Replacement string")
                (:name "description" :type :string :description "What this change does (for logging)"))
              :required '("path" "old_str" "new_str")
              :safety-level :cautious
              :categories '(:filesystem :editing)
              :handler (lambda (args)
                         (let* ((path (gethash "path" args))
                                (old-str (gethash "old_str" args))
                                (new-str (gethash "new_str" args))
                                (description (gethash "description" args))
                                (resolved (agent-q::resolve-project-path path)))
                           ;; Validate path
                           (unless resolved
                             (return-from edit-file-handler
                               (format nil "Error: Path '~A' is outside project root" path)))

                           (handler-case
                               (let* ((content (read-file-content (namestring resolved)))
                                      (match-count (count-substring old-str content)))
                                 (cond
                                   ;; No matches
                                   ((zerop match-count)
                                    (format nil "Error: String not found in ~A~%~%Searched for:~%~A"
                                           path (truncate-for-display old-str 200)))

                                   ;; Multiple matches - need more context
                                   ((> match-count 1)
                                    (format nil "Error: Found ~D matches in ~A.~%~%~
                                               Please provide more surrounding context to make a unique match."
                                           match-count path))

                                   ;; Exactly one match - apply edit
                                   (t
                                    (let* ((new-content (cl-ppcre:regex-replace
                                                        (cl-ppcre:quote-meta-chars old-str)
                                                        content
                                                        new-str
                                                        :preserve-case nil))
                                           (diff (generate-mini-diff old-str new-str)))
                                      ;; Write via Emacs
                                      (write-file-content (namestring resolved) new-content)
                                      ;; Return confirmation
                                      (format nil "~A Edit applied to ~A~%~%~@[Change: ~A~%~]~A"
                                             #\HEAVY_CHECK_MARK path description diff)))))
                             (error (e)
                               (format nil "Error editing file: ~A" e))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run test to verify it passes**

Expected: PASS

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add edit_file tool with str_replace semantics"
```

---

### Task 9: Update Test Package and ASDF

**Files:**
- Modify: `tests/package.lisp`
- Modify: `agent-q.asd`

**Step 1: Update tests/package.lisp**

Add to exports:
```lisp
#:filesystem-tests
#:filesystem-tools-tests
```

**Step 2: Update agent-q.asd**

Add `(:file "filesystem-tests")` to the tests module.

**Step 3: Run full test suite**

```bash
(asdf:test-system :agent-q)
```

Expected: All tests pass

**Step 4: Commit**

```bash
git add tests/package.lisp agent-q.asd
git commit -m "chore(tests): add filesystem tests to test suite"
```

---

## Final Verification

### Task 10: End-to-End Integration Test

**Step 1: Manual verification in REPL**

```lisp
;; Load the system
(ql:quickload :agent-q)

;; Check tools are registered
(length (agent-q.tools:get-agent-q-tools))  ; Should be > 18

;; Test list_directory
(let ((handler (agent-q-tests::find-tool-handler "list_directory")))
  (funcall handler (agent-q-tests::plist-to-hash-table '(:path "."))))

;; Test get_project_root
(let ((handler (agent-q-tests::find-tool-handler "get_project_root")))
  (funcall handler (make-hash-table :test 'equal)))
```

**Step 2: Run full test suite**

```lisp
(asdf:test-system :agent-q)
```

Expected: All tests pass including new filesystem tests

**Step 3: Final commit**

```bash
git add -A
git commit -m "feat(filesystem): complete Phase 1 - core navigation tools

Implements:
- *project-root* configuration with auto-detection
- resolve-project-path with boundary checking
- list_directory tool (safe)
- get_file_info tool (safe)
- get_project_root tool (safe)
- edit_file tool (cautious) with str_replace semantics

All paths validated against project root for security.
Canon: canon/features/file-system-tools/"
```

---

## Summary

**Phase 1 Complete - 10 Tasks**

| Task | Description | Safety |
|------|-------------|--------|
| 1 | *project-root* variable | Config |
| 2 | Auto-detection (git, ASDF) | Config |
| 3 | resolve-project-path | Security |
| 4 | list_directory | :safe |
| 5 | get_file_info | :safe |
| 6 | get_project_root | :safe |
| 7 | count-substring helper | Internal |
| 8 | edit_file | :cautious |
| 9 | Test infrastructure | Tests |
| 10 | Integration verification | QA |

**Total estimated time:** 2-3 hours with TDD

**Next phases:** See main plan for Phase 2 (directory_tree, search_files) and Phase 3 (file lifecycle).
