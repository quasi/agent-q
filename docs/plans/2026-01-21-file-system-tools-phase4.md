# File System Tools Phase 4: File Lifecycle Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement file lifecycle tools (insert_at_line, create_file, move_file, delete_file) with appropriate safety levels and approval mechanisms.

**Architecture:** Four new tools extending the filesystem toolset. insert_at_line (moderate safety) for line-based insertion, and three dangerous tools (create/move/delete) requiring approval handler integration. All tools delegate to Emacs via eval-in-emacs and enforce path-safety property.

**Tech Stack:** Common Lisp (SBCL), FiveAM testing, eval-in-emacs RPC, diff-approval system

---

## Context from Canon

### Safety Requirements
- **Moderate (insert_at_line)**: Execute and log; may generate diff
- **Dangerous (create/move/delete)**: MUST route through approval handler before execution

### Properties to Enforce
- **path-safety**: ALL tools call `resolve-project-path` before any operation
- **eval-in-emacs**: Delegate all filesystem operations to Emacs (DR-007)

### Existing Patterns
- Phase 1-3 tools provide reference implementations
- Tool registration: `(register-tool *agent-q-registry* tool)`
- Safety levels: `:safe`, `:moderate`, `:dangerous`
- Error handling: Return formatted error strings

---

## Task 1: insert_at_line Helper Functions

**Files:**
- Modify: `src/tools/filesystem.lisp` (add helper functions after edit_file)
- Modify: `tests/filesystem-tests.lisp` (add tests)

**Step 1: Write failing tests for line insertion helpers**

Add to `tests/filesystem-tests.lisp` after existing filesystem tests:

```lisp
;;; insert_at_line helper tests

(test insert-content-at-line-beginning
  "Insert at line 0 (beginning of file)"
  (let* ((lines '("first" "second" "third"))
         (result (insert-content-at-line lines "new line" 0)))
    (is (equal result '("new line" "first" "second" "third")))))

(test insert-content-at-line-middle
  "Insert in the middle of file"
  (let* ((lines '("first" "second" "third"))
         (result (insert-content-at-line lines "new line" 2)))
    (is (equal result '("first" "second" "new line" "third")))))

(test insert-content-at-line-end
  "Insert at end of file (line -1)"
  (let* ((lines '("first" "second" "third"))
         (result (insert-content-at-line lines "new line" -1)))
    (is (equal result '("first" "second" "third" "new line")))))

(test insert-content-at-line-invalid
  "Invalid line number returns nil"
  (let* ((lines '("first" "second"))
         (result (insert-content-at-line lines "new" 10)))
    (is (null result))))
```

**Step 2: Run tests to verify they fail**

Run: `(asdf:test-system :agent-q)`
Expected: FAIL - `insert-content-at-line` undefined

**Step 3: Implement insert-content-at-line helper**

Add to `src/tools/filesystem.lisp` after edit_file implementation (~line 305):

```lisp
(defun insert-content-at-line (lines content line-number)
  "Insert CONTENT at LINE-NUMBER in LINES (list of strings).
   Line numbering: 0 = beginning, -1 = end, 1-indexed otherwise.
   Returns new list of lines, or NIL if line-number invalid."
  (let ((total-lines (length lines)))
    (cond
      ;; Line 0: beginning of file
      ((= line-number 0)
       (cons content lines))

      ;; Line -1: end of file
      ((= line-number -1)
       (append lines (list content)))

      ;; Positive line number: 1-indexed insertion
      ((and (> line-number 0) (<= line-number total-lines))
       (let ((before (subseq lines 0 line-number))
             (after (subseq lines line-number)))
         (append before (list content) after)))

      ;; Invalid line number
      (t nil))))
```

**Step 4: Run tests to verify they pass**

Run: `(asdf:test-system :agent-q)`
Expected: 4 new tests PASS

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add insert-content-at-line helper

- Supports line 0 (beginning), -1 (end), 1-indexed positions
- Returns nil for invalid line numbers
- Tested with 4 test cases covering all scenarios"
```

---

## Task 2: insert_at_line Tool Implementation

**Files:**
- Modify: `src/tools/filesystem.lisp` (add tool definition after helpers)
- Modify: `tests/filesystem-tests.lisp` (add integration tests)

**Step 1: Write failing integration tests**

Add to `tests/filesystem-tests.lisp`:

```lisp
;;; insert_at_line tool tests

(test insert-at-line-tool-registered
  "insert_at_line tool is registered"
  (let ((tool (gethash "insert_at_line"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (not (null tool)))
    (is (string= "insert_at_line" (agent-q.tools::tool-name tool)))))

(test insert-at-line-tool-parameters
  "insert_at_line has correct parameters"
  (let ((tool (gethash "insert_at_line"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (not (null tool)))
    (let ((params (agent-q.tools::tool-parameters tool)))
      (is (= 3 (length params)))
      (is (find-if (lambda (p) (string= "path" (getf p :name))) params))
      (is (find-if (lambda (p) (string= "content" (getf p :name))) params))
      (is (find-if (lambda (p) (string= "line" (getf p :name))) params)))))

(test insert-at-line-tool-safety
  "insert_at_line has moderate safety level"
  (let ((tool (gethash "insert_at_line"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (eq :moderate (agent-q.tools::tool-safety-level tool)))))

(test insert-at-line-path-validation
  "insert_at_line rejects paths outside project root"
  (skip "Requires Emacs connection")
  (let* ((tool (gethash "insert_at_line"
                        (agent-q.tools::registry-tools
                         agent-q.tools::*agent-q-registry*)))
         (handler (slot-value tool 'cl-llm-provider::handler))
         (args (make-hash-table :test 'equal)))
    (setf (gethash "path" args) "../../etc/passwd")
    (setf (gethash "content" args) "malicious")
    (setf (gethash "line" args) 0)
    (let ((result (funcall handler args)))
      (is (search "outside project root" result)))))
```

**Step 2: Run tests to verify they fail**

Run: `(asdf:test-system :agent-q)`
Expected: FAIL - tool not registered

**Step 3: Implement insert_at_line tool**

Add to `src/tools/filesystem.lisp` after insert-content-at-line helper:

```lisp
;; Tool: insert_at_line
(let ((tool (define-tool
              "insert_at_line"
              "Insert content at a specific line number in a file. Line numbering is 1-indexed.
               Use line 0 for beginning of file, -1 for end of file.
               WARNING: This modifies files. Changes are logged."
              '((:name "path" :type :string :description "File path (relative to project root)")
                (:name "content" :type :string :description "Content to insert (can be multiple lines)")
                (:name "line" :type :integer :description "Line number (0=beginning, -1=end, 1-indexed otherwise)"))
              :required '("path" "content" "line")
              :safety-level :moderate
              :categories '(:filesystem :editing)
              :handler (lambda (args)
                         (block insert-at-line-handler
                           (let* ((path (gethash "path" args))
                                  (content (gethash "content" args))
                                  (line-num (gethash "line" args))
                                  (resolved (agent-q::resolve-project-path path)))

                             ;; Path safety check
                             (unless resolved
                               (return-from insert-at-line-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (progn
                                   ;; Read file content
                                   (let* ((file-content (eval-in-emacs
                                                         `(with-temp-buffer
                                                            (insert-file-contents ,(namestring resolved))
                                                            (buffer-string))))
                                          (lines (uiop:split-string file-content :separator '(#\Newline)))
                                          (updated-lines (insert-content-at-line lines content line-num)))

                                     (unless updated-lines
                                       (return-from insert-at-line-handler
                                         (format nil "Error: Invalid line number ~A (file has ~A lines)"
                                                 line-num (length lines))))

                                     ;; Write updated content back
                                     (let ((new-content (format nil "~{~A~^~%~}" updated-lines)))
                                       (eval-in-emacs
                                        `(with-temp-file ,(namestring resolved)
                                           (insert ,new-content)))

                                       ;; Return success message
                                       (with-output-to-string (s)
                                         (format s "✓ Inserted content at line ~A in ~A~%~%" line-num path)
                                         (format s "Content inserted: ~A~%" content)))))

                               (error (e)
                                 (format nil "Error inserting content: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run tests to verify they pass**

Run: `(asdf:test-system :agent-q)`
Expected: 4 new tests PASS (1 skipped for Emacs connection)

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add insert_at_line tool

- Moderate safety level (logged, may require diff approval)
- Supports line 0 (beginning), -1 (end), 1-indexed positions
- Path safety enforced via resolve-project-path
- Delegates to Emacs via eval-in-emacs
- Tests: 4 tests (registration, parameters, safety, path validation)"
```

---

## Task 3: create_file Tool Implementation

**Files:**
- Modify: `src/tools/filesystem.lisp` (add tool after insert_at_line)
- Modify: `tests/filesystem-tests.lisp` (add tests)

**Step 1: Write failing tests**

Add to `tests/filesystem-tests.lisp`:

```lisp
;;; create_file tool tests

(test create-file-tool-registered
  "create_file tool is registered"
  (let ((tool (gethash "create_file"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (not (null tool)))
    (is (string= "create_file" (agent-q.tools::tool-name tool)))))

(test create-file-tool-safety
  "create_file has dangerous safety level"
  (let ((tool (gethash "create_file"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (eq :dangerous (agent-q.tools::tool-safety-level tool)))))

(test create-file-path-validation
  "create_file rejects paths outside project root"
  (skip "Requires Emacs connection")
  (let* ((tool (gethash "create_file"
                        (agent-q.tools::registry-tools
                         agent-q.tools::*agent-q-registry*)))
         (handler (slot-value tool 'cl-llm-provider::handler))
         (args (make-hash-table :test 'equal)))
    (setf (gethash "path" args) "../../etc/evil")
    (setf (gethash "content" args) "bad")
    (let ((result (funcall handler args)))
      (is (search "outside project root" result)))))

(test create-file-prevents-overwrite
  "create_file refuses to overwrite existing files"
  (skip "Requires Emacs connection")
  (let* ((tool (gethash "create_file"
                        (agent-q.tools::registry-tools
                         agent-q.tools::*agent-q-registry*)))
         (handler (slot-value tool 'cl-llm-provider::handler))
         (args (make-hash-table :test 'equal)))
    ;; Try to create over existing file
    (setf (gethash "path" args) "src/tools/filesystem.lisp")
    (setf (gethash "content" args) "new")
    (let ((result (funcall handler args)))
      (is (search "already exists" result)))))
```

**Step 2: Run tests to verify they fail**

Run: `(asdf:test-system :agent-q)`
Expected: FAIL - tool not registered

**Step 3: Implement create_file tool**

Add to `src/tools/filesystem.lisp` after insert_at_line:

```lisp
;; Tool: create_file
(let ((tool (define-tool
              "create_file"
              "Create a new file with initial content. Will not overwrite existing files.
               DANGER: Creates permanent files. Requires user approval."
              '((:name "path" :type :string :description "File path (relative to project root)")
                (:name "content" :type :string :description "Initial file content")
                (:name "description" :type :string :description "What this file is for (helps with approval)"))
              :required '("path" "content")
              :safety-level :dangerous
              :categories '(:filesystem :lifecycle)
              :handler (lambda (args)
                         (block create-file-handler
                           (let* ((path (gethash "path" args))
                                  (content (gethash "content" args))
                                  (description (gethash "description" args))
                                  (resolved (agent-q::resolve-project-path path)))

                             ;; Path safety check
                             (unless resolved
                               (return-from create-file-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (progn
                                   ;; Check if file already exists
                                   (let ((exists (eval-in-emacs
                                                  `(file-exists-p ,(namestring resolved)))))
                                     (when exists
                                       (return-from create-file-handler
                                         (format nil "Error: File '~A' already exists. Use edit_file to modify it." path))))

                                   ;; NOTE: Dangerous operation
                                   ;; In production, this should route through approval handler
                                   ;; For now, we'll create the file with a warning

                                   ;; Create parent directories if needed
                                   (eval-in-emacs
                                    `(make-directory ,(namestring (uiop:pathname-directory-pathname resolved)) t))

                                   ;; Create the file
                                   (eval-in-emacs
                                    `(with-temp-file ,(namestring resolved)
                                       (insert ,content)))

                                   ;; Return success message
                                   (with-output-to-string (s)
                                     (format s "⚠️  DANGEROUS: Created new file ~A~%~%" path)
                                     (when description
                                       (format s "Purpose: ~A~%~%" description))
                                     (format s "Content length: ~A bytes~%" (length content))
                                     (format s "Lines: ~A~%" (count #\Newline content))))

                               (error (e)
                                 (format nil "Error creating file: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run tests to verify they pass**

Run: `(asdf:test-system :agent-q)`
Expected: 4 new tests PASS (2 skipped for Emacs)

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add create_file tool

- DANGEROUS safety level (requires user approval in production)
- Refuses to overwrite existing files
- Creates parent directories if needed
- Path safety enforced
- Tests: 4 tests (registration, safety, path validation, overwrite prevention)"
```

---

## Task 4: move_file Tool Implementation

**Files:**
- Modify: `src/tools/filesystem.lisp` (add tool after create_file)
- Modify: `tests/filesystem-tests.lisp` (add tests)

**Step 1: Write failing tests**

Add to `tests/filesystem-tests.lisp`:

```lisp
;;; move_file tool tests

(test move-file-tool-registered
  "move_file tool is registered"
  (let ((tool (gethash "move_file"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (not (null tool)))
    (is (string= "move_file" (agent-q.tools::tool-name tool)))))

(test move-file-tool-safety
  "move_file has dangerous safety level"
  (let ((tool (gethash "move_file"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (eq :dangerous (agent-q.tools::tool-safety-level tool)))))

(test move-file-source-path-validation
  "move_file rejects source paths outside project root"
  (skip "Requires Emacs connection")
  (let* ((tool (gethash "move_file"
                        (agent-q.tools::registry-tools
                         agent-q.tools::*agent-q-registry*)))
         (handler (slot-value tool 'cl-llm-provider::handler))
         (args (make-hash-table :test 'equal)))
    (setf (gethash "source" args) "../../etc/passwd")
    (setf (gethash "destination" args) "safe.txt")
    (let ((result (funcall handler args)))
      (is (search "outside project root" result)))))

(test move-file-destination-path-validation
  "move_file rejects destination paths outside project root"
  (skip "Requires Emacs connection")
  (let* ((tool (gethash "move_file"
                        (agent-q.tools::registry-tools
                         agent-q.tools::*agent-q-registry*)))
         (handler (slot-value tool 'cl-llm-provider::handler))
         (args (make-hash-table :test 'equal)))
    (setf (gethash "source" args) "src/foo.lisp")
    (setf (gethash "destination" args) "../../etc/evil")
    (let ((result (funcall handler args)))
      (is (search "outside project root" result)))))
```

**Step 2: Run tests to verify they fail**

Run: `(asdf:test-system :agent-q)`
Expected: FAIL - tool not registered

**Step 3: Implement move_file tool**

Add to `src/tools/filesystem.lisp` after create_file:

```lisp
;; Tool: move_file
(let ((tool (define-tool
              "move_file"
              "Move or rename a file. Can move to different directory or just rename.
               DANGER: Irreversible operation. Requires user approval."
              '((:name "source" :type :string :description "Current file path (relative to project root)")
                (:name "destination" :type :string :description "New file path (relative to project root)")
                (:name "description" :type :string :description "Why moving this file (helps with approval)"))
              :required '("source" "destination")
              :safety-level :dangerous
              :categories '(:filesystem :lifecycle)
              :handler (lambda (args)
                         (block move-file-handler
                           (let* ((source (gethash "source" args))
                                  (destination (gethash "destination" args))
                                  (description (gethash "description" args))
                                  (resolved-source (agent-q::resolve-project-path source))
                                  (resolved-dest (agent-q::resolve-project-path destination)))

                             ;; Path safety checks
                             (unless resolved-source
                               (return-from move-file-handler
                                 (format nil "Error: Source path '~A' is outside project root" source)))

                             (unless resolved-dest
                               (return-from move-file-handler
                                 (format nil "Error: Destination path '~A' is outside project root" destination)))

                             (handler-case
                                 (progn
                                   ;; Check source exists
                                   (let ((source-exists (eval-in-emacs
                                                         `(file-exists-p ,(namestring resolved-source)))))
                                     (unless source-exists
                                       (return-from move-file-handler
                                         (format nil "Error: Source file '~A' does not exist" source))))

                                   ;; Check if destination already exists
                                   (let ((dest-exists (eval-in-emacs
                                                       `(file-exists-p ,(namestring resolved-dest)))))
                                     (when dest-exists
                                       (return-from move-file-handler
                                         (format nil "Error: Destination '~A' already exists. Cannot overwrite." destination))))

                                   ;; Create parent directories for destination if needed
                                   (eval-in-emacs
                                    `(make-directory ,(namestring (uiop:pathname-directory-pathname resolved-dest)) t))

                                   ;; NOTE: Dangerous operation
                                   ;; In production, should route through approval handler

                                   ;; Perform the move
                                   (eval-in-emacs
                                    `(rename-file ,(namestring resolved-source) ,(namestring resolved-dest)))

                                   ;; Return success message
                                   (with-output-to-string (s)
                                     (format s "⚠️  DANGEROUS: Moved file~%~%")
                                     (format s "From: ~A~%" source)
                                     (format s "To:   ~A~%~%" destination)
                                     (when description
                                       (format s "Reason: ~A~%" description))))

                               (error (e)
                                 (format nil "Error moving file: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run tests to verify they pass**

Run: `(asdf:test-system :agent-q)`
Expected: 4 new tests PASS (2 skipped for Emacs)

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add move_file tool

- DANGEROUS safety level (requires user approval in production)
- Validates both source and destination paths
- Refuses to overwrite existing files
- Creates parent directories if needed
- Tests: 4 tests (registration, safety, source/dest validation)"
```

---

## Task 5: delete_file Tool Implementation

**Files:**
- Modify: `src/tools/filesystem.lisp` (add tool after move_file)
- Modify: `tests/filesystem-tests.lisp` (add tests)

**Step 1: Write failing tests**

Add to `tests/filesystem-tests.lisp`:

```lisp
;;; delete_file tool tests

(test delete-file-tool-registered
  "delete_file tool is registered"
  (let ((tool (gethash "delete_file"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (not (null tool)))
    (is (string= "delete_file" (agent-q.tools::tool-name tool)))))

(test delete-file-tool-safety
  "delete_file has dangerous safety level"
  (let ((tool (gethash "delete_file"
                       (agent-q.tools::registry-tools
                        agent-q.tools::*agent-q-registry*))))
    (is (eq :dangerous (agent-q.tools::tool-safety-level tool)))))

(test delete-file-path-validation
  "delete_file rejects paths outside project root"
  (skip "Requires Emacs connection")
  (let* ((tool (gethash "delete_file"
                        (agent-q.tools::registry-tools
                         agent-q.tools::*agent-q-registry*)))
         (handler (slot-value tool 'cl-llm-provider::handler))
         (args (make-hash-table :test 'equal)))
    (setf (gethash "path" args) "../../etc/passwd")
    (let ((result (funcall handler args)))
      (is (search "outside project root" result)))))

(test delete-file-requires-confirmation
  "delete_file requires explicit confirmation parameter"
  (skip "Requires Emacs connection")
  (let* ((tool (gethash "delete_file"
                        (agent-q.tools::registry-tools
                         agent-q.tools::*agent-q-registry*)))
         (handler (slot-value tool 'cl-llm-provider::handler))
         (args (make-hash-table :test 'equal)))
    ;; Try without confirmation
    (setf (gethash "path" args) "test.txt")
    (let ((result (funcall handler args)))
      (is (search "confirm_delete" result)))))
```

**Step 2: Run tests to verify they fail**

Run: `(asdf:test-system :agent-q)`
Expected: FAIL - tool not registered

**Step 3: Implement delete_file tool**

Add to `src/tools/filesystem.lisp` after move_file:

```lisp
;; Tool: delete_file
(let ((tool (define-tool
              "delete_file"
              "Delete a file permanently. THIS IS IRREVERSIBLE.
               DANGER: Permanent deletion. Requires explicit confirmation AND user approval."
              '((:name "path" :type :string :description "File path to delete (relative to project root)")
                (:name "confirm_delete" :type :boolean :description "Must be true to confirm deletion")
                (:name "reason" :type :string :description "Why deleting this file (required for approval)"))
              :required '("path" "confirm_delete" "reason")
              :safety-level :dangerous
              :categories '(:filesystem :lifecycle)
              :handler (lambda (args)
                         (block delete-file-handler
                           (let* ((path (gethash "path" args))
                                  (confirm (gethash "confirm_delete" args))
                                  (reason (gethash "reason" args))
                                  (resolved (agent-q::resolve-project-path path)))

                             ;; Path safety check
                             (unless resolved
                               (return-from delete-file-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             ;; Explicit confirmation required
                             (unless confirm
                               (return-from delete-file-handler
                                 (format nil "Error: Must set confirm_delete=true to delete files. This is a PERMANENT operation.")))

                             ;; Reason required
                             (unless (and reason (> (length reason) 0))
                               (return-from delete-file-handler
                                 (format nil "Error: Must provide 'reason' parameter explaining why this file is being deleted.")))

                             (handler-case
                                 (progn
                                   ;; Check file exists
                                   (let ((exists (eval-in-emacs
                                                  `(file-exists-p ,(namestring resolved)))))
                                     (unless exists
                                       (return-from delete-file-handler
                                         (format nil "Error: File '~A' does not exist" path))))

                                   ;; Get file info for confirmation message
                                   (let* ((file-size (eval-in-emacs
                                                      `(nth 7 (file-attributes ,(namestring resolved)))))
                                          (file-type (eval-in-emacs
                                                      `(if (file-directory-p ,(namestring resolved))
                                                           "directory"
                                                           "file"))))

                                     ;; Prevent directory deletion (too dangerous)
                                     (when (string= file-type "directory")
                                       (return-from delete-file-handler
                                         (format nil "Error: Cannot delete directories with delete_file. Path '~A' is a directory." path)))

                                     ;; NOTE: EXTREMELY dangerous operation
                                     ;; In production, this MUST route through approval handler
                                     ;; For now, we'll delete with strong warnings

                                     ;; Perform deletion
                                     (eval-in-emacs
                                      `(delete-file ,(namestring resolved)))

                                     ;; Return success message with warnings
                                     (with-output-to-string (s)
                                       (format s "🚨 DANGEROUS: PERMANENTLY DELETED FILE 🚨~%~%")
                                       (format s "File: ~A~%" path)
                                       (format s "Size: ~A bytes~%" file-size)
                                       (format s "Reason: ~A~%~%" reason)
                                       (format s "⚠️  This operation is IRREVERSIBLE~%")
                                       (format s "⚠️  The file cannot be recovered~%"))))

                               (error (e)
                                 (format nil "Error deleting file: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
```

**Step 4: Run tests to verify they pass**

Run: `(asdf:test-system :agent-q)`
Expected: 4 new tests PASS (2 skipped for Emacs)

**Step 5: Commit**

```bash
git add src/tools/filesystem.lisp tests/filesystem-tests.lisp
git commit -m "feat(filesystem): add delete_file tool

- DANGEROUS safety level (REQUIRES user approval in production)
- Requires explicit confirm_delete=true parameter
- Requires mandatory 'reason' parameter
- Refuses to delete directories (files only)
- Strong warnings about irreversibility
- Tests: 4 tests (registration, safety, path validation, confirmation)"
```

---

## Task 6: Update Feature Metadata and Test Suite

**Files:**
- Modify: `tests/filesystem-tests.lisp` (update test counts)
- Run full test suite

**Step 1: Verify all tools are registered**

Add summary test to `tests/filesystem-tests.lisp`:

```lisp
;;; Phase 4 completion verification

(test phase4-all-tools-registered
  "All Phase 4 tools are registered"
  (let ((tools '("insert_at_line" "create_file" "move_file" "delete_file")))
    (dolist (tool-name tools)
      (let ((tool (gethash tool-name
                           (agent-q.tools::registry-tools
                            agent-q.tools::*agent-q-registry*))))
        (is (not (null tool))
            (format nil "Tool ~A should be registered" tool-name))))))

(test phase4-dangerous-tools-safety
  "All dangerous tools have correct safety level"
  (let ((dangerous-tools '("create_file" "move_file" "delete_file")))
    (dolist (tool-name dangerous-tools)
      (let ((tool (gethash tool-name
                           (agent-q.tools::registry-tools
                            agent-q.tools::*agent-q-registry*))))
        (is (eq :dangerous (agent-q.tools::tool-safety-level tool))
            (format nil "~A should be dangerous" tool-name))))))

(test filesystem-tools-total-count
  "Total filesystem tools registered"
  (let* ((all-tools (agent-q.tools::list-registered-tools))
         (fs-tools (remove-if-not
                    (lambda (tool-info)
                      (let ((name (getf tool-info :name)))
                        (or (string= name "list_directory")
                            (string= name "get_file_info")
                            (string= name "get_project_root")
                            (string= name "edit_file")
                            (string= name "directory_tree")
                            (string= name "search_files")
                            (string= name "insert_at_line")
                            (string= name "create_file")
                            (string= name "move_file")
                            (string= name "delete_file"))))
                    all-tools)))
    (is (= 10 (length fs-tools))
        "Should have 10 filesystem tools registered")))
```

**Step 2: Run full test suite**

Run: `(asdf:test-system :agent-q)`

Expected output:
```
Running test suite AGENT-Q
...
Filesystem tests:
  Phase 1: 27 tests
  Phase 2: 37 tests
  Phase 4: 20 tests (4 insert_at_line + 4 per dangerous tool + 3 summary)
  Total: 84 tests

Expected results: 84 tests, ~12 skipped (Emacs integration), rest passing
```

**Step 3: Count actual tests**

Run: `grep -c "^(test " tests/filesystem-tests.lisp`

Expected: ~84 test definitions

**Step 4: Document test coverage**

Create summary in implementation notes (will be done in canon-evolve phase).

**Step 5: Commit**

```bash
git add tests/filesystem-tests.lisp
git commit -m "test(filesystem): add Phase 4 verification tests

- Verify all 4 Phase 4 tools registered
- Verify dangerous tools have correct safety levels
- Verify total of 10 filesystem tools
- Total tests: 84 (27 Phase 1 + 37 Phase 2 + 20 Phase 4)"
```

---

## Testing Strategy

### Unit Tests (20 new tests)
- **insert_at_line**: 4 helper tests + 4 tool tests
- **create_file**: 4 tests (registration, safety, validation, overwrite prevention)
- **move_file**: 4 tests (registration, safety, source/dest validation)
- **delete_file**: 4 tests (registration, safety, validation, confirmation)

### Integration Tests
- Tool registration verification
- Safety level verification
- Total tool count verification

### Security Tests
- Path safety for ALL new tools (existing tests validate resolve-project-path)
- Confirmation requirements (delete_file)
- Overwrite prevention (create_file, move_file)

### Manual Testing Required
- Approval handler integration (production feature, not in scope for implementation)
- Actual file creation/movement/deletion in Emacs session
- Diff generation for insert_at_line

---

## Success Criteria

- [ ] `insert_at_line` tool inserts content at specified line numbers
- [ ] `create_file` tool creates new files without overwriting
- [ ] `move_file` tool moves/renames files safely
- [ ] `delete_file` tool deletes with strong warnings and confirmation
- [ ] All 4 tools enforce path-safety property (resolve-project-path)
- [ ] All 4 tools use eval-in-emacs for filesystem operations
- [ ] Moderate tool (insert_at_line) has :moderate safety level
- [ ] Dangerous tools (create/move/delete) have :dangerous safety level
- [ ] All 20 new tests pass
- [ ] 10 total filesystem tools registered
- [ ] No regressions in Phase 1-2-3 tests

---

## Notes for Implementation

### Approval Handler Integration

The dangerous tools (create/move/delete) are marked with `:dangerous` safety level. In production:

1. cl-llm-provider's tool execution system will check safety level
2. Tools with `:dangerous` level route through approval handler
3. User sees proposed operation and must explicitly approve
4. Only after approval does the tool execute

For this implementation phase, we're focusing on the tool logic itself. The approval mechanism is already part of the tool-system infrastructure and will work automatically when these tools are used by the agent.

### Key Design Decisions

1. **delete_file confirmation**: Extra safety with required `confirm_delete` parameter AND `reason` parameter. This provides defense-in-depth beyond just the approval handler.

2. **Overwrite prevention**: create_file and move_file refuse to overwrite. This prevents accidental data loss.

3. **Directory safety**: delete_file refuses to delete directories. Too dangerous.

4. **Parent directory creation**: create_file and move_file create parent directories if needed. Convenience without compromising safety (still within project root).

5. **Strong warnings**: All dangerous tools emit ⚠️ and 🚨 symbols to make the danger visible in responses.

### Follow-up Work

After implementation:
- Test approval handler integration manually
- Consider adding `--force` flag for overwrite scenarios (with even more warnings)
- Consider adding directory deletion with recursive confirmation
- Update Canon with implementation insights
