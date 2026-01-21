# Agent-Q Phase 3 Specification: The Autonomous Developer

## Overview

**Goal:** Elevate Agent-Q to handle complex multi-step development tasks autonomously, including debugging with the condition system, running tests, tracing execution, and building persistent knowledge.

**Prerequisites:** Phase 2 complete - agent can introspect and execute code.

**Success Criteria:** Agent can handle a request like "add input validation to the API layer and ensure all tests pass" - understanding the codebase, making changes, running tests, fixing failures, and updating knowledge base with learnings.

---

## New Capabilities

| Capability | Description |
|------------|-------------|
| **Condition System** | See restarts, invoke them, understand conditions |
| **Testing** | Run tests, parse results, iterate until passing |
| **Tracing** | Trace function calls, observe behavior |
| **Knowledge Base** | Persist learnings across sessions |
| **Context Summarization** | Manage long conversations efficiently |

---

## File Structure (Additions)

```
agent-q/
├── src/
│   ├── [Phase 1 & 2 files]
│   ├── tools/
│   │   ├── condition-tools.lisp     # Debugger/condition tools
│   │   ├── testing-tools.lisp       # Test framework integration
│   │   └── tracing-tools.lisp       # Tracing tools
│   ├── knowledge/
│   │   ├── schema.lisp              # Database schema
│   │   ├── storage.lisp             # SQLite interface
│   │   └── queries.lisp             # Knowledge queries
│   └── context/
│       └── summarizer.lisp          # Context summarization
```

---

## Condition System Tools

```lisp
;;; tools/condition-tools.lisp

(defvar *debugger-context* nil
  "Current debugger context when in a break.")

(defstruct debugger-context
  condition
  restarts
  backtrace
  timestamp)

(define-tool list-restarts
    (:description "When in the debugger, list all available restarts with descriptions."
     :parameters (:type "object" :properties () :required ())
     :trust-level :safe
     :category :debugging)
  (unless *debugger-context*
    (return-from list-restarts "Not currently in debugger."))
  (with-output-to-string (s)
    (format s "Condition: ~A~%~%Available Restarts:~%"
            (debugger-context-condition *debugger-context*))
    (loop for restart in (debugger-context-restarts *debugger-context*)
          for i from 0
          do (format s "  [~D] ~A~%" i (restart-name restart)))))

(define-tool invoke-restart
    (:description "Invoke a restart by name or index."
     :parameters (:type "object"
                  :properties (:restart (:type "string"
                                        :description "Restart name or index"))
                  :required ("restart"))
     :trust-level :dangerous
     :category :debugging)
  (let* ((restart-spec (getf params :restart))
         (restart (find-restart-by-spec restart-spec)))
    (unless restart
      (return-from invoke-restart
        (format nil "Restart ~A not found" restart-spec)))
    (setf *debugger-context* nil)
    (invoke-restart restart)
    "Restart invoked."))

(define-tool inspect-condition
    (:description "Get detailed information about the current condition."
     :parameters (:type "object" :properties () :required ())
     :trust-level :safe
     :category :debugging)
  (unless *debugger-context*
    (return-from inspect-condition "Not currently in debugger."))
  (let ((condition (debugger-context-condition *debugger-context*)))
    (with-output-to-string (s)
      (format s "Type: ~A~%Message: ~A~%~%Slots:~%"
              (type-of condition) condition)
      (dolist (slot (closer-mop:class-slots (class-of condition)))
        (let ((name (closer-mop:slot-definition-name slot)))
          (format s "  ~A: ~S~%" name
                  (if (slot-boundp condition name)
                      (slot-value condition name)
                      '#:unbound)))))))

(define-tool get-backtrace
    (:description "Get current backtrace/stack trace."
     :parameters (:type "object"
                  :properties (:limit (:type "integer" :description "Max frames"))
                  :required ())
     :trust-level :safe
     :category :debugging)
  (if *debugger-context*
      (debugger-context-backtrace *debugger-context*)
      (capture-backtrace (or (getf params :limit) 20))))
```

---

## Testing Tools

```lisp
;;; tools/testing-tools.lisp

(defvar *test-framework* :auto
  "Test framework: :fiveam, :parachute, or :auto")

(define-tool run-test
    (:description "Run a specific test and return results."
     :parameters (:type "object"
                  :properties (:test (:type "string" :description "Test name")
                               :package (:type "string" :description "Package"))
                  :required ("test"))
     :trust-level :cautious
     :category :testing)
  (let* ((framework (detect-test-framework))
         (test-name (getf params :test))
         (pkg (find-package (or (getf params :package) *package*))))
    (case framework
      (:fiveam (run-fiveam-test test-name pkg))
      (:parachute (run-parachute-test test-name pkg))
      (t "No test framework detected."))))

(define-tool run-test-suite
    (:description "Run all tests in a suite and return comprehensive results."
     :parameters (:type "object"
                  :properties (:suite (:type "string" :description "Suite name")
                               :package (:type "string" :description "Package"))
                  :required ())
     :trust-level :cautious
     :category :testing)
  (let ((framework (detect-test-framework)))
    (case framework
      (:fiveam (run-fiveam-suite (getf params :suite) (getf params :package)))
      (:parachute (run-parachute-suite (getf params :suite) (getf params :package)))
      (t "No test framework detected."))))

(define-tool list-tests
    (:description "List all available tests."
     :parameters (:type "object"
                  :properties (:package (:type "string") :suite (:type "string"))
                  :required ())
     :trust-level :safe
     :category :testing)
  (let ((framework (detect-test-framework)))
    (case framework
      (:fiveam (list-fiveam-tests (getf params :package) (getf params :suite)))
      (:parachute (list-parachute-tests (getf params :package)))
      (t "No test framework detected."))))

;; Implementation helpers
(defun detect-test-framework ()
  (cond
    ((find-package :fiveam) :fiveam)
    ((find-package :parachute) :parachute)
    (t nil)))

(defun run-fiveam-test (test-name pkg)
  (let* ((sym (find-symbol (string-upcase test-name) pkg))
         (output (make-string-output-stream)))
    (unless sym (return-from run-fiveam-test "Test not found."))
    (multiple-value-bind (success results)
        (let ((*standard-output* output))
          (fiveam:run sym))
      (format nil "Test: ~A~%Status: ~A~%Output: ~A"
              test-name (if success "PASSED" "FAILED")
              (get-output-stream-string output)))))

(defun run-fiveam-suite (suite-name pkg-name)
  (let* ((pkg (find-package pkg-name))
         (suite (and suite-name (find-symbol (string-upcase suite-name) pkg))))
    (unless suite (return-from run-fiveam-suite "Suite not found."))
    (multiple-value-bind (success results)
        (fiveam:run suite)
      (format nil "Suite: ~A~%Status: ~A~%Passed: ~D Failed: ~D"
              suite-name (if success "ALL PASSED" "SOME FAILED")
              (count-if (lambda (r) (typep r 'fiveam::test-passed)) results)
              (count-if (lambda (r) (typep r 'fiveam::test-failure)) results)))))
```

---

## Tracing Tools

```lisp
;;; tools/tracing-tools.lisp

(defvar *traced-functions* (make-hash-table :test 'equal))

(define-tool trace-function
    (:description "Start tracing calls to a function."
     :parameters (:type "object"
                  :properties (:function (:type "string" :description "Function name")
                               :package (:type "string" :description "Package"))
                  :required ("function"))
     :trust-level :safe
     :category :tracing)
  (let* ((fn-name (getf params :function))
         (pkg (find-package (or (getf params :package) *package*)))
         (sym (find-symbol (string-upcase fn-name) pkg)))
    (unless (and sym (fboundp sym))
      (return-from trace-function "Function not found."))
    (eval `(trace ,sym))
    (setf (gethash fn-name *traced-functions*) sym)
    (format nil "Now tracing ~A" sym)))

(define-tool untrace-function
    (:description "Stop tracing a function."
     :parameters (:type "object"
                  :properties (:function (:type "string" :description "Function name"))
                  :required ("function"))
     :trust-level :safe
     :category :tracing)
  (let ((sym (gethash (getf params :function) *traced-functions*)))
    (if sym
        (progn (eval `(untrace ,sym))
               (remhash (getf params :function) *traced-functions*)
               (format nil "Stopped tracing ~A" sym))
        "Function was not being traced.")))

(define-tool untrace-all
    (:description "Stop tracing all functions."
     :parameters (:type "object" :properties () :required ())
     :trust-level :safe
     :category :tracing)
  (maphash (lambda (name sym)
             (declare (ignore name))
             (eval `(untrace ,sym)))
           *traced-functions*)
  (let ((count (hash-table-count *traced-functions*)))
    (clrhash *traced-functions*)
    (format nil "Stopped tracing ~D functions" count)))

(define-tool list-traced-functions
    (:description "List currently traced functions."
     :parameters (:type "object" :properties () :required ())
     :trust-level :safe
     :category :tracing)
  (with-output-to-string (s)
    (format s "Traced Functions:~%")
    (if (zerop (hash-table-count *traced-functions*))
        (format s "  (none)")
        (maphash (lambda (name sym)
                   (format s "  ~A (~A)~%" name sym))
                 *traced-functions*))))
```

---

## Knowledge Base

### Database Schema

```lisp
;;; knowledge/schema.lisp

(defparameter *kb-schema*
  "CREATE TABLE IF NOT EXISTS knowledge_entries (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    project TEXT,
    category TEXT NOT NULL,
    title TEXT NOT NULL,
    content TEXT NOT NULL,
    tags TEXT,
    source_file TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    relevance_score REAL DEFAULT 1.0
  );
  
  CREATE INDEX IF NOT EXISTS idx_project ON knowledge_entries(project);
  CREATE INDEX IF NOT EXISTS idx_category ON knowledge_entries(category);
  
  CREATE VIRTUAL TABLE IF NOT EXISTS knowledge_fts USING fts5(
    title, content, tags,
    content='knowledge_entries',
    content_rowid='id'
  );")

;; Categories
;; :pattern      - Code patterns and idioms
;; :bug-fix      - Bug fixes and solutions
;; :architecture - Architecture decisions
;; :convention   - Project conventions
;; :preference   - User preferences
;; :context      - Background context
```

### Storage Interface

```lisp
;;; knowledge/storage.lisp

(defvar *kb-connection* nil)
(defvar *kb-path* (merge-pathnames ".agent-q/kb.db" (user-homedir-pathname)))

(defun kb-init ()
  "Initialize knowledge base."
  (ensure-directories-exist *kb-path*)
  (setf *kb-connection* (sqlite:connect *kb-path*))
  (sqlite:execute-non-query *kb-connection* *kb-schema*))

(defun kb-store (category title content &key project tags source-file)
  "Store a knowledge entry."
  (sqlite:execute-non-query *kb-connection*
    "INSERT INTO knowledge_entries (project, category, title, content, tags, source_file)
     VALUES (?, ?, ?, ?, ?, ?)"
    project (string-downcase (symbol-name category))
    title content
    (when tags (json:encode-json-to-string tags))
    source-file))

(defun kb-search (query &key project category (limit 10))
  "Search knowledge base."
  (sqlite:execute-to-list *kb-connection*
    (format nil
      "SELECT * FROM knowledge_entries ke
       JOIN knowledge_fts ON ke.id = knowledge_fts.rowid
       WHERE knowledge_fts MATCH ?
       ~@[AND project = ?~]
       ~@[AND category = ?~]
       ORDER BY bm25(knowledge_fts) * relevance_score
       LIMIT ~D" project category limit)
    query project category))
```

### Knowledge Tools

```lisp
;;; tools/knowledge-tools.lisp

(define-tool search-knowledge-base
    (:description "Search persistent knowledge base for relevant past solutions and learnings."
     :parameters (:type "object"
                  :properties (:query (:type "string" :description "Search query")
                               :category (:type "string" :description "Filter: pattern, bug-fix, architecture, convention, preference, context")
                               :project (:type "string" :description "Filter to project")
                               :limit (:type "integer" :description "Max results"))
                  :required ("query"))
     :trust-level :safe
     :category :knowledge)
  (unless *kb-connection* (kb-init))
  (let ((results (kb-search (getf params :query)
                           :project (getf params :project)
                           :category (getf params :category)
                           :limit (or (getf params :limit) 10))))
    (if results
        (with-output-to-string (s)
          (format s "Found ~D entries:~%~%" (length results))
          (dolist (entry results)
            (format s "## ~A~%~A~%~%---~%~%"
                    (fourth entry) (fifth entry))))
        "No relevant knowledge found.")))

(define-tool store-in-knowledge-base
    (:description "Store knowledge for future reference."
     :parameters (:type "object"
                  :properties (:category (:type "string" :description "Category")
                               :title (:type "string" :description "Title")
                               :content (:type "string" :description "Content")
                               :tags (:type "array" :items (:type "string"))
                               :project (:type "string"))
                  :required ("category" "title" "content"))
     :trust-level :safe
     :category :knowledge)
  (unless *kb-connection* (kb-init))
  (kb-store (intern (string-upcase (getf params :category)) :keyword)
            (getf params :title)
            (getf params :content)
            :project (getf params :project)
            :tags (getf params :tags))
  (format nil "Stored: ~A" (getf params :title)))

(define-tool get-recent-knowledge
    (:description "Get recently stored knowledge entries."
     :parameters (:type "object"
                  :properties (:project (:type "string")
                               :category (:type "string")
                               :limit (:type "integer"))
                  :required ())
     :trust-level :safe
     :category :knowledge)
  (unless *kb-connection* (kb-init))
  (let ((results (sqlite:execute-to-list *kb-connection*
                  (format nil "SELECT * FROM knowledge_entries
                               ~@[WHERE project = ?~]
                               ORDER BY updated_at DESC
                               LIMIT ~D"
                          (getf params :project)
                          (or (getf params :limit) 20))
                  (getf params :project))))
    (with-output-to-string (s)
      (dolist (entry results)
        (format s "- [~A] ~A~%" (third entry) (fourth entry))))))
```

---

## Context Summarization

```lisp
;;; context/summarizer.lisp

(defvar *max-context-tokens* 8000)
(defvar *summarization-threshold* 0.8)

(defun estimate-tokens (text)
  "Rough token estimation."
  (ceiling (length text) 4))

(defun context-needs-summarization-p (context-manager)
  (let ((total (loop for item across (context-items context-manager)
                    sum (estimate-tokens (context-item-content item)))))
    (> total (* *max-context-tokens* *summarization-threshold*))))

(defun summarize-context (context-manager agent)
  "Summarize older context items using the LLM."
  (let* ((items (context-items context-manager))
         (n (length items))
         (keep-recent 5)
         (to-summarize (when (> n keep-recent)
                        (subseq items 0 (- n keep-recent)))))
    (when to-summarize
      (let* ((content (format nil "~{~A~%---~%~}"
                             (map 'list #'context-item-content to-summarize)))
             (summary (send-to-agent agent
                       (format nil "Summarize concisely, preserving key technical details:~%~%~A"
                               content)
                       :include-context nil)))
        (setf (context-items context-manager)
              (concatenate 'vector
                          (vector (make-instance 'context-item
                                   :type :summary
                                   :content summary))
                          (subseq items (- n keep-recent))))))))
```

---

## Object Inspection Tool

```lisp
;;; tools/introspection.lisp (addition)

(define-tool inspect-object
    (:description "Deep inspection of a live object. Evaluate an expression and inspect the result."
     :parameters (:type "object"
                  :properties (:expression (:type "string" :description "Expression to evaluate and inspect")
                               :package (:type "string" :description "Package context")
                               :depth (:type "integer" :description "Inspection depth (default 2)"))
                  :required ("expression"))
     :trust-level :cautious
     :category :introspection)
  (let* ((pkg (find-package (or (getf params :package) *package*)))
         (depth (or (getf params :depth) 2))
         (*package* pkg))
    (handler-case
        (let ((obj (eval (read-from-string (getf params :expression)))))
          (inspect-object-recursive obj depth))
      (error (e)
        (format nil "Error: ~A" e)))))

(defun inspect-object-recursive (obj depth &optional (indent 0))
  "Recursively inspect an object."
  (with-output-to-string (s)
    (let ((prefix (make-string indent :initial-element #\Space)))
      (format s "~A~S~%" prefix obj)
      (format s "~AType: ~A~%" prefix (type-of obj))
      (when (and (> depth 0) (typep obj 'standard-object))
        (format s "~ASlots:~%" prefix)
        (dolist (slot (closer-mop:class-slots (class-of obj)))
          (let ((name (closer-mop:slot-definition-name slot)))
            (format s "~A  ~A: " prefix name)
            (if (slot-boundp obj name)
                (let ((value (slot-value obj name)))
                  (if (and (> depth 1) (typep value 'standard-object))
                      (format s "~%~A" (inspect-object-recursive value (1- depth) (+ indent 4)))
                      (format s "~S~%" value)))
                (format s "#<unbound>~%"))))))))
```

---

## Updated System Prompt

```lisp
(defparameter *phase-3-system-prompt*
  "You are Agent-Q, an AI deeply integrated into a live Common Lisp environment.

## Capabilities

### Introspection
describe-symbol, apropos-search, function-arglist, who-calls, who-references,
list-package-symbols, class-slots, class-hierarchy, method-specializers,
macroexpand-form, inspect-object

### Execution
eval-form, eval-in-package, compile-form, get-last-error, get-repl-history,
get-compilation-notes

### Buffer Operations
read-file, write-file, read-buffer, write-to-buffer, search-in-buffer

### Debugging (NEW)
list-restarts, invoke-restart, inspect-condition, get-backtrace

### Testing (NEW)
run-test, run-test-suite, list-tests

### Tracing (NEW)
trace-function, untrace-function, untrace-all, list-traced-functions

### Knowledge (NEW)
search-knowledge-base, store-in-knowledge-base, get-recent-knowledge

## Complex Task Workflow

1. Search knowledge base for relevant past solutions
2. Use introspection to understand current code
3. Make changes incrementally, testing each step
4. Run tests to verify changes
5. If tests fail, debug and fix
6. Store successful solutions in knowledge base

## Debugging Workflow

1. inspect-condition to understand the error
2. get-backtrace to see call chain
3. list-restarts to see options
4. Fix code or invoke-restart

## Remember to Learn

Store useful patterns, solutions, and decisions in the knowledge base.")
```

---

## Testing Criteria

### Condition System
- [ ] list-restarts shows correct options
- [ ] invoke-restart works
- [ ] inspect-condition shows slots
- [ ] get-backtrace captures stack

### Testing Integration  
- [ ] run-test executes single test
- [ ] run-test-suite runs all tests
- [ ] list-tests enumerates available tests
- [ ] Failures clearly reported

### Tracing
- [ ] trace-function enables tracing
- [ ] untrace-function disables
- [ ] Multiple functions traceable

### Knowledge Base
- [ ] Store entries
- [ ] Search retrieves relevant entries
- [ ] Categories work
- [ ] Full-text search works

### Context Summarization
- [ ] Detects when summarization needed
- [ ] Produces useful summaries
- [ ] Preserves recent context

---

## Definition of Done

Phase 3 is complete when:

1. ✅ Condition system tools functional
2. ✅ Testing integration with FiveAM (minimum)
3. ✅ Tracing tools working
4. ✅ Knowledge base storing and searching
5. ✅ Context summarization working
6. ✅ Agent completes multi-step tasks
7. ✅ Agent learns from interactions
8. ✅ Object inspection with depth control
