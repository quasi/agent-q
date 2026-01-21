# Agent-Q Phase 2 Specification: The REPL-Aware Agent

## Overview

**Goal:** Transform Agent-Q from a simple chat interface into a REPL-aware agent that can introspect the running Lisp image, execute code, and observe results. This is where Agent-Q becomes fundamentally different from generic code assistants.

**Prerequisites:** Phase 1 complete and working.

**Success Criteria:** Agent can autonomously use introspection tools to understand code, execute proposed solutions, observe results, and iterate until the task is complete.

---

## Architecture Evolution

```
┌───────────────────────────────────────────────────────────────────┐
│                         EMACS (Elisp)                             │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │                    sly-agent-q.el                           │  │
│  │  [Phase 1 components]                                       │  │
│  │  + Tool execution bridge (elisp-side tools)                 │  │
│  │  + Tool approval UI (when configured)                       │  │
│  │  + Enhanced conversation display (tool calls shown)         │  │
│  └──────────────────────────┬──────────────────────────────────┘  │
│                             │                                     │
└─────────────────────────────┼─────────────────────────────────────┘
                              │ SLY RPC (bidirectional)
┌─────────────────────────────┼─────────────────────────────────────┐
│                             ▼                                     │
│                       LISP IMAGE                                  │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │                      agent-q                                │  │
│  │                                                             │  │
│  │  ┌─────────────────────────────────────────────────────┐    │  │
│  │  │                  TOOL SYSTEM (NEW)                  │    │  │
│  │  │                                                     │    │  │
│  │  │  ┌─────────────┐  ┌─────────────┐  ┌────────────┐   │    │  │
│  │  │  │   Tool      │  │    Tool     │  │   Tool     │   │    │  │
│  │  │  │  Registry   │  │  Executor   │  │  Results   │   │    │  │
│  │  │  └──────┬──────┘  └──────┬──────┘  └─────┬──────┘   │    │  │
│  │  │         └────────────────┴───────────────┘          │    │  │
│  │  └──────────────────────────┬──────────────────────────┘    │  │
│  │                             │                               │  │
│  │  ┌──────────────────────────┴──────────────────────────┐    │  │
│  │  │                    TOOL SETS                        │    │  │
│  │  │                                                     │    │  │
│  │  │  ┌────────────────┐  ┌────────────────────────────┐ │    │  │
│  │  │  │  Introspection │  │       Execution            │ │    │  │
│  │  │  │                │  │                            │ │    │  │
│  │  │  │ describe-sym   │  │ eval-form                  │ │    │  │
│  │  │  │ apropos        │  │ eval-in-package            │ │    │  │
│  │  │  │ function-args  │  │ compile-form               │ │    │  │
│  │  │  │ who-calls      │  │ get-last-error             │ │    │  │
│  │  │  │ who-refs       │  │ get-repl-history           │ │    │  │
│  │  │  │ pkg-symbols    │  │ get-compilation-notes      │ │    │  │
│  │  │  │ class-slots    │  │                            │ │    │  │
│  │  │  │ class-hier     │  │                            │ │    │  │
│  │  │  │ macroexpand    │  │                            │ │    │  │
│  │  │  └────────────────┘  └────────────────────────────┘ │    │  │
│  │  │                                                     │    │  │
│  │  │  ┌────────────────────────────────────────────────┐ │    │  │
│  │  │  │        Buffer Tools (Elisp-bridged)            │ │    │  │
│  │  │  │                                                │ │    │  │
│  │  │  │ read-file, write-file, read-buffer,            │ │    │  │
│  │  │  │ write-to-buffer, search-in-buffer              │ │    │  │
│  │  │  └────────────────────────────────────────────────┘ │    │  │
│  │  └─────────────────────────────────────────────────────┘    │  │
│  │                             │                               │  │
│  │                    ┌────────▼────────┐                      │  │
│  │                    │   Agent Loop    │                      │  │
│  │                    │   (enhanced)    │                      │  │
│  │                    └────────┬────────┘                      │  │
│  │                             │                               │  │
│  │                    ┌────────▼────────┐                      │  │
│  │                    │ cl-llm-provider │                      │  │
│  │                    │  (with tools)   │                      │  │
│  │                    └─────────────────┘                      │  │
│  └─────────────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────────────┘
```

---

## File Structure (Additions)

```
agent-q/
├── src/
│   ├── [Phase 1 files]
│   ├── tools/
│   │   ├── protocol.lisp          # Tool definition protocol
│   │   ├── registry.lisp          # Tool registry
│   │   ├── executor.lisp          # Tool execution engine
│   │   ├── introspection.lisp     # CL introspection tools
│   │   ├── execution.lisp         # Code execution tools
│   │   └── buffer.lisp            # Buffer tools (elisp bridge)
│   └── agent-loop.lisp            # Enhanced agent loop with tool use
└── contrib/
    └── sly-agent-q/
        ├── sly-agent-q.el         # Updated
        └── sly-agent-q-tools.el   # Elisp-side tool handlers
```

---

## Tool System Design

### Tool Definition Protocol

```lisp
;;; tools/protocol.lisp

(defclass tool ()
  ((name :initarg :name 
         :accessor tool-name
         :type string)
   (description :initarg :description 
                :accessor tool-description
                :type string
                :documentation "Description for LLM to understand when to use")
   (parameters :initarg :parameters 
               :accessor tool-parameters
               :documentation "JSON Schema plist for parameters")
   (handler :initarg :handler 
            :accessor tool-handler
            :type function
            :documentation "Function to call: (lambda (params) ...) -> result")
   (trust-level :initarg :trust-level
                :accessor tool-trust-level
                :initform :safe
                :type (member :safe :cautious :dangerous)
                :documentation ":safe = auto-execute, :cautious = log, :dangerous = require approval")
   (category :initarg :category
             :accessor tool-category
             :initform :general
             :type (member :introspection :execution :buffer :system))))

;; Tool result structure
(defclass tool-result ()
  ((tool-call-id :initarg :id :accessor tool-result-id)
   (success :initarg :success :accessor tool-result-success :type boolean)
   (content :initarg :content :accessor tool-result-content)
   (error-message :initarg :error :accessor tool-result-error :initform nil)))

;; Macro for easy tool definition
(defmacro define-tool (name (&key description parameters trust-level category) &body handler-body)
  "Define a tool with NAME.
   PARAMETERS should be a JSON Schema plist.
   HANDLER-BODY receives a plist of arguments."
  `(register-tool
    (make-instance 'tool
      :name ,(string-downcase (symbol-name name))
      :description ,description
      :parameters ',parameters
      :trust-level ,(or trust-level :safe)
      :category ,(or category :general)
      :handler (lambda (params)
                 (declare (ignorable params))
                 ,@handler-body))))
```

### Tool Registry

```lisp
;;; tools/registry.lisp

(defvar *tool-registry* (make-hash-table :test 'equal)
  "Registry of all available tools, keyed by name.")

(defun register-tool (tool)
  "Register a tool in the registry."
  (setf (gethash (tool-name tool) *tool-registry*) tool))

(defun get-tool (name)
  "Get a tool by name."
  (gethash name *tool-registry*))

(defun list-tools (&key category)
  "List all registered tools, optionally filtered by category."
  (let ((tools nil))
    (maphash (lambda (name tool)
               (declare (ignore name))
               (when (or (null category) 
                        (eq category (tool-category tool)))
                 (push tool tools)))
             *tool-registry*)
    (nreverse tools)))

(defun tools-to-schema ()
  "Convert all tools to the schema format expected by cl-llm-provider."
  (mapcar #'tool-to-schema (list-tools)))

(defun tool-to-schema (tool)
  "Convert a single tool to LLM-compatible schema."
  (list :type "function"
        :function (list :name (tool-name tool)
                       :description (tool-description tool)
                       :parameters (tool-parameters tool))))
```

### Tool Executor

```lisp
;;; tools/executor.lisp

(defvar *tool-execution-hooks* nil
  "List of functions called before/after tool execution.
   Each is (lambda (phase tool params result) ...) where phase is :before or :after")

(defvar *approval-handler* nil
  "Function to call for tools requiring approval.
   (lambda (tool params) ...) -> :approved, :denied, or :modified with new params")

(defun execute-tool-call (tool-call)
  "Execute a tool call from the LLM.
   TOOL-CALL is a plist (:id ID :name NAME :arguments ARGS-PLIST)"
  (let* ((id (getf tool-call :id))
         (name (getf tool-call :name))
         (args (getf tool-call :arguments))
         (tool (get-tool name)))
    (unless tool
      (return-from execute-tool-call
        (make-instance 'tool-result
          :id id
          :success nil
          :error (format nil "Unknown tool: ~A" name))))
    
    ;; Check trust level
    (when (and (eq (tool-trust-level tool) :dangerous)
               *approval-handler*)
      (let ((approval (funcall *approval-handler* tool args)))
        (case approval
          (:denied 
           (return-from execute-tool-call
             (make-instance 'tool-result
               :id id
               :success nil
               :error "Tool execution denied by user")))
          (:modified
           ;; approval-handler can return (:modified new-args)
           (when (listp approval)
             (setf args (second approval)))))))
    
    ;; Execute with hooks
    (run-hooks :before tool args nil)
    (handler-case
        (let ((result (funcall (tool-handler tool) args)))
          (run-hooks :after tool args result)
          (make-instance 'tool-result
            :id id
            :success t
            :content (format-tool-result result)))
      (error (e)
        (make-instance 'tool-result
          :id id
          :success nil
          :error (format nil "Tool error: ~A" e))))))

(defun format-tool-result (result)
  "Format a tool result for inclusion in LLM context.
   Handles various result types appropriately."
  (typecase result
    (string result)
    (null "nil")
    (list (with-output-to-string (s)
            (pprint result s)))
    (t (prin1-to-string result))))

(defun run-hooks (phase tool args result)
  (dolist (hook *tool-execution-hooks*)
    (ignore-errors
      (funcall hook phase tool args result))))
```

---

## Introspection Tools

```lisp
;;; tools/introspection.lisp

(define-tool describe-symbol
    (:description "Get detailed information about a symbol including its type, value, documentation, and definition location."
     :parameters (:type "object"
                  :properties (:symbol (:type "string" 
                                        :description "The symbol name to describe")
                               :package (:type "string"
                                        :description "Package name (optional, defaults to current)"))
                  :required ("symbol"))
     :trust-level :safe
     :category :introspection)
  (let* ((sym-name (getf params :symbol))
         (pkg-name (or (getf params :package) *package*))
         (pkg (find-package pkg-name))
         (sym (and pkg (find-symbol sym-name pkg))))
    (unless sym
      (return-from describe-symbol 
        (format nil "Symbol ~A not found in package ~A" sym-name pkg-name)))
    (with-output-to-string (s)
      (describe sym s))))

(define-tool apropos-search
    (:description "Search for symbols matching a pattern. Returns symbol names, types, and packages."
     :parameters (:type "object"
                  :properties (:pattern (:type "string"
                                         :description "Search pattern (substring match)")
                               :package (:type "string"
                                        :description "Limit search to this package (optional)"))
                  :required ("pattern"))
     :trust-level :safe
     :category :introspection)
  (let ((pattern (getf params :pattern))
        (pkg-name (getf params :package)))
    (with-output-to-string (s)
      (if pkg-name
          (apropos pattern (find-package pkg-name) s)
          (apropos pattern nil s)))))

(define-tool function-arglist
    (:description "Get the argument list (lambda list) for a function."
     :parameters (:type "object"
                  :properties (:function (:type "string"
                                          :description "Function name")
                               :package (:type "string"
                                        :description "Package name (optional)"))
                  :required ("function"))
     :trust-level :safe
     :category :introspection)
  (let* ((fn-name (getf params :function))
         (pkg (find-package (or (getf params :package) *package*)))
         (sym (and pkg (find-symbol fn-name pkg))))
    (if (and sym (fboundp sym))
        (format nil "~A" (sb-introspect:function-lambda-list sym))  ; SBCL-specific, need portability layer
        (format nil "Function ~A not found or not fbound" fn-name))))

(define-tool who-calls
    (:description "Find all functions that call the specified function."
     :parameters (:type "object"
                  :properties (:function (:type "string"
                                          :description "Function name to find callers of")
                               :package (:type "string"
                                        :description "Package name (optional)"))
                  :required ("function"))
     :trust-level :safe
     :category :introspection)
  (let* ((fn-name (getf params :function))
         (pkg (find-package (or (getf params :package) *package*)))
         (sym (and pkg (find-symbol fn-name pkg))))
    (if sym
        ;; Use SLY's xref infrastructure
        (format nil "~{~A~%~}" 
                (swank:xref :calls sym))
        (format nil "Symbol ~A not found" fn-name))))

(define-tool who-references
    (:description "Find all code that references (reads) the specified variable."
     :parameters (:type "object"
                  :properties (:variable (:type "string"
                                          :description "Variable name")
                               :package (:type "string"
                                        :description "Package name (optional)"))
                  :required ("variable"))
     :trust-level :safe
     :category :introspection)
  (let* ((var-name (getf params :variable))
         (pkg (find-package (or (getf params :package) *package*)))
         (sym (and pkg (find-symbol var-name pkg))))
    (if sym
        (format nil "~{~A~%~}" 
                (swank:xref :references sym))
        (format nil "Symbol ~A not found" var-name))))

(define-tool list-package-symbols
    (:description "List all exported symbols from a package with their types."
     :parameters (:type "object"
                  :properties (:package (:type "string"
                                        :description "Package name")
                               :include-internal (:type "boolean"
                                                 :description "Include internal symbols (default false)"))
                  :required ("package"))
     :trust-level :safe
     :category :introspection)
  (let* ((pkg-name (getf params :package))
         (include-internal (getf params :include-internal))
         (pkg (find-package pkg-name)))
    (unless pkg
      (return-from list-package-symbols
        (format nil "Package ~A not found" pkg-name)))
    (with-output-to-string (s)
      (if include-internal
          (do-symbols (sym pkg)
            (format s "~A: ~A~%" sym (symbol-type-description sym)))
          (do-external-symbols (sym pkg)
            (format s "~A: ~A~%" sym (symbol-type-description sym)))))))

(defun symbol-type-description (sym)
  "Return a string describing what kind of thing SYM names."
  (cond
    ((fboundp sym)
     (cond
       ((macro-function sym) "macro")
       ((typep (symbol-function sym) 'generic-function) "generic-function")
       (t "function")))
    ((boundp sym) "variable")
    ((find-class sym nil) "class")
    (t "unbound")))

(define-tool class-slots
    (:description "Get slot definitions for a class including names, types, and initargs."
     :parameters (:type "object"
                  :properties (:class (:type "string"
                                      :description "Class name")
                               :package (:type "string"
                                        :description "Package name (optional)"))
                  :required ("class"))
     :trust-level :safe
     :category :introspection)
  (let* ((class-name (getf params :class))
         (pkg (find-package (or (getf params :package) *package*)))
         (sym (and pkg (find-symbol class-name pkg)))
         (class (and sym (find-class sym nil))))
    (unless class
      (return-from class-slots
        (format nil "Class ~A not found" class-name)))
    (closer-mop:ensure-finalized class)
    (with-output-to-string (s)
      (format s "Class: ~A~%Slots:~%" class-name)
      (dolist (slot (closer-mop:class-slots class))
        (format s "  ~A~%    Initarg: ~A~%    Type: ~A~%    Initform: ~A~%"
                (closer-mop:slot-definition-name slot)
                (closer-mop:slot-definition-initargs slot)
                (closer-mop:slot-definition-type slot)
                (closer-mop:slot-definition-initform slot))))))

(define-tool class-hierarchy
    (:description "Get the superclasses and subclasses of a class."
     :parameters (:type "object"
                  :properties (:class (:type "string"
                                      :description "Class name")
                               :package (:type "string"
                                        :description "Package name (optional)"))
                  :required ("class"))
     :trust-level :safe
     :category :introspection)
  (let* ((class-name (getf params :class))
         (pkg (find-package (or (getf params :package) *package*)))
         (sym (and pkg (find-symbol class-name pkg)))
         (class (and sym (find-class sym nil))))
    (unless class
      (return-from class-hierarchy
        (format nil "Class ~A not found" class-name)))
    (closer-mop:ensure-finalized class)
    (with-output-to-string (s)
      (format s "Class: ~A~%~%" class-name)
      (format s "Superclasses:~%")
      (dolist (super (closer-mop:class-precedence-list class))
        (format s "  ~A~%" (class-name super)))
      (format s "~%Direct Subclasses:~%")
      (dolist (sub (closer-mop:class-direct-subclasses class))
        (format s "  ~A~%" (class-name sub))))))

(define-tool method-specializers
    (:description "List all methods for a generic function with their specializers."
     :parameters (:type "object"
                  :properties (:generic-function (:type "string"
                                                  :description "Generic function name")
                               :package (:type "string"
                                        :description "Package name (optional)"))
                  :required ("generic-function"))
     :trust-level :safe
     :category :introspection)
  (let* ((gf-name (getf params :generic-function))
         (pkg (find-package (or (getf params :package) *package*)))
         (sym (and pkg (find-symbol gf-name pkg)))
         (gf (and sym (fboundp sym) 
                  (typep (symbol-function sym) 'generic-function)
                  (symbol-function sym))))
    (unless gf
      (return-from method-specializers
        (format nil "Generic function ~A not found" gf-name)))
    (with-output-to-string (s)
      (format s "Generic Function: ~A~%Lambda List: ~A~%~%Methods:~%"
              gf-name
              (closer-mop:generic-function-lambda-list gf))
      (dolist (method (closer-mop:generic-function-methods gf))
        (format s "  ~A ~A~%"
                (mapcar (lambda (spec)
                          (if (typep spec 'class)
                              (class-name spec)
                              spec))
                        (closer-mop:method-specializers method))
                (closer-mop:method-qualifiers method))))))

(define-tool macroexpand-form
    (:description "Expand macros in a Lisp form. Use macroexpand-1 for single expansion or macroexpand for full expansion."
     :parameters (:type "object"
                  :properties (:form (:type "string"
                                     :description "The Lisp form to expand (as a string)")
                               :full (:type "boolean"
                                     :description "If true, use macroexpand (full), otherwise macroexpand-1")
                               :package (:type "string"
                                        :description "Package context for reading the form"))
                  :required ("form"))
     :trust-level :safe
     :category :introspection)
  (let* ((form-string (getf params :form))
         (full (getf params :full))
         (pkg (find-package (or (getf params :package) *package*))))
    (handler-case
        (let* ((form (let ((*package* pkg))
                      (read-from-string form-string)))
               (expanded (if full
                            (macroexpand form)
                            (macroexpand-1 form))))
          (with-output-to-string (s)
            (pprint expanded s)))
      (error (e)
        (format nil "Error expanding form: ~A" e)))))
```

---

## Execution Tools

```lisp
;;; tools/execution.lisp

(defvar *last-error* nil
  "Most recent error condition and backtrace.")

(defvar *repl-history* (make-array 100 :fill-pointer 0 :adjustable t)
  "Recent REPL interactions.")

(defvar *compilation-notes* nil
  "Recent compilation warnings and errors.")

(define-tool eval-form
    (:description "Evaluate a Lisp form and return the result. Use this to test code, check values, or run computations."
     :parameters (:type "object"
                  :properties (:form (:type "string"
                                     :description "The Lisp form to evaluate")
                               :package (:type "string"
                                        :description "Package context for evaluation"))
                  :required ("form"))
     :trust-level :cautious  ; Log but allow
     :category :execution)
  (let* ((form-string (getf params :form))
         (pkg (find-package (or (getf params :package) *package*))))
    (handler-case
        (let* ((*package* pkg)
               (form (read-from-string form-string))
               (values (multiple-value-list (eval form))))
          ;; Record in history
          (record-repl-interaction form-string values)
          ;; Format result
          (with-output-to-string (s)
            (format s "Result~P: ~%" (length values))
            (dolist (v values)
              (format s "  ~S~%" v))))
      (error (e)
        (setf *last-error* (list :condition e 
                                :form form-string
                                :backtrace (capture-backtrace)))
        (format nil "Error: ~A" e)))))

(define-tool eval-in-package
    (:description "Evaluate a form in a specific package context."
     :parameters (:type "object"
                  :properties (:form (:type "string"
                                     :description "The Lisp form to evaluate")
                               :package (:type "string"
                                        :description "Package name (required)"))
                  :required ("form" "package"))
     :trust-level :cautious
     :category :execution)
  (let ((pkg (find-package (getf params :package))))
    (unless pkg
      (return-from eval-in-package
        (format nil "Package ~A not found" (getf params :package))))
    (let ((*package* pkg))
      ;; Delegate to eval-form's handler
      (funcall (tool-handler (get-tool "eval-form"))
               (list :form (getf params :form) :package (getf params :package))))))

(define-tool compile-form
    (:description "Compile a Lisp form and load it. Returns any warnings or errors from compilation."
     :parameters (:type "object"
                  :properties (:form (:type "string"
                                     :description "The Lisp form to compile (typically a defun, defclass, etc.)")
                               :package (:type "string"
                                        :description "Package context"))
                  :required ("form"))
     :trust-level :cautious
     :category :execution)
  (let* ((form-string (getf params :form))
         (pkg (find-package (or (getf params :package) *package*)))
         (warnings nil)
         (errors nil))
    (handler-case
        (let ((*package* pkg))
          ;; Capture compilation output
          (handler-bind 
              ((warning (lambda (w)
                         (push (format nil "Warning: ~A" w) warnings)
                         (muffle-warning w)))
               (error (lambda (e)
                       (push (format nil "Error: ~A" e) errors))))
            (let* ((form (read-from-string form-string))
                   (result (compile nil `(lambda () ,form))))
              (funcall result)  ; Execute to load definition
              (setf *compilation-notes* (append errors warnings))
              (with-output-to-string (s)
                (format s "Compilation successful.~%")
                (when warnings
                  (format s "Warnings:~%~{  ~A~%~}" (nreverse warnings)))))))
      (error (e)
        (setf *last-error* (list :condition e
                                :form form-string
                                :type :compilation))
        (format nil "Compilation failed: ~A" e)))))

(define-tool get-last-error
    (:description "Get details about the most recent error including the condition, form that caused it, and backtrace."
     :parameters (:type "object"
                  :properties ()
                  :required ())
     :trust-level :safe
     :category :execution)
  (if *last-error*
      (with-output-to-string (s)
        (format s "Last Error:~%")
        (format s "  Condition: ~A~%" (getf *last-error* :condition))
        (format s "  Form: ~A~%" (getf *last-error* :form))
        (format s "  Backtrace:~%~A" (getf *last-error* :backtrace)))
      "No recent errors recorded."))

(define-tool get-repl-history
    (:description "Get recent REPL interactions (forms evaluated and their results)."
     :parameters (:type "object"
                  :properties (:limit (:type "integer"
                                      :description "Number of recent interactions to return (default 10)"))
                  :required ())
     :trust-level :safe
     :category :execution)
  (let* ((limit (or (getf params :limit) 10))
         (start (max 0 (- (fill-pointer *repl-history*) limit))))
    (with-output-to-string (s)
      (format s "Recent REPL History:~%")
      (loop for i from start below (fill-pointer *repl-history*)
            for entry = (aref *repl-history* i)
            do (format s "~%[~D] ~A~%=> ~A~%"
                      i
                      (getf entry :form)
                      (getf entry :result))))))

(define-tool get-compilation-notes
    (:description "Get recent compilation warnings and errors."
     :parameters (:type "object"
                  :properties ()
                  :required ())
     :trust-level :safe
     :category :execution)
  (if *compilation-notes*
      (with-output-to-string (s)
        (format s "Recent Compilation Notes:~%~{  ~A~%~}" *compilation-notes*))
      "No recent compilation notes."))

;; Helper functions
(defun record-repl-interaction (form result)
  (when (>= (fill-pointer *repl-history*) (array-dimension *repl-history* 0))
    (vector-pop *repl-history*))  ; Remove oldest
  (vector-push-extend (list :form form 
                           :result (format nil "~{~S~^, ~}" result)
                           :timestamp (get-universal-time))
                     *repl-history*))

(defun capture-backtrace ()
  "Capture current backtrace as a string."
  (with-output-to-string (s)
    (sb-debug:print-backtrace :stream s :count 20)))  ; SBCL-specific
```

---

## Buffer Tools (Elisp Bridge)

```lisp
;;; tools/buffer.lisp

;; These tools delegate to Elisp via SLY RPC

(define-tool read-file
    (:description "Read the contents of a file from the filesystem."
     :parameters (:type "object"
                  :properties (:path (:type "string"
                                     :description "File path to read"))
                  :required ("path"))
     :trust-level :safe
     :category :buffer)
  (let ((path (getf params :path)))
    ;; Delegate to elisp
    (swank:eval-in-emacs 
     `(with-temp-buffer
        (insert-file-contents ,path)
        (buffer-string)))))

(define-tool write-file
    (:description "Write content to a file."
     :parameters (:type "object"
                  :properties (:path (:type "string"
                                     :description "File path to write")
                               :content (:type "string"
                                        :description "Content to write"))
                  :required ("path" "content"))
     :trust-level :dangerous  ; Require approval
     :category :buffer)
  (let ((path (getf params :path))
        (content (getf params :content)))
    (swank:eval-in-emacs
     `(with-temp-file ,path
        (insert ,content)))
    (format nil "Written ~D bytes to ~A" (length content) path)))

(define-tool read-buffer
    (:description "Read the contents of an Emacs buffer."
     :parameters (:type "object"
                  :properties (:buffer (:type "string"
                                       :description "Buffer name (optional, defaults to current)")
                               :start (:type "integer"
                                      :description "Start position (optional)")
                               :end (:type "integer"
                                    :description "End position (optional)"))
                  :required ())
     :trust-level :safe
     :category :buffer)
  (let ((buffer (getf params :buffer))
        (start (getf params :start))
        (end (getf params :end)))
    (swank:eval-in-emacs
     `(with-current-buffer ,(or buffer (current-buffer))
        (buffer-substring-no-properties 
         ,(or start '(point-min))
         ,(or end '(point-max)))))))

(define-tool write-to-buffer
    (:description "Insert text into an Emacs buffer at a specific position or at point."
     :parameters (:type "object"
                  :properties (:content (:type "string"
                                        :description "Content to insert")
                               :buffer (:type "string"
                                       :description "Buffer name (optional, defaults to current)")
                               :position (:type "integer"
                                         :description "Position to insert at (optional, defaults to point)"))
                  :required ("content"))
     :trust-level :cautious
     :category :buffer)
  (let ((content (getf params :content))
        (buffer (getf params :buffer))
        (position (getf params :position)))
    (swank:eval-in-emacs
     `(with-current-buffer ,(or buffer '(current-buffer))
        ,(when position `(goto-char ,position))
        (insert ,content)))
    (format nil "Inserted ~D characters" (length content))))

(define-tool search-in-buffer
    (:description "Search for text in a buffer using regex or literal string."
     :parameters (:type "object"
                  :properties (:pattern (:type "string"
                                        :description "Search pattern")
                               :buffer (:type "string"
                                       :description "Buffer name (optional)")
                               :regex (:type "boolean"
                                      :description "If true, pattern is a regex")
                               :all-matches (:type "boolean"
                                            :description "If true, return all matches"))
                  :required ("pattern"))
     :trust-level :safe
     :category :buffer)
  (let ((pattern (getf params :pattern))
        (buffer (getf params :buffer))
        (regex (getf params :regex))
        (all-matches (getf params :all-matches)))
    (swank:eval-in-emacs
     `(with-current-buffer ,(or buffer '(current-buffer))
        (save-excursion
          (goto-char (point-min))
          (let ((matches nil)
                (search-fn (if ,regex #'re-search-forward #'search-forward)))
            (while (funcall search-fn ,pattern nil t)
              (push (list :match (match-string 0)
                         :position (match-beginning 0)
                         :line (line-number-at-pos))
                    matches)
              ,(unless all-matches '(return)))
            (nreverse matches)))))))
```

---

## Enhanced Agent Loop

```lisp
;;; agent-loop.lisp

(defmethod send-to-agent :around ((agent agent) user-message &key include-context)
  "Enhanced agent loop with tool use capability."
  (let* ((context-string (when include-context
                          (context-to-string 
                           (conversation-context 
                            (agent-conversation agent)))))
         (full-message (if context-string
                          (format nil "## Context~%~%~A~%~%## Request~%~%~A"
                                  context-string user-message)
                          user-message))
         (conversation (agent-conversation agent)))
    
    ;; Add user message to history
    (add-message conversation :user full-message)
    
    ;; Build messages for API
    (let ((messages (build-messages conversation agent)))
      
      ;; Agent loop - iterate until no more tool calls
      (loop
        (let ((response (cl-llm-provider:chat 
                        (agent-provider agent)
                        :model (agent-model agent)
                        :messages messages
                        :tools (tools-to-schema)
                        :max-tokens 4096)))
          
          ;; Check finish reason
          (let ((finish-reason (getf response :finish-reason))
                (content (getf response :content))
                (tool-calls (getf response :tool-calls)))
            
            ;; Add assistant message to history
            (when content
              (add-message conversation :assistant content))
            
            ;; If tool calls, execute them
            (if (and tool-calls (eq finish-reason :tool-calls))
                (let ((results (execute-tool-calls tool-calls)))
                  ;; Add tool results to messages
                  (setf messages 
                        (append messages
                                (list (make-assistant-message-with-tools content tool-calls))
                                (mapcar #'make-tool-result-message results))))
                
                ;; No tool calls - we're done
                (return content))))))))

(defun execute-tool-calls (tool-calls)
  "Execute multiple tool calls and return results."
  (mapcar #'execute-tool-call tool-calls))

(defun make-assistant-message-with-tools (content tool-calls)
  "Create assistant message structure with tool calls."
  (list :role :assistant
        :content content
        :tool-calls tool-calls))

(defun make-tool-result-message (result)
  "Create tool result message for API."
  (list :role :tool
        :tool-call-id (tool-result-id result)
        :content (if (tool-result-success result)
                    (tool-result-content result)
                    (format nil "Error: ~A" (tool-result-error result)))))
```

---

## cl-llm-provider Requirements (Phase 2)

The following capabilities MUST exist in cl-llm-provider:

### Required API Extensions

```lisp
;; Tool-enabled chat
(cl-llm-provider:chat provider
  :model "claude-sonnet-4-20250514"
  :messages messages
  :tools tool-schemas        ; NEW: List of tool definitions
  :tool-choice :auto         ; NEW: :auto, :required, :none, or specific tool
  :max-tokens 4096)

;; Response structure must include:
;; (:content "..." 
;;  :tool-calls ((:id "call_xxx" :name "tool-name" :arguments (:key value ...)) ...)
;;  :finish-reason :stop | :tool-calls | :length
;;  :usage (:input-tokens N :output-tokens M))

;; Tool schema format (standard function calling format):
;; (:type "function"
;;  :function (:name "tool-name"
;;             :description "What this tool does"
;;             :parameters (:type "object"
;;                         :properties (...)
;;                         :required (...))))
```

### Implementation Checklist for cl-llm-provider

- [ ] `tools` parameter in chat function
- [ ] `tool-choice` parameter
- [ ] Tool call parsing from response
- [ ] `:tool-calls` finish reason detection
- [ ] Tool result message type (`:role :tool`)
- [ ] JSON Schema validation for tool parameters (optional but nice)

---

## Updated System Prompt

```lisp
(defparameter *base-system-prompt*
  "You are Agent-Q, an AI assistant integrated into a live Common Lisp development environment.

## Your Capabilities
You have access to the running Lisp image through tools. You can:

### Introspection
- `describe-symbol`: Get detailed info about any symbol
- `apropos-search`: Find symbols matching a pattern
- `function-arglist`: Get function signatures
- `who-calls`: Find all callers of a function
- `who-references`: Find references to variables
- `list-package-symbols`: List symbols in a package
- `class-slots`: Inspect class structure
- `class-hierarchy`: See inheritance relationships
- `method-specializers`: List methods on a generic function
- `macroexpand-form`: Expand macros

### Execution
- `eval-form`: Evaluate Lisp code and see results
- `compile-form`: Compile and load definitions
- `get-last-error`: See recent errors and backtraces
- `get-repl-history`: See recent REPL interactions

### Buffer Operations
- `read-file`: Read file contents
- `write-file`: Write to files (use carefully)
- `read-buffer`: Read Emacs buffer contents
- `write-to-buffer`: Insert text into buffers
- `search-in-buffer`: Search in buffers

## How To Work
1. When asked to understand code, use introspection tools first
2. When asked to fix bugs, examine the error, relevant code, and callers
3. When writing new code, test it with eval-form before presenting
4. Iterate: if something doesn't work, examine why and try again
5. Explain your reasoning when it helps the developer understand

## Guidelines
- Write idiomatic Common Lisp
- Use introspection to understand before changing
- Test your code before presenting final solutions
- Be precise about packages and symbols
- When you make changes, explain what you changed and why"
  "Phase 2 system prompt with tool awareness")
```

---

## Testing Criteria

### Tool Tests

```lisp
;; Each tool should have tests:
- describe-symbol on function, variable, class, macro
- apropos finds expected symbols
- who-calls returns known callers
- eval-form executes correctly
- compile-form compiles and loads
- Error handling for invalid inputs
```

### Integration Tests

```lisp
;; Agent loop tests
- Agent can describe a symbol when asked
- Agent uses eval-form to test code
- Agent iterates when first attempt fails
- Tool results appear in conversation
- Trust levels are respected
```

### End-to-End Scenarios

```
Scenario 1: "Describe the function PROCESS-DATA in MY-APP package"
- Agent should use describe-symbol
- Return comprehensive info

Scenario 2: "There's a bug in PARSE-INPUT, it fails on empty strings"
- Agent should describe-symbol to understand it
- Agent should eval-form to reproduce
- Agent should propose fix
- Agent should eval-form to verify fix

Scenario 3: "Add a new method to SERIALIZE for MY-CLASS"
- Agent should class-slots to understand structure
- Agent should method-specializers to see existing methods
- Agent should compile-form to add method
- Agent should eval-form to test
```

---

## Definition of Done

Phase 2 is complete when:

1. ✅ Tool system infrastructure exists (registry, executor, protocol)
2. ✅ All introspection tools implemented and working
3. ✅ All execution tools implemented and working
4. ✅ Buffer tools bridged to Elisp
5. ✅ Agent loop handles tool calls autonomously
6. ✅ Trust levels configurable and respected
7. ✅ cl-llm-provider has required tool support
8. ✅ System prompt updated with tool documentation
9. ✅ Agent can complete the three test scenarios above
10. ✅ Error handling is robust (bad tool calls don't crash)
