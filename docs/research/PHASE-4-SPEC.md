# Agent-Q Phase 4 Specification: The Intelligent Partner

## Overview

**Goal:** Transform Agent-Q from a reactive assistant into an intelligent development partner that deeply understands your codebase, can perform sophisticated refactoring, and provides proactive insights.

**Prerequisites:** Phases 1-3 complete.

**Success Criteria:** Agent can handle requests like "this module is getting unwieldy - suggest how to restructure it" by understanding the semantic structure, relationships, and usage patterns to propose meaningful architectural improvements.

---

## New Capabilities

| Capability | Description |
|------------|-------------|
| **Semantic Model** | Live index of definitions, relationships, dependencies |
| **Profiling** | Profile execution, analyze performance |
| **Multi-file Refactoring** | Safe refactoring across files with reference tracking |
| **Pattern Recognition** | Detect repeated code, suggest abstractions |
| **Project Understanding** | ASDF awareness, dependency tracking |

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                              LISP IMAGE                                 │
│                                                                         │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                            agent-q                                │  │
│  │                                                                   │  │
│  │  [Phase 1-3 components]                                           │  │
│  │                                                                   │  │
│  │  ┌─────────────────────────────────────────────────────────────┐  │  │
│  │  │                  SEMANTIC INDEX (NEW)                       │  │  │
│  │  │                                                             │  │  │
│  │  │  ┌─────────────┐ ┌─────────────┐ ┌───────────────────────┐  │  │  │
│  │  │  │ Definition  │ │ Dependency  │ │ Usage Statistics      │  │  │  │
│  │  │  │ Registry    │ │ Graph       │ │                       │  │  │  │
│  │  │  │             │ │             │ │ - Call frequency      │  │  │  │
│  │  │  │ - Functions │ │ - Calls     │ │ - Modification time   │  │  │  │
│  │  │  │ - Classes   │ │ - Inherits  │ │ - Complexity metrics  │  │  │  │
│  │  │  │ - Variables │ │ - Uses      │ │                       │  │  │  │
│  │  │  │ - Macros    │ │ - Exports   │ │                       │  │  │  │
│  │  │  └─────────────┘ └─────────────┘ └───────────────────────┘  │  │  │
│  │  │                                                             │  │  │
│  │  │  Change Tracking & Auto-Update                              │  │  │
│  │  └─────────────────────────────────────────────────────────────┘  │  │
│  │                                                                   │  │
│  │  ┌─────────────────────────────────────────────────────────────┐  │  │
│  │  │                 PROFILING SUBSYSTEM (NEW)                   │  │  │
│  │  │                                                             │  │  │
│  │  │  - Execution profiling                                      │  │  │
│  │  │  - Memory profiling                                         │  │  │
│  │  │  - Hot spot detection                                       │  │  │
│  │  │  - Call graph analysis                                      │  │  │
│  │  └─────────────────────────────────────────────────────────────┘  │  │
│  │                                                                   │  │
│  │  ┌─────────────────────────────────────────────────────────────┐  │  │
│  │  │                REFACTORING ENGINE (NEW)                     │  │  │
│  │  │                                                             │  │  │
│  │  │  - Safe rename (all references)                             │  │  │
│  │  │  - Extract function/method                                  │  │  │
│  │  │  - Inline function                                          │  │  │
│  │  │  - Move to package                                          │  │  │
│  │  │  - Preview changes before applying                          │  │  │
│  │  └─────────────────────────────────────────────────────────────┘  │  │
│  │                                                                   │  │
│  │  ┌─────────────────────────────────────────────────────────────┐  │  │
│  │  │                 PATTERN ANALYZER (NEW)                      │  │  │
│  │  │                                                             │  │  │
│  │  │  - Code duplication detection                               │  │  │
│  │  │  - Common pattern recognition                               │  │  │
│  │  │  - Abstraction suggestions                                  │  │  │
│  │  │  - Code smell detection                                     │  │  │
│  │  └─────────────────────────────────────────────────────────────┘  │  │
│  │                                                                   │  │
│  │  ┌─────────────────────────────────────────────────────────────┐  │  │
│  │  │                 PROJECT AWARENESS (NEW)                     │  │  │
│  │  │                                                             │  │  │
│  │  │  - ASDF system parsing                                      │  │  │
│  │  │  - Dependency tracking                                      │  │  │
│  │  │  - Build order understanding                                │  │  │
│  │  │  - Multi-system project support                             │  │  │
│  │  └─────────────────────────────────────────────────────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## File Structure (Additions)

```
agent-q/
├── src/
│   ├── [Phase 1-3 files]
│   ├── semantic/
│   │   ├── index.lisp           # Main semantic index
│   │   ├── definitions.lisp     # Definition tracking
│   │   ├── dependencies.lisp    # Dependency graph
│   │   ├── change-tracker.lisp  # Change detection
│   │   └── metrics.lisp         # Code metrics
│   ├── profiling/
│   │   ├── profiler.lisp        # Profiling wrapper
│   │   └── analysis.lisp        # Profile analysis
│   ├── refactoring/
│   │   ├── engine.lisp          # Refactoring core
│   │   ├── rename.lisp          # Safe rename
│   │   ├── extract.lisp         # Extract function
│   │   └── preview.lisp         # Change preview
│   ├── patterns/
│   │   ├── detector.lisp        # Pattern detection
│   │   ├── duplication.lisp     # Code duplication
│   │   └── suggestions.lisp     # Improvement suggestions
│   └── project/
│       ├── asdf-parser.lisp     # ASDF system parsing
│       └── dependencies.lisp    # Project dependencies
```

---

## Semantic Index

### Data Structures

```lisp
;;; semantic/definitions.lisp

(defclass definition ()
  ((name :initarg :name :accessor def-name)
   (kind :initarg :kind :accessor def-kind
         :type (member :function :macro :generic-function :method
                       :class :struct :variable :constant :package))
   (package :initarg :package :accessor def-package)
   (source-file :initarg :source-file :accessor def-source-file)
   (source-position :initarg :source-position :accessor def-source-position)
   (docstring :initarg :docstring :accessor def-docstring)
   (lambda-list :initarg :lambda-list :accessor def-lambda-list)
   (created-at :initform (get-universal-time) :accessor def-created-at)
   (modified-at :initform (get-universal-time) :accessor def-modified-at)
   (metrics :initform nil :accessor def-metrics)))

(defclass function-definition (definition)
  ((calls :initform nil :accessor def-calls
          :documentation "Functions this function calls")
   (called-by :initform nil :accessor def-called-by
              :documentation "Functions that call this")
   (complexity :initform nil :accessor def-complexity)))

(defclass class-definition (definition)
  ((superclasses :initform nil :accessor def-superclasses)
   (subclasses :initform nil :accessor def-subclasses)
   (slots :initform nil :accessor def-slots)
   (methods :initform nil :accessor def-methods)))
```

### Index Manager

```lisp
;;; semantic/index.lisp

(defvar *semantic-index* nil
  "The global semantic index instance.")

(defclass semantic-index ()
  ((definitions :initform (make-hash-table :test 'equal)
                :accessor index-definitions
                :documentation "All known definitions, keyed by qualified name")
   (dependency-graph :initform (make-instance 'dependency-graph)
                     :accessor index-dependencies)
   (packages :initform (make-hash-table :test 'equal)
             :accessor index-packages)
   (files :initform (make-hash-table :test 'equal)
          :accessor index-files
          :documentation "Definitions by file")
   (last-updated :initform nil :accessor index-last-updated)))

(defun init-semantic-index ()
  "Initialize or reset the semantic index."
  (setf *semantic-index* (make-instance 'semantic-index)))

(defun index-definition (def)
  "Add or update a definition in the index."
  (let* ((key (qualified-name def))
         (existing (gethash key (index-definitions *semantic-index*))))
    (when existing
      (setf (def-modified-at def) (get-universal-time)))
    (setf (gethash key (index-definitions *semantic-index*)) def)
    ;; Update file index
    (when (def-source-file def)
      (pushnew def (gethash (def-source-file def) 
                           (index-files *semantic-index*))))
    def))

(defun find-definition (name &optional package)
  "Find a definition by name."
  (let ((key (if package
                (format nil "~A:~A" (package-name package) name)
                name)))
    (gethash key (index-definitions *semantic-index*))))

(defun qualified-name (def)
  "Get the qualified name for a definition."
  (format nil "~A:~A" 
          (package-name (def-package def))
          (def-name def)))
```

### Dependency Graph

```lisp
;;; semantic/dependencies.lisp

(defclass dependency-graph ()
  ((edges :initform (make-hash-table :test 'equal)
          :accessor graph-edges
          :documentation "Adjacency list: from -> list of (to . relationship)")
   (reverse-edges :initform (make-hash-table :test 'equal)
                  :accessor graph-reverse-edges
                  :documentation "Reverse adjacency for fast lookups")))

(deftype relationship ()
  '(member :calls :references :specializes :inherits :uses :exports))

(defun add-dependency (from to relationship)
  "Add a dependency edge."
  (let ((graph (index-dependencies *semantic-index*)))
    (push (cons to relationship) (gethash from (graph-edges graph)))
    (push (cons from relationship) (gethash to (graph-reverse-edges graph)))))

(defun get-dependencies (name &key relationship)
  "Get all things NAME depends on."
  (let ((all (gethash name (graph-edges (index-dependencies *semantic-index*)))))
    (if relationship
        (remove-if-not (lambda (e) (eq (cdr e) relationship)) all)
        all)))

(defun get-dependents (name &key relationship)
  "Get all things that depend on NAME."
  (let ((all (gethash name (graph-reverse-edges (index-dependencies *semantic-index*)))))
    (if relationship
        (remove-if-not (lambda (e) (eq (cdr e) relationship)) all)
        all)))
```

### Change Tracking

```lisp
;;; semantic/change-tracker.lisp

(defvar *track-changes* t
  "Whether to track changes to definitions.")

(defun hook-into-compilation ()
  "Hook into the compilation process to track changes."
  ;; SBCL-specific
  #+sbcl
  (pushnew 'on-compile-definition sb-ext:*eval-hook*))

(defun on-compile-definition (form)
  "Called when a definition is compiled."
  (when *track-changes*
    (case (car form)
      ((defun defmacro) (index-function-definition form))
      ((defclass) (index-class-definition form))
      ((defvar defparameter defconstant) (index-variable-definition form))
      ((defgeneric) (index-generic-definition form))
      ((defmethod) (index-method-definition form))))
  form)

(defun index-function-definition (form)
  "Extract and index a function definition."
  (destructuring-bind (deftype name lambda-list &body body) form
    (declare (ignore body))
    (let ((def (make-instance 'function-definition
                 :name name
                 :kind (if (eq deftype 'defmacro) :macro :function)
                 :package *package*
                 :lambda-list lambda-list
                 :docstring (when (stringp (car body)) (car body)))))
      ;; Analyze calls
      (setf (def-calls def) (extract-calls form))
      (index-definition def))))

(defun extract-calls (form)
  "Extract function calls from a form."
  (let ((calls nil))
    (labels ((walk (f)
               (when (consp f)
                 (when (and (symbolp (car f)) (fboundp (car f)))
                   (pushnew (car f) calls))
                 (mapc #'walk (cdr f)))))
      (walk form))
    calls))
```

---

## Profiling Tools

```lisp
;;; profiling/profiler.lisp

(define-tool profile-form
    (:description "Profile execution of a Lisp form. Returns timing, allocation, and call count data."
     :parameters (:type "object"
                  :properties (:form (:type "string" :description "Form to profile")
                               :package (:type "string" :description "Package context")
                               :iterations (:type "integer" :description "Times to run (default 1)"))
                  :required ("form"))
     :trust-level :cautious
     :category :profiling)
  (let* ((pkg (find-package (or (getf params :package) *package*)))
         (iterations (or (getf params :iterations) 1))
         (*package* pkg)
         (form (read-from-string (getf params :form))))
    #+sbcl
    (with-output-to-string (s)
      (sb-profile:reset)
      ;; Profile all functions called
      (sb-profile:profile "CL-USER")  ; Adjust as needed
      (dotimes (i iterations)
        (eval form))
      (sb-profile:report :stream s)
      (sb-profile:unprofile))
    #-sbcl
    (format nil "Profiling requires SBCL implementation.")))

(define-tool get-profile-report
    (:description "Get the last profiling report with analysis."
     :parameters (:type "object" :properties () :required ())
     :trust-level :safe
     :category :profiling)
  (if *last-profile-report*
      *last-profile-report*
      "No profiling data available. Run profile-form first."))

(define-tool find-hot-spots
    (:description "Analyze profiling data to find performance bottlenecks."
     :parameters (:type "object"
                  :properties (:threshold (:type "number" :description "Minimum % to report (default 5)"))
                  :required ())
     :trust-level :safe
     :category :profiling)
  ;; Parse profiling output and identify hot spots
  (let ((threshold (or (getf params :threshold) 5)))
    (format nil "Hot spots (>~D% of time):~%~A" 
            threshold
            (extract-hot-spots *last-profile-report* threshold))))

(define-tool analyze-call-graph
    (:description "Generate a call graph for a function showing what it calls and what calls it."
     :parameters (:type "object"
                  :properties (:function (:type "string" :description "Function to analyze")
                               :package (:type "string")
                               :depth (:type "integer" :description "How many levels deep (default 2)"))
                  :required ("function"))
     :trust-level :safe
     :category :profiling)
  (let* ((fn-name (getf params :function))
         (depth (or (getf params :depth) 2))
         (def (find-definition fn-name)))
    (if def
        (with-output-to-string (s)
          (format s "Call Graph for ~A:~%~%" fn-name)
          (format s "Calls:~%")
          (print-call-tree def :calls depth s)
          (format s "~%Called by:~%")
          (print-call-tree def :called-by depth s))
        (format nil "Function ~A not found in index." fn-name))))
```

---

## Refactoring Tools

```lisp
;;; refactoring/engine.lisp

(defvar *pending-refactoring* nil
  "Pending refactoring changes awaiting confirmation.")

(defstruct refactoring-change
  type          ; :rename, :extract, :inline, :move
  description
  files         ; Affected files
  changes       ; List of (file . text-changes)
  preview)      ; Human-readable preview

(define-tool rename-symbol
    (:description "Safely rename a symbol across all files. Shows preview of changes first."
     :parameters (:type "object"
                  :properties (:symbol (:type "string" :description "Current symbol name")
                               :new-name (:type "string" :description "New name")
                               :package (:type "string" :description "Package")
                               :preview-only (:type "boolean" :description "If true, only show preview"))
                  :required ("symbol" "new-name"))
     :trust-level :dangerous
     :category :refactoring)
  (let* ((sym-name (getf params :symbol))
         (new-name (getf params :new-name))
         (pkg (find-package (or (getf params :package) *package*)))
         (sym (find-symbol (string-upcase sym-name) pkg))
         (references (find-all-references sym)))
    (unless sym
      (return-from rename-symbol "Symbol not found."))
    (let ((changes (plan-rename sym new-name references)))
      (setf *pending-refactoring* changes)
      (with-output-to-string (s)
        (format s "Rename: ~A -> ~A~%~%" sym-name new-name)
        (format s "Affected files: ~{~A~^, ~}~%~%" (refactoring-change-files changes))
        (format s "Preview:~%~A~%~%" (refactoring-change-preview changes))
        (if (getf params :preview-only)
            (format s "Use apply-refactoring to apply these changes.")
            (format s "Changes pending. Use apply-refactoring or cancel-refactoring."))))))

(define-tool extract-function
    (:description "Extract a region of code into a new function."
     :parameters (:type "object"
                  :properties (:start-line (:type "integer" :description "Start line")
                               :end-line (:type "integer" :description "End line")
                               :file (:type "string" :description "Source file")
                               :new-name (:type "string" :description "Name for new function"))
                  :required ("start-line" "end-line" "file" "new-name"))
     :trust-level :dangerous
     :category :refactoring)
  (let* ((file (getf params :file))
         (start (getf params :start-line))
         (end (getf params :end-line))
         (new-name (getf params :new-name))
         (code (extract-region file start end)))
    (multiple-value-bind (new-fn call-site free-vars)
        (analyze-for-extraction code new-name)
      (let ((changes (make-refactoring-change
                      :type :extract
                      :description (format nil "Extract lines ~D-~D to function ~A" start end new-name)
                      :files (list file)
                      :changes (list (cons file (list :add new-fn :replace call-site)))
                      :preview (format nil "New function:~%~A~%~%Replaced with:~%~A" new-fn call-site))))
        (setf *pending-refactoring* changes)
        (format nil "~A~%~%Free variables: ~{~A~^, ~}~%Use apply-refactoring to proceed."
                (refactoring-change-preview changes) free-vars)))))

(define-tool apply-refactoring
    (:description "Apply the pending refactoring changes."
     :parameters (:type "object" :properties () :required ())
     :trust-level :dangerous
     :category :refactoring)
  (unless *pending-refactoring*
    (return-from apply-refactoring "No pending refactoring."))
  (let ((result (execute-refactoring *pending-refactoring*)))
    (setf *pending-refactoring* nil)
    result))

(define-tool cancel-refactoring
    (:description "Cancel the pending refactoring."
     :parameters (:type "object" :properties () :required ())
     :trust-level :safe
     :category :refactoring)
  (if *pending-refactoring*
      (progn
        (setf *pending-refactoring* nil)
        "Refactoring cancelled.")
      "No pending refactoring."))

;; Helper functions
(defun find-all-references (sym)
  "Find all references to a symbol across the codebase."
  (let ((refs nil))
    ;; Use SLY's xref capabilities
    (dolist (ref-type '(:calls :references :specializes :binds))
      (dolist (ref (swank:xref ref-type sym))
        (push (list :type ref-type :location ref) refs)))
    refs))

(defun plan-rename (sym new-name references)
  "Plan a rename refactoring."
  (let ((files (remove-duplicates 
                (mapcar (lambda (r) (getf (second r) :file)) references)
                :test #'equal))
        (text-changes nil))
    ;; Group changes by file
    (dolist (ref references)
      (let* ((file (getf (second ref) :file))
             (pos (getf (second ref) :position))
             (change (list :position pos 
                          :old (symbol-name sym)
                          :new new-name)))
        (push change (getf text-changes file))))
    (make-refactoring-change
     :type :rename
     :description (format nil "Rename ~A to ~A" (symbol-name sym) new-name)
     :files files
     :changes text-changes
     :preview (format-rename-preview sym new-name references))))
```

---

## Pattern Detection

```lisp
;;; patterns/detector.lisp

(define-tool find-code-duplication
    (:description "Find duplicated code patterns across the codebase."
     :parameters (:type "object"
                  :properties (:package (:type "string" :description "Package to analyze")
                               :min-size (:type "integer" :description "Minimum duplicate size (default 5 forms)"))
                  :required ())
     :trust-level :safe
     :category :patterns)
  (let* ((pkg (find-package (or (getf params :package) *package*)))
         (min-size (or (getf params :min-size) 5))
         (duplicates (find-duplicates pkg min-size)))
    (if duplicates
        (with-output-to-string (s)
          (format s "Found ~D potential duplications:~%~%" (length duplicates))
          (dolist (dup duplicates)
            (format s "Pattern found ~D times:~%~A~%~%Locations:~%~{  ~A~%~}~%---~%~%"
                    (length (getf dup :locations))
                    (getf dup :pattern)
                    (getf dup :locations))))
        "No significant code duplication found.")))

(define-tool suggest-abstractions
    (:description "Analyze code and suggest potential abstractions or improvements."
     :parameters (:type "object"
                  :properties (:package (:type "string" :description "Package to analyze")
                               :function (:type "string" :description "Specific function to analyze"))
                  :required ())
     :trust-level :safe
     :category :patterns)
  (let ((suggestions nil))
    ;; Analyze for various patterns
    (push (analyze-loop-patterns) suggestions)
    (push (analyze-condition-handling) suggestions)
    (push (analyze-parameter-patterns) suggestions)
    (with-output-to-string (s)
      (format s "Improvement Suggestions:~%~%")
      (dolist (sug (remove nil suggestions))
        (format s "~A~%~%" sug)))))

(define-tool detect-code-smells
    (:description "Detect potential code quality issues."
     :parameters (:type "object"
                  :properties (:package (:type "string" :description "Package to analyze"))
                  :required ())
     :trust-level :safe
     :category :patterns)
  (let* ((pkg (find-package (or (getf params :package) *package*)))
         (smells nil))
    ;; Check various code smells
    (dolist (fn (package-functions pkg))
      ;; Long function
      (when (> (function-length fn) 50)
        (push (list :smell :long-function :function fn 
                   :length (function-length fn)) smells))
      ;; High complexity
      (when (> (cyclomatic-complexity fn) 10)
        (push (list :smell :high-complexity :function fn
                   :complexity (cyclomatic-complexity fn)) smells))
      ;; Too many parameters
      (when (> (length (function-lambda-list fn)) 6)
        (push (list :smell :many-parameters :function fn
                   :count (length (function-lambda-list fn))) smells)))
    (format-smells smells)))

(defun find-duplicates (package min-size)
  "Find duplicate code patterns in package."
  (let ((forms (collect-forms package))
        (fingerprints (make-hash-table :test 'equal))
        (duplicates nil))
    ;; Create fingerprints for subsequences
    (dolist (form forms)
      (let ((fp (fingerprint-form form)))
        (push form (gethash fp fingerprints))))
    ;; Find fingerprints with multiple entries
    (maphash (lambda (fp forms)
               (when (and (> (length forms) 1)
                         (>= (form-size (car forms)) min-size))
                 (push (list :pattern (car forms)
                            :locations (mapcar #'form-location forms))
                       duplicates)))
             fingerprints)
    duplicates))
```

---

## Project Awareness

```lisp
;;; project/asdf-parser.lisp

(define-tool analyze-asdf-system
    (:description "Analyze an ASDF system definition to understand project structure."
     :parameters (:type "object"
                  :properties (:system (:type "string" :description "System name"))
                  :required ("system"))
     :trust-level :safe
     :category :project)
  (let ((system (asdf:find-system (getf params :system) nil)))
    (unless system
      (return-from analyze-asdf-system "System not found."))
    (with-output-to-string (s)
      (format s "System: ~A~%~%" (asdf:component-name system))
      (format s "Version: ~A~%" (asdf:component-version system))
      (format s "Description: ~A~%~%" (asdf:system-description system))
      (format s "Dependencies:~%~{  ~A~%~}~%" 
              (asdf:system-depends-on system))
      (format s "Components:~%")
      (dolist (comp (asdf:component-children system))
        (format s "  ~A (~A)~%" 
                (asdf:component-name comp)
                (type-of comp))))))

(define-tool get-load-order
    (:description "Get the correct load order for system components."
     :parameters (:type "object"
                  :properties (:system (:type "string" :description "System name"))
                  :required ("system"))
     :trust-level :safe
     :category :project)
  (let* ((system (asdf:find-system (getf params :system) nil))
         (plan (asdf:make-plan 'asdf:load-op system)))
    (with-output-to-string (s)
      (format s "Load order for ~A:~%~%" (getf params :system))
      (loop for action in (asdf:plan-actions plan)
            for i from 1
            do (format s "~3D. ~A~%" i 
                      (asdf:component-name (asdf:action-component action)))))))

(define-tool find-system-for-file
    (:description "Find which ASDF system a file belongs to."
     :parameters (:type "object"
                  :properties (:file (:type "string" :description "File path"))
                  :required ("file"))
     :trust-level :safe
     :category :project)
  (let* ((file (pathname (getf params :file)))
         (system (find-system-containing file)))
    (if system
        (format nil "File ~A belongs to system ~A" 
                (getf params :file)
                (asdf:component-name system))
        "File not found in any loaded ASDF system.")))

(define-tool show-package-dependencies
    (:description "Show dependencies between packages in a system."
     :parameters (:type "object"
                  :properties (:system (:type "string" :description "System name"))
                  :required ("system"))
     :trust-level :safe
     :category :project)
  (let* ((system (asdf:find-system (getf params :system) nil))
         (packages (collect-system-packages system))
         (deps (analyze-package-dependencies packages)))
    (with-output-to-string (s)
      (format s "Package dependencies in ~A:~%~%" (getf params :system))
      (dolist (pkg packages)
        (let ((uses (package-use-list (find-package pkg))))
          (format s "~A uses: ~{~A~^, ~}~%" pkg 
                  (mapcar #'package-name uses)))))))
```

---

## Semantic Tools

```lisp
;;; tools/semantic-tools.lisp

(define-tool rebuild-index
    (:description "Rebuild the semantic index for a package or system."
     :parameters (:type "object"
                  :properties (:package (:type "string" :description "Package to index")
                               :system (:type "string" :description "ASDF system to index"))
                  :required ())
     :trust-level :safe
     :category :semantic)
  (cond
    ((getf params :system)
     (index-asdf-system (getf params :system)))
    ((getf params :package)
     (index-package (getf params :package)))
    (t (return-from rebuild-index "Specify :package or :system")))
  "Index rebuilt.")

(define-tool query-dependencies
    (:description "Query the dependency graph for a symbol."
     :parameters (:type "object"
                  :properties (:symbol (:type "string" :description "Symbol name")
                               :direction (:type "string" :description "dependencies or dependents")
                               :relationship (:type "string" :description "Filter: calls, references, inherits, etc."))
                  :required ("symbol"))
     :trust-level :safe
     :category :semantic)
  (let* ((sym (getf params :symbol))
         (direction (or (getf params :direction) "both"))
         (rel (when (getf params :relationship)
               (intern (string-upcase (getf params :relationship)) :keyword))))
    (with-output-to-string (s)
      (when (member direction '("dependencies" "both") :test #'string-equal)
        (format s "~A depends on:~%" sym)
        (dolist (dep (get-dependencies sym :relationship rel))
          (format s "  ~A (~A)~%" (car dep) (cdr dep))))
      (when (member direction '("dependents" "both") :test #'string-equal)
        (format s "~%Depends on ~A:~%" sym)
        (dolist (dep (get-dependents sym :relationship rel))
          (format s "  ~A (~A)~%" (car dep) (cdr dep)))))))

(define-tool get-definition-info
    (:description "Get comprehensive info about a definition from the semantic index."
     :parameters (:type "object"
                  :properties (:name (:type "string" :description "Definition name")
                               :package (:type "string" :description "Package"))
                  :required ("name"))
     :trust-level :safe
     :category :semantic)
  (let ((def (find-definition (getf params :name) 
                              (find-package (or (getf params :package) *package*)))))
    (if def
        (with-output-to-string (s)
          (format s "Name: ~A~%" (def-name def))
          (format s "Kind: ~A~%" (def-kind def))
          (format s "Package: ~A~%" (package-name (def-package def)))
          (format s "File: ~A~%" (def-source-file def))
          (when (def-docstring def)
            (format s "Documentation: ~A~%" (def-docstring def)))
          (when (def-lambda-list def)
            (format s "Lambda list: ~A~%" (def-lambda-list def)))
          (when (typep def 'function-definition)
            (format s "Calls: ~{~A~^, ~}~%" (def-calls def))
            (format s "Called by: ~{~A~^, ~}~%" (def-called-by def))))
        "Definition not found in index.")))
```

---

## Updated System Prompt

```lisp
(defparameter *phase-4-system-prompt*
  "You are Agent-Q, an intelligent development partner deeply integrated into a Common Lisp environment.

## Full Capabilities

### From Previous Phases
[All introspection, execution, buffer, debugging, testing, tracing, knowledge tools]

### Semantic Understanding (NEW)
- `rebuild-index`: Index package/system definitions
- `query-dependencies`: Explore dependency relationships
- `get-definition-info`: Get comprehensive definition data

### Profiling (NEW)
- `profile-form`: Profile code execution
- `find-hot-spots`: Identify performance bottlenecks
- `analyze-call-graph`: Visualize call relationships

### Refactoring (NEW)
- `rename-symbol`: Safe rename across files
- `extract-function`: Extract code to new function
- `apply-refactoring`: Apply pending changes
- `cancel-refactoring`: Cancel pending changes

### Pattern Analysis (NEW)
- `find-code-duplication`: Detect duplicate code
- `suggest-abstractions`: Get improvement suggestions
- `detect-code-smells`: Find quality issues

### Project Awareness (NEW)
- `analyze-asdf-system`: Understand system structure
- `get-load-order`: See compilation order
- `find-system-for-file`: Map files to systems
- `show-package-dependencies`: See package relationships

## Being an Intelligent Partner

You now have deep understanding of codebases. Use this to:

1. **Proactively identify issues**: When analyzing code, look for duplication, complexity, and improvement opportunities.

2. **Suggest architectural improvements**: Use dependency analysis to understand structure and suggest better organization.

3. **Safe refactoring**: Always preview changes before applying. Track all references.

4. **Performance awareness**: Profile before optimizing. Focus on hot spots.

5. **Learn the codebase**: Build up knowledge through the semantic index. Remember patterns and conventions.

## Complex Request Workflow

For requests like 'restructure this module':
1. Analyze current structure with semantic tools
2. Find dependencies and dependents
3. Detect patterns and duplication
4. Propose refactoring plan with previews
5. Apply changes incrementally with tests
6. Verify no regressions")
```

---

## Testing Criteria

### Semantic Index
- [ ] Index captures functions, classes, variables
- [ ] Dependencies tracked accurately
- [ ] Change detection updates index
- [ ] Query tools return correct data

### Profiling
- [ ] Profile form captures timing
- [ ] Hot spots identified correctly
- [ ] Call graph generated accurately

### Refactoring
- [ ] Rename finds all references
- [ ] Preview shows accurate changes
- [ ] Apply modifies files correctly
- [ ] Cancel clears pending state

### Patterns
- [ ] Duplication detected
- [ ] Suggestions are actionable
- [ ] Code smells identified

### Project
- [ ] ASDF systems parsed correctly
- [ ] Load order determined
- [ ] Package dependencies mapped

---

## Definition of Done

Phase 4 is complete when:

1. ✅ Semantic index captures all definition types
2. ✅ Dependency graph populated and queryable
3. ✅ Change tracking keeps index updated
4. ✅ Profiling tools functional
5. ✅ Rename refactoring working safely
6. ✅ Extract function working
7. ✅ Duplication detection working
8. ✅ ASDF integration complete
9. ✅ Agent provides architectural insights
10. ✅ Full workflow scenarios pass
