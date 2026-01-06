;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q.TOOLS; Base: 10 -*-

(in-package :agent-q.tools)

;;; Introspection Tools
;;;
;;; Read-only tools for inspecting the running Lisp image.
;;; All tools in this file are :safe - they never modify state.

;;; 1. describe-symbol

(let ((tool (define-tool
              "describe_symbol"
              "Get detailed information about a Lisp symbol including its type, value, documentation, and definition location."
              '((:name "symbol" :type :string :description "The symbol name to describe")
                (:name "package" :type :string :description "Package name (optional, defaults to current package)"))
              :required '("symbol")
              :safety-level :safe
              :categories '(:introspection)
              :handler (lambda (args)
                         (let* ((sym-name (gethash "symbol" args))
                                (pkg-name (gethash "package" args))
                                (pkg (if pkg-name
                                        (find-package pkg-name)
                                        *package*)))
                           (if (not pkg)
                               (format nil "Package ~A not found" pkg-name)
                               (multiple-value-bind (sym status)
                                   (find-symbol (string-upcase sym-name) pkg)
                                 (if sym
                                     (with-output-to-string (s)
                                       (describe sym s))
                                     (format nil "Symbol ~A not found in package ~A (status: ~A)"
                                            sym-name (package-name pkg) status)))))))))
  (register-tool *agent-q-registry* tool))

;;; 2. apropos-search

(let ((tool (define-tool
		"apropos_search"
              "Search for symbols matching a pattern. Returns symbol names, types, and packages. Useful for discovering available functions and variables."
              '((:name "pattern" :type :string :description "Search pattern (substring match, case-insensitive)")
                (:name "package" :type :string :description "Limit search to this package (optional)"))
              :required '("pattern")
              :safety-level :safe
              :categories '(:introspection)
              :handler (lambda (args)
                         (let ((pattern (gethash "pattern" args))
                               (pkg-name (gethash "package" args)))
                           ;; Handle package not found case first
			       (if (and pkg-name (not (find-package pkg-name)))
				   (format nil "Package ~A not found" pkg-name)
				   ;; Otherwise, get results and format them
				   (with-output-to-string (s)
				     (let ((results (if pkg-name
							(apropos-list pattern (find-package pkg-name))
							(apropos-list pattern))))
				       (format s "Found ~D symbol~:P matching '~A':~%~%"
					       (length results) pattern)
				       (dolist (sym results)
					 (let ((s-type (cond
							 ((fboundp sym)
							  (cond
							    ((macro-function sym) "macro")
							    ((typep (symbol-function sym) 'generic-function) "generic-function")
							    (t "function")))
							 ((boundp sym) "variable")
							 ((find-class sym nil) "class")
							 (t "symbol"))))
					   (format s "~A::~A [~A]~%"
						   (package-name (symbol-package sym))
						   (symbol-name sym)
						   s-type)))))))))))
  (register-tool *agent-q-registry* tool))

;;; 3. function-arglist

(let ((tool (define-tool
              "function_arglist"
              "Get the argument list (lambda list) for a function. Shows required, optional, rest, and keyword parameters."
              '((:name "function" :type :string :description "Function name")
                (:name "package" :type :string :description "Package name (optional)"))
              :required '("function")
              :safety-level :safe
              :categories '(:introspection)
              :handler (lambda (args)
                         (let* ((fn-name (gethash "function" args))
                                (pkg (find-package (or (gethash "package" args) *package*)))
                                (sym (and pkg (find-symbol (string-upcase fn-name) pkg))))
                           (cond
                             ((not pkg)
                              (format nil "Package ~A not found" (gethash "package" args)))
                             ((not sym)
                              (format nil "Symbol ~A not found in package ~A"
                                     fn-name (package-name pkg)))
                             ((not (fboundp sym))
                              (format nil "~A is not a function" sym))
                             (t
                              ;; Try SLYNK arglist if available, otherwise use fallback
                              (let ((arglist
                                     (handler-case
                                         (cond
                                           ;; Try SLYNK (SLY) if available
                                           ((find-package :slynk-backend)
                                            (let* ((slynk-backend (find-package :slynk-backend))
                                                   (arglist-fn (find-symbol "ARGLIST" slynk-backend)))
                                              (when arglist-fn
                                                (funcall arglist-fn sym))))
                                           ;; Fallback: try SBCL introspection if available
                                           ((find-package :sb-introspect)
                                            (let* ((sb-introspect (find-package :sb-introspect))
                                                   (lambda-list-fn (find-symbol "FUNCTION-LAMBDA-LIST" sb-introspect)))
                                              (when lambda-list-fn
                                                (funcall lambda-list-fn sym))))
                                           ;; Generic fallback
                                           (t :not-available))
                                       (error () :not-available))))
                                (format nil "~A ~A" sym
                                       (if (eq arglist :not-available)
                                           "(arglist not available)"
                                           arglist))))))))))
  (register-tool *agent-q-registry* tool))

;;; 4. who-calls

(let ((tool (define-tool
              "who_calls"
              "Find all functions that call the specified function. Uses cross-reference database."
              '((:name "function" :type :string :description "Function name to find callers of")
                (:name "package" :type :string :description "Package name (optional)"))
              :required '("function")
              :safety-level :safe
              :categories '(:introspection :xref)
              :handler (lambda (args)
                         (let* ((fn-name (gethash "function" args))
                                (pkg (find-package (or (gethash "package" args) *package*)))
                                (sym (and pkg (find-symbol (string-upcase fn-name) pkg))))
                           (cond
                             ((not pkg)
                              (format nil "Package ~A not found" (gethash "package" args)))
                             ((not sym)
                              (format nil "Symbol ~A not found in package ~A"
                                     fn-name (package-name pkg)))
                             (t
                              ;; Try SLYNK xref if available
                              (let ((callers
                                     (handler-case
                                         (cond
                                           ((find-package :slynk)
                                            (let* ((slynk (find-package :slynk))
                                                   (xref-fn (find-symbol "XREF" slynk)))
                                              (when xref-fn
                                                (funcall xref-fn :calls sym))))
                                           (t :not-available))
                                       (error () :not-available))))
                                (if (eq callers :not-available)
                                    (format nil "XREF not available (requires SLYNK connection)")
                                    (if callers
                                        (with-output-to-string (s)
                                          (format s "Functions calling ~A:~%~%" sym)
                                          (dolist (caller callers)
                                            (format s "  ~A~%" caller)))
                                        (format nil "No callers found for ~A" sym)))))))))))
  (register-tool *agent-q-registry* tool))

;;; 5. who-references

(let ((tool (define-tool
              "who_references"
              "Find all code that references (reads) the specified variable or constant."
              '((:name "variable" :type :string :description "Variable or constant name")
                (:name "package" :type :string :description "Package name (optional)"))
              :required '("variable")
              :safety-level :safe
              :categories '(:introspection :xref)
              :handler (lambda (args)
                         (let* ((var-name (gethash "variable" args))
                                (pkg (find-package (or (gethash "package" args) *package*)))
                                (sym (and pkg (find-symbol (string-upcase var-name) pkg))))
                           (cond
                             ((not pkg)
                              (format nil "Package ~A not found" (gethash "package" args)))
                             ((not sym)
                              (format nil "Symbol ~A not found in package ~A"
                                     var-name (package-name pkg)))
                             (t
                              ;; Try SLYNK xref if available
                              (let ((refs
                                     (handler-case
                                         (cond
                                           ((find-package :slynk)
                                            (let* ((slynk (find-package :slynk))
                                                   (xref-fn (find-symbol "XREF" slynk)))
                                              (when xref-fn
                                                (funcall xref-fn :references sym))))
                                           (t :not-available))
                                       (error () :not-available))))
                                (if (eq refs :not-available)
                                    (format nil "XREF not available (requires SLYNK connection)")
                                    (if refs
                                        (with-output-to-string (s)
                                          (format s "References to ~A:~%~%" sym)
                                          (dolist (ref refs)
                                            (format s "  ~A~%" ref)))
                                        (format nil "No references found for ~A" sym)))))))))))
  (register-tool *agent-q-registry* tool))

;;; 6. list-package-symbols

(let ((tool (define-tool
              "list_package_symbols"
              "List all exported symbols from a package with their types. Optionally include internal symbols."
              '((:name "package" :type :string :description "Package name")
                (:name "include_internal" :type :boolean :description "Include internal symbols (default false)"))
              :required '("package")
              :safety-level :safe
              :categories '(:introspection :packages)
              :handler (lambda (args)
                         (let* ((pkg-name (gethash "package" args))
                                (include-internal (gethash "include_internal" args))
                                (pkg (find-package pkg-name)))
                           (if (not pkg)
                               (format nil "Package ~A not found" pkg-name)
                               (with-output-to-string (s)
                                 (let ((symbols '()))
                                   (if include-internal
                                       (do-symbols (sym pkg)
                                         (when (eq (symbol-package sym) pkg)
                                           (push sym symbols)))
                                       (do-external-symbols (sym pkg)
                                         (push sym symbols)))
                                   (setf symbols (sort symbols #'string< :key #'symbol-name))
                                   (format s "Package ~A (~D symbol~:P):~%~%"
                                          (package-name pkg) (length symbols))
                                   (dolist (sym symbols)
                                     (let ((type (cond
                                                  ((fboundp sym)
                                                   (cond
                                                     ((macro-function sym) "macro")
                                                     ((typep (symbol-function sym) 'generic-function)
                                                      "generic-function")
                                                     (t "function")))
                                                  ((boundp sym) "variable")
                                                  ((find-class sym nil) "class")
                                                  (t "symbol"))))
                                       (format s "  ~A [~A]~%" (symbol-name sym) type)))))))))))
  (register-tool *agent-q-registry* tool))

;;; 7. class-slots

(let ((tool (define-tool
              "class_slots"
              "Get slot definitions for a CLOS class including names, types, initargs, and initforms."
              '((:name "class" :type :string :description "Class name")
                (:name "package" :type :string :description "Package name (optional)"))
              :required '("class")
              :safety-level :safe
              :categories '(:introspection :clos)
              :handler (lambda (args)
                         (let* ((class-name (gethash "class" args))
                                (pkg (find-package (or (gethash "package" args) *package*)))
                                (sym (and pkg (find-symbol (string-upcase class-name) pkg)))
                                (class (and sym (find-class sym nil))))
                           (cond
                             ((not pkg)
                              (format nil "Package ~A not found" (gethash "package" args)))
                             ((not sym)
                              (format nil "Symbol ~A not found in package ~A"
                                     class-name (package-name pkg)))
                             ((not class)
                              (format nil "~A is not a class" sym))
                             (t
                              (closer-mop:ensure-finalized class)
                              (with-output-to-string (s)
                                (format s "Class: ~A~%~%" sym)
                                (let ((slots (closer-mop:class-slots class)))
                                  (format s "Slots (~D):~%~%" (length slots))
                                  (dolist (slot slots)
                                    (format s "  ~A~%"
                                           (closer-mop:slot-definition-name slot))
                                    (let ((initargs (closer-mop:slot-definition-initargs slot))
                                          (type (closer-mop:slot-definition-type slot))
                                          (initform (closer-mop:slot-definition-initform slot)))
                                      (when initargs
                                        (format s "    Initarg: ~{~A~^, ~}~%" initargs))
                                      (unless (eq type t)
                                        (format s "    Type: ~A~%" type))
                                      (when initform
                                        (format s "    Initform: ~S~%" initform))
                                      (terpri s))))))))))))
  (register-tool *agent-q-registry* tool))

;;; 8. class-hierarchy

(let ((tool (define-tool
              "class_hierarchy"
              "Get the superclasses and subclasses of a CLOS class. Shows full class precedence list."
              '((:name "class" :type :string :description "Class name")
                (:name "package" :type :string :description "Package name (optional)"))
              :required '("class")
              :safety-level :safe
              :categories '(:introspection :clos)
              :handler (lambda (args)
                         (let* ((class-name (gethash "class" args))
                                (pkg (find-package (or (gethash "package" args) *package*)))
                                (sym (and pkg (find-symbol (string-upcase class-name) pkg)))
                                (class (and sym (find-class sym nil))))
                           (cond
                             ((not pkg)
                              (format nil "Package ~A not found" (gethash "package" args)))
                             ((not sym)
                              (format nil "Symbol ~A not found in package ~A"
                                     class-name (package-name pkg)))
                             ((not class)
                              (format nil "~A is not a class" sym))
                             (t
                              (closer-mop:ensure-finalized class)
                              (with-output-to-string (s)
                                (format s "Class: ~A~%~%" sym)
                                (format s "Class Precedence List:~%")
                                (dolist (super (closer-mop:class-precedence-list class))
                                  (format s "  ~A~%" (class-name super)))
                                (terpri s)
                                (let ((subs (closer-mop:class-direct-subclasses class)))
                                  (if subs
                                      (progn
                                        (format s "Direct Subclasses (~D):~%" (length subs))
                                        (dolist (sub subs)
                                          (format s "  ~A~%" (class-name sub))))
                                      (format s "No direct subclasses~%")))))))))))
  (register-tool *agent-q-registry* tool))

;;; 9. macroexpand-form

(let ((tool (define-tool
              "macroexpand_form"
              "Expand macros in a Lisp form. Use for understanding macro transformations."
              '((:name "form" :type :string :description "The Lisp form to expand (as a string)")
                (:name "full" :type :boolean :description "If true, use full macroexpand, otherwise macroexpand-1 (default false)")
                (:name "package" :type :string :description "Package context for reading the form (optional)"))
              :required '("form")
              :safety-level :safe
              :categories '(:introspection)
              :handler (lambda (args)
                         (let* ((form-string (gethash "form" args))
                                (full (gethash "full" args))
                                (pkg (find-package (or (gethash "package" args) *package*))))
                           (if (not pkg)
                               (format nil "Package ~A not found" (gethash "package" args))
                               (handler-case
                                   (let* ((*package* pkg)
                                          (form (read-from-string form-string))
                                          (expanded (if full
                                                       (macroexpand form)
                                                       (macroexpand-1 form))))
                                     (with-output-to-string (s)
                                       (format s "Original:~%")
                                       (pprint form s)
                                       (format s "~%~%Expanded (~A):~%"
                                              (if full "full" "one step"))
                                       (pprint expanded s)))
                                 (error (e)
                                   (format nil "Error expanding form: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
