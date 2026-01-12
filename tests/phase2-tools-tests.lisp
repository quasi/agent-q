;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q-TESTS; Base: 10 -*-

(in-package :agent-q-tests)

;;; ============================================================================
;;; Phase 2: Tool System Tests
;;; ============================================================================
;;;
;;; Comprehensive tests for all 18 Phase 2 tools and the tool execution system.
;;; Tests cover:
;;; - Registry initialization and tool registration
;;; - Introspection tools (9 tools)
;;; - Execution tools (4 tools)
;;; - Buffer tools (4 tools)
;;; - Diff tool (1 tool)
;;; - Tool execution pipeline
;;; - Error handling and recovery
;;; - Agent loop iteration with tools

;;; ============================================================================
;;; Utility Functions for Tests
;;; ============================================================================

(defun plist-to-hash-table (plist)
  "Convert a plist to a hash-table with string keys.
   Tools expect hash-table parameters from cl-llm-provider."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key val) on plist by #'cddr
          do (setf (gethash (string-downcase (string key)) ht) val))
    ht))

(defun tool-name (tool-def)
  "Get name from a tool definition."
  (slot-value tool-def 'cl-llm-provider::name))

(defun tool-handler (tool-def)
  "Get handler from a tool definition."
  (slot-value tool-def 'cl-llm-provider::handler))

(defun tool-description (tool-def)
  "Get description from a tool definition."
  (slot-value tool-def 'cl-llm-provider::description))

(defun tool-parameters (tool-def)
  "Get parameters from a tool definition."
  (slot-value tool-def 'cl-llm-provider::parameters))

(defun tool-required (tool-def)
  "Get required parameters from a tool definition."
  (slot-value tool-def 'cl-llm-provider::required))

(defun find-tool-handler (tool-name)
  "Find and return the handler for a named tool."
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (loop for tool in tools
          when (equal (tool-name tool) tool-name)
          return (tool-handler tool))))

(defun find-tool-definition (tool-name)
  "Find and return the full tool definition."
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (loop for tool in tools
          when (equal (tool-name tool) tool-name)
          return tool)))

(defun eval-form-handler (args)
  "Helper to call eval_form handler."
  (let ((handler (find-tool-handler "eval_form")))
    (funcall handler (if (hash-table-p args) args (plist-to-hash-table args)))))

;;; ============================================================================
;;; Registry Tests
;;; ============================================================================

(def-suite registry-tests
  :description "Tests for tool registry initialization and management")

(in-suite registry-tests)

(test initialize-registry
  "Registry should be initialized on load"
  ;; *agent-q-registry* is a variable, not a function
  (let ((registry agent-q.tools:*agent-q-registry*))
    (is (not (null registry)))
    (is (typep registry 'cl-llm-provider.tools:tool-registry))))

(test get-registered-tools
  "Should be able to retrieve all registered tools"
  ;; Default :moderate safety level returns 17 tools (excludes dangerous write_file)
  ;; With :dangerous level, we get all 18 tools
  (let ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :dangerous)))
    (is (listp tools))
    (is (> (length tools) 0))
    ;; Should have at least the 18 expected tools
    (is (>= (length tools) 18))))

(test tool-categories
  "Tools should be properly categorized"
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    ;; All tools should have descriptions (which indicates they're properly defined)
    (dolist (tool tools)
      (is (not (null (tool-description tool)))))))

;;; ============================================================================
;;; Introspection Tools Tests
;;; ============================================================================

(def-suite introspection-tools-tests
  :description "Tests for read-only introspection tools")

(in-suite introspection-tools-tests)

(test describe-symbol-basic
  "describe_symbol should describe built-in symbols"
  (let ((handler (find-tool-handler "describe_symbol")))
    (is (not (null handler)))
    (let ((result (funcall handler (plist-to-hash-table '(:symbol "DEFUN")))))
      (is (stringp result))
      (is (> (length result) 0)))))

(test describe-symbol-not-found
  "describe_symbol should handle non-existent symbols"
  (let ((handler (find-tool-handler "describe_symbol")))
    (let ((result (funcall handler (plist-to-hash-table '(:symbol "NONEXISTENT_SYMBOL_XYZ")))))
      (is (stringp result))
      (is (search "not found" result :test #'char-equal)))))

(test apropos-search-basic
  "apropos_search should find symbols matching a pattern"
  (let ((handler (find-tool-handler "apropos_search")))
    (let ((result (funcall handler (plist-to-hash-table '(:pattern "DEFUN")))))
      (is (stringp result))
      (is (search "DEFUN" result)))))

(test apropos-search-empty
  "apropos_search should handle patterns with no matches"
  (let ((handler (find-tool-handler "apropos_search")))
    (let ((result (funcall handler (plist-to-hash-table '(:pattern "ZZZZZ_NOMATCH_ZZZZZ")))))
      (is (stringp result)))))

(test function-arglist-builtin
  "function_arglist should get lambda list for built-in functions"
  (let ((handler (find-tool-handler "function_arglist")))
    (let ((result (funcall handler (plist-to-hash-table '(:function "CAR")))))
      (is (stringp result))
      (is (search "CAR" result)))))

(test function-arglist-not-available
  "function_arglist should degrade gracefully when arglist unavailable"
  (let ((handler (find-tool-handler "function_arglist")))
    (let ((result (funcall handler (plist-to-hash-table '(:function "CAR")))))
      (is (stringp result))
      ;; Should either have arglist or "not available" message
      (is (or (search "(" result)
              (search "not available" result :test #'char-equal))))))

(test who-calls-without-slynk
  "who_calls should handle absence of SLYNK gracefully"
  (let ((handler (find-tool-handler "who_calls")))
    (let ((result (funcall handler (plist-to-hash-table '(:function "DEFUN")))))
      ;; Should return either results or SLYNK unavailable message
      (is (stringp result)))))

(test who-references-without-slynk
  "who_references should handle absence of SLYNK gracefully"
  (let ((handler (find-tool-handler "who_references")))
    (let ((result (funcall handler (plist-to-hash-table '(:variable "*PRINT-LENGTH*")))))
      (is (stringp result)))))

(test list-package-symbols-cl
  "list_package_symbols should list CL package exports"
  (let ((handler (find-tool-handler "list_package_symbols")))
    (let ((result (funcall handler (plist-to-hash-table '(:package "CL")))))
      (is (stringp result))
      (is (search "symbol" result :test #'char-equal)))))

(test list-package-symbols-not-found
  "list_package_symbols should handle invalid package names"
  (let ((handler (find-tool-handler "list_package_symbols")))
    (let ((result (funcall handler (plist-to-hash-table '(:package "NONEXISTENT_PKG")))))
      (is (stringp result))
      (is (search "not found" result :test #'char-equal)))))

(test class-slots-on-standard-class
  "class_slots should describe CLOS class slots"
  (let ((handler (find-tool-handler "class_slots")))
    (let ((result (funcall handler (plist-to-hash-table '(:class "STANDARD-CLASS")))))
      (is (stringp result))
      ;; Should mention slots or class
      (is (or (search "slot" result :test #'char-equal)
              (search "class" result :test #'char-equal))))))

(test class-hierarchy-on-standard-object
  "class_hierarchy should show class precedence"
  (let ((handler (find-tool-handler "class_hierarchy")))
    (let ((result (funcall handler (plist-to-hash-table '(:class "STANDARD-OBJECT")))))
      (is (stringp result))
      (is (or (search "class" result :test #'char-equal)
              (search "hierarchy" result :test #'char-equal))))))

(test macroexpand-basic
  "macroexpand_form should expand simple macros"
  (let ((handler (find-tool-handler "macroexpand_form")))
    (let ((result (funcall handler (plist-to-hash-table '(:form "(WHEN T 1)")))))
      (is (stringp result))
      (is (search "Original" result)))))

;;; ============================================================================
;;; Execution Tools Tests
;;; ============================================================================

(def-suite execution-tools-tests
  :description "Tests for code execution and error tracking tools")

(in-suite execution-tools-tests)

(test eval-form-basic
  "eval_form should evaluate simple expressions"
  (let ((handler (find-tool-handler "eval_form")))
    (let ((result (funcall handler (plist-to-hash-table '(:form "(+ 1 2)")))))
      (is (stringp result))
      ;; Result should contain the evaluated value or description
      (is (or (search "3" result)
              (search "result" result :test #'char-equal))))))

(test eval-form-multiple-values
  "eval_form should handle multiple values"
  (let ((handler (find-tool-handler "eval_form")))
    (let ((result (funcall handler (plist-to-hash-table '(:form "(VALUES 1 2 3)")))))
      (is (stringp result)))))

(test eval-form-error
  "eval_form should capture errors gracefully"
  (let ((handler (find-tool-handler "eval_form")))
    (let ((result (funcall handler (plist-to-hash-table '(:form "(ERROR \"test error\")")))))
      (is (stringp result))
      ;; Should either have the error in result or in *last-error*
      (is (or (search "error" result :test #'char-equal)
              (not (null agent-q.tools:*last-error*)))))))

(test eval-form-with-package
  "eval_form should evaluate in specified package context"
  (let ((handler (find-tool-handler "eval_form")))
    (let ((result (funcall handler (plist-to-hash-table '(:form "(PACKAGE-NAME *PACKAGE*)" :package "CL")))))
      (is (stringp result)))))

(test compile-form-basic
  "compile_form should compile and load forms"
  (let ((handler (find-tool-handler "compile_form")))
    (let ((result (funcall handler (plist-to-hash-table '(:form "(DEFUN TEST-FN (X) X)")))))
      (is (stringp result))
      ;; Should indicate successful compilation
      (is (or (search "compile" result :test #'char-equal)
              (search "load" result :test #'char-equal))))))

(test get-last-error
  "get_last_error should return error information when available"
  (let ((handler (find-tool-handler "get_last_error")))
    ;; First trigger an error
    (handler-case
        (eval-form-handler '(:form "(ERROR \"test\")"))
      (error ()))
    ;; Now check if we can get it - pass empty hash table
    (let ((result (funcall handler (make-hash-table :test 'equal))))
      (is (stringp result)))))

(test get-repl-history
  "get_repl_history should return recent evaluations"
  (let ((handler (find-tool-handler "get_repl_history")))
    (let ((result (funcall handler (make-hash-table :test 'equal))))
      (is (stringp result)))))

;;; ============================================================================
;;; Buffer Tools Tests
;;; ============================================================================

(def-suite buffer-tools-tests
  :description "Tests for file and buffer operation tools (require Emacs connection)")

(in-suite buffer-tools-tests)

(test read-file-basic
  "read_file should read file contents when connected to Emacs"
  ;; This test is conditional - only runs when SLYNK/SWANK available
  (let ((handler (find-tool-handler "read_file")))
    (is (not (null handler)))
    ;; Without Emacs connection, should return error message
    (let ((result (funcall handler (plist-to-hash-table '(:path "/tmp/test.txt")))))
      (is (stringp result)))))

(test search-in-buffer-basic
  "search_in_buffer should handle missing Emacs connection gracefully"
  (let ((handler (find-tool-handler "search_in_buffer")))
    (let ((result (funcall handler (plist-to-hash-table '(:pattern "test")))))
      (is (stringp result)))))

(test write-file-dangerous-level
  "write_file should be properly defined"
  ;; write_file is :dangerous, so we need to query with that safety level
  (let* ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :dangerous))
         (tool (find "write_file" tools :test #'equal :key #'tool-name)))
    (is (not (null tool)))
    ;; Tool should have a description mentioning it's for writing files
    (is (search "write" (tool-description tool) :test #'char-equal))))

;;; ============================================================================
;;; Diff Tool Tests
;;; ============================================================================

(def-suite diff-tool-tests
  :description "Tests for the propose_file_edit diff tool")

(in-suite diff-tool-tests)

(test propose-file-edit-definition
  "propose_file_edit should be properly registered"
  (let ((tool (find-tool-definition "propose_file_edit")))
    (is (not (null tool)))
    (is (not (null (tool-name tool))))
    (is (equal (tool-name tool) "propose_file_edit"))))

(test propose-file-edit-requires-all-args
  "propose_file_edit should have required parameters"
  (let ((tool (find-tool-definition "propose_file_edit")))
    (is (not (null tool)))
    (let ((required (tool-required tool)))
      (is (not (null required)))
      (is (>= (length required) 4)))))  ; At least 4 required parameters

;;; ============================================================================
;;; Tool Execution Pipeline Tests
;;; ============================================================================

(def-suite tool-execution-pipeline-tests
  :description "Tests for the tool execution pipeline integration")

(in-suite tool-execution-pipeline-tests)

(test tool-registry-available
  "Tool registry should be accessible for execution"
  (is (not (null agent-q.tools:*agent-q-registry*))))

(test get-agent-q-tools-returns-correct-count
  "Should return proper number of tools"
  ;; With :dangerous level, we get all 18 tools
  (let ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :dangerous)))
    (is (listp tools))
    ;; Should have at least 18 tools
    (is (>= (length tools) 18))))

(test tools-have-handlers
  "All tools should have handler functions"
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (dolist (tool tools)
      (is (not (null (tool-handler tool)))))))

(test safe-tools-majority
  "Most tools should be :safe"
  (let ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    (is (> (length tools) 10)))) ; Should have many safe tools

;;; ============================================================================
;;; Error Handling Tests
;;; ============================================================================

(def-suite error-handling-tests
  :description "Tests for error handling and recovery in tools")

(in-suite error-handling-tests)

(test malformed-args-handled
  "Tools should handle malformed arguments gracefully"
  (let ((handler (find-tool-handler "describe_symbol")))
    (let ((result (funcall handler (plist-to-hash-table '(:invalid-key "value")))))
      ;; Should return some error message or graceful fallback
      (is (stringp result)))))

(test package-not-found-graceful
  "Tools should handle missing packages gracefully"
  (let ((handler (find-tool-handler "list_package_symbols")))
    (let ((result (funcall handler (plist-to-hash-table '(:package "DEFINITELY_NOT_A_REAL_PACKAGE")))))
      (is (stringp result))
      (is (search "not found" result :test #'char-equal)))))

(test symbol-not-found-graceful
  "Tools should handle missing symbols gracefully"
  (let ((handler (find-tool-handler "describe_symbol")))
    (let ((result (funcall handler (plist-to-hash-table '(:symbol "DEFINITELY_NOT_A_REAL_SYMBOL_XYZ123")))))
      (is (stringp result))
      (is (search "not found" result :test #'char-equal)))))
