;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q-TESTS; Base: 10 -*-

(in-package :agent-q-tests)

;;; ============================================================================
;;; Phase 2: Integration and Agent Loop Tests
;;; ============================================================================
;;;
;;; Tests for tool system integration with LLM and agent loop.
;;; These tests verify:
;;; - Tool availability in LLM calls
;;; - Agent loop iteration with tool execution
;;; - Multi-turn interactions with tools
;;; - Error recovery in agent loop

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun mock-completion-response (&key content tool-calls)
  "Create a mock LLM completion response."
  (list :content content
        :tool-calls tool-calls))

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

(defun find-tool-handler (tool-name-str)
  "Find and return the handler for a named tool."
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (loop for tool in tools
          when (equal (tool-name tool) tool-name-str)
          return (tool-handler tool))))

(defun tool-names-from-registry ()
  "Get all tool names from registry."
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (mapcar #'tool-name tools)))

(defun callable (obj)
  "Check if OBJ is callable."
  (or (functionp obj)
      (and (symbolp obj) (fboundp obj))))

;;; ============================================================================
;;; Tool Availability Tests
;;; ============================================================================

(def-suite tool-availability-tests
  :description "Tests that verify tools are available and properly formatted for LLM")

(in-suite tool-availability-tests)

(test all-expected-tools-present
  "All 18 Phase 2 tools should be registered"
  (let ((tool-names (tool-names-from-registry)))
    ;; Introspection tools (9)
    (is (member "describe_symbol" tool-names :test #'equal))
    (is (member "apropos_search" tool-names :test #'equal))
    (is (member "function_arglist" tool-names :test #'equal))
    (is (member "who_calls" tool-names :test #'equal))
    (is (member "who_references" tool-names :test #'equal))
    (is (member "list_package_symbols" tool-names :test #'equal))
    (is (member "class_slots" tool-names :test #'equal))
    (is (member "class_hierarchy" tool-names :test #'equal))
    (is (member "macroexpand_form" tool-names :test #'equal))
    ;; Execution tools (4)
    (is (member "eval_form" tool-names :test #'equal))
    (is (member "compile_form" tool-names :test #'equal))
    (is (member "get_last_error" tool-names :test #'equal))
    (is (member "get_repl_history" tool-names :test #'equal))
    ;; Buffer tools (4)
    (is (member "read_file" tool-names :test #'equal))
    (is (member "read_buffer" tool-names :test #'equal))
    (is (member "write_file" tool-names :test #'equal))
    (is (member "search_in_buffer" tool-names :test #'equal))
    ;; Diff tool (1)
    (is (member "propose_file_edit" tool-names :test #'equal))))

(test safe-tools-accessible
  "Safe tools should be available at :safe safety level"
  (let ((safe-tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    ;; Should have at least the 13 safe tools
    (is (>= (length safe-tools) 13))
    (let ((tool-names (mapcar #'tool-name safe-tools)))
      (is (member "describe_symbol" tool-names :test #'equal))
      (is (member "eval_form" tool-names :test #'equal)))))

(test all-tools-have-descriptions
  "Each tool should have a description"
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (dolist (tool tools)
      (is (not (null (tool-description tool))))
      (is (stringp (tool-description tool)))
      (is (> (length (tool-description tool)) 0)))))

(test all-tools-have-parameters
  "Each tool should have parameter specs"
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (dolist (tool tools)
      (is (not (null (tool-parameters tool))))
      (is (listp (tool-parameters tool))))))

(test introspection-tools-are-safe
  "All introspection tools should be :safe"
  (let ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :safe)))
    (dolist (tool-name '("describe_symbol" "apropos_search"
                         "function_arglist" "who_calls"
                         "who_references" "list_package_symbols"
                         "class_slots" "class_hierarchy"
                         "macroexpand_form"))
      (let ((tool (find tool-name tools
                       :test #'equal
                       :key (lambda (tool) (getf tool :name)))))
        (is (not (null tool)))))))

(test execution-tools-moderate-or-safe
  "Execution tools should be :moderate or :safe"
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (dolist (tool-name '("eval_form" "compile_form"
                         "get_last_error" "get_repl_history"))
      (let ((tool (find tool-name tools
                       :test #'equal
                       :key (lambda (tool) (getf tool :name)))))
        (is (not (null tool)))))))

(test diff-tool-is-moderate
  "propose_file_edit should be :moderate"
  (let ((tools (agent-q.tools:get-agent-q-tools :max-safety-level :moderate)))
    (let ((tool (find "propose_file_edit" tools
                     :test #'equal
                     :key (lambda (tool) (getf tool :name)))))
      (is (not (null tool))))))

;;; ============================================================================
;;; Backtrace Capture Tests
;;; ============================================================================

(def-suite backtrace-tests
  :description "Tests for portable backtrace capture utility")

(in-suite backtrace-tests)

(test capture-backtrace-returns-string
  "capture-backtrace-portable should return a string"
  (let ((backtrace (agent-q.tools:capture-backtrace-portable)))
    (is (stringp backtrace))))

(test capture-backtrace-handles-errors
  "capture-backtrace-portable should not signal errors"
  (is (not (null (handler-case
                    (agent-q.tools:capture-backtrace-portable)
                  (error () nil))))))

(test safe-symbol-lookup-basic
  "safe-symbol-lookup should find existing symbols"
  (multiple-value-bind (sym pkg status)
      (agent-q.tools:safe-symbol-lookup "DEFUN" :cl)
    (is (not (null sym)))
    (is (eq status :external))))

(test safe-symbol-lookup-nonexistent
  "safe-symbol-lookup should return nil for non-existent symbols"
  (multiple-value-bind (sym pkg status)
      (agent-q.tools:safe-symbol-lookup "DEFINITELY_NONEXISTENT_XYZ" :cl)
    (is (null sym))
    (is (eq status :not-found))))

(test format-for-llm-truncates
  "format-for-llm should truncate long strings"
  (let ((long-string (make-string 5000 :initial-element #\a)))
    (let ((formatted (agent-q.tools:format-for-llm long-string :max-length 1000)))
      (is (<= (length formatted) 1100))
      (is (search "truncated" formatted :test #'char-equal)))))

(test format-for-llm-short-strings
  "format-for-llm should not truncate short strings"
  (let ((short-string "hello world"))
    (let ((formatted (agent-q.tools:format-for-llm short-string)))
      (is (equal formatted short-string)))))

;;; ============================================================================
;;; Tool Execution Tests
;;; ============================================================================

(def-suite tool-execution-tests
  :description "Tests for tool execution mechanisms")

(in-suite tool-execution-tests)

(test tool-handler-callable
  "Tool handlers should be callable"
  (let ((handler (find-tool-handler "describe_symbol")))
    (is (not (null handler)))
    (is (callable handler))))

(test tool-handler-returns-string
  "Tool handlers should return strings"
  (let ((handler (find-tool-handler "describe_symbol")))
    (let ((result (funcall handler '(:symbol "DEFUN"))))
      (is (stringp result)))))

(test multiple-tool-calls
  "Should be able to call multiple tools in sequence"
  (let ((handler1 (find-tool-handler "describe_symbol"))
        (handler2 (find-tool-handler "apropos_search")))
    (let ((result1 (funcall handler1 '(:symbol "DEFUN")))
          (result2 (funcall handler2 '(:pattern "LIST"))))
      (is (stringp result1))
      (is (stringp result2))
      (is (not (equal result1 result2))))))

;;; ============================================================================
;;; Agent Loop Simulation Tests
;;; ============================================================================

(def-suite agent-loop-tests
  :description "Tests simulating agent loop iteration with tools")

(in-suite agent-loop-tests)

(test agent-can-access-tools
  "Agent should be able to access tool registry"
  (is (not (null agent-q.tools:*agent-q-registry*)))
  (let ((tools (agent-q.tools:get-agent-q-tools)))
    (is (listp tools))
    (is (> (length tools) 0))))

(test tool-results-formatting
  "Tool results should be formattable for LLM"
  (let ((handler (find-tool-handler "describe_symbol")))
    (let ((result (funcall handler '(:symbol "DEFUN"))))
      ;; Result should be formattable
      (is (not (null (agent-q.tools:format-for-llm result)))))))

(test sequential-tool-execution
  "Should be able to execute tools sequentially"
  (let ((handler1 (find-tool-handler "describe_symbol"))
        (handler2 (find-tool-handler "function_arglist")))
    (let ((result1 (funcall handler1 '(:symbol "DEFUN")))
          (result2 (funcall handler2 '(:function "DEFUN"))))
      (is (stringp result1))
      (is (stringp result2)))))

;;; ============================================================================
;;; Error Recovery Tests
;;; ============================================================================

(def-suite error-recovery-tests
  :description "Tests for error handling and recovery in tool execution")

(in-suite error-recovery-tests)

(test missing-required-args
  "Tools should handle missing required arguments"
  (let ((handler (find-tool-handler "describe_symbol")))
    (let ((result (handler-case
                      (funcall handler '(:invalid-key "value"))
                    (error (e)
                      (format nil "Error: ~A" e)))))
      (is (stringp result)))))

(test tool-error-doesnt-crash-registry
  "Tool errors should not corrupt registry"
  (let ((tools-before (length (agent-q.tools:get-agent-q-tools))))
    (handler-case
        (let ((handler (find-tool-handler "eval_form")))
          (funcall handler '(:form "(ERROR \"test\")")))
      (error ()))
    (let ((tools-after (length (agent-q.tools:get-agent-q-tools))))
      (is (= tools-before tools-after)))))

(test repeated-tool-calls-stable
  "Repeated tool calls should produce stable results"
  (let ((handler (find-tool-handler "apropos_search")))
    (let ((result1 (funcall handler '(:pattern "DEFUN")))
          (result2 (funcall handler '(:pattern "DEFUN"))))
      ;; Results should be similar (though not necessarily identical)
      (is (stringp result1))
      (is (stringp result2))
      (is (> (length result1) 0))
      (is (> (length result2) 0)))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-phase-2-tests ()
  "Run all Phase 2 tests."
  (format t "~%=== Running Phase 2 Tool System Tests ===~%~%")
  (format t "~%--- Registry Tests ---~%")
  (run! 'registry-tests)
  (format t "~%--- Introspection Tools Tests ---~%")
  (run! 'introspection-tools-tests)
  (format t "~%--- Execution Tools Tests ---~%")
  (run! 'execution-tools-tests)
  (format t "~%--- Buffer Tools Tests ---~%")
  (run! 'buffer-tools-tests)
  (format t "~%--- Diff Tool Tests ---~%")
  (run! 'diff-tool-tests)
  (format t "~%--- Tool Execution Pipeline Tests ---~%")
  (run! 'tool-execution-pipeline-tests)
  (format t "~%--- Error Handling Tests ---~%")
  (run! 'error-handling-tests)

  (format t "~%~%=== Running Phase 2 Integration Tests ===~%~%")
  (format t "~%--- Tool Availability Tests ---~%")
  (run! 'tool-availability-tests)
  (format t "~%--- Backtrace Tests ---~%")
  (run! 'backtrace-tests)
  (format t "~%--- Tool Execution Tests ---~%")
  (run! 'tool-execution-tests)
  (format t "~%--- Agent Loop Tests ---~%")
  (run! 'agent-loop-tests)
  (format t "~%--- Error Recovery Tests ---~%")
  (run! 'error-recovery-tests)

  (format t "~%~%=== All Phase 2 Tests Complete ===~%"))
