;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage #:agent-q-tests
  (:use #:cl #:fiveam)
  (:import-from #:agent-q.tools
                #:*agent-q-registry*
                #:get-agent-q-tools
                #:*last-error*
                #:*repl-history*
                #:capture-backtrace-portable
                #:safe-symbol-lookup
                #:format-for-llm
                #:eval-in-emacs)
  (:import-from #:agent-q
                #:send-to-llm-with-tools
                #:execute-tool-calls-safe
                #:tool-results-to-messages
                #:build-messages-for-llm)
  (:import-from #:cl-llm-provider
                #:complete
                #:response-content
                #:response-tool-calls)
  (:export
   ;; Test suites
   #:core-functionality
   #:phase-2-tools
   #:registry-tests
   #:introspection-tools-tests
   #:execution-tools-tests
   #:buffer-tools-tests
   #:diff-tool-tests
   #:tool-execution-pipeline-tests
   #:error-handling-tests

   ;; Test utilities
   #:find-tool-handler
   #:find-tool-definition
   #:eval-form-handler
   #:run-phase-2-tests
   #:run-core-tests
   #:run-agent-q-tests))
