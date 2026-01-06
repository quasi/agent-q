;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage #:agent-q.tools
  (:use #:cl)
  (:import-from #:cl-llm-provider
                #:define-tool)
  (:import-from #:cl-llm-provider.tools
                #:make-tool-registry
                #:register-tool
                #:tools-for-llm
                #:execute-tool-calls
                #:execution-results-to-tool-messages
                #:registry-tools)
  (:export
   ;; Registry
   #:*agent-q-registry*
   #:register-agent-q-tools
   #:get-agent-q-tools

   ;; State tracking
   #:*last-error*
   #:*repl-history*

   ;; Utility functions
   #:capture-backtrace-portable
   #:safe-symbol-lookup
   #:format-for-llm
   #:eval-in-emacs))
