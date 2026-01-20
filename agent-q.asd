;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defsystem :agent-q
  :description "AI-powered agentic extension for Common Lisp development"
  :author "Abhijit Rao <quasi@quasilabs.in>"
  :license "MIT"
  :version "0.2.0"  ; Phase 2!
  :serial t
  :depends-on (:cl-llm-provider        ; LLM communication (includes tool support)
               :closer-mop)            ; CLOS introspection
                                       ; Note: SWANK/SLY required at runtime for Emacs integration
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                            (:file "config")
                            (:file "context")
                            (:file "conversation")
                            (:file "session")
                            (:file "prompts")
                            (:module "tools"
                             :serial t
                             :components ((:file "package")
                                         (:file "registry")
                                         (:file "introspection")
                                         (:file "execution")
                                         (:file "buffer")
                                         (:file "diff")))
                            (:file "llm-integration")
                            (:file "streaming")
                            (:file "observability")
                            (:file "cost")
                            (:file "agent")
                            (:file "sly-interface"))))

  :in-order-to ((test-op (test-op :agent-q/tests))))

;;; Test system
(defsystem :agent-q/tests
  :description "Tests for Agent-Q (all phases)"
  :author "Abhijit Rao <quasi@quasilabs.in>"
  :license "MIT"
  :version "0.3.0"
  :serial t
  :depends-on (:agent-q :fiveam :cl-ppcre)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                            (:file "core-tests")
                            (:file "session-tests")
                            (:file "phase2-tools-tests")
                            (:file "phase2-integration-tests")
                            (:file "filesystem-tests"))))
  :perform (test-op (op c)
             (uiop:symbol-call :agent-q-tests '#:run-agent-q-tests)))
