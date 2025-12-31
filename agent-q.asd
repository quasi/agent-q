;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defsystem :agent-q
  :description "AI-powered agentic extension for Common Lisp development"
  :author "Abhijit Rao <quasi@quasilabs.in>"
  :license "MIT"
  :version "0.1.5"
  :serial t
  :depends-on (:cl-llm-provider)   ; LLM communication
                                   ; Note: SWANK/SLY required at runtime for Emacs integration
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                            (:file "config")
                            (:file "context")
                            (:file "conversation")
                            (:file "prompts")
                            (:file "agent")
                            (:file "sly-interface")))))
