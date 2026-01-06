;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage #:agent-q
  (:use #:cl)
  (:import-from #:cl-llm-provider.tools
                #:execute-tool-calls
                #:execution-results-to-tool-messages
                #:tools-for-llm)
  (:export
   ;; Config
   #:*default-provider*
   #:*default-model*
   #:*provider-instance*
   #:*verbose-mode*
   #:configure

   ;; Context
   #:make-context-item
   #:context-item
   #:context-item-type
   #:context-item-content
   #:context-item-metadata
   #:add-context
   #:clear-context
   #:get-context
   #:context-to-string

   ;; Conversation
   #:make-message
   #:message
   #:message-role
   #:message-content
   #:add-message
   #:get-messages
   #:clear-conversation
   #:new-conversation

   ;; Agent
   #:send-to-agent
   #:get-last-response

   ;; Prompts
   #:*base-system-prompt*
   #:load-project-prompt
   #:compose-system-prompt

   ;; LLM Integration (Phase 2)
   #:send-to-llm-with-tools
   #:execute-tool-calls-safe
   #:tool-results-to-messages
   #:build-messages-for-llm

   ;; SLY interface
   #:agent-q-send
   #:agent-q-add-context
   #:agent-q-clear-context
   #:agent-q-get-context-summary
   #:agent-q-new-conversation
   #:agent-q-get-conversation-history
   #:agent-q-configure))
