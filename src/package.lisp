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
   #:sync-from-cl-llm-provider
   #:get-config-info

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

   ;; Session
   #:session
   #:session-id
   #:session-name
   #:session-created-at
   #:session-updated-at
   #:session-conversation
   #:session-model
   #:session-metadata
   #:session-messages
   #:session-add-message
   #:session-message-count
   #:session-add-tokens
   #:make-session
   #:generate-session-id

   ;; Session Manager
   #:session-manager
   #:*session-manager*
   #:sessions-directory
   #:current-session
   #:session-cache
   #:ensure-session-manager
   #:ensure-current-session

   ;; Session Persistence
   #:save-session
   #:load-session
   #:delete-session
   #:list-sessions
   #:search-sessions
   #:switch-session
   #:create-session

   ;; Session Serialization
   #:session-to-plist
   #:plist-to-session
   #:message-to-plist
   #:plist-to-message

   ;; Agent
   #:send-to-agent
   #:get-last-response

   ;; Prompts
   #:*base-system-prompt*
   #:load-project-prompt
   #:compose-system-prompt

   ;; LLM Integration (Phase 2)
   #:send-to-llm-with-tools
   #:send-to-llm-streaming
   #:execute-tool-calls-safe
   #:tool-results-to-messages
   #:build-messages-for-llm

   ;; Streaming
   #:*streaming-callback*
   #:make-emacs-streaming-callback
   #:make-emacs-complete-callback
   #:make-emacs-error-callback

   ;; SLY interface
   #:agent-q-send
   #:agent-q-add-context
   #:agent-q-clear-context
   #:agent-q-get-context-summary
   #:agent-q-new-conversation
   #:agent-q-get-conversation-history
   #:agent-q-configure

   ;; SLY interface: Session management
   #:agent-q-create-session
   #:agent-q-switch-session
   #:agent-q-save-session
   #:agent-q-delete-session
   #:agent-q-rename-session
   #:agent-q-list-sessions
   #:agent-q-search-sessions
   #:agent-q-get-session-info))
