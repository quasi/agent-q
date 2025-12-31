;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage #:agent-q
  (:use #:cl)
  (:export
   ;; Config
   #:*default-provider*
   #:*default-model*
   #:*provider-instance*
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

   ;; SLY interface
   #:agent-q-send
   #:agent-q-add-context
   #:agent-q-clear-context
   #:agent-q-get-context-summary
   #:agent-q-new-conversation
   #:agent-q-configure))
