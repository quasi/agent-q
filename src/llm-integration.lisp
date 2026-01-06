;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; LLM Integration Layer
;;;
;;; Bridge between Agent-Q and cl-llm-provider for tool-enabled conversations.
;;; This file provides the core functions for calling the LLM with tools and
;;; executing tool calls that come back from the LLM.

(defun send-to-llm-with-tools (messages system-prompt &key (max-safety-level :moderate))
  "Send messages to LLM with tools enabled.

   MESSAGES: List of message plists for the conversation
   SYSTEM-PROMPT: System prompt string
   MAX-SAFETY-LEVEL: Maximum safety level for tools (:safe, :moderate, or :dangerous)

   Returns: completion-response object from cl-llm-provider"
  (let ((tools (agent-q.tools:get-agent-q-tools
                :max-safety-level max-safety-level)))
    (cl-llm-provider:complete
     messages
     :provider *provider-instance*
     :system system-prompt
     :tools tools
     :max-tokens 4096)))

(defun execute-tool-calls-safe (response)
  "Execute tool calls from an LLM response.

   RESPONSE: completion-response object from cl-llm-provider

   Returns: List of (tool-call . result) pairs from execution,
            or NIL if there are no tool calls"
  (when (cl-llm-provider:response-tool-calls response)
    (handler-case
        (execute-tool-calls
         response
         :registry agent-q.tools:*agent-q-registry*
         :max-safety-level :moderate)
      (error (e)
        (format t "~&[AGENT-Q ERROR] Tool execution failed: ~A~%" e)
        ;; Return error as tool result so agent can see what went wrong
        (list (cons (first (cl-llm-provider:response-tool-calls response))
                   (format nil "Tool execution error: ~A" e)))))))

(defun tool-results-to-messages (execution-results)
  "Convert tool execution results to message plists for LLM.

   EXECUTION-RESULTS: List of (tool-call . result) pairs

   Returns: List of message plists with :role \"tool\""
  (execution-results-to-tool-messages execution-results))

(defun build-messages-for-llm (conversation &optional additional-user-message)
  "Build message list from conversation history for LLM.

   CONVERSATION: conversation object
   ADDITIONAL-USER-MESSAGE: Optional additional user message to append

   Returns: List of message plists

   Note: Debug messages are filtered out - they're for display only,
   not to be sent to the LLM API."
  (let ((messages (mapcar (lambda (msg)
                           (list :role (string-downcase
                                       (symbol-name (message-role msg)))
                                 :content (message-content msg)))
                         ;; Filter out debug messages - they're for display only
                         (remove-if (lambda (msg)
                                     (eq (message-role msg) :debug))
                                   (conversation-messages conversation)))))
    (if additional-user-message
        (append messages (list (list :role "user"
                                    :content additional-user-message)))
        messages)))
