;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Agent class

(defclass agent ()
  ((provider :initarg :provider
             :accessor agent-provider
             :documentation "cl-llm-provider instance")
   (model :initarg :model
          :accessor agent-model
          :initform *default-model*)
   (conversation :initarg :conversation
                 :accessor agent-conversation
                 :initform (make-instance 'conversation))
   (system-prompt :initarg :system-prompt
                  :accessor agent-system-prompt
                  :initform *base-system-prompt*)))

(defvar *current-agent* nil
  "The currently active agent instance")

;;; Main agent function

(defgeneric send-to-agent (agent user-message &key include-context)
  (:documentation "Send a message to the agent and get a response.
   If include-context is true, prepend accumulated context to the message."))

(defmethod send-to-agent ((agent agent) user-message &key include-context)
  "Send message to LLM via cl-llm-provider and return response."
  (let* ((conversation (agent-conversation agent))
         ;; Build full message with context if requested
         (context-string (when include-context
                          (context-to-string
                           (conversation-context conversation))))
         (full-message (if context-string
                          (format nil "~A~%~%## Request~%~%~A"
                                  context-string user-message)
                          user-message)))

    ;; Add user message to history
    (add-message conversation :user full-message)

    ;; Build messages list for cl-llm-provider
    ;; Format: List of plists with :role and :content
    (let* ((history-msgs (conversation-messages conversation))
           (messages (mapcar (lambda (msg)
                              (list :role (string-downcase
                                          (symbol-name (message-role msg)))
                                    :content (message-content msg)))
                            history-msgs)))

      ;; Call LLM via cl-llm-provider
      (handler-case
          (let ((response (cl-llm-provider:complete
                          messages
                          :provider (or (agent-provider agent)
                                       *provider-instance*)
                          :system (agent-system-prompt agent)
                          :max-tokens 4096)))

            ;; Extract content from response
            (let ((content (cl-llm-provider:response-content response)))
              ;; Add assistant response to conversation history
              (add-message conversation :assistant content)

              ;; Return content
              content))

        ;; Error handling
        (cl-llm-provider:provider-authentication-error (e)
          (format nil "Authentication failed: ~A. Check your API key." e))

        (cl-llm-provider:provider-rate-limit-error ()
          (format nil "Rate limited. Please try again later."))

        (cl-llm-provider:provider-api-error (e)
          (format nil "API error: ~A" e))

        (error (e)
          (format nil "Unexpected error: ~A" e))))))

(defun get-last-response ()
  "Get the last assistant response from current agent."
  (when *current-agent*
    (let* ((conv (agent-conversation *current-agent*))
           (msgs (conversation-messages conv))
           (assistant-msgs (remove-if-not
                           (lambda (m) (eq (message-role m) :assistant))
                           msgs)))
      (when assistant-msgs
        (message-content (car (last assistant-msgs)))))))

;;; Initialize default agent if needed

(defun ensure-agent ()
  "Ensure *current-agent* exists, create if needed."
  (unless *current-agent*
    (setf *current-agent*
          (make-instance 'agent
                        :provider *provider-instance*
                        :system-prompt *base-system-prompt*))))
