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
  "Send message to agent with tool execution loop.
   This implements the agentic loop: LLM responds → execute tools → send results back → repeat until done."
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

    ;; Build initial messages list
    (let ((messages (build-messages-for-llm conversation)))

      ;; Agent loop: iterate until LLM stops requesting tools
      (handler-case
          (loop with max-iterations = 10
                for iteration from 1 to max-iterations
                do
                (progn
                  (format t "~&[AGENT-Q] Iteration ~D~%" iteration)

                  ;; Call LLM with tools
                  (let ((response (send-to-llm-with-tools
                                  messages
                                  (agent-system-prompt agent)
                                  :max-safety-level :moderate)))

                    (cond
                      ;; Case 1: Text response with no tool calls - we're done
                      ((and (cl-llm-provider:response-content response)
                            (null (cl-llm-provider:response-tool-calls response)))
                       (let ((content (cl-llm-provider:response-content response)))
                         (format t "~&[AGENT-Q] Agent completed (text response)~%")
                         ;; Add assistant response to conversation history
                         (add-message conversation :assistant content)
                         (return content)))

                      ;; Case 2: Tool calls - execute and continue loop
                      ((cl-llm-provider:response-tool-calls response)
                       (let ((num-tools (length (cl-llm-provider:response-tool-calls response))))
                         (format t "~&[AGENT-Q] Executing ~D tool call~:P~%" num-tools)

                         ;; Verbose: Log which tools are being called
                         (when *verbose-mode*
                           (let* ((tool-names (mapcar (lambda (tc)
                                                        (cl-llm-provider:tool-call-name tc))
                                                     (cl-llm-provider:response-tool-calls response)))
                                  (msg (format nil "⚙ Calling tools: ~{~A~^, ~}" tool-names)))
                             (add-message conversation :debug msg)
                             ;; Send to Emacs immediately
                             (agent-q.tools:eval-in-emacs
                              `(sly-agent-q--append-to-conversation 'debug ,msg)))))

                       ;; Execute tools
                       (let* ((exec-results (execute-tool-calls-safe response))
                              (tool-msgs (tool-results-to-messages exec-results))
                              (assistant-msg (cl-llm-provider:response-message response)))

                         ;; Verbose: Log tool results
                         (when *verbose-mode*
                           (dolist (result exec-results)
                             (let* ((tool-call (car result))
                                    (tool-result (cdr result))
                                    (tool-name (cl-llm-provider:tool-call-name tool-call))
                                    (result-str (etypecase tool-result
                                                  (string tool-result)
                                                  (condition (format nil "ERROR: ~A" tool-result))
                                                  (t (format nil "~S" tool-result))))
                                    ;; Truncate long results
                                    (truncated (if (> (length result-str) 200)
                                                  (format nil "~A... [truncated, ~D chars total]"
                                                         (subseq result-str 0 200)
                                                         (length result-str))
                                                  result-str))
                                    (msg (format nil "  → ~A: ~A" tool-name truncated)))
                               (add-message conversation :debug msg)
                               ;; Send to Emacs immediately
                               (agent-q.tools:eval-in-emacs
                                `(sly-agent-q--append-to-conversation 'debug ,msg)))))

                         ;; Add assistant message text to conversation history
                         ;; (We save text for display, but keep full message with tool_calls for LLM)
                         (add-message conversation :assistant
                                     (or (getf assistant-msg :content) ""))

                         ;; Append assistant message (WITH tool_calls) and tool results to messages
                         ;; This is critical: the assistant message must include tool_calls so that
                         ;; the tool result messages can reference them via tool_call_id
                         (setf messages (append messages
                                               (list assistant-msg)
                                               tool-msgs))))

                      ;; Case 3: Unexpected response format
                      (t
                       (let ((err-msg "Unexpected response format from LLM"))
                         (format t "~&[AGENT-Q ERROR] ~A~%" err-msg)
                         (return err-msg))))))

                finally
                ;; Max iterations reached
                (let ((msg "Maximum iterations reached. The agent may be stuck in a loop."))
                  (format t "~&[AGENT-Q WARNING] ~A~%" msg)
                  (return msg)))

        ;; Error handling
        (cl-llm-provider:provider-authentication-error (e)
          (format nil "Authentication failed: ~A. Check your API key." e))

        (cl-llm-provider:provider-rate-limit-error ()
          (format nil "Rate limited. Please try again later."))

        (cl-llm-provider:provider-api-error (e)
          (format nil "API error: ~A" e))

        (error (e)
          (format nil "Unexpected error: ~A~%~%Backtrace: ~A"
                 e (agent-q.tools:capture-backtrace-portable)))))))

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
