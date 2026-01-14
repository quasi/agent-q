;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-
;;; ABOUTME: Streaming callback infrastructure for sending LLM responses to Emacs

(in-package :agent-q)

;;; Streaming Infrastructure
;;;
;;; Provides callback mechanisms for streaming LLM responses to Emacs.
;;; These callbacks integrate with cl-llm-provider's streaming API and
;;; use eval-in-emacs to send chunks to the Emacs chat interface.

(defvar *streaming-callback* nil
  "Global streaming callback for current request.
   Function: (lambda (chunk) ...)

   This variable can be bound dynamically to override the default
   streaming behavior during a request.")

(defun make-emacs-streaming-callback ()
  "Create a streaming callback that sends chunks to Emacs.

   Returns a function suitable for cl-llm-provider's :on-chunk callback.
   The callback extracts the delta text from each chunk and sends it
   to Emacs via eval-in-emacs for immediate display in the chat buffer.

   The Elisp function `agent-q--append-response-chunk` is called with
   each text delta."
  (lambda (chunk)
    (let ((delta (cl-llm-provider:chunk-delta chunk)))
      (when (and delta (> (length delta) 0))
        ;; Send to Emacs for immediate display
        (agent-q.tools:eval-in-emacs
         `(agent-q--append-response-chunk ,delta))))))

(defun make-emacs-complete-callback ()
  "Create a completion callback that notifies Emacs streaming is done.

   Returns a function suitable for cl-llm-provider's :on-complete callback.
   The callback sends the full accumulated content to Emacs so the chat
   interface can finalize the response display.

   The Elisp function `agent-q--finalize-response` is called with
   full-content. Token usage is sent separately via `agent-q--update-token-usage`
   if available in the final chunk."
  (lambda (full-content final-chunk)
    ;; Finalize the response with full content
    (agent-q.tools:eval-in-emacs
     `(agent-q--finalize-response ,full-content))
    ;; Also send token usage if available
    (when final-chunk
      (let ((usage (cl-llm-provider:chunk-usage final-chunk)))
        (when usage
          (agent-q.tools:eval-in-emacs
           `(agent-q--update-token-usage
             ,(getf usage :prompt-tokens)
             ,(getf usage :completion-tokens))))))))

(defun make-emacs-error-callback ()
  "Create an error callback that notifies Emacs of streaming errors.

   Returns a function suitable for cl-llm-provider's :on-error callback.
   The callback formats the error condition as a string and sends it
   to Emacs so the chat interface can display the error.

   The Elisp function `agent-q--streaming-error` is called with
   the error message string."
  (lambda (error)
    (agent-q.tools:eval-in-emacs
     `(agent-q--streaming-error ,(format nil "~A" error)))))
