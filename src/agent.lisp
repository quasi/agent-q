;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-
;;; ABOUTME: Core agent loop with streaming support and tool execution

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
                  :initform *base-system-prompt*)
   ;; Streaming support
   (streaming-callback :initarg :streaming-callback
                       :accessor agent-streaming-callback
                       :initform nil
                       :documentation "Callback for streaming chunks")))

(defvar *current-agent* nil
  "The currently active agent instance")

;;; Helper functions

(defun get-config-info ()
  "Return current provider and model configuration as a plist.
   Returns (:provider <keyword> :provider-name <string> :model <string>) on success,
   or (:error <string>) if provider is not configured.

   Requires *provider-instance* to be configured before calling.
   The provider instance must have its default-model slot populated
   (done by sync-from-cl-llm-provider or configure)."
  (if *provider-instance*
      (list :provider (cl-llm-provider:provider-type *provider-instance*)
            :provider-name (cl-llm-provider:provider-name *provider-instance*)
            :model (cl-llm-provider:provider-default-model *provider-instance*))
      (list :error "Provider not configured. Please configure cl-llm-provider first.")))

;;; Main agent function

(defgeneric send-to-agent (agent user-message &key include-context)
  (:documentation "Send a message to the agent and get a response.
   If include-context is true, prepend accumulated context to the message."))

(defun report-session-info-to-emacs (model provider input-tokens output-tokens)
  "Report session info to Emacs chat buffer and update session metadata.
   MODEL - model name string
   PROVIDER - provider keyword (e.g., :anthropic)
   INPUT-TOKENS - total prompt tokens used
   OUTPUT-TOKENS - total completion tokens used"
  ;; Update session metadata if session manager is active
  (when *session-manager*
    (let ((session (current-session *session-manager*)))
      (when session
        (when input-tokens
          (session-add-tokens session input-tokens output-tokens))
        (when model
          (setf (session-model session) model))
        (when provider
          (let ((meta (session-metadata session)))
            (setf (getf meta :provider) provider)
            (setf (session-metadata session) meta))))))
  ;; Notify Emacs
  (when (and input-tokens output-tokens)
    (agent-q.tools:eval-in-emacs
     `(agent-q-chat-set-session-info
       ,model
       ,(when provider (symbol-name provider))
       ,input-tokens
       ,output-tokens))))

(defun finish-reason-is-tool-call-p (finish-reason)
  "Check if FINISH-REASON indicates tool calls were requested.
   Handles both :TOOL_CALLS (from API) and :TOOL-CALLS (normalized) variants."
  (when finish-reason
    (let ((name (symbol-name finish-reason)))
      (or (string-equal name "TOOL_CALLS")
          (string-equal name "TOOL-CALLS")
          ;; Anthropic uses :TOOL_USE
          (string-equal name "TOOL_USE")))))

(defun finish-reason-is-stop-p (finish-reason)
  "Check if FINISH-REASON indicates natural completion.
   Handles :STOP, :END_TURN (Anthropic), etc."
  (when finish-reason
    (let ((name (symbol-name finish-reason)))
      (or (string-equal name "STOP")
          (string-equal name "END_TURN")
          (string-equal name "END-TURN")))))

(defmethod send-to-agent ((agent agent) user-message &key include-context)
  "Send message to agent with streaming support and tool execution loop.
   This implements the agentic loop: LLM responds → execute tools → send results back → repeat until done.
   Text responses are streamed to Emacs incrementally for better UX.

   Streaming integration:
   - When callbacks are passed to send-to-llm-streaming, cl-llm-provider reads all chunks
     internally and calls the callbacks. After completion, the stream object contains
     the accumulated content and final chunk data.
   - The on-chunk callback sends text deltas to Emacs for incremental display.
   - The on-complete callback captures the final chunk for finish reason and usage.
   - After streaming completes, we check the finish reason to decide next steps.
   - For tool calls, we fall back to synchronous request to get full tool call data
     (streaming API doesn't fully support tool call extraction yet)."
  (let* ((conversation (agent-conversation agent))
         ;; Build full message with context if requested
         (context-string (when include-context
                          (context-to-string
                           (conversation-context conversation))))
         (full-message (if context-string
                          (format nil "~A~%~%## Request~%~%~A"
                                  context-string user-message)
                          user-message))
         ;; Token tracking - accumulate across all iterations
         (total-input-tokens 0)
         (total-output-tokens 0)
         ;; Track the actual model used (from response, not defaults)
         (actual-model nil)
         ;; Derive provider type from the agent's provider instance
         (actual-provider (when (agent-provider agent)
                           (cl-llm-provider:provider-type (agent-provider agent)))))

    ;; Add user message to history
    (add-message conversation :user full-message)

    ;; Notify Emacs that streaming is starting
    (agent-q.tools:eval-in-emacs '(agent-q--start-streaming))

    ;; Build initial messages list
    (let ((messages (build-messages-for-llm conversation)))

      ;; Agent loop: iterate until LLM stops requesting tools
      (handler-case
          (loop with max-iterations = 10
                for iteration from 1 to max-iterations
                do
                (progn
                  (format t "~&[AGENT-Q] Iteration ~D (streaming)~%" iteration)

                  ;; Variables to capture streaming results via callbacks
                  (let ((captured-final-chunk nil))

                    ;; Use streaming for the LLM request
                    ;; When callbacks are provided, complete-stream reads all chunks internally
                    ;; and calls our callbacks. After it returns, stream contains accumulated data.
                    (let ((stream (send-to-llm-streaming
                                   messages
                                   (agent-system-prompt agent)
                                   :max-safety-level :moderate
                                   :on-chunk (make-emacs-streaming-callback)
                                   :on-complete (lambda (full-content final-chunk)
                                                 (declare (ignore full-content))
                                                 ;; Capture the final chunk for finish reason and usage
                                                 (setf captured-final-chunk final-chunk))
                                   :on-error (make-emacs-error-callback))))

                      ;; At this point, streaming is complete (callbacks already fired)
                      ;; Extract results from the stream object and captured final chunk
                      (let* ((accumulated-content (cl-llm-provider:stream-accumulated-content stream))
                             (final-finish-reason (when captured-final-chunk
                                                   (cl-llm-provider:chunk-finish-reason captured-final-chunk)))
                             (final-usage (when captured-final-chunk
                                           (cl-llm-provider:chunk-usage captured-final-chunk))))

                        ;; Update model info from provider (stream-model not exported)
                        ;; Get model from provider's default-model since streaming doesn't expose it
                        (unless actual-model
                          (setf actual-model
                                (when (agent-provider agent)
                                  (cl-llm-provider:provider-default-model (agent-provider agent)))))

                        ;; Debug: check what we got from streaming
                        (format t "~&[AGENT-Q] DEBUG: final-chunk=~S~%" captured-final-chunk)
                        (format t "~&[AGENT-Q] DEBUG: final-usage=~S~%" final-usage)
                        (format t "~&[AGENT-Q] DEBUG: finish-reason=~S~%" final-finish-reason)

                        ;; Accumulate token usage
                        (when final-usage
                          (incf total-input-tokens (or (getf final-usage :prompt-tokens) 0))
                          (incf total-output-tokens (or (getf final-usage :completion-tokens) 0)))

                        ;; Check finish reason to determine next action
                        (cond
                          ;; Case 1: Natural completion - we're done
                          ((finish-reason-is-stop-p final-finish-reason)
                           (format t "~&[AGENT-Q] Agent completed (streaming)~%")
                           ;; Finalize streaming in Emacs
                           (agent-q.tools:eval-in-emacs
                            `(agent-q--finalize-response ,accumulated-content))
                           ;; Add assistant response to conversation history
                           (add-message conversation :assistant accumulated-content)
                           ;; Report session info to Emacs
                           (report-session-info-to-emacs
                            actual-model actual-provider
                            total-input-tokens total-output-tokens)
                           (return accumulated-content))

                          ;; Case 2: Tool calls requested
                          ;; Note: Streaming doesn't fully support tool call data extraction yet,
                          ;; so we make a synchronous call to get the tool calls
                          ((finish-reason-is-tool-call-p final-finish-reason)
                           (format t "~&[AGENT-Q] Tool calls detected, executing...~%")

                           ;; Make synchronous call to get full tool call data
                           (let ((response (send-to-llm-with-tools
                                           messages
                                           (agent-system-prompt agent)
                                           :max-safety-level :moderate)))

                             ;; Get usage from sync response if streaming didn't provide it
                             ;; (Many providers don't include usage in streaming tool call responses)
                             (when (and (null final-usage)
                                       (cl-llm-provider:response-usage response))
                               (let ((sync-usage (cl-llm-provider:response-usage response)))
                                 (format t "~&[AGENT-Q] DEBUG: Using sync response usage: ~S~%" sync-usage)
                                 (incf total-input-tokens (or (getf sync-usage :prompt-tokens) 0))
                                 (incf total-output-tokens (or (getf sync-usage :completion-tokens) 0))))

                             (when (cl-llm-provider:response-tool-calls response)
                               (let ((num-tools (length (cl-llm-provider:response-tool-calls response))))
                                 (format t "~&[AGENT-Q] Executing ~D tool call~:P~%" num-tools)

                                 ;; Verbose: Log which tools are being called
                                 (when *verbose-mode*
                                   (let* ((tool-names (mapcar (lambda (tc)
                                                               (cl-llm-provider:tool-call-name tc))
                                                             (cl-llm-provider:response-tool-calls response)))
                                          (msg (format nil "⚙ Calling tools: ~{~A~^, ~}" tool-names)))
                                     (add-message conversation :debug msg)
                                     (agent-q.tools:eval-in-emacs
                                      `(sly-agent-q--append-to-conversation 'debug ,msg))))

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
                                              (truncated (if (> (length result-str) 200)
                                                            (format nil "~A... [truncated, ~D chars total]"
                                                                   (subseq result-str 0 200)
                                                                   (length result-str))
                                                            result-str))
                                              (msg (format nil "  → ~A: ~A" tool-name truncated)))
                                         (add-message conversation :debug msg)
                                         (agent-q.tools:eval-in-emacs
                                          `(sly-agent-q--append-to-conversation 'debug ,msg)))))

                                   ;; Add assistant message text to conversation history
                                   (add-message conversation :assistant
                                               (or (getf assistant-msg :content) ""))

                                   ;; Append assistant message (WITH tool_calls) and tool results to messages
                                   (setf messages (append messages
                                                         (list assistant-msg)
                                                         tool-msgs)))))))

                          ;; Case 3: No finish reason or streaming completed without explicit stop
                          ;; This can happen if the model just stopped (treat as completion)
                          ((and (> (length accumulated-content) 0)
                                (null final-finish-reason))
                           (format t "~&[AGENT-Q] Stream ended (no explicit finish reason)~%")
                           (agent-q.tools:eval-in-emacs
                            `(agent-q--finalize-response ,accumulated-content))
                           (add-message conversation :assistant accumulated-content)
                           (report-session-info-to-emacs
                            actual-model actual-provider
                            total-input-tokens total-output-tokens)
                           (return accumulated-content))

                          ;; Case 4: Unexpected state
                          (t
                           (let ((err-msg (format nil "Unexpected finish reason: ~A" final-finish-reason)))
                             (format t "~&[AGENT-Q ERROR] ~A~%" err-msg)
                             (agent-q.tools:eval-in-emacs
                              `(agent-q--streaming-error ,err-msg))
                             (report-session-info-to-emacs
                              actual-model actual-provider
                              total-input-tokens total-output-tokens)
                             (return err-msg))))))))  ; close cond, let*, let(stream), let(captured), progn

                finally
                ;; Max iterations reached
                (let ((msg "Maximum iterations reached. The agent may be stuck in a loop."))
                  (format t "~&[AGENT-Q WARNING] ~A~%" msg)
                  (agent-q.tools:eval-in-emacs
                   `(agent-q--streaming-error ,msg))
                  (report-session-info-to-emacs
                   actual-model actual-provider
                   total-input-tokens total-output-tokens)
                  (return msg)))  ; close let(msg), finally clause, loop

        ;; Error handling
        (cl-llm-provider:provider-authentication-error (e)
          (agent-q.tools:eval-in-emacs
           `(agent-q--streaming-error ,(format nil "Authentication failed: ~A" e)))
          (format nil "Authentication failed: ~A. Check your API key." e))

        (cl-llm-provider:provider-rate-limit-error ()
          (agent-q.tools:eval-in-emacs
           '(agent-q--streaming-error "Rate limited. Please try again later."))
          (format nil "Rate limited. Please try again later."))

        (cl-llm-provider:provider-api-error (e)
          (agent-q.tools:eval-in-emacs
           `(agent-q--streaming-error ,(format nil "API error: ~A" e)))
          (format nil "API error: ~A" e))

        (error (e)
          (agent-q.tools:eval-in-emacs
           `(agent-q--streaming-error ,(format nil "Error: ~A" e)))
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

;;;; Context Window Awareness
;;;;
;;;; Functions to check model context limits and warn when approaching capacity.
;;;; Uses cl-llm-provider metadata API for model-specific limits.

(defun get-context-limit ()
  "Get context window size for current model.

   Uses cl-llm-provider:model-metadata to retrieve the :context-window
   property for the configured model. Falls back to conservative defaults
   based on provider type if metadata is unavailable.

   Returns token limit as integer.

   Example:
     (get-context-limit) => 200000  ; for Claude models

   Default fallbacks by provider:
     :anthropic   -> 200000
     :openai      -> 128000
     :ollama      -> 8192
     :openrouter  -> 128000
     other        -> 8192"
  (if *provider-instance*
      (let* ((model (cl-llm-provider:provider-default-model *provider-instance*)))
        (if model
            (let ((meta (cl-llm-provider:model-metadata *provider-instance* model)))
              (if (and meta (getf meta :context-window))
                  (getf meta :context-window)
                  (get-context-limit-fallback *provider-instance*)))
            (get-context-limit-fallback *provider-instance*)))
      ;; No provider configured, use conservative default
      8192))

(defun get-context-limit-fallback (provider)
  "Return conservative context window default based on provider type.

   PROVIDER - cl-llm-provider instance

   Returns integer token limit."
  (case (cl-llm-provider:provider-type provider)
    (:anthropic 200000)    ; Claude 3.x models
    (:openai 128000)       ; GPT-4 Turbo
    (:ollama 8192)         ; Varies by model, use conservative
    (:openrouter 128000)   ; Pass-through, assume modern model
    (otherwise 8192)))

(defun check-context-limit (messages &key (threshold 0.8) (system-prompt nil))
  "Check if messages fit within model's context window.

   MESSAGES - List of message plists (:role :content)
   THRESHOLD - Warning threshold as fraction of limit (default 0.8 = 80%)
   SYSTEM-PROMPT - Optional system prompt to include in token count

   Uses cl-llm-provider:count-tokens to estimate total tokens.
   Signals a warning if approaching limit (above threshold).

   Returns plist with:
     :tokens       - Estimated token count
     :limit        - Context window limit
     :usage        - Usage as percentage (0-100)
     :warning      - T if above threshold, NIL otherwise

   Example:
     (let ((result (check-context-limit messages)))
       (when (getf result :warning)
         (format t \"Warning: ~D% of context used~%\"
                 (getf result :usage))))"
  (let* ((limit (get-context-limit))
         ;; Estimate tokens using cl-llm-provider
         (estimated-tokens (if system-prompt
                               (cl-llm-provider:count-tokens-with-system
                                messages system-prompt)
                               (cl-llm-provider:count-tokens messages)))
         (threshold-tokens (* limit threshold))
         (usage-pct (if (> limit 0)
                       (round (* 100.0 (/ estimated-tokens limit)))
                       0))
         (warning-p (> estimated-tokens threshold-tokens)))

    ;; Emit warning if above threshold
    (when warning-p
      (warn "Messages (~:D tokens) approaching context limit (~:D). ~
             Using ~D% of available context."
            estimated-tokens limit usage-pct))

    ;; Verbose logging
    (when (and *verbose-mode* (not warning-p))
      (format t "~&[AGENT-Q] Context usage: ~:D/~:D tokens (~D%)~%"
              estimated-tokens limit usage-pct))

    (list :tokens estimated-tokens
          :limit limit
          :usage usage-pct
          :warning warning-p)))

;;; Initialize default agent if needed

(defun ensure-agent ()
  "Ensure *current-agent* exists, create if needed.
   Also ensures session manager is initialized with a current session.
   Requires *provider-instance* to be configured before calling."
  (unless *current-agent*
    (unless *provider-instance*
      (error "Cannot create agent: *provider-instance* is not configured. ~
              Please ensure cl-llm-provider is configured before starting agent-q."))
    (setf *current-agent*
          (make-instance 'agent
                        :provider *provider-instance*
                        :system-prompt *base-system-prompt*)))
  ;; Ensure session manager and current session exist
  (ensure-session-manager)
  (ensure-current-session)
  *current-agent*)
