# Agent-Q cl-llm-provider Integration Upgrade Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Upgrade Agent-Q's integration with cl-llm-provider to use streaming responses, observability hooks, and metadata APIs for real-time chat, comprehensive logging, and cost tracking.

**Architecture:** Replace synchronous `complete` calls with streaming `complete-stream` API. Add observability hooks at application startup. Integrate metadata API for cost estimation and provider capability checking.

**Tech Stack:** Common Lisp (cl-llm-provider, Agent-Q), Emacs Lisp (sly-agent-q)

---

## Background

The cl-llm-provider library has added Phase 1 features:
- **Streaming API**: `complete-stream` with `:on-chunk`, `:on-complete`, `:on-error` callbacks
- **Observability Hooks**: `make-hooks`, `add-hook`, `*global-hooks*` for logging/metrics
- **Token Counting**: `count-tokens`, `count-tokens-with-system`
- **Cost Estimation**: `estimate-cost`, `format-cost`
- **Metadata API**: `provider-supports-p`, `model-metadata`, `provider-capabilities`

Agent-Q currently uses synchronous `complete` which causes:
1. Display lag - responses only appear after full completion
2. No incremental feedback during long responses
3. Limited observability and logging

---

## Task 1: Add Streaming Support to LLM Integration

**Files:**
- Modify: `src/llm-integration.lisp`

**Step 1: Write the streaming function signature**

```lisp
(defun send-to-llm-streaming (messages system-prompt
                               &key (max-safety-level :moderate)
                                    on-chunk
                                    on-complete
                                    on-error)
  "Send messages to LLM with streaming enabled.

   MESSAGES: List of message plists for the conversation
   SYSTEM-PROMPT: System prompt string
   MAX-SAFETY-LEVEL: Maximum safety level for tools
   ON-CHUNK: Callback (lambda (chunk) ...) called for each chunk
   ON-COMPLETE: Callback (lambda (full-content final-chunk) ...) called when done
   ON-ERROR: Callback (lambda (error) ...) called on error

   Returns: completion-stream object"
  (let ((tools (agent-q.tools:get-agent-q-tools
                :max-safety-level max-safety-level)))
    (cl-llm-provider:complete-stream
     messages
     :provider *provider-instance*
     :system system-prompt
     :tools tools
     :max-tokens 4096
     :on-chunk on-chunk
     :on-complete on-complete
     :on-error on-error)))
```

**Step 2: Verify the function compiles**

Run: `(ql:quickload "agent-q")` in REPL
Expected: No compilation errors

**Step 3: Commit**

```bash
git add src/llm-integration.lisp
git commit -m "feat(llm): add streaming LLM function send-to-llm-streaming"
```

---

## Task 2: Update Agent Loop for Streaming

**Files:**
- Modify: `src/agent.lisp`

**Step 1: Add streaming state tracking to agent class**

```lisp
;; Add slot to agent class
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
   ;; New: streaming state
   (streaming-callback :initarg :streaming-callback
                       :accessor agent-streaming-callback
                       :initform nil
                       :documentation "Callback for streaming chunks")))
```

**Step 2: Create streaming-aware send-to-agent method**

Replace the existing `send-to-agent` method with a streaming version:

```lisp
(defmethod send-to-agent ((agent agent) user-message &key include-context stream-callback)
  "Send message to agent with streaming support and tool execution loop.

   STREAM-CALLBACK: Optional (lambda (delta accumulated) ...) called for each text chunk.
   When provided, response text streams incrementally. When nil, waits for complete response."
  ;; ... (see detailed implementation in Task 2)
  )
```

**Step 3: Handle streaming in the tool loop**

The key insight: streaming works for text responses, but tool calls arrive as complete chunks. The agent loop needs to:
1. Start streaming for text responses
2. Detect tool calls and pause streaming
3. Execute tools synchronously
4. Resume streaming for tool result responses

**Step 4: Commit**

```bash
git add src/agent.lisp
git commit -m "feat(agent): add streaming support to agent loop"
```

---

## Task 3: Create Streaming Callback Infrastructure

**Files:**
- Create: `src/streaming.lisp`

**Step 1: Write the streaming callback interface**

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Streaming Infrastructure
;;;
;;; Provides callback mechanisms for streaming LLM responses to Emacs.

(defvar *streaming-callback* nil
  "Global streaming callback for current request.
   Function: (lambda (delta accumulated) ...)")

(defun make-emacs-streaming-callback ()
  "Create a streaming callback that sends chunks to Emacs.
   Returns a function suitable for :on-chunk callback."
  (lambda (chunk)
    (let ((delta (cl-llm-provider:chunk-delta chunk)))
      (when (and delta (> (length delta) 0))
        ;; Send to Emacs for immediate display
        (agent-q.tools:eval-in-emacs
         `(agent-q--append-response-chunk ,delta))))))

(defun make-emacs-complete-callback ()
  "Create a completion callback that notifies Emacs streaming is done.
   Returns a function suitable for :on-complete callback."
  (lambda (full-content final-chunk)
    (declare (ignore full-content))
    ;; Extract usage from final chunk
    (let ((usage (cl-llm-provider:chunk-usage final-chunk)))
      (agent-q.tools:eval-in-emacs
       `(agent-q--finalize-response
         ,(when usage (getf usage :prompt-tokens))
         ,(when usage (getf usage :completion-tokens)))))))

(defun make-emacs-error-callback ()
  "Create an error callback that notifies Emacs of streaming errors.
   Returns a function suitable for :on-error callback."
  (lambda (error)
    (agent-q.tools:eval-in-emacs
     `(agent-q--streaming-error ,(format nil "~A" error)))))
```

**Step 2: Verify compilation**

Run: `(ql:quickload "agent-q")`
Expected: No errors

**Step 3: Add to agent-q.asd**

Add `(:file "streaming")` to the system definition after `llm-integration`.

**Step 4: Commit**

```bash
git add src/streaming.lisp agent-q.asd
git commit -m "feat(streaming): add Emacs streaming callback infrastructure"
```

---

## Task 4: Implement Streaming Agent Loop

**Files:**
- Modify: `src/agent.lisp`

**Step 1: Write the streaming-aware agent loop**

```lisp
(defmethod send-to-agent ((agent agent) user-message &key include-context)
  "Send message to agent with streaming and tool execution loop.
   Streams text responses incrementally to Emacs."
  (let* ((conversation (agent-conversation agent))
         ;; Build full message with context if requested
         (context-string (when include-context
                          (context-to-string
                           (conversation-context conversation))))
         (full-message (if context-string
                          (format nil "~A~%~%## Request~%~%~A"
                                  context-string user-message)
                          user-message))
         ;; Token tracking
         (total-input-tokens 0)
         (total-output-tokens 0)
         (actual-model nil)
         (actual-provider (when (agent-provider agent)
                           (cl-llm-provider:provider-type (agent-provider agent)))))

    ;; Add user message to history
    (add-message conversation :user full-message)

    ;; Notify Emacs streaming is starting
    (agent-q.tools:eval-in-emacs '(agent-q--start-streaming))

    (let ((messages (build-messages-for-llm conversation))
          (accumulated-content ""))

      (handler-case
          (loop with max-iterations = 10
                for iteration from 1 to max-iterations
                do
                (progn
                  (format t "~&[AGENT-Q] Iteration ~D~%" iteration)

                  ;; Use streaming request
                  (let ((stream (send-to-llm-streaming
                                 messages
                                 (agent-system-prompt agent)
                                 :max-safety-level :moderate
                                 :on-chunk (make-emacs-streaming-callback)
                                 :on-complete (make-emacs-complete-callback)
                                 :on-error (make-emacs-error-callback))))

                    ;; Read all chunks
                    (setf accumulated-content "")
                    (loop for chunk = (cl-llm-provider:read-stream-chunk stream)
                          while chunk
                          do
                          (let ((delta (cl-llm-provider:chunk-delta chunk)))
                            (when delta
                              (setf accumulated-content
                                    (concatenate 'string accumulated-content delta))))
                          finally
                          ;; Capture model and usage from final state
                          (setf actual-model (cl-llm-provider:stream-model stream))
                          (let ((chunks (cl-llm-provider:stream-chunks stream)))
                            (when chunks
                              (let ((final-chunk (car (last chunks))))
                                (when-let ((usage (cl-llm-provider:chunk-usage final-chunk)))
                                  (incf total-input-tokens (or (getf usage :prompt-tokens) 0))
                                  (incf total-output-tokens (or (getf usage :completion-tokens) 0)))))))

                    ;; Check for tool calls in final chunk
                    (let* ((chunks (cl-llm-provider:stream-chunks stream))
                           (final-chunk (car (last chunks)))
                           (finish-reason (when final-chunk
                                           (cl-llm-provider:chunk-finish-reason final-chunk))))

                      (cond
                        ;; Text response complete
                        ((eq finish-reason :stop)
                         (format t "~&[AGENT-Q] Agent completed (streaming)~%")
                         (add-message conversation :assistant accumulated-content)
                         (report-session-info-to-emacs
                          actual-model actual-provider
                          total-input-tokens total-output-tokens)
                         (return accumulated-content))

                        ;; Tool calls
                        ((eq finish-reason :tool-calls)
                         ;; Handle tool execution (similar to existing code)
                         ;; ... tool execution logic ...
                         )

                        ;; Other cases
                        (t
                         (format t "~&[AGENT-Q] Unexpected finish reason: ~A~%" finish-reason)
                         (return accumulated-content)))))))

                finally
                (report-session-info-to-emacs
                 actual-model actual-provider
                 total-input-tokens total-output-tokens)
                (return "Maximum iterations reached."))

        ;; Error handling
        (error (e)
          (agent-q.tools:eval-in-emacs
           `(agent-q--streaming-error ,(format nil "~A" e)))
          (format nil "Error: ~A" e))))))
```

**Step 2: Test streaming manually**

In REPL:
```lisp
(agent-q:configure :provider :anthropic :model "claude-sonnet-4-20250514")
(agent-q:send "Hello, count from 1 to 5")
```

Expected: Text appears incrementally in chat buffer

**Step 3: Commit**

```bash
git add src/agent.lisp
git commit -m "feat(agent): implement streaming agent loop"
```

---

## Task 5: Update Elisp Streaming Handlers

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-chat.el`

**Step 1: Add streaming notification functions**

These functions are called from CL via `eval-in-emacs`:

```elisp
(defun agent-q--start-streaming ()
  "Called when streaming response begins.
Prepares the chat buffer for incremental text."
  (with-current-buffer (get-buffer-create agent-q-chat-buffer-name)
    ;; Initialize streaming state
    (setq agent-q--streaming-marker (copy-marker agent-q--output-end-marker))
    (setq agent-q--pending-response "")
    ;; Insert assistant header
    (save-excursion
      (goto-char agent-q--output-end-marker)
      (let ((inhibit-read-only t))
        (insert "\n" (propertize "Assistant:" 'face 'agent-q-header-face) "\n")
        (set-marker agent-q--output-end-marker (point))
        (set-marker agent-q--streaming-marker (point))))
    (redisplay t)))

(defun agent-q--streaming-error (error-message)
  "Called when streaming encounters an error.
ERROR-MESSAGE is the error description string."
  (with-current-buffer (get-buffer-create agent-q-chat-buffer-name)
    (save-excursion
      (goto-char (or agent-q--streaming-marker agent-q--output-end-marker))
      (let ((inhibit-read-only t))
        (insert "\n" (propertize (format "[Error: %s]" error-message)
                                'face 'error) "\n")
        (set-marker agent-q--output-end-marker (point))))
    (setq agent-q--streaming-marker nil)
    (redisplay t)))
```

**Step 2: Verify existing functions work with streaming**

The existing functions should already work:
- `agent-q--append-response-chunk` - appends text at streaming marker
- `agent-q--finalize-response` - cleans up streaming state

**Step 3: Test in Emacs**

1. Open Agent-Q chat: `M-x agent-q-chat`
2. Send a message: "Count from 1 to 10"
3. Expected: Numbers appear one by one, not all at once

**Step 4: Commit**

```bash
git add contrib/sly-agent-q/sly-agent-q-chat.el
git commit -m "feat(chat): add streaming start and error handlers"
```

---

## Task 6: Add Observability Hooks

**Files:**
- Create: `src/observability.lisp`

**Step 1: Write observability infrastructure**

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Observability Infrastructure
;;;
;;; Hooks for logging, metrics, and debugging LLM interactions.

(defvar *agent-q-hooks* nil
  "Global hooks for Agent-Q LLM interactions.")

(defvar *request-log* nil
  "Chronological log of LLM requests and responses.")

(defvar *request-metrics* nil
  "Aggregated metrics: request counts, token usage, timing.")

(defun setup-observability (&key (level :info) (log-to-file nil))
  "Configure observability hooks for Agent-Q.

   LEVEL: Logging level (:debug, :info, :warn)
   LOG-TO-FILE: Optional file path for logging

   Returns: Configured hooks object"
  (let* ((stream (if log-to-file
                    (open log-to-file :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create)
                    *standard-output*))
         (hooks (cl-llm-provider:make-logging-hooks
                 :stream stream
                 :level level)))

    ;; Add custom before-request hook for Agent-Q specific logging
    (cl-llm-provider:add-hook hooks :before-request
      (lambda (provider model messages)
        (push (list :timestamp (get-universal-time)
                    :event :request
                    :provider (cl-llm-provider:provider-type provider)
                    :model model
                    :message-count (length messages))
              *request-log*)))

    ;; Add after-response hook for metrics
    (cl-llm-provider:add-hook hooks :after-response
      (lambda (provider model response timing)
        (let* ((usage (cl-llm-provider:response-usage response))
               (tokens (when usage (getf usage :total-tokens))))
          (push (list :timestamp (get-universal-time)
                      :event :response
                      :provider (cl-llm-provider:provider-type provider)
                      :model model
                      :timing timing
                      :tokens tokens)
                *request-log*))))

    ;; Store and return
    (setf *agent-q-hooks* hooks)
    (setf cl-llm-provider:*global-hooks* hooks)
    hooks))

(defun clear-request-log ()
  "Clear the request log."
  (setf *request-log* nil))

(defun get-request-stats ()
  "Get summary statistics from request log.
   Returns plist with :total-requests, :total-tokens, :avg-timing"
  (let ((requests (remove-if-not (lambda (e) (eq (getf e :event) :response))
                                 *request-log*)))
    (list :total-requests (length requests)
          :total-tokens (reduce #'+ requests
                                :key (lambda (e) (or (getf e :tokens) 0))
                                :initial-value 0)
          :avg-timing (if requests
                         (/ (reduce #'+ requests
                                    :key (lambda (e) (or (getf e :timing) 0))
                                    :initial-value 0)
                            (length requests))
                         0))))
```

**Step 2: Add to agent-q.asd**

Add `(:file "observability")` after `streaming`.

**Step 3: Initialize hooks in configure function**

Modify `src/config.lisp` to call `setup-observability` during configuration.

**Step 4: Commit**

```bash
git add src/observability.lisp agent-q.asd src/config.lisp
git commit -m "feat(observability): add logging hooks and metrics collection"
```

---

## Task 7: Add Cost Estimation API

**Files:**
- Create: `src/cost.lisp`

**Step 1: Write cost estimation functions**

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Cost Estimation
;;;
;;; Pre-flight cost estimation and tracking for LLM requests.

(defun estimate-request-cost (messages &key system-prompt (max-tokens 4096))
  "Estimate cost for a request before sending.

   MESSAGES: List of message plists
   SYSTEM-PROMPT: Optional system prompt string
   MAX-TOKENS: Expected maximum output tokens

   Returns: (values input-cost output-cost total-cost) in USD,
            or NIL if pricing unavailable"
  (when *provider-instance*
    (let ((model (cl-llm-provider:provider-default-model *provider-instance*)))
      (multiple-value-bind (input-cost output-cost total-cost)
          (cl-llm-provider:estimate-cost
           messages
           :provider *provider-instance*
           :model model
           :system system-prompt
           :max-tokens max-tokens)
        (values input-cost output-cost total-cost)))))

(defun format-cost-usd (cost)
  "Format a cost value for display."
  (if cost
      (cl-llm-provider:format-cost cost nil)
      "N/A"))

(defun check-budget (messages &key system-prompt (max-tokens 4096) (budget 0.10))
  "Check if estimated request cost is within budget.

   BUDGET: Maximum allowed cost in USD (default $0.10)

   Returns: T if within budget, signals error if exceeded"
  (multiple-value-bind (input-cost output-cost total-cost)
      (estimate-request-cost messages
                            :system-prompt system-prompt
                            :max-tokens max-tokens)
    (if total-cost
        (if (<= total-cost budget)
            (progn
              (format t "~&[AGENT-Q] Estimated cost: ~A (within budget ~A)~%"
                      (format-cost-usd total-cost)
                      (format-cost-usd budget))
              t)
            (error "Estimated cost ~A exceeds budget ~A"
                   (format-cost-usd total-cost)
                   (format-cost-usd budget)))
        (progn
          (format t "~&[AGENT-Q] Cost estimation unavailable for model~%")
          t))))

(defun get-session-cost ()
  "Calculate total cost for current session based on logged token usage.
   Returns plist with :input-cost, :output-cost, :total-cost"
  (let ((input-tokens 0)
        (output-tokens 0))
    ;; Sum from request log
    (dolist (entry *request-log*)
      (when (eq (getf entry :event) :response)
        (let ((tokens (getf entry :tokens)))
          (when tokens
            ;; Rough split: assume 20% output, 80% input
            (incf input-tokens (floor (* tokens 0.8)))
            (incf output-tokens (floor (* tokens 0.2)))))))

    ;; Get pricing from model metadata
    (when *provider-instance*
      (let* ((model (cl-llm-provider:provider-default-model *provider-instance*))
             (meta (cl-llm-provider:model-metadata *provider-instance* model)))
        (if meta
            (let ((input-price (getf meta :input-cost-per-1m-tokens))
                  (output-price (getf meta :output-cost-per-1m-tokens)))
              (list :input-cost (* input-tokens (/ input-price 1000000.0))
                    :output-cost (* output-tokens (/ output-price 1000000.0))
                    :total-cost (+ (* input-tokens (/ input-price 1000000.0))
                                  (* output-tokens (/ output-price 1000000.0)))
                    :input-tokens input-tokens
                    :output-tokens output-tokens))
            (list :input-cost nil
                  :output-cost nil
                  :total-cost nil
                  :input-tokens input-tokens
                  :output-tokens output-tokens))))))
```

**Step 2: Add to agent-q.asd**

Add `(:file "cost")` after `observability`.

**Step 3: Export cost functions**

Add to `src/package.lisp`:
```lisp
(:export
 ;; ... existing exports ...
 #:estimate-request-cost
 #:format-cost-usd
 #:check-budget
 #:get-session-cost)
```

**Step 4: Commit**

```bash
git add src/cost.lisp agent-q.asd src/package.lisp
git commit -m "feat(cost): add cost estimation and budget checking"
```

---

## Task 8: Add Metadata API Integration

**Files:**
- Modify: `src/config.lisp`
- Modify: `src/agent.lisp`

**Step 1: Add capability checking during configuration**

```lisp
(defun configure (&key provider model api-key)
  "Configure Agent-Q with LLM provider settings.

   Validates provider supports required capabilities (tools)."
  ;; ... existing code ...

  ;; Check required capabilities
  (unless (cl-llm-provider:provider-supports-p *provider-instance* :tools)
    (warn "Provider ~A may not support tool calling"
          (cl-llm-provider:provider-name *provider-instance*)))

  ;; Check streaming support
  (unless (cl-llm-provider:provider-supports-p *provider-instance* :streaming)
    (warn "Provider ~A may not support streaming. Falling back to synchronous."
          (cl-llm-provider:provider-name *provider-instance*)))

  ;; Log configuration
  (format t "~&[AGENT-Q] Configured: ~A (~A)~%"
          (cl-llm-provider:provider-name *provider-instance*)
          model)
  (format t "~&[AGENT-Q] Capabilities: ~S~%"
          (cl-llm-provider:provider-capabilities *provider-instance*))

  *provider-instance*)
```

**Step 2: Use metadata for context window awareness**

```lisp
(defun get-context-limit ()
  "Get context window size for current model.
   Returns token limit or default if unavailable."
  (when *provider-instance*
    (let* ((model (cl-llm-provider:provider-default-model *provider-instance*))
           (meta (cl-llm-provider:model-metadata *provider-instance* model)))
      (if meta
          (getf meta :context-window)
          ;; Conservative defaults by provider
          (case (cl-llm-provider:provider-type *provider-instance*)
            (:anthropic 200000)
            (:openai 128000)
            (:ollama 8192)
            (t 8192))))))

(defun check-context-limit (messages)
  "Check if messages fit within model's context window.
   Signals warning if approaching limit."
  (let ((estimated-tokens (cl-llm-provider:count-tokens messages))
        (limit (get-context-limit)))
    (when (> estimated-tokens (* limit 0.8))
      (warn "Messages (~D tokens) approaching context limit (~D)"
            estimated-tokens limit))
    estimated-tokens))
```

**Step 3: Commit**

```bash
git add src/config.lisp src/agent.lisp
git commit -m "feat(metadata): add capability checking and context awareness"
```

---

## Task 9: Update Elisp Session Display

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-chat.el`

**Step 1: Add cost display to session info**

```elisp
(defun agent-q-chat-set-session-info (model provider input-tokens output-tokens &optional cost)
  "Update session info display in header line.
MODEL, PROVIDER: Current model and provider names
INPUT-TOKENS, OUTPUT-TOKENS: Token counts
COST: Optional cost in USD"
  (setq agent-q--session-model model)
  (setq agent-q--session-provider provider)
  (setq agent-q--session-input-tokens input-tokens)
  (setq agent-q--session-output-tokens output-tokens)
  (setq agent-q--session-cost cost)
  (agent-q--update-header-line))

(defun agent-q--update-header-line ()
  "Update the header line with session info."
  (when (eq major-mode 'agent-q-chat-mode)
    (setq header-line-format
          (format " Agent-Q | %s (%s) | Tokens: %d/%d%s"
                  (or agent-q--session-model "?")
                  (or agent-q--session-provider "?")
                  (or agent-q--session-input-tokens 0)
                  (or agent-q--session-output-tokens 0)
                  (if agent-q--session-cost
                      (format " | Cost: $%.4f" agent-q--session-cost)
                    "")))))
```

**Step 2: Commit**

```bash
git add contrib/sly-agent-q/sly-agent-q-chat.el
git commit -m "feat(chat): add cost display to session header"
```

---

## Task 10: Add Tests for New Features

**Files:**
- Create: `test/streaming-test.lisp`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-chat-test.el`

**Step 1: Write CL streaming tests**

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q-TESTS; Base: 10 -*-

(in-package :agent-q-tests)

(def-test streaming-callback-creation ()
  "Test that streaming callbacks are created correctly."
  (let ((chunk-cb (agent-q::make-emacs-streaming-callback))
        (complete-cb (agent-q::make-emacs-complete-callback))
        (error-cb (agent-q::make-emacs-error-callback)))
    (is (functionp chunk-cb))
    (is (functionp complete-cb))
    (is (functionp error-cb))))

(def-test cost-estimation ()
  "Test cost estimation functions."
  (let ((messages '((:role "user" :content "Hello world"))))
    ;; Without provider, should return nil
    (let ((agent-q:*provider-instance* nil))
      (is (null (agent-q:estimate-request-cost messages))))))

(def-test observability-setup ()
  "Test observability hook setup."
  (let ((hooks (agent-q::setup-observability :level :warn)))
    (is (not (null hooks)))
    (is (eq cl-llm-provider:*global-hooks* hooks))))
```

**Step 2: Write Elisp streaming tests**

Add to `sly-agent-q-chat-test.el`:

```elisp
(ert-deftest agent-q-chat/streaming/start ()
  "Test streaming start handler."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--start-streaming)
    (should (markerp agent-q--streaming-marker))
    (should (string-match-p "Assistant:" (buffer-string)))))

(ert-deftest agent-q-chat/streaming/append-chunk ()
  "Test streaming chunk append."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--start-streaming)
    (agent-q--append-response-chunk "Hello ")
    (agent-q--append-response-chunk "World")
    (should (string-match-p "Hello World" (buffer-string)))))

(ert-deftest agent-q-chat/streaming/error ()
  "Test streaming error handler."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--start-streaming)
    (agent-q--streaming-error "Test error")
    (should (string-match-p "Error: Test error" (buffer-string)))
    (should (null agent-q--streaming-marker))))
```

**Step 3: Run tests**

```bash
# Elisp tests
cd contrib/sly-agent-q/test
emacs --batch -l run.el

# CL tests (manual)
(ql:quickload "agent-q-tests")
(agent-q-tests:run-tests)
```

**Step 4: Commit**

```bash
git add test/streaming-test.lisp contrib/sly-agent-q/test/sly-agent-q-chat-test.el
git commit -m "test: add streaming and observability tests"
```

---

## Task 11: Update Documentation

**Files:**
- Modify: `README.md`
- Modify: `CLAUDE.md`

**Step 1: Update README with new features**

Add section on streaming and observability:

```markdown
## Streaming Responses

Agent-Q now streams LLM responses in real-time:

- Text appears incrementally as the model generates it
- Tool calls are indicated with debug messages
- Progress feedback for long responses

## Observability

Built-in logging and metrics:

```lisp
;; Enable detailed logging
(agent-q:setup-observability :level :debug)

;; View request statistics
(agent-q:get-request-stats)
;; => (:TOTAL-REQUESTS 5 :TOTAL-TOKENS 1234 :AVG-TIMING 1.5)

;; Get session cost
(agent-q:get-session-cost)
;; => (:TOTAL-COST 0.0123 :INPUT-TOKENS 800 :OUTPUT-TOKENS 400)
```
```

**Step 2: Update CLAUDE.md with architecture changes**

Document the new streaming architecture and hook system.

**Step 3: Commit**

```bash
git add README.md CLAUDE.md
git commit -m "docs: add streaming and observability documentation"
```

---

## Task 12: Integration Testing

**Files:** None (manual testing)

**Step 1: Test streaming end-to-end**

1. Start fresh Emacs session
2. Load Agent-Q: `(ql:quickload "agent-q")`
3. Configure: `(agent-q:configure :provider :anthropic :model "claude-sonnet-4-20250514")`
4. Open chat: `M-x agent-q-chat`
5. Send message: "Count from 1 to 20"
6. Verify: Numbers appear incrementally, not all at once

**Step 2: Test tool calls with streaming**

1. Send: "What functions are defined in agent.lisp?"
2. Verify: Tool call debug messages appear
3. Verify: Results stream in after tool execution

**Step 3: Test observability**

1. In REPL: `(agent-q:get-request-stats)`
2. Verify: Shows request count and token usage
3. In REPL: `(agent-q:get-session-cost)`
4. Verify: Shows cost breakdown

**Step 4: Test error handling**

1. Disconnect network briefly
2. Send message
3. Verify: Error displayed in chat, not crash

---

## Summary

| Task | Component | Complexity |
|------|-----------|------------|
| 1 | Streaming LLM function | Simple |
| 2 | Agent class updates | Simple |
| 3 | Streaming callback infrastructure | Medium |
| 4 | Streaming agent loop | Complex |
| 5 | Elisp streaming handlers | Medium |
| 6 | Observability hooks | Medium |
| 7 | Cost estimation API | Simple |
| 8 | Metadata API integration | Simple |
| 9 | Elisp session display | Simple |
| 10 | Tests | Medium |
| 11 | Documentation | Simple |
| 12 | Integration testing | Manual |

**Total estimated tasks:** 12
**Critical path:** Tasks 1-5 (streaming), then 6-8 (observability/cost)
