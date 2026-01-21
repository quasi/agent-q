# cl-llm-provider Integration Guide for Agent-Q

This document describes how Agent-Q integrates with `cl-llm-provider` and identifies any gaps or enhancements needed.

---

## Executive Summary

**Great news:** cl-llm-provider already has comprehensive support for everything Agent-Q needs:

| Requirement | Status | Notes |
|-------------|--------|-------|
| Basic chat completion | ✅ Complete | `complete` function |
| Multi-provider support | ✅ Complete | Anthropic, OpenAI, Ollama, OpenRouter |
| System prompt | ✅ Complete | `:system` parameter |
| Tool definitions | ✅ Complete | `tool-definition` class, `define-tool` |
| Tool calling | ✅ Complete | Full tool-use flow supported |
| Tool results | ✅ Complete | `make-tool-result` function |
| Tool execution | ✅ Complete | `execute-tool`, `execute-tool-calls` |
| Safety levels | ✅ Complete | `:safe`, `:moderate`, `:dangerous` |
| Tool registry | ✅ Complete | Full registry with discovery |
| Approval system | ✅ Complete | Callbacks, interactive approval |
| Validators | ✅ Complete | Range, pattern, length, enum, custom |
| Lifecycle hooks | ✅ Complete | `:on-start`, `:on-complete`, `:on-error` |
| Error handling | ✅ Complete | Full condition hierarchy |
| Token usage | ✅ Complete | In response object |
| Performance profiling | ✅ Complete | Optional timing data |

**Agent-Q can use cl-llm-provider as-is.** This document serves as an integration guide.

---

## API Usage for Agent-Q

### Basic Completion

```lisp
(use-package :cl-llm-provider)

;; Simple message
(let ((response (complete '((:role "user" :content "Hello")))))
  (response-content response))

;; With system prompt
(complete messages
          :system "You are Agent-Q, a Lisp development assistant."
          :model "claude-sonnet-4-20250514"
          :max-tokens 4096)
```

### Provider Setup

```lisp
;; Create provider (reads API key from env var automatically)
(defvar *anthropic* (make-provider :anthropic))
(defvar *openai* (make-provider :openai :model "gpt-4-turbo"))
(defvar *ollama* (make-provider :ollama 
                                :base-url "http://localhost:11434"
                                :model "llama2"))

;; Use specific provider
(complete messages :provider *anthropic*)
```

### Conversation History

```lisp
;; Build multi-turn conversation
(let* ((turn1 (complete '((:role "user" :content "Define a factorial function"))))
       (turn2 (complete (list '(:role "user" :content "Define a factorial function")
                             (response-message turn1)
                             '(:role "user" :content "Now optimize it for tail recursion")))))
  (response-content turn2))
```

---

## Tool Integration for Agent-Q

### Defining Agent-Q Tools

```lisp
(use-package :cl-llm-provider)
(use-package :cl-llm-provider.tools)

;; Introspection tool (safe, read-only)
(define-tool "describe_symbol"
  "Get detailed information about a Lisp symbol"
  '((:name "symbol" :type :string :description "The symbol name")
    (:name "package" :type :string :description "Package name (optional)"))
  :required '("symbol")
  :safety-level :safe
  :categories '(:introspection)
  :handler (lambda (args)
             (let* ((sym-name (getf args :symbol))
                    (pkg (find-package (or (getf args :package) *package*)))
                    (sym (find-symbol sym-name pkg)))
               (with-output-to-string (s)
                 (describe sym s)))))

;; Execution tool (cautious, modifies state)
(define-tool "eval_form"
  "Evaluate a Lisp form and return the result"
  '((:name "form" :type :string :description "Lisp form to evaluate")
    (:name "package" :type :string :description "Package context"))
  :required '("form")
  :safety-level :moderate
  :categories '(:execution)
  :requires-approval nil  ; Auto-approve in Agent-Q context
  :handler (lambda (args)
             (let* ((form-str (getf args :form))
                    (pkg (find-package (or (getf args :package) "CL-USER")))
                    (*package* pkg)
                    (form (read-from-string form-str)))
               (format nil "~S" (eval form)))))

;; File write tool (dangerous, requires approval)
(define-tool "write_file"
  "Write content to a file"
  '((:name "path" :type :string :description "File path")
    (:name "content" :type :string :description "Content to write"))
  :required '("path" "content")
  :safety-level :dangerous
  :categories '(:filesystem :destructive)
  :requires-approval :always
  :parameter-validators '(("path" . (:pattern "^/home/|^/tmp/")))
  :handler (lambda (args)
             (with-open-file (f (getf args :path) 
                               :direction :output 
                               :if-exists :supersede)
               (write-string (getf args :content) f))
             "File written successfully"))
```

### Creating the Agent-Q Tool Registry

```lisp
(defvar *agent-q-registry* 
  (make-tool-registry 
   :name "agent-q"
   :default-safety-level :safe))

;; Register tools
(register-tool *agent-q-registry* *describe-symbol-tool*)
(register-tool *agent-q-registry* *eval-form-tool*)
(register-tool *agent-q-registry* *write-file-tool*)
;; ... register more tools

;; Set up logging hooks
(setf (registry-global-hooks *agent-q-registry*)
      (list :on-start (lambda (call args)
                        (format t "~&[AGENT-Q] Executing: ~A~%" 
                                (tool-call-name call)))
            :on-complete (lambda (call args result)
                           (format t "~&[AGENT-Q] Completed: ~A~%" 
                                   (tool-call-name call)))
            :on-error (lambda (call args condition)
                        (format t "~&[AGENT-Q] Error in ~A: ~A~%" 
                                (tool-call-name call) condition))))
```

### Agent Loop with Tools

```lisp
(defun agent-q-send (user-message &key (max-iterations 10))
  "Send message to Agent-Q and handle tool calls automatically."
  (let ((messages (list `(:role "user" :content ,user-message)))
        (tools (tools-for-llm :registry *agent-q-registry*)))
    
    (loop for iteration from 1 to max-iterations
          for response = (complete messages
                                   :system *agent-q-system-prompt*
                                   :tools tools
                                   :provider *anthropic*)
          do
          (cond
            ;; Final text response
            ((and (response-content response)
                  (not (response-tool-calls response)))
             (return (response-content response)))
            
            ;; Tool calls - execute and continue
            ((response-tool-calls response)
             (let ((results (execute-tool-calls response
                             :registry *agent-q-registry*
                             :max-safety-level :moderate)))
               ;; Add assistant message and tool results to history
               (setf messages 
                     (append messages
                             (list (response-message response))
                             (execution-results-to-tool-messages results)))))
            
            ;; No content and no tools - unexpected
            (t
             (return (format nil "Unexpected finish: ~A" 
                            (response-finish-reason response)))))
          
          finally (return "Max iterations reached"))))
```

### Handling Different Safety Levels

```lisp
;; For autonomous operation (Phase 2+), use configurable safety
(defvar *agent-q-max-safety* :moderate)

;; Get only safe tools for certain operations
(defun get-safe-tools ()
  (tools-for-llm :registry *agent-q-registry* 
                 :max-safety-level :safe))

;; Get all tools for privileged operations
(defun get-all-tools ()
  (tools-for-llm :registry *agent-q-registry*))

;; Execute with safety check
(execute-tool-calls response
                    :registry *agent-q-registry*
                    :max-safety-level *agent-q-max-safety*
                    :approval-callback 
                    (make-safety-based-callback 
                     :max-level :moderate
                     :on-exceed :prompt))
```

### Interactive Approval for Dangerous Tools

```lisp
;; For Phase 2 dangerous tools, use interactive approval
(defvar *agent-q-approval-callback*
  (make-interactive-approval-callback :stream *query-io*))

;; Or create custom approval that integrates with Emacs
(defun agent-q-emacs-approval (tool-def tool-call arguments)
  "Request approval through Emacs minibuffer."
  (let* ((tool-name (tool-definition-name tool-def))
         (prompt (format nil "Agent-Q wants to execute ~A with args ~S. Allow? (y/n) "
                        tool-name arguments))
         ;; Call back to Emacs via SLY
         (approved (swank:eval-in-emacs 
                   `(yes-or-no-p ,prompt))))
    (if approved :approved :rejected)))

(setf *agent-q-approval-callback* #'agent-q-emacs-approval)
```

---

## Response Handling

### Accessing Response Fields

```lisp
(let ((response (complete messages :tools tools)))
  ;; Text content
  (response-content response)
  
  ;; Full assistant message (for history)
  (response-message response)
  
  ;; Tool calls (if any)
  (response-tool-calls response)
  
  ;; Finish reason (:stop, :tool-calls, :length, etc.)
  (response-finish-reason response)
  
  ;; Token usage
  (let ((usage (response-usage response)))
    (getf usage :prompt-tokens)
    (getf usage :completion-tokens)
    (getf usage :total-tokens))
  
  ;; Performance timing (if enabled)
  (when *performance-profiling*
    (let ((perf (response-performance response)))
      (getf perf :encode-time)
      (getf perf :api-time)
      (getf perf :decode-time)))
  
  ;; Raw response for debugging
  (response-raw response))
```

### Tool Call Structure

```lisp
;; Each tool call has:
(dolist (call (response-tool-calls response))
  (tool-call-id call)        ; "call_abc123"
  (tool-call-name call)      ; "describe_symbol"  
  (tool-call-arguments call)) ; (:symbol "process-data" :package "MY-APP")
```

### Creating Tool Results

```lisp
;; After executing a tool, create result message
(make-tool-result "call_abc123" "Symbol PROCESS-DATA: Function...")

;; Or use execution-results-to-tool-messages for batch
(let ((results (execute-tool-calls response :registry registry)))
  (execution-results-to-tool-messages results))
```

---

## Error Handling

### Error Hierarchy

```
llm-provider-error (base)
├── provider-configuration-error
├── provider-api-error
│   ├── provider-rate-limit-error
│   └── provider-authentication-error
└── tool-schema-error

;; From cl-llm-provider.tools:
├── tool-validation-error
├── tool-approval-error
├── tool-approval-required
└── tool-safety-violation
```

### Handling in Agent-Q

```lisp
(defun agent-q-safe-send (message)
  "Send with comprehensive error handling."
  (handler-case
      (agent-q-send message)
    
    ;; API errors
    (provider-authentication-error (e)
      (format nil "Authentication failed. Check API key."))
    
    (provider-rate-limit-error (e)
      (format nil "Rate limited. Retry after ~A seconds."
              (error-retry-after e)))
    
    (provider-api-error (e)
      (format nil "API error ~A: ~A"
              (error-status-code e)
              (error-message e)))
    
    ;; Tool errors
    (tool-safety-violation (e)
      (format nil "Tool ~A exceeds safety level ~A"
              (tool-definition-name (error-tool e))
              (error-required-level e)))
    
    (tool-validation-error (e)
      (format nil "Invalid argument ~A: ~A"
              (error-parameter e)
              (error-value e)))
    
    (tool-approval-error (e)
      (format nil "Tool execution not approved"))
    
    ;; Catch-all
    (error (e)
      (format nil "Unexpected error: ~A" e))))
```

---

## Configuration

### Global Defaults

```lisp
;; Set defaults for all Agent-Q requests
(configure-defaults 
 :provider (make-provider :anthropic)
 :model "claude-sonnet-4-20250514"
 :temperature 0.7
 :max-tokens 4096)
```

### Per-Request Override

```lisp
;; Override for specific requests
(complete messages 
          :temperature 0.2      ; More deterministic for code
          :max-tokens 8192)     ; Longer for complex tasks
```

---

## Mapping cl-llm-provider to Agent-Q Architecture

### Tool Categories → Agent-Q Categories

| cl-llm-provider Category | Agent-Q Usage |
|--------------------------|---------------|
| `:search` | Not used |
| `:database` | Knowledge base queries |
| `:filesystem` | Buffer/file tools |
| `:calculation` | Not used |
| `:destructive` | File writes, state changes |
| `:external-api` | Not used |
| Custom `:introspection` | describe, apropos, who-calls |
| Custom `:execution` | eval, compile |
| Custom `:debugging` | restarts, conditions |
| Custom `:testing` | run-test, run-suite |

### Safety Levels → Trust Levels

| cl-llm-provider | Agent-Q Trust | Operations |
|-----------------|---------------|------------|
| `:safe` | Auto-execute | describe, apropos, read-file |
| `:moderate` | Log & execute | eval, compile, write-buffer |
| `:dangerous` | Require approval | write-file, delete-file, invoke-restart |

---

## What Agent-Q Needs to Build

Given that cl-llm-provider provides all the LLM communication infrastructure, Agent-Q needs to build:

### Phase 1 (Foundation)
- **Context Manager** - Accumulate context from buffers/regions
- **Conversation Manager** - Track message history per-conversation  
- **System Prompt Composer** - Build prompts from base + project + task
- **SLY Interface** - RPC endpoints for Elisp communication
- **Elisp UI** - sly-agent-q.el for buffer integration

### Phase 2 (Tools)
- **Tool Definitions** - Define introspection/execution tools using `define-tool`
- **Tool Handlers** - Implement actual tool functionality (the lambdas)
- **Buffer Tools Bridge** - Elisp-side tool handlers via SLY
- **Tool Registry Setup** - Configure registry with Agent-Q tools

### Phase 3+ (Advanced)
- **Knowledge Base** - SQLite storage (separate from cl-llm-provider)
- **Semantic Index** - Code understanding (separate from cl-llm-provider)
- **Context Summarization** - LLM-based summarization using cl-llm-provider

---

## Example: Complete Agent-Q Integration

```lisp
;;;; agent-q/src/llm-integration.lisp

(in-package :agent-q)

(use-package :cl-llm-provider)
(use-package :cl-llm-provider.tools)

;;; Provider configuration
(defvar *agent-q-provider* nil)

(defun configure-agent-q-llm (&key provider model api-key)
  "Configure the LLM provider for Agent-Q."
  (setf *agent-q-provider*
        (make-provider (or provider :anthropic)
                      :model (or model "claude-sonnet-4-20250514")
                      :api-key api-key)))

;;; Tool registry
(defvar *agent-q-tools* (make-tool-registry :name "agent-q"))

(defun register-agent-q-tool (tool)
  "Register a tool with Agent-Q."
  (register-tool *agent-q-tools* tool))

;;; Main agent function
(defun send-to-llm (messages &key system include-tools)
  "Send messages to LLM, optionally with tools."
  (let ((tools (when include-tools
                (tools-for-llm :registry *agent-q-tools*
                              :max-safety-level :moderate))))
    (complete messages
              :provider *agent-q-provider*
              :system system
              :tools tools)))

;;; Tool execution
(defun execute-llm-tool-calls (response)
  "Execute tool calls from LLM response."
  (when (response-tool-calls response)
    (execute-tool-calls response
                        :registry *agent-q-tools*
                        :max-safety-level :moderate)))

;;; Agentic loop
(defun agent-loop (initial-message system-prompt)
  "Run agent loop until completion."
  (let ((messages (list `(:role "user" :content ,initial-message))))
    (loop for response = (send-to-llm messages 
                                      :system system-prompt
                                      :include-tools t)
          do (cond
               ;; Done - return content
               ((and (response-content response)
                     (null (response-tool-calls response)))
                (return (response-content response)))
               
               ;; Execute tools and continue
               ((response-tool-calls response)
                (let ((results (execute-llm-tool-calls response)))
                  (setf messages
                        (append messages
                                (list (response-message response))
                                (execution-results-to-tool-messages results)))))
               
               ;; Unexpected
               (t (return nil))))))
```

---

## Summary

cl-llm-provider is **fully ready** for Agent-Q integration. The library provides:

1. **Complete tool calling support** with the exact API pattern Agent-Q needs
2. **Safety levels and approval system** matching Agent-Q's trust model  
3. **Tool registry** for organizing and discovering tools
4. **Validators and hooks** for robust tool execution
5. **Multi-provider support** for flexibility
6. **Clean error handling** with CL conditions

Agent-Q's focus should be on:
- Building the **actual tool implementations** (introspection, execution, buffer ops)
- Creating the **context and conversation management** layer
- Implementing the **Elisp/SLY integration**
- Building the **knowledge base and semantic index** (Phase 3+)

**No modifications to cl-llm-provider are required.**
