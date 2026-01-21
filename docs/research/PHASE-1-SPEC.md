# Agent-Q Phase 1 Specification: The Foundation (MVP)

## Overview

**Goal:** Establish the core infrastructure for Agent-Q - a working system that can accept user input, accumulate context, communicate with LLMs, and insert responses into Emacs buffers.

**Success Criteria:** User can mark a region of code, add it to context, give an instruction like "document this function", and have the agent's response inserted into a buffer.

**Estimated Scope:** Foundation for all future phases. No tool use yet - pure prompt/response.

---

## Architecture

```
┌─────────────────────────────────────────────────┐
│                 EMACS (Elisp)                   │
│  ┌─────────────────────────────────────────┐    │
│  │           sly-agent-q.el                │    │
│  │  - Minor mode                           │    │
│  │  - Keybindings                          │    │
│  │  - Context picker UI                    │    │
│  │  - Conversation buffer                  │    │
│  │  - Region/buffer operations             │    │
│  └─────────────────┬───────────────────────┘    │
│                    │ SLY RPC                    │
└────────────────────┼────────────────────────────┘
                     │
┌────────────────────┼────────────────────────────┐
│                    ▼                            │
│              LISP IMAGE                         │
│  ┌─────────────────────────────────────────┐    │
│  │           agent-q (ASDF system)         │    │
│  │                                         │    │
│  │  ┌─────────────┐  ┌─────────────────┐   │    │
│  │  │   Context   │  │  Conversation   │   │    │
│  │  │   Manager   │  │    Manager      │   │    │
│  │  └──────┬──────┘  └────────┬────────┘   │    │
│  │         └──────────┬───────┘            │    │
│  │                    ▼                    │    │
│  │            ┌──────────────┐             │    │
│  │            │  Agent Core  │             │    │
│  │            └───────┬──────┘             │    │
│  │                    │                    │    │
│  │            ┌───────▼──────┐             │    │
│  │            │ cl-llm-      │             │    │
│  │            │ provider     │             │    │
│  │            └──────────────┘             │    │
│  └─────────────────────────────────────────┘    │
└─────────────────────────────────────────────────┘
```

---

## File Structure

```
agent-q/
├── agent-q.asd                    # ASDF system definition
├── README.md
├── src/
│   ├── package.lisp               # Package definition
│   ├── config.lisp                # Configuration management
│   ├── context.lisp               # Context accumulation
│   ├── conversation.lisp          # Conversation/message history
│   ├── agent.lisp                 # Core agent loop
│   ├── prompts.lisp               # System prompt management
│   └── sly-interface.lisp         # SLY RPC endpoints
└── contrib/
    └── sly-agent-q/
        ├── sly-agent-q.el         # Emacs integration
        └── agent-q-faces.el       # Custom faces for UI (optional)
```

---

## Data Structures

### CL Side

```lisp
;;; package.lisp
(defpackage #:agent-q
  (:use #:cl)
  (:export
   ;; Config
   #:*default-provider*
   #:*default-model*
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
```

```lisp
;;; context.lisp

(defclass context-item ()
  ((id :initarg :id 
       :accessor context-item-id
       :initform (generate-id))
   (item-type :initarg :type 
              :accessor context-item-type
              :type (member :code :text :file :repl-history :error :custom)
              :documentation "Type of context item")
   (content :initarg :content 
            :accessor context-item-content
            :type string
            :documentation "The actual content")
   (metadata :initarg :metadata 
             :accessor context-item-metadata
             :initform nil
             :documentation "Plist of metadata: source file, line numbers, package, etc.")
   (timestamp :initarg :timestamp 
              :accessor context-item-timestamp
              :initform (get-universal-time))))

(defclass context-manager ()
  ((items :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor context-items)
   (max-items :initarg :max-items
              :initform 50
              :accessor context-max-items
              :documentation "Sliding window size")))

;; Key functions
(defgeneric add-context (manager item-or-content &key type metadata))
(defgeneric clear-context (manager))
(defgeneric get-context (manager &key types limit))
(defgeneric context-to-string (manager)
  (:documentation "Format context for inclusion in LLM prompt"))
```

```lisp
;;; conversation.lisp

(defclass message ()
  ((role :initarg :role 
         :accessor message-role
         :type (member :system :user :assistant))
   (content :initarg :content 
            :accessor message-content
            :type string)
   (timestamp :initarg :timestamp 
              :accessor message-timestamp
              :initform (get-universal-time))))

(defclass conversation ()
  ((id :initarg :id 
       :accessor conversation-id
       :initform (generate-id))
   (messages :initform nil 
             :accessor conversation-messages)
   (context-manager :initarg :context 
                    :accessor conversation-context
                    :initform (make-instance 'context-manager))
   (created-at :initform (get-universal-time)
               :accessor conversation-created-at)
   (project :initarg :project 
            :accessor conversation-project
            :initform nil
            :documentation "Project identifier for persistence")))

;; Key functions
(defgeneric add-message (conversation role content))
(defgeneric get-messages (conversation &key limit))
(defgeneric clear-conversation (conversation))
```

```lisp
;;; agent.lisp

(defclass agent ()
  ((provider :initarg :provider
             :accessor agent-provider
             :documentation "cl-llm-provider instance")
   (model :initarg :model
          :accessor agent-model)
   (conversation :initarg :conversation
                 :accessor agent-conversation
                 :initform (make-instance 'conversation))
   (system-prompt :initarg :system-prompt
                  :accessor agent-system-prompt
                  :initform *base-system-prompt*)))

(defvar *current-agent* nil
  "The currently active agent instance")

;; Main entry point
(defgeneric send-to-agent (agent user-message &key include-context)
  (:documentation "Send a message to the agent and get a response.
   If include-context is true, prepend accumulated context to the message."))
```

### Elisp Side

```elisp
;;; sly-agent-q.el

;; Customization group
(defgroup sly-agent-q nil
  "Agent-Q: AI-powered assistant for Common Lisp development"
  :group 'sly
  :prefix "sly-agent-q-")

;; User options
(defcustom sly-agent-q-keymap-prefix "C-c q"
  "Prefix for Agent-Q keybindings."
  :type 'string
  :group 'sly-agent-q)

(defcustom sly-agent-q-conversation-buffer-name "*Agent-Q*"
  "Name of the conversation buffer."
  :type 'string
  :group 'sly-agent-q)

(defcustom sly-agent-q-insert-response-at-point t
  "If non-nil, insert agent responses at point. Otherwise show in conversation buffer."
  :type 'boolean
  :group 'sly-agent-q)

;; State
(defvar sly-agent-q--context-items '()
  "List of pending context items to be sent with next message.")

;; Faces
(defface sly-agent-q-user-face
  '((t :foreground "cyan" :weight bold))
  "Face for user messages in conversation buffer."
  :group 'sly-agent-q)

(defface sly-agent-q-assistant-face
  '((t :foreground "green"))
  "Face for assistant messages in conversation buffer."
  :group 'sly-agent-q)

(defface sly-agent-q-context-face
  '((t :foreground "yellow" :slant italic))
  "Face for context indicators."
  :group 'sly-agent-q)
```

---

## API Specification

### SLY RPC Endpoints (CL → Elisp calls from CL)

These are functions defined in CL that Elisp can call via SLY:

```lisp
;;; sly-interface.lisp

(defun agent-q-send (message &key include-context)
  "Send MESSAGE to the agent. Returns response string.
   If INCLUDE-CONTEXT is true, accumulated context is included."
  ...)

(defun agent-q-add-context (content &key type metadata)
  "Add CONTENT to the current context.
   TYPE is one of: :code :text :file :repl-history :error :custom
   METADATA is a plist with keys like :filename :start-line :end-line :package"
  ...)

(defun agent-q-clear-context ()
  "Clear all accumulated context."
  ...)

(defun agent-q-get-context-summary ()
  "Return a summary of current context for display.
   Returns plist: (:count N :types (list of types) :preview STRING)"
  ...)

(defun agent-q-new-conversation (&key project)
  "Start a new conversation, optionally associated with PROJECT."
  ...)

(defun agent-q-configure (&key provider model api-key)
  "Configure the agent. Returns T on success, error message on failure."
  ...)

(defun agent-q-get-conversation-history (&key limit)
  "Return recent conversation history for display.
   Returns list of (:role ROLE :content CONTENT :timestamp TIME)"
  ...)
```

### Elisp Commands (User-facing)

```elisp
;; Context commands
(defun sly-agent-q-add-region-to-context (start end)
  "Add the selected region to Agent-Q context."
  (interactive "r")
  ...)

(defun sly-agent-q-add-buffer-to-context ()
  "Add the entire current buffer to Agent-Q context."
  (interactive)
  ...)

(defun sly-agent-q-add-defun-to-context ()
  "Add the current top-level form to Agent-Q context."
  (interactive)
  ...)

(defun sly-agent-q-clear-context ()
  "Clear all accumulated context."
  (interactive)
  ...)

(defun sly-agent-q-show-context ()
  "Display current context summary in minibuffer or popup."
  (interactive)
  ...)

;; Conversation commands
(defun sly-agent-q-send (message)
  "Send MESSAGE to the agent."
  (interactive "sMessage: ")
  ...)

(defun sly-agent-q-send-region-with-instruction (start end instruction)
  "Send region with an instruction. Adds region to context first."
  (interactive "r\nsInstruction: ")
  ...)

(defun sly-agent-q-new-conversation ()
  "Start a new conversation."
  (interactive)
  ...)

(defun sly-agent-q-show-conversation ()
  "Show/switch to the conversation buffer."
  (interactive)
  ...)

;; Response handling
(defun sly-agent-q-insert-last-response ()
  "Insert the last agent response at point."
  (interactive)
  ...)

(defun sly-agent-q-copy-last-response ()
  "Copy the last agent response to kill ring."
  (interactive)
  ...)

;; Quick actions (common tasks)
(defun sly-agent-q-document-defun ()
  "Ask agent to document the current function."
  (interactive)
  ...)

(defun sly-agent-q-explain-region (start end)
  "Ask agent to explain the selected code."
  (interactive "r")
  ...)

(defun sly-agent-q-fix-error ()
  "Send recent error to agent and ask for fix."
  (interactive)
  ...)
```

### Keybindings

```elisp
;; Default keymap (under C-c q prefix)
(defvar sly-agent-q-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Context
    (define-key map (kbd "c r") #'sly-agent-q-add-region-to-context)
    (define-key map (kbd "c b") #'sly-agent-q-add-buffer-to-context)
    (define-key map (kbd "c d") #'sly-agent-q-add-defun-to-context)
    (define-key map (kbd "c c") #'sly-agent-q-clear-context)
    (define-key map (kbd "c s") #'sly-agent-q-show-context)
    
    ;; Conversation
    (define-key map (kbd "s") #'sly-agent-q-send)
    (define-key map (kbd "r") #'sly-agent-q-send-region-with-instruction)
    (define-key map (kbd "n") #'sly-agent-q-new-conversation)
    (define-key map (kbd "v") #'sly-agent-q-show-conversation)
    
    ;; Response
    (define-key map (kbd "i") #'sly-agent-q-insert-last-response)
    (define-key map (kbd "w") #'sly-agent-q-copy-last-response)
    
    ;; Quick actions
    (define-key map (kbd "q d") #'sly-agent-q-document-defun)
    (define-key map (kbd "q e") #'sly-agent-q-explain-region)
    (define-key map (kbd "q f") #'sly-agent-q-fix-error)
    
    map))
```

---

## System Prompts

### Base System Prompt

```lisp
;;; prompts.lisp

(defparameter *base-system-prompt*
  "You are Agent-Q, an AI assistant specialized in Common Lisp development.

You are integrated into a live Common Lisp development environment (SLY/Emacs).
You help developers write, understand, debug, and improve Common Lisp code.

## Your Capabilities (Current Phase)
- Receive code snippets and context from the developer's buffers
- Provide explanations, documentation, and code suggestions
- Help debug errors and suggest fixes

## Guidelines
1. Write idiomatic Common Lisp code following community conventions
2. Prefer standard CL constructs over implementation-specific extensions unless asked
3. Include docstrings for functions and explain complex macros
4. When fixing bugs, explain what was wrong and why your fix works
5. Be concise but thorough - developers value precision

## Context
You will receive context about the developer's current work, including:
- Code snippets they've selected
- File information (name, package)
- REPL history when relevant
- Error messages and conditions

Use this context to provide relevant, targeted assistance.

## Response Format
- For code: Use proper Common Lisp formatting
- For explanations: Be clear and technical
- When suggesting changes: Show the complete corrected code, not just fragments"
  "Base system prompt for Agent-Q")
```

### Project Prompt Loading

```lisp
(defun load-project-prompt (project-root)
  "Load project-specific prompt from PROJECT-ROOT/.agent-q/system-prompt.md
   Returns nil if not found."
  (let ((prompt-file (merge-pathnames 
                      ".agent-q/system-prompt.md" 
                      project-root)))
    (when (probe-file prompt-file)
      (with-open-file (s prompt-file :direction :input)
        (let ((content (make-string (file-length s))))
          (read-sequence content s)
          content)))))

(defun compose-system-prompt (&key project-root task-prompt)
  "Compose full system prompt from base + project + task prompts."
  (with-output-to-string (s)
    (write-string *base-system-prompt* s)
    (when project-root
      (let ((project-prompt (load-project-prompt project-root)))
        (when project-prompt
          (format s "~%~%## Project-Specific Instructions~%~%~A" project-prompt))))
    (when task-prompt
      (format s "~%~%## Current Task~%~%~A" task-prompt))))
```

---

## cl-llm-provider Integration

### Required Interface (What Agent-Q needs)

```lisp
;;; Expected cl-llm-provider API

;; Provider creation
(cl-llm-provider:make-provider :anthropic :api-key "...")
(cl-llm-provider:make-provider :openai :api-key "...")

;; Basic chat completion
(cl-llm-provider:chat provider
  :model "claude-sonnet-4-20250514"
  :messages '((:role :system :content "You are helpful")
              (:role :user :content "Hello"))
  :max-tokens 4096)

;; Response structure expected
;; => (:content "Response text"
;;     :usage (:input-tokens N :output-tokens M)
;;     :finish-reason :stop)  ; or :length
```

### Agent-Q Wrapper

```lisp
;;; agent.lisp

(defmethod send-to-agent ((agent agent) user-message &key include-context)
  (let* ((context-string (when include-context
                          (context-to-string 
                           (conversation-context 
                            (agent-conversation agent)))))
         (full-message (if context-string
                          (format nil "## Context~%~%~A~%~%## Request~%~%~A"
                                  context-string user-message)
                          user-message))
         (conversation (agent-conversation agent)))
    
    ;; Add user message to history
    (add-message conversation :user full-message)
    
    ;; Build messages for API
    (let* ((messages (cons (list :role :system 
                                :content (agent-system-prompt agent))
                          (mapcar (lambda (msg)
                                   (list :role (message-role msg)
                                         :content (message-content msg)))
                                 (conversation-messages conversation))))
           ;; Call LLM
           (response (cl-llm-provider:chat 
                     (agent-provider agent)
                     :model (agent-model agent)
                     :messages messages
                     :max-tokens 4096)))
      
      ;; Extract and store response
      (let ((content (getf response :content)))
        (add-message conversation :assistant content)
        content))))
```

---

## Configuration

### Config File Location

```
~/.config/agent-q/config.lisp
```

### Config Structure

```lisp
;;; config.lisp

(defparameter *default-provider* :anthropic)
(defparameter *default-model* "claude-sonnet-4-20250514")
(defparameter *api-keys* (make-hash-table :test 'equal))

(defun load-config ()
  "Load configuration from user config file."
  (let ((config-file (merge-pathnames 
                      ".config/agent-q/config.lisp"
                      (user-homedir-pathname))))
    (when (probe-file config-file)
      (load config-file))))

(defun configure (&key provider model api-key)
  "Configure agent settings."
  (when provider (setf *default-provider* provider))
  (when model (setf *default-model* model))
  (when api-key 
    (setf (gethash *default-provider* *api-keys*) api-key)))
```

### Example User Config

```lisp
;;; ~/.config/agent-q/config.lisp

(in-package #:agent-q)

(configure :provider :anthropic
           :model "claude-sonnet-4-20250514"
           :api-key (uiop:getenv "ANTHROPIC_API_KEY"))
```

---

## Testing Criteria

### Unit Tests (CL)

```lisp
;; Context tests
- Adding items to context
- Context overflow (sliding window)
- Context serialization to string
- Metadata preservation

;; Conversation tests  
- Message addition
- Role validation
- History retrieval

;; Prompt tests
- Base prompt loading
- Project prompt loading
- Prompt composition
```

### Integration Tests

```lisp
;; End-to-end (requires API key)
- Send simple message, receive response
- Send message with context
- New conversation clears history
- Configuration changes apply
```

### Elisp Tests

```elisp
;; UI tests
- Region selection adds to context
- Keybindings are active in sly-mode
- Conversation buffer displays correctly
- Response insertion works
```

---

## Implementation Order

1. **CL Core First**
   1. `package.lisp` - Define package and exports
   2. `config.lisp` - Configuration basics
   3. `context.lisp` - Context manager
   4. `conversation.lisp` - Conversation manager
   5. `prompts.lisp` - System prompts
   6. `agent.lisp` - Agent core
   7. `sly-interface.lisp` - SLY RPC endpoints

2. **Elisp Integration**
   1. `sly-agent-q.el` - Basic structure, minor mode
   2. Context commands (add region, clear, show)
   3. Send command (basic)
   4. Conversation buffer
   5. Response handling (insert, copy)
   6. Quick action commands

3. **Testing & Polish**
   1. Unit tests for CL
   2. Integration test with real API
   3. Documentation
   4. README with setup instructions

---

## Open Questions for Implementation

1. **Streaming:** Should responses stream token-by-token or return complete? (Start with complete, add streaming later)

2. **Async:** Should `agent-q-send` block or be async? (Start with blocking for simplicity)

3. **Error handling:** How to surface LLM API errors to user? (Display in minibuffer + log)

4. **Rate limiting:** Any client-side rate limiting? (Defer to Phase 2)

---

## Definition of Done

Phase 1 is complete when:

1. ✅ User can `M-x sly-agent-q-add-region-to-context` to add code
2. ✅ User can `M-x sly-agent-q-send` to send a message
3. ✅ Response appears in conversation buffer
4. ✅ User can `M-x sly-agent-q-insert-last-response` to insert code
5. ✅ Context accumulates across multiple adds
6. ✅ New conversation clears context and history
7. ✅ Project-specific prompts load from `.agent-q/system-prompt.md`
8. ✅ Configuration works via config file
9. ✅ Basic documentation exists
