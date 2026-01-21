# Agent-Q Phase 1 Execution Plan (UPDATED with cl-llm-provider)

## Overview

Phase 1 establishes the foundation for Agent-Q: a working system integrating Emacs (SLY) with a Common Lisp agent core that communicates with LLMs via **cl-llm-provider**. This plan follows idiomatic CL library conventions from the cl-library-craft skill.

## Architecture Style Selection

**Selected Template:** Edi Weitz style (flat structure, `:serial t`)

**Rationale:**
- General-purpose library (not web/ORM → not Fukamachi style)
- Medium complexity (7 core CL files, 1 Elisp file)
- Linear dependency chain (context → conversation → agent → sly-interface)
- Traditional CL library domain

## File Structure

```
agent-q/
├── agent-q.asd                    # ASDF system definition
├── src/
│   ├── package.lisp               # Package definition & exports
│   ├── config.lisp                # Configuration + provider management
│   ├── context.lisp               # Context accumulation (classes + methods)
│   ├── conversation.lisp          # Message history management
│   ├── prompts.lisp               # System prompt composition
│   ├── agent.lisp                 # Core agent loop + LLM integration
│   └── sly-interface.lisp         # SLY RPC endpoints
├── contrib/
│   └── sly-agent-q/
│       └── sly-agent-q.el         # Emacs integration
├── specs/                          # (Already exists)
├── README.md
└── LICENSE
```

**Note:** No `specials.lisp` or `conditions.lisp` in Phase 1 - config uses defparameter/defvar directly in `config.lisp`, custom conditions deferred to Phase 2+.

---

## Implementation Order (Strict Sequence)

### Stage 1: CL Core Infrastructure (Bottom-Up)

#### File 1: `agent-q.asd`
**Purpose:** ASDF system definition
**Dependencies:** None
**Creates:** Build system

**Contents:**
```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defsystem :agent-q
  :description "AI-powered agentic extension for Common Lisp development"
  :author "Your Name <your.email@example.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (:swank              ; For SLY integration
               :cl-llm-provider)   ; LLM communication
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                            (:file "config")
                            (:file "context")
                            (:file "conversation")
                            (:file "prompts")
                            (:file "agent")
                            (:file "sly-interface")))))
```

**Load Instructions:**
```lisp
;; First time setup
(ql:quickload "cl-llm-provider")
(asdf:load-system :agent-q)
```

**Verification:**
- [ ] Loads without errors: `(asdf:load-system :agent-q)`
- [ ] Version metadata present
- [ ] cl-llm-provider dependency resolves

---

#### File 2: `src/package.lisp`
**Purpose:** Package definition with complete export list
**Dependencies:** None
**Creates:** `:agent-q` package

**Contents:** (Per Phase 1 spec lines 82-128)
```lisp
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
```

**Verification:**
- [ ] Package exists: `(find-package :agent-q)`
- [ ] All exports accessible

---

#### File 3: `src/config.lisp`
**Purpose:** Configuration variables, provider management
**Dependencies:** `package.lisp`
**Creates:** Config management + LLM provider instance

**Key Elements:**
- `*default-provider*` (defparameter) - :anthropic, :openai, :ollama, etc.
- `*default-model*` (defparameter) - Model name string
- `*provider-instance*` (defvar) - Active cl-llm-provider instance
- `load-config` function
- `configure` function (exported) - Creates provider instance

**Implementation:**

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Configuration variables

(defparameter *default-provider* :anthropic
  "Default LLM provider. Options: :anthropic, :openai, :ollama, :openrouter")

(defparameter *default-model* "claude-sonnet-4-20250514"
  "Default model name for the provider.")

(defvar *provider-instance* nil
  "Active cl-llm-provider instance.")

;;; Configuration functions

(defun load-config ()
  "Load configuration from user config file at ~/.config/agent-q/config.lisp"
  (let ((config-file (merge-pathnames
                      ".config/agent-q/config.lisp"
                      (user-homedir-pathname))))
    (when (probe-file config-file)
      (handler-case
          (load config-file)
        (error (e)
          (warn "Failed to load config file ~A: ~A" config-file e)
          nil)))))

(defun configure (&key provider model api-key base-url)
  "Configure agent settings and create provider instance.

  PROVIDER - Provider type (:anthropic, :openai, :ollama, :openrouter)
  MODEL - Model name string
  API-KEY - API key (optional, reads from env var if not provided)
  BASE-URL - Base URL for provider (optional, for Ollama/custom endpoints)

  Returns T on success, NIL on failure."
  (handler-case
      (progn
        ;; Update defaults
        (when provider (setf *default-provider* provider))
        (when model (setf *default-model* model))

        ;; Create provider instance using cl-llm-provider
        (setf *provider-instance*
              (cl-llm-provider:make-provider
               *default-provider*
               :model *default-model*
               :api-key api-key
               :base-url base-url))
        t)
    (error (e)
      (warn "Failed to configure provider: ~A" e)
      nil)))

;;; Initialize provider on load (reads from env vars)
(unless *provider-instance*
  (configure))
```

**Implementation Notes:**
- `cl-llm-provider:make-provider` automatically reads API keys from env vars:
  - `ANTHROPIC_API_KEY` for Anthropic
  - `OPENAI_API_KEY` for OpenAI
  - etc.
- Config file location: `~/.config/agent-q/config.lisp`
- Use `handler-case` for error handling
- Initialize provider on load so system is ready to use

**Verification:**
- [ ] `(configure :provider :anthropic)` succeeds (with valid API key in env)
- [ ] `*provider-instance*` is non-nil after configure
- [ ] Config file loads if present
- [ ] Defaults work without explicit configuration

---

#### File 4: `src/context.lisp`
**Purpose:** Context accumulation classes and methods
**Dependencies:** `config.lisp`
**Creates:** Context management subsystem

**Key Classes:**
- `context-item` (Per Phase 1 spec lines 133-151)
  - Slots: `id`, `item-type`, `content`, `metadata`, `timestamp`
  - Use simple counter for ID generation

- `context-manager` (Per Phase 1 spec lines 153-159)
  - Slot: `items` (adjustable vector with fill-pointer)
  - Slot: `max-items` (default 50, sliding window)

**Key Generics & Methods:**
- `add-context (manager item-or-content &key type metadata)`
- `clear-context (manager)`
- `get-context (manager &key types limit)`
- `context-to-string (manager)` - formats for LLM prompt

**Implementation:**

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; ID generation
(defvar *context-id-counter* 0)

(defun generate-id ()
  "Generate unique ID for context items."
  (format nil "ctx-~D" (incf *context-id-counter*)))

;;; Context item class

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
             :documentation "Plist of metadata: :filename, :start-line, :end-line, :package, etc.")
   (timestamp :initarg :timestamp
              :accessor context-item-timestamp
              :initform (get-universal-time))))

(defun make-context-item (content &key (type :code) metadata)
  "Create a new context item."
  (make-instance 'context-item
                 :content content
                 :type type
                 :metadata metadata))

;;; Context manager class

(defclass context-manager ()
  ((items :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor context-items)
   (max-items :initarg :max-items
              :initform 50
              :accessor context-max-items
              :documentation "Sliding window size")))

;;; Context methods

(defgeneric add-context (manager item-or-content &key type metadata)
  (:documentation "Add context item to manager. Can pass context-item or raw content."))

(defmethod add-context ((manager context-manager) (item context-item) &key type metadata)
  "Add existing context-item to manager."
  (declare (ignore type metadata))
  (let ((items (context-items manager)))
    ;; Check if we've exceeded max
    (when (>= (length items) (context-max-items manager))
      ;; Remove oldest (shift array)
      (setf items (subseq items 1))
      (setf (context-items manager)
            (make-array (length items)
                       :adjustable t
                       :fill-pointer (length items)
                       :initial-contents items)))
    ;; Add new item
    (vector-push-extend item (context-items manager))))

(defmethod add-context ((manager context-manager) (content string) &key (type :code) metadata)
  "Add content as new context item."
  (let ((item (make-context-item content :type type :metadata metadata)))
    (add-context manager item)))

(defgeneric clear-context (manager)
  (:documentation "Clear all context items."))

(defmethod clear-context ((manager context-manager))
  "Clear all context items."
  (setf (context-items manager)
        (make-array 0 :adjustable t :fill-pointer 0)))

(defgeneric get-context (manager &key types limit)
  (:documentation "Get context items, optionally filtered by types and limited in count."))

(defmethod get-context ((manager context-manager) &key types limit)
  "Get context items."
  (let* ((items (coerce (context-items manager) 'list))
         (filtered (if types
                      (remove-if-not (lambda (item)
                                      (member (context-item-type item) types))
                                    items)
                      items))
         (limited (if limit
                     (subseq filtered (max 0 (- (length filtered) limit)))
                     filtered)))
    limited))

(defgeneric context-to-string (manager)
  (:documentation "Format context for inclusion in LLM prompt."))

(defmethod context-to-string ((manager context-manager))
  "Format all context items as markdown for LLM."
  (with-output-to-string (s)
    (format s "## Context~%~%")
    (loop for item across (context-items manager)
          for type = (context-item-type item)
          for content = (context-item-content item)
          for metadata = (context-item-metadata item)
          do
          (format s "### ~A"
                  (string-capitalize (symbol-name type)))
          ;; Add metadata to header if present
          (when metadata
            (let ((filename (getf metadata :filename))
                  (start-line (getf metadata :start-line))
                  (end-line (getf metadata :end-line)))
              (when filename
                (format s " (from ~A" filename)
                (when (and start-line end-line)
                  (format s ":~D-~D" start-line end-line))
                (format s ")"))))
          (format s "~%```lisp~%~A~%```~%~%" content))))
```

**Verification:**
- [ ] Add 3 items, verify count: `(length (context-items mgr))`
- [ ] Add 51 items to 50-max manager, verify oldest dropped
- [ ] `context-to-string` produces readable markdown output
- [ ] Metadata preserved through add/get cycle
- [ ] Clear works

---

#### File 5: `src/conversation.lisp`
**Purpose:** Message history management
**Dependencies:** `context.lisp`
**Creates:** Conversation tracking

**Implementation:** (Per Phase 1 spec lines 172-203)

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Message class

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

(defun make-message (role content)
  "Create a new message."
  (make-instance 'message :role role :content content))

;;; Conversation class

(defvar *conversation-id-counter* 0)

(defclass conversation ()
  ((id :initarg :id
       :accessor conversation-id
       :initform (format nil "conv-~D" (incf *conversation-id-counter*)))
   (messages :initform nil
             :accessor conversation-messages
             :documentation "List of messages (append to end)")
   (context-manager :initarg :context
                    :accessor conversation-context
                    :initform (make-instance 'context-manager))
   (created-at :initform (get-universal-time)
               :accessor conversation-created-at)
   (project :initarg :project
            :accessor conversation-project
            :initform nil
            :documentation "Project identifier for persistence")))

;;; Conversation methods

(defgeneric add-message (conversation role content)
  (:documentation "Add a message to the conversation."))

(defmethod add-message ((conv conversation) role content)
  "Add message with role and content."
  (let ((msg (make-message role content)))
    (setf (conversation-messages conv)
          (append (conversation-messages conv) (list msg)))
    msg))

(defgeneric get-messages (conversation &key limit)
  (:documentation "Get messages, optionally limited to last N."))

(defmethod get-messages ((conv conversation) &key limit)
  "Get messages from conversation."
  (let ((msgs (conversation-messages conv)))
    (if limit
        (subseq msgs (max 0 (- (length msgs) limit)))
        msgs)))

(defgeneric clear-conversation (conversation)
  (:documentation "Clear messages but preserve context manager."))

(defmethod clear-conversation ((conv conversation))
  "Clear conversation messages."
  (setf (conversation-messages conv) nil))

(defun new-conversation (&key project)
  "Create a new conversation."
  (make-instance 'conversation :project project))
```

**Verification:**
- [ ] Add 5 messages, verify order and count
- [ ] `get-messages :limit 2` returns last 2
- [ ] `clear-conversation` empties messages, preserves context-manager
- [ ] Each conversation has unique ID

---

#### File 6: `src/prompts.lisp`
**Purpose:** System prompt management and composition
**Dependencies:** `conversation.lisp`
**Creates:** Prompt building

**Implementation:** (Per Phase 1 spec lines 438-500)

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Base system prompt

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

;;; Project prompt loading

(defun load-project-prompt (project-root)
  "Load project-specific prompt from PROJECT-ROOT/.agent-q/system-prompt.md
   Returns nil if not found."
  (let ((prompt-file (merge-pathnames
                      ".agent-q/system-prompt.md"
                      project-root)))
    (when (probe-file prompt-file)
      (handler-case
          (with-open-file (s prompt-file :direction :input)
            (let ((content (make-string (file-length s))))
              (read-sequence content s)
              content))
        (error (e)
          (warn "Failed to load project prompt from ~A: ~A" prompt-file e)
          nil)))))

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

**Verification:**
- [ ] `*base-system-prompt*` is non-empty string
- [ ] `load-project-prompt` returns nil for non-existent file
- [ ] `load-project-prompt` returns content for existing file
- [ ] `compose-system-prompt` combines all three components

---

#### File 7: `src/agent.lisp`
**Purpose:** Core agent loop and LLM integration via cl-llm-provider
**Dependencies:** `prompts.lisp`, `cl-llm-provider`
**Creates:** Agent orchestration with REAL LLM calls

**Implementation:**

```lisp
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

        (cl-llm-provider:provider-rate-limit-error (e)
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
```

**Implementation Notes:**
- Uses `cl-llm-provider:complete` for LLM calls (per integration spec lines 41-48)
- Message format: List of plists `(:role "user" :content "...")`
- Role must be string: "user", "assistant", "system"
- Response accessed via `cl-llm-provider:response-content`
- Error handling for all provider error types (per integration spec lines 328-364)
- Provider instance from `*provider-instance*` or agent's provider slot

**Verification:**
- [ ] `send-to-agent` without context returns real LLM response
- [ ] `send-to-agent` with context includes context in message
- [ ] User message added to conversation history
- [ ] Assistant response added to conversation history
- [ ] Multiple calls accumulate history correctly
- [ ] Error handling works (test with invalid API key)
- [ ] Token usage accessible via `response-usage` (optional)

---

#### File 8: `src/sly-interface.lisp`
**Purpose:** SLY RPC endpoints
**Dependencies:** `agent.lisp`
**Creates:** Emacs ↔ Lisp communication

**Implementation:** (Per Phase 1 spec lines 288-321)

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; SLY RPC endpoints - callable from Elisp

(defun agent-q-send (message &key include-context)
  "Send MESSAGE to the agent. Returns response string.
   If INCLUDE-CONTEXT is true, accumulated context is included."
  (ensure-agent)
  (send-to-agent *current-agent* message :include-context include-context))

(defun agent-q-add-context (content &key (type :code) metadata)
  "Add CONTENT to the current context.
   TYPE is one of: :code :text :file :repl-history :error :custom
   METADATA is a plist with keys like :filename :start-line :end-line :package"
  (ensure-agent)
  (add-context (conversation-context (agent-conversation *current-agent*))
               content
               :type type
               :metadata metadata)
  t)

(defun agent-q-clear-context ()
  "Clear all accumulated context."
  (when *current-agent*
    (clear-context (conversation-context (agent-conversation *current-agent*)))
    t))

(defun agent-q-get-context-summary ()
  "Return a summary of current context for display.
   Returns plist: (:count N :types (list of types) :preview STRING)"
  (if *current-agent*
      (let* ((ctx-mgr (conversation-context (agent-conversation *current-agent*)))
             (items (coerce (context-items ctx-mgr) 'list))
             (count (length items))
             (types (remove-duplicates (mapcar #'context-item-type items)))
             (preview (if (> count 0)
                         (format nil "~D item~:P: ~{~A~^, ~}"
                                count
                                (mapcar (lambda (type)
                                         (string-capitalize (symbol-name type)))
                                       types))
                         "No context")))
        (list :count count :types types :preview preview))
      (list :count 0 :types nil :preview "No agent initialized")))

(defun agent-q-new-conversation (&key project)
  "Start a new conversation, optionally associated with PROJECT."
  (ensure-agent)
  (setf (agent-conversation *current-agent*)
        (new-conversation :project project))
  t)

(defun agent-q-configure (&key provider model api-key)
  "Configure the agent. Returns T on success, error message on failure."
  (configure :provider provider :model model :api-key api-key))

(defun agent-q-get-conversation-history (&key limit)
  "Return recent conversation history for display.
   Returns list of plists (:role ROLE :content CONTENT :timestamp TIME)"
  (if *current-agent*
      (let ((messages (get-messages (agent-conversation *current-agent*)
                                   :limit limit)))
        (mapcar (lambda (msg)
                  (list :role (message-role msg)
                        :content (message-content msg)
                        :timestamp (message-timestamp msg)))
                messages))
      nil))
```

**Implementation Notes:**
- All functions operate on `*current-agent*`
- `ensure-agent` creates agent on first call
- SLY makes these directly callable from Elisp
- Return values must be serializable (strings, lists, plists, numbers)

**Verification:**
- [ ] Each function callable from REPL
- [ ] `agent-q-send` routes to `send-to-agent` and returns LLM response
- [ ] `agent-q-add-context` adds to current conversation's context
- [ ] `agent-q-get-context-summary` returns correct plist
- [ ] `agent-q-new-conversation` creates fresh conversation
- [ ] Functions work without prior `ensure-agent` call

---

### Stage 2: Elisp Integration

#### File 9: `contrib/sly-agent-q/sly-agent-q.el`

**Purpose:** Emacs UI and SLY RPC client
**Dependencies:** SLY loaded in Emacs
**Creates:** User interface

**(No changes from original plan - see original plan for full Elisp implementation)**

**Key points:**
- Use `sly-eval-async` to call CL functions
- Example: `(sly-eval-async '(agent-q:agent-q-send "message" :include-context t) #'callback)`
- Display responses in conversation buffer
- Keybindings under C-c q prefix

---

### Stage 3: Testing & Documentation

#### Integration Testing (Manual - with REAL LLM)

**Prerequisites:**
- Valid API key in environment variable (e.g., `ANTHROPIC_API_KEY`)
- Internet connection
- cl-llm-provider loaded

**Test Scenario 1: Basic LLM Flow**
1. Start SLY, load agent-q system
2. Verify provider configured: `agent-q:*provider-instance*` should be non-nil
3. Enable `sly-agent-q-mode`
4. Mark a defun, `C-c q c d` (add defun to context)
5. `C-c q s`, enter "Explain this function"
6. **Verify real LLM response** in conversation buffer
7. `C-c q i` to insert response at point

**Test Scenario 2: Context Accumulation with LLM**
1. Add region 1 to context (a function definition)
2. Add region 2 to context (another function)
3. `C-c q c s` to show context summary (should show "2 items")
4. Send message: "How do these two functions relate?"
5. **Verify LLM response references both functions**
6. Check conversation buffer shows context was included

**Test Scenario 3: Multi-turn Conversation**
1. Send: "Write a function to reverse a list"
2. **Verify LLM provides implementation**
3. Send: "Now make it tail-recursive"
4. **Verify LLM modifies based on previous context**
5. Verify conversation buffer shows full history

**Test Scenario 4: Error Handling**
1. Stop provider or use invalid API key
2. Send message
3. **Verify error message displayed** (not crash)
4. Restore API key
5. Send message again
6. **Verify recovery and normal operation**

**Test Scenario 5: Project Prompt**
1. Create `.agent-q/system-prompt.md` in project root
2. Add: "This project uses custom list utilities. Prefer recursive solutions."
3. New conversation
4. Send: "Write a list function"
5. **Verify LLM response follows project prompt**

**Test Scenario 6: Configuration**
1. Create `~/.config/agent-q/config.lisp`:
   ```lisp
   (in-package :agent-q)
   (configure :provider :anthropic
              :model "claude-sonnet-4-20250514")
   ```
2. Reload agent-q
3. Verify config applied
4. Send test message
5. **Verify correct model used** (check response quality)

#### Documentation Files

**README.md**
- Overview of Agent-Q
- **Prerequisites**: cl-llm-provider, API keys
- Installation instructions (Quicklisp local-projects)
- **Configuration guide** (API keys, providers)
- Basic usage guide
- Keybinding reference
- Troubleshooting section (API errors, missing keys)

**LICENSE**
- MIT License

---

## Critical Implementation Notes

### cl-llm-provider Integration

**Message Format:**
```lisp
;; cl-llm-provider expects list of plists
(list (:role "user" :content "Hello")
      (:role "assistant" :content "Hi there")
      (:role "user" :content "How are you?"))
```

**Provider Creation:**
```lisp
;; Auto-reads from env vars
(cl-llm-provider:make-provider :anthropic)

;; Explicit API key
(cl-llm-provider:make-provider :anthropic :api-key "sk-...")
```

**Complete Call:**
```lisp
(cl-llm-provider:complete messages
                          :provider *provider-instance*
                          :system "System prompt here"
                          :max-tokens 4096)
```

**Response Access:**
```lisp
(cl-llm-provider:response-content response)      ; Main text
(cl-llm-provider:response-message response)      ; Full message for history
(cl-llm-provider:response-usage response)        ; Token counts
(cl-llm-provider:response-finish-reason response) ; :stop, :length, etc.
```

### Error Handling Strategy

1. **CL Side:**
   - Wrap `complete` calls in `handler-case`
   - Catch all `cl-llm-provider` condition types
   - Return user-friendly error messages
   - Log errors for debugging

2. **Elisp Side:**
   - Check for error strings in response
   - Display in minibuffer
   - Log to `*Messages*`
   - Allow retry

### API Key Management

**Environment Variables (Recommended):**
```bash
export ANTHROPIC_API_KEY="sk-ant-..."
export OPENAI_API_KEY="sk-..."
```

**Config File Alternative:**
```lisp
;;; ~/.config/agent-q/config.lisp
(in-package :agent-q)
(configure :provider :anthropic
           :api-key (uiop:getenv "ANTHROPIC_API_KEY"))
```

### SLY RPC Mechanism

- Functions in `:agent-q` package are callable via `sly-eval`
- No special macros needed
- Return values must be serializable
- Errors propagate to Emacs as error messages

---

## Definition of Done (Checklist)

### CL Core
- [ ] All 7 CL files load without errors
- [ ] `(asdf:load-system :agent-q)` succeeds
- [ ] cl-llm-provider dependency loads
- [ ] All exported symbols accessible
- [ ] Config creates provider instance successfully
- [ ] `*provider-instance*` is valid cl-llm-provider object
- [ ] Context manager handles 50+ items (sliding window)
- [ ] Conversation tracks multi-turn history
- [ ] Prompts compose correctly
- [ ] **Agent makes REAL LLM calls via cl-llm-provider**
- [ ] **LLM responses are coherent and relevant**
- [ ] Error handling catches API errors gracefully
- [ ] SLY interface functions callable from REPL

### Elisp Integration
- [ ] Minor mode activates in `lisp-mode`
- [ ] All keybindings work
- [ ] Add region/buffer/defun to context works
- [ ] **Send message calls Lisp and returns REAL LLM response**
- [ ] Conversation buffer displays messages
- [ ] Insert response at point works
- [ ] Copy response to kill ring works
- [ ] Context summary displays correctly

### Documentation
- [ ] README with installation, configuration, and usage
- [ ] **API key setup instructions**
- [ ] LICENSE file present
- [ ] Inline docstrings on all exported symbols
- [ ] Comments explain non-obvious code

### Integration Tests (Manual)
- [ ] **Full flow with real LLM: mark code → send → get response → insert**
- [ ] Context accumulation works with LLM understanding context
- [ ] Multi-turn conversations maintain history
- [ ] New conversation clears state
- [ ] Project prompt influences LLM behavior
- [ ] Error handling works (invalid key, network error)
- [ ] Token usage tracking works (optional)

---

## Timeline Estimation

**CL Core:** 6-8 hours
- ASDF + package: 30 min
- Config + provider setup: 1.5 hours
- Context: 2 hours
- Conversation: 1.5 hours
- Prompts: 1 hour
- Agent + cl-llm-provider integration: 2 hours
- SLY interface: 1 hour

**Elisp Integration:** 4-6 hours
*(Same as before)*

**Testing & Docs:** 3-4 hours
- Manual testing with real LLM: 2 hours
- Documentation + API key setup: 1.5 hours
- Bug fixes: 30 min

**Total:** 13-18 hours (spread over 2-3 days)

---

## Next Steps (Immediate Actions)

1. **Verify cl-llm-provider is available:**
   ```lisp
   (ql:quickload "cl-llm-provider")
   ```

2. **Set up API key:**
   ```bash
   export ANTHROPIC_API_KEY="your-key-here"
   ```

3. **Create directory structure:**
   ```bash
   cd /Users/quasi/quasilabs/projects/agent-q
   mkdir -p src contrib/sly-agent-q
   ```

4. **Implement CL files in order 1-8:**
   - Test each file: `(load "src/package.lisp")` etc.
   - Test with real LLM calls after agent.lisp

5. **Implement Elisp file:**
   - Start with basic `sly-agent-q-send`
   - Test with real LLM immediately

6. **Integration test with real LLM:**
   - Run all test scenarios
   - Verify responses are coherent

7. **Document API key setup in README**

---

## Risk Mitigation

**Risk 1:** API rate limits
**Mitigation:** Catch rate-limit errors, display clear message, implement backoff if needed

**Risk 2:** API costs during testing
**Mitigation:** Use shorter test messages, consider Ollama for free testing, document costs

**Risk 3:** Network/API failures
**Mitigation:** Comprehensive error handling, clear error messages, retry capability

**Risk 4:** Context too large for token limits
**Mitigation:** Document max context size, add warning when approaching limits (Phase 2 feature)

---

This updated plan provides complete integration with **cl-llm-provider** for real LLM communication in Phase 1. The system will make actual API calls and provide genuine AI-powered assistance from the start.
