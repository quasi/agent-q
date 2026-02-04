# Agent-Q Core Vocabulary

**Source:** Triangulated from code, docs, and tests
**Confidence:** High (0.92 average)
**Last Updated:** 2026-01-17

---

## Domain Model

```
┌─────────────┐
│   Session   │──────┐
└─────────────┘      │
                     │ contains
                     ▼
            ┌──────────────┐
            │ Conversation │
            └──────────────┘
                     │
                     ├── has ─────────┐
                     │                ▼
                     │       ┌────────────────┐
                     │       │ Context Manager│
                     │       └────────────────┘
                     │                │
                     │                │ contains
                     │                ▼
                     │       ┌─────────────┐
                     │       │Context Items│
                     │       └─────────────┘
                     │
                     └── contains ────┐
                                      ▼
                             ┌────────────┐
                             │  Messages  │
                             └────────────┘
```

---

## Core Entities

### Agent
**Type:** Orchestrator
**Source:** specs/PHASE-1-SPEC.md:208, src/agent.lisp
**Confidence:** 0.95

The core orchestrator that manages LLM communication, context accumulation, and conversation flow. One agent exists per Lisp image (global singleton pattern).

**Properties:**
- Has a conversation
- Has a streaming callback (optional)
- Communicates with LLM provider
- Executes tool calls

**Invariants:**
- `*current-agent*` is global singleton
- Not thread-safe
- Must be initialized before use

---

### Context Item
**Type:** Data Structure
**Source:** specs/PHASE-1-SPEC.md:133, src/context.lisp:14
**Confidence:** 1.00

A piece of code or text with metadata, tracked for inclusion in LLM prompts.

**Structure:**
```lisp
(defclass context-item ()
  ((id :type string)                    ; "ctx-N" (monotonic)
   (item-type :type (member ...))       ; One of 6 types
   (content :type string)               ; The actual content
   (metadata :type list)                ; Plist (unstructured)
   (timestamp :type universal-time)))   ; Creation time (immutable)
```

**Types (member set):**
- `:code` - Code snippet
- `:text` - Plain text
- `:file` - File contents
- `:repl-history` - REPL interaction
- `:error` - Error output
- `:custom` - Extensibility point (undocumented)

**Metadata Keys (common, not enforced):**
- `:filename` - Source file path
- `:start-line` - Beginning line number
- `:end-line` - Ending line number
- `:package` - Lisp package context

**Invariants:**
- `id` is unique and monotonic (ctx-1, ctx-2, ...)
- `item-type` must be from fixed set
- `content` must be string
- `timestamp` is immutable after creation

**Confidence:** 1.00 (verified by tests + code)

---

### Context Manager
**Type:** Data Structure
**Source:** specs/PHASE-1-SPEC.md:153, src/context.lisp:43
**Confidence:** 1.00

Manages a sliding window of context items (max 50 items). When capacity is exceeded, oldest item is removed (FIFO).

**Structure:**
```lisp
(defclass context-manager ()
  ((items :type adjustable-vector)     ; Vector of context-items
   (max-items :initform 50)))          ; Capacity (configurable)
```

**Operations:**
- `add-context` - Add item (removes oldest if full)
- `clear-context` - Remove all items
- `get-context` - Retrieve items (optionally filtered)
- `context-to-string` - Format as markdown for LLM

**Invariants:**
- Maximum 50 items (sliding window)
- Items never removed individually (only via FIFO or clear)
- Items ordered chronologically (oldest first)

**Confidence:** 1.00 (verified by docs + tests)

---

### Message
**Type:** Data Structure
**Source:** specs/PHASE-1-SPEC.md:172, src/conversation.lisp:8
**Confidence:** 0.95

Single turn in a conversation with role, content, and timestamp.

**Structure:**
```lisp
(defclass message ()
  ((role :type (member ...))           ; One of 4 roles
   (content :type string)              ; Message text
   (timestamp :type universal-time)))  ; Creation time (immutable)
```

**Roles (member set):**
- `:system` - System/instruction message
- `:user` - User input
- `:assistant` - LLM response
- `:debug` - Debug/logging (⚠️ undocumented)

**Invariants:**
- `role` must be from fixed set
- `content` must be string
- `timestamp` is immutable after creation

**Notes:**
- `:debug` role exists in code but not in PHASE-1-SPEC
- Likely used for internal logging/debugging
- No tests explicitly verify :debug usage

**Confidence:** 0.95 (`:debug` role undocumented)

---

### Conversation
**Type:** Data Structure
**Source:** specs/PHASE-1-SPEC.md:183, src/conversation.lisp:25
**Confidence:** 0.95

Sequence of messages with associated context manager. Container for multi-turn dialogue.

**Structure:**
```lisp
(defclass conversation ()
  ((id :type string)                   ; "conv-N" (monotonic)
   (messages :type list)               ; List of message objects
   (context-manager :type context-manager)
   (created-at :type universal-time)
   (project :type string)))            ; Optional project ID
```

**Operations:**
- `add-message` - Append message (append-only)
- `get-messages` - Retrieve messages (optionally limited)
- `clear-conversation` - Remove all messages

**Invariants:**
- Messages are append-only (no removal or editing)
- `id` is unique and monotonic
- Each conversation has its own context-manager

**Confidence:** 0.95

---

### Session
**Type:** Data Structure
**Source:** src/session.lisp:13, CLAUDE.md:393 (⚠️ not in PHASE-1-SPEC)
**Confidence:** 0.90

Persistent conversation with metadata, stored in SQLite. Wrapper around conversation for persistence across Emacs restarts.

**Structure:**
```lisp
(defclass session ()
  ((id :type string)                   ; "session-YYYYMMDD-HHMMSS-XXXX"
   (name :type string)                 ; User-friendly name
   (created-at :type universal-time)   ; Immutable
   (updated-at :type universal-time)   ; Updates on message add
   (conversation :type conversation)
   (model :type string)                ; e.g., "claude-sonnet-4-20250514"
   (metadata :type list)))             ; :total-input-tokens, :total-output-tokens
```

**ID Format:**
- Pattern: `session-YYYYMMDD-HHMMSS-XXXX`
- Example: `session-20260117-120000-A3F2`
- Components: ISO date + 24-hour time + 4-digit hex random

**Operations (RPC endpoints):**
- `agent-q-create-session` - Create new session
- `agent-q-switch-session` - Switch to existing session
- `agent-q-save-session` - Persist to disk
- `agent-q-delete-session` - Remove from disk and cache
- `agent-q-rename-session` - Update session name
- `agent-q-list-sessions` - List all sessions
- `agent-q-search-sessions` - Search by content
- `agent-q-get-session-info` - Get current session metadata

**Invariants:**
- `id` is immutable after generation
- `created-at` is immutable
- `updated-at` changes on message addition
- Sessions persist across Emacs restarts (SQLite)
- Session switching auto-saves previous session

**Confidence:** 0.90 (implemented but not formally specified)

---

### Session Manager
**Type:** Service
**Source:** src/session.lisp:80
**Confidence:** 0.85

Global singleton managing session lifecycle, caching, and persistence.

**Structure:**
```lisp
(defclass session-manager ()
  ((sessions-directory :type pathname)  ; ~/.agent-q/sessions/
   (current-session :type session)     ; Active session
   (session-cache :type hash-table)))  ; Loaded sessions
```

**Responsibilities:**
- CRUD operations on sessions
- Auto-save with 60-second interval (configurable)
- Session caching for performance
- Current session tracking

**Invariants:**
- `*session-manager*` is global singleton
- Only one session is current at a time
- Cache prevents duplicate loading from disk

**Confidence:** 0.85

---

## Tool System Vocabulary

### Tool
**Type:** Executable Capability
**Source:** specs/PHASE-2-SPEC.md:110, src/tools/registry.lisp
**Confidence:** 0.90

An executable capability with name, description, parameters, handler function, and trust level.

**Structure (via `define-tool`):**
```lisp
(define-tool
  "tool_name"              ; Snake_case name for LLM
  "Description..."         ; Human-readable description
  parameters               ; List of (:name :type :description) plists
  :required '("param1")    ; Required parameter names
  :safety-level :safe      ; Trust level
  :categories '(:introspection)
  :handler (lambda (args) ...))
```

**Safety Levels:**
- `:safe` - Auto-execute (no approval needed)
- `:cautious` - Log but auto-execute
- `:moderate` - May require approval
- `:dangerous` - Always requires approval

**Categories:**
- `:introspection` - Read-only inspection (9 tools)
- `:execution` - Code execution (4 tools)
- `:buffer` - File/buffer manipulation (4 tools)
- `:diff` - Diff generation and approval (1 tool)

**Confidence:** 0.90

---

### Tool Registry
**Type:** Service
**Source:** specs/PHASE-2-SPEC.md:167, src/tools/registry.lisp
**Confidence:** 0.90

Hash table storing all available tools, keyed by name. Provides discovery and lookup.

**Global Instance:**
- `*agent-q-registry*` - Singleton registry
- Populated at load time via `define-tool` macro

**Operations:**
- `register-tool` - Add tool to registry
- `get-tool` - Lookup by name
- `list-tools` - Get all tools (optionally filtered by category/safety)

**Confidence:** 0.90

---

### Tool Result
**Type:** Data Structure
**Source:** specs/PHASE-2-SPEC.md:140
**Confidence:** 0.85

Result of tool execution, returned to LLM for next iteration.

**Structure:**
```lisp
(list :id "call-id"
      :success t/nil
      :content "result text"
      :error "error message")  ; Optional, if success=nil
```

**Confidence:** 0.85

---

## Diff System Vocabulary

### Hunk
**Type:** Data Unit
**Source:** docs/DIFF-IMPLEMENTATION.AGENT.md:14
**Confidence:** 1.00

Contiguous block of changes in unified diff format, delimited by `@@` header lines.

**Format:**
```diff
@@ -10,3 +10,4 @@
 context line
+added line
-removed line
 context line
```

**Confidence:** 1.00 (precisely specified + tested)

---

### Hunk State
**Type:** Enum
**Source:** docs/DIFF-IMPLEMENTATION.AGENT.md:14
**Confidence:** 1.00

State of a hunk in the review process.

**Values:**
- `nil` (pending) - Not yet reviewed
- `'applied` - Applied to target file
- `'rejected` - Rejected by user

**State Machine:**
- `nil → applied` (press 'a', executes diff-apply-hunk)
- `nil → rejected` (press 'r', no file change)
- `applied → nil` (press SPC, visual only—file remains modified)
- `rejected → applied` (press SPC, attempts diff-apply-hunk)

**Invariant:**
- Applied hunks cannot be unapplied (file changes are permanent)

**Confidence:** 1.00 (verified by tests)

---

### Diff Buffer
**Type:** UI Component
**Source:** docs/DIFF-IMPLEMENTATION.AGENT.md:16
**Confidence:** 0.95

Temporary buffer displaying unified diff in `sly-agent-q-diff-mode` (derived from `diff-mode`).

**Buffer-Local Variables:**
- `sly-agent-q-diff--path` - Absolute path to target file
- `sly-agent-q-diff--original` - Expected original content
- `sly-agent-q-diff--modified` - Proposed new content
- `sly-agent-q-diff--description` - Change explanation
- `sly-agent-q-diff--decision` - "accepted" or "rejected"
- `sly-agent-q-diff--hunk-states` - Alist mapping position → state

**Confidence:** 0.95

---

### Recursive Edit
**Type:** Emacs Mechanism
**Source:** docs/DIFF-IMPLEMENTATION.AGENT.md:19
**Confidence:** 0.95

Emacs mechanism that blocks execution until user exits with a decision. Used to make diff approval synchronous from CL perspective.

**Flow:**
1. CL calls `(eval-in-emacs '(sly-agent-q-show-diff-and-wait ...))`
2. Emacs displays diff buffer
3. Emacs enters `recursive-edit` (BLOCKS)
4. User reviews hunks, presses 'q'
5. Emacs exits `recursive-edit`, returns decision string
6. CL receives "accepted" or "rejected"

**Confidence:** 0.95

---

## Context Management UI Vocabulary

### @-Mention
**Type:** UI Pattern
**Source:** docs/guides/context-management.md:9
**Confidence:** 0.95

Inline completion for attaching files, symbols, or buffers to chat messages using `@` prefix.

**Trigger:** Type `@` followed by 2+ characters (configurable)

**Completion Types:**
- Files: `@src/agent.lisp`
- Symbols: `@make-context-item`
- Buffers: `@*scratch*`

**Confidence:** 0.95 (99 tests verify)

---

### Context Pill
**Type:** UI Component
**Source:** docs/guides/context-management.md:16, README.md:27
**Confidence:** 0.95

Visual indicator `[@name]` for attached context items, rendered with text properties.

**Properties:**
- Clickable (visit source on click)
- Removable (DEL key bypasses read-only)
- Displays name and type
- Stores full data in text properties

**Confidence:** 0.95 (tests verify rendering + interaction)

---

### Context Panel
**Type:** UI Component
**Source:** README.md:28, docs/guides/context-management.md
**Confidence:** 0.90

Sidebar showing all attached context items with type, size, and actions.

**Displays:**
- Item type (file, symbol, buffer)
- Item size (bytes)
- Actions (view, remove)

**Confidence:** 0.90 (tests verify, but less comprehensive than pills)

---

## Streaming Vocabulary

### Streaming Callback
**Type:** Function
**Source:** src/streaming.lisp, specs/plans/2026-01-13-streaming-observability-upgrade.md
**Confidence:** 0.75

Closure that handles streaming LLM response chunks in real-time.

**Events:**
- `:chunk` - Text chunk arrived
- `:complete` - Response finished
- `:error` - Error occurred

**Hybrid Approach:**
- Text responses stream (token-by-token)
- Tool calls fall back to synchronous (reliability)

**Confidence:** 0.75 (code exists, needs formal spec)

---

## Cost Vocabulary

### Budget
**Type:** Constraint
**Source:** src/cost.lisp:70
**Confidence:** 0.90

Maximum allowed cost in USD for a single request. Default: $0.10

**Behavior:**
- Pre-flight check before LLM call
- Signals `budget-exceeded-error` if exceeded
- Fail-open: Returns T with warning if estimation unavailable

**Confidence:** 0.90

---

### Budget Exceeded Error
**Type:** Condition
**Source:** src/cost.lisp:14
**Confidence:** 1.00

Condition signaled when estimated request cost exceeds budget.

**Slots:**
- `estimated-cost` - Estimated cost in USD
- `budget` - Budget limit in USD

**Confidence:** 1.00 (defined in code)

---

## Undocumented Terms

### :debug Message Role
**Source:** src/conversation.lisp:10
**Confidence:** 1.00 (exists in code)
**Documentation:** ❌ Not in PHASE-1-SPEC.md

Likely used for internal logging or debugging messages. Not tested explicitly.

**Recommendation:** Document intended use or remove if unused.

---

### :custom Context Type
**Source:** src/context.lisp:20
**Confidence:** 1.00 (exists in code)
**Documentation:** ⚠️ Mentioned but not explained in PHASE-1-SPEC.md:16

Extensibility point for future context types beyond the 5 standard types.

**Recommendation:** Document intended use cases.

---

## Platform Dependencies

### Emacs
**Type:** Platform
**Confidence:** 1.00

The extensible text editor hosting Agent-Q's UI layer. All user interaction flows through Emacs buffers, modes, and keybindings. Agent-Q operates as a minor mode within Emacs.

---

### Elisp (Emacs Lisp)
**Type:** Language
**Confidence:** 1.00

The scripting language of Emacs. Agent-Q's frontend — chat interface, diff review, context completion, session management UI — is implemented in Elisp within the `sly-agent-q` contrib package.

---

### Common Lisp
**Type:** Language
**Confidence:** 1.00

Agent-Q's backend implementation language. The agent, tools, sessions, and context management run in a Common Lisp image connected to Emacs via SLY.

---

### SLY
**Type:** Framework
**Confidence:** 1.00

Superior Lisp Interaction Mode for Emacs. Provides the RPC bridge (`eval-in-emacs`, `defslyfun`) between Agent-Q's Common Lisp backend and Emacs frontend.

---

### Claude
**Type:** External Service
**Confidence:** 1.00

Anthropic's large language model. The primary AI model Agent-Q communicates with via `cl-llm-provider` for instruction processing, code generation, and tool orchestration.

---

### ASDF
**Type:** Build System
**Confidence:** 1.00

Another System Definition Facility. The Common Lisp build system used to define and load Agent-Q as a system (`agent-q.asd`). Also used for project root auto-detection.

---

## Feature Lifecycle

### Phase
**Type:** Development Stage
**Confidence:** 1.00

A numbered development stage (1-4) in Agent-Q's incremental build plan:
- **Phase 1:** Foundation (context management, conversation, chat interface)
- **Phase 2:** Tool System (18 tools, diff approval, file system tools)
- **Phase 3:** Autonomous Developer (sessions, context completion, knowledge base)
- **Phase 4:** Intelligent Partner (condition handling, test generation, project memory)

---

### Feature Status
**Type:** Lifecycle Enum
**Confidence:** 1.00

Maturity indicator for features and contracts. One of:
- `draft` — Specification in early development, may change substantially
- `planned` — Designed but not yet implemented
- `specified` — Formal specification exists, implementation may be incomplete
- `implemented` — Code exists, may not be formally specified or fully tested
- `verified` — Implemented with passing tests confirming the specification
- `stable` — Specification complete, implementation matches, API not expected to change

---

## Execution Architecture

### Executor
**Type:** Component
**Source:** src/tools/registry.lisp:execute-tool-calls
**Confidence:** 0.85

The component within `execute-tool-calls` that dispatches tool invocations, enforces safety levels via the approval handler, formats results, and catches errors. Processes a list of tool-call plists from LLM responses.

---

### Approval Handler
**Type:** Protocol
**Source:** src/tools/registry.lisp
**Confidence:** 0.85

A function bound to `*approval-handler*` that is invoked when a dangerous tool (safety-level `:dangerous`) requires user confirmation before execution. Returns one of:
- `:approved` — Execute with original arguments
- `:denied` — Cancel execution, return error to LLM
- `(:modified new-args)` — Execute with modified arguments

---

### Introspection
**Type:** Tool Category
**Source:** specs/PHASE-2-SPEC.md
**Confidence:** 0.90

Tool category for read-only tools that inspect the Lisp image without modifying state. Safety level `:safe` (auto-execute, no approval needed). Includes: describe-symbol, apropos-search, function-arglist, list-package-symbols, etc. (9 tools).

---

### eval-in-emacs
**Type:** RPC Mechanism
**Source:** src/tools/filesystem.lisp, SLY protocol
**Confidence:** 0.95

RPC mechanism for delegating operations from the Common Lisp image to Emacs. Used by file system tools (directory listing, file operations), diff approval (showing diff buffer), and chat interface (streaming display). All Emacs-side operations are mediated through this mechanism.

---

### Emacs Key Notation
**Type:** Convention
**Confidence:** 1.00

Standard Emacs notation for keybindings used throughout Agent-Q's documentation:
- `SPC` — Space key (toggle hunk state in diff approval)
- `RET` — Return/Enter key (send message in chat)
- `C-c C-c` — Accept all hunks in diff approval
- `C-c C-k` — Reject all hunks in diff approval
- `C-c C-q` — Agent-Q prefix key (reserved for minor modes)

---

### Error Condition
**Type:** Pattern
**Confidence:** 1.00

A named error type in the Common Lisp condition system, signaled when an operation fails. Agent-Q defines domain-specific conditions (e.g., `BUDGET-EXCEEDED-ERROR`, `NO-PROJECT-ROOT-ERROR`) documented in their respective contracts. Error condition names follow ALL-CAPS-WITH-HYPHENS convention.

---

## Vocabulary Statistics

| Category | Terms | Convergent | Code-Only | Confidence |
|----------|-------|------------|-----------|------------|
| Core | 7 | 6 | 1 | 0.94 |
| Tools | 3 | 3 | 0 | 0.88 |
| Diff | 4 | 4 | 0 | 0.98 |
| Context UI | 3 | 3 | 0 | 0.93 |
| Streaming | 1 | 0 | 1 | 0.75 |
| Cost | 2 | 2 | 0 | 0.95 |
| Undocumented | 2 | 0 | 2 | 1.00 |
| Platform | 6 | 6 | 0 | 1.00 |
| Lifecycle | 2 | 2 | 0 | 1.00 |
| Architecture | 6 | 5 | 1 | 0.93 |
| **Total** | **31** | **28** | **3** | **0.95** |

---

**Glossary Completeness:** High
**Source Fidelity:** 93% convergent
**Next Review:** When adding Phase 4 features
