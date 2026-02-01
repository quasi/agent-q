# AGENT.md

## Project Context

**Name**: Agent-Q
**Language**: Common Lisp, Emacs Lisp
**Framework**: SLY (Superior Lisp Interaction Mode)
**Test Runner**: ERT (Emacs), FiveAM (Common Lisp - planned)
**Architecture**: Client-server (Emacs ↔ Common Lisp REPL via SLYNK)

## Project Purpose

Agent-Q is an AI-powered agentic extension for Common Lisp development. It integrates with SLY/Emacs to create an autonomous development partner that can introspect a running Lisp image, execute code, observe results, and iterate until tasks are complete.

**Key Differentiator**: REPL-aware autonomy. Unlike generic AI assistants, Agent-Q has direct access to a live Common Lisp environment through 18+ tools.

## Build Commands

### Common Lisp Side

```bash
# Load via Quicklisp
sbcl --eval "(ql:quickload :agent-q)"

# Run tests (planned - currently 0 CL tests)
sbcl --eval "(ql:quickload :agent-q)" \
     --eval "(asdf:test-system :agent-q)"
```

### Emacs Lisp Side

```bash
# Run test suite (165 tests)
cd contrib/sly-agent-q/test
emacs --batch -l run.el

# Run specific test file
emacs --batch -l run.el -l test-chat.el -f ert-run-tests-batch-and-exit
```

### Linting

```bash
# Common Lisp (manual review, no automated linter configured)
# Emacs Lisp (built-in byte-compile warnings)
emacs --batch -f batch-byte-compile contrib/sly-agent-q/*.el
```

## Code Conventions

### Common Lisp

**Naming:**
- Functions: `verb-noun` (e.g., `make-context-item`, `add-message`)
- Classes: `noun` (e.g., `context-manager`, `conversation`)
- Global variables: `*earmuffs*` (e.g., `*current-agent*`, `*session-manager*`)
- Constants: `+plus-signs+`

**Packages:**
- Core: `agent-q` (nickname: `aq`)
- Tools: `agent-q.tools` (nickname: `aq.tools`)
- Session: `agent-q.session`
- Cost: `agent-q.cost`

**Structure:**
- Use CLOS for data modeling
- Prefer keyword arguments for public APIs
- Use plists for unstructured metadata
- Docstrings required for exported symbols

**Error Handling:**
- Define typed conditions (e.g., `budget-exceeded-error`)
- Use `handler-case` for expected errors
- Let unexpected errors propagate to REPL

### Emacs Lisp

**Naming:**
- Functions: `sly-agent-q-verb-noun` (e.g., `sly-agent-q-send-message`)
- Variables: `sly-agent-q-noun` (e.g., `sly-agent-q-conversation-buffer`)
- Private functions: double dash (e.g., `sly-agent-q--internal-helper`)

**Structure:**
- Group related functions with comment headers
- Use lexical binding (all files have `;;; lexical-binding: t`)
- Prefer `cl-lib` over obsolete `cl`

**RPC Calls:**
```elisp
;; Pattern: (sly-eval '(package:function args))
(sly-eval '(agent-q:agent-q-send message))
```

## Architecture Rules

### RULE 1: Session-First Conversation Lookup

**Rule:** Always look up conversations via their parent session, never store conversations as standalone globals.

**Correct:**
```lisp
(defun get-current-conversation ()
  (when-let ((session (session-manager-current-session *session-manager*)))
    (session-conversation session)))
```

**Violation:**
```lisp
;; WRONG: Don't store conversations separately
(defvar *current-conversation* nil)
```

**Rationale:** See `canon/core/decisions/0001-session-conversation-unification.md`. This prevents message persistence bugs where session and conversation get out of sync.

---

### RULE 2: Streaming Text, Synchronous Tools

**Rule:** Stream text responses token-by-token. Fall back to synchronous execution for tool calls.

**Correct:**
```lisp
;; Text response: stream
(make-streaming-callback
  :on-chunk (lambda (chunk) (display-chunk chunk))
  :on-complete (lambda () (finalize-response)))

;; Tool call: synchronous
(execute-tool-synchronously tool-call)
```

**Violation:**
```lisp
;; WRONG: Don't stream tool calls
(stream-tool-execution tool-call) ; Too complex, unreliable
```

**Rationale:** See `canon/core/decisions/0002-streaming-tool-fallback.md`. Tool calls are state-changing and require reliable execution order.

---

### RULE 3: Elisp-First Testing

**Rule:** Write tests on Emacs Lisp side first. Common Lisp side testing is deferred to Phase 4.

**Current State:**
- Elisp: 165 tests, 100% passing (ERT framework)
- Common Lisp: 64 filesystem tests only

**Rationale:** See `canon/core/decisions/0003-elisp-first-testing.md`. UI behavior is testable in Elisp; CL business logic is verified via integration testing.

---

### RULE 4: Fail-Open Cost Estimation

**Rule:** Cost estimation should warn but not block if pricing data is unavailable.

**Correct:**
```lisp
(handler-case
    (check-budget-before-call request)
  (pricing-unavailable (e)
    (warn "Cost estimation unavailable: ~A" e)
    t)) ; Allow request
```

**Violation:**
```lisp
;; WRONG: Don't hard-fail on missing pricing
(unless (has-pricing-data model)
  (error "Cannot proceed without cost estimate"))
```

**Rationale:** See `canon/core/decisions/0004-fail-open-cost-estimation.md`. Availability > perfect cost tracking.

---

### RULE 5: Phased Chat Development

**Rule:** Conversation features are built incrementally across phases. Don't couple unrelated chat features.

**Phase 1:** Context + basic conversation
**Phase 2:** Tool integration
**Phase 3:** Sessions + streaming + completion
**Phase 4:** Advanced features (semantic search, profiling)

**Rationale:** See `canon/core/decisions/0005-phased-chat-development.md`.

---

### RULE 6: Partial Feature Stabilization

**Rule:** Features can be marked "stable" even if not all contracts are implemented.

**Example:** `file-system-tools` is stable with 6/10 contracts implemented. Remaining 4 are planned but not blocking.

**Rationale:** See `canon/core/decisions/0006-partial-feature-stabilization.md`. Allows production use of working subset.

## File Locations

| Type | Location |
|------|----------|
| Core CL source | `src/` |
| Tool implementations | `src/tools/` |
| Emacs extension | `contrib/sly-agent-q/` |
| Tests (Elisp) | `contrib/sly-agent-q/test/` |
| Tests (CL, planned) | `tests/` |
| Specifications | `canon/` |
| Plans | `docs/plans/` |
| User documentation | `docs/` |
| Design decisions | `canon/core/decisions/` |

## Invariants

### Context Management

**INV-1:** Context manager must never exceed 50 items. Oldest item is evicted when capacity is reached (FIFO).

**Verification:**
```lisp
(assert (<= (length (context-manager-items ctx-mgr)) 50))
```

**INV-2:** Context item IDs are monotonic and unique: `ctx-1`, `ctx-2`, ..., `ctx-N`.

**Verification:**
```lisp
(assert (string= (context-item-id item) (format nil "ctx-~D" N)))
```

---

### Conversation

**INV-3:** Messages are append-only. No removal or editing after creation.

**Verification:**
```lisp
;; After adding message:
(assert (member new-message (conversation-messages conv)))
;; Message list only grows:
(assert (>= (length (conversation-messages conv)) previous-length))
```

---

### Session

**INV-4:** Session IDs are immutable after generation. Format: `session-YYYYMMDD-HHMMSS-XXXX`.

**Verification:**
```lisp
(assert (string-match-p "^session-[0-9]{8}-[0-9]{6}-[0-9A-F]{4}$"
                        (session-id session)))
```

**INV-5:** Only one session is current at any time.

**Verification:**
```lisp
(assert (or (null (session-manager-current-session *session-manager*))
            (typep (session-manager-current-session *session-manager*) 'session)))
```

---

### Tool System

**INV-6:** Tool names are unique in the registry.

**Verification:**
```lisp
(assert (= (hash-table-count *agent-q-registry*)
           (length (get-agent-q-tools))))
```

**INV-7:** Tools with `:dangerous` safety level must never auto-execute.

**Verification:**
```lisp
(assert (not (auto-execute-p tool)))
; where (eq (tool-safety-level tool) :dangerous)
```

---

### Diff System

**INV-8:** Applied hunks cannot be unapplied. File changes are permanent until manual undo.

**Verification:**
```elisp
;; After applying hunk:
(assert (eq (sly-agent-q-diff--hunk-state pos) 'applied))
;; Toggling to pending doesn't undo file changes
```

---

### Budget

**INV-9:** Budget checks occur before LLM API call, not after.

**Verification:**
```lisp
(assert (check-budget-before-call request))
(call-llm request) ; Only reached if budget check passes
```

## Common Patterns

### Adding a New Tool

```lisp
;; In src/tools/your-category.lisp
(define-tool
  "tool_name"
  "Description for LLM (be specific about behavior)"
  '((:name "param1" :type "string" :description "What this param does")
    (:name "param2" :type "integer" :description "What this param does"))
  :required '("param1")
  :safety-level :safe ; or :cautious, :moderate, :dangerous
  :categories '(:your-category)
  :handler (lambda (args)
             (let ((param1 (getf args :param1))
                   (param2 (getf args :param2 0))) ; Optional with default
               ;; Implementation
               (list :success t :content "Result"))))
```

### Adding RPC Endpoint (Emacs ← CL)

```lisp
;; In src/session.lisp (or relevant file)
(defun agent-q-your-function (arg1 arg2)
  "Docstring describing what this does."
  (do-something arg1 arg2))

;; In contrib/sly-agent-q/sly-agent-q.el
(defun sly-agent-q-your-function (arg1 arg2)
  "Call agent-q-your-function via RPC."
  (sly-eval `(agent-q:agent-q-your-function ,arg1 ,arg2)))
```

### Adding Context to Chat

```elisp
;; From Emacs
(sly-agent-q-add-defun-to-context) ; Adds current top-level form
(sly-agent-q-send-with-context "Your instruction")

;; From CL (programmatic)
(add-context *context-manager*
             (make-context-item :type :code
                               :content "(defun ...)"
                               :metadata '(:filename "src/foo.lisp")))
```

## Testing Strategy

### Elisp Tests (Current)

**Coverage:** Chat UI, context management, diff review, sessions, RPC calls

**How to Add:**
```elisp
;; In contrib/sly-agent-q/test/test-your-feature.el
(ert-deftest test-your-feature ()
  "Test description."
  (should (equal expected-value (your-function args)))
  (should-error (your-function-invalid-args)))
```

### CL Tests (Future - Phase 4)

**Target Coverage:** Tool system, agent loop, LLM integration

**Planned Framework:** FiveAM

## Navigation

For detailed Canon navigation and feature specifications, see `canon/INDEX.md`.

## Dependencies

### External Libraries

- **cl-llm-provider** (Critical): Multi-provider LLM communication
  - Path: `/Users/quasi/quasilabs/projects/cl-llm-provider/`
  - Used for: Anthropic, OpenAI, Ollama support

- **SLY** (Required): Emacs integration and SLYNK protocol
  - Install: MELPA package `sly`

- **closer-mop** (Required): Portable MOP for introspection tools
  - Install: Quicklisp

### Internal Dependencies

```
tool-system ← All tool categories
diff-approval ← file-system-tools
session-management ← conversation ← context-management
```

## Known Gaps

### High Priority

1. **Common Lisp test coverage**: 0 unit tests for core logic (agent, conversation, tools)
2. **Session management spec**: 8 RPC endpoints implemented but not formally specified
3. **Streaming specification**: Code exists (`src/streaming.lisp`) but needs formal spec

### Medium Priority

4. **Tool system tests**: 18 tools, 0 CL-side tests
5. **Observability formalization**: Code-only, needs elevation to Canon
6. **Cost estimation spec**: Implementation complete, needs specification

### Low Priority

7. **`:debug` message role**: Exists in code but undocumented
8. **`:custom` context type**: Extensibility point, usage unclear
9. **Concurrency model**: Single-threaded, undocumented

## Quick Start for Contributors

1. **Read vocabulary**: `canon/core/foundation/vocabulary.md` (17 terms, 15 minutes)
2. **Understand architecture**: `canon/core/decisions/*.md` (6 ADRs, 30 minutes)
3. **Find a feature**: `canon/features/` (browse by confidence score)
4. **Check tests**: Run Elisp suite to verify environment
5. **Make changes**: Follow patterns above, update Canon with `canon-evolve`

## Development Workflow

```bash
# 1. Load Agent-Q
sbcl --eval "(ql:quickload :agent-q)"

# 2. Make changes to src/

# 3. Reload
sbcl --eval "(ql:quickload :agent-q :force t)"

# 4. Test (Elisp side)
cd contrib/sly-agent-q/test && emacs --batch -l run.el

# 5. Update Canon (use canon-evolve skill in Claude Code)

# 6. Commit
git add .
git commit -m "feat: your change"
```

## Communication Channels

- **Issues**: GitHub issue tracker
- **Specs**: `canon/` directory (single source of truth)
- **Rationale**: `canon/core/decisions/` (ADRs)

---

**Last Updated:** 2026-01-22
**Canon Version:** 0.5.0
**Status:** Active development (Phase 3 stable, Phase 4 planned)
