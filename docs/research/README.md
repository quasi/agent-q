# Agent-Q: AI-Powered Agentic Extensions for Common Lisp Development

## Vision

Agent-Q transforms the classic Lisp development experience into an AI-augmented powerhouse. By deeply integrating with SLY/Emacs and the running Lisp image, Agent-Q creates a development partner that can:

- **Introspect** the live image to understand code semantically
- **Execute** code in the REPL and observe results
- **Iterate** autonomously until tasks are complete
- **Learn** from interactions and build persistent knowledge

This is not just "Copilot for Lisp" - it's an agent that leverages Lisp's unique introspection capabilities to truly understand and work with your code.

## Implementation Status

**Current State (January 2026):**
- ✅ **Phase 1 Complete** - Foundation with full LLM integration and Emacs UI
- 🔄 **Phase 2 In Progress** - Tool system functional with introspection, execution, and diff approval
- 🔄 **Phase 3 Partial** - Session persistence implemented; condition system and knowledge base pending
- ⏳ **Phase 4 Planned** - Semantic analysis and refactoring capabilities

**Key Achievements:**
- Comprehensive chat interface with markdown rendering and streaming
- Session management with SQLite-backed persistence (moved to CL from Elisp)
- Full tool system with safety levels and approval workflows
- Complete Elisp test suite (chat, sessions, diff approval)

**See:** `PHASE1-IMPLEMENTATION-SUMMARY.md` for detailed Phase 1 completion report

---

## The Superpower

The closed loop:

```
LLM generates code → Agent evaluates in REPL → Agent observes result → Agent iterates
```

Unlike other language environments where AI assistants can only suggest code and hope it works, Agent-Q can:

1. Propose a solution
2. Actually run it
3. See what happened
4. Fix issues and try again

Combined with Lisp's introspection (describe, apropos, who-calls, etc.), the agent isn't guessing - it's *asking* the image what's true.

---

## Architecture Overview

```
┌─────────────────────────────────────┐
│              EMACS                  │
│  ┌─────────────────────────────┐    │
│  │      sly-agent-q.el         │    │  User Interface
│  │  - Context management       │    │  - Add regions to context
│  │  - Conversation UI          │    │  - Send instructions
│  │  - Buffer tools             │    │  - View responses
│  └──────────────┬──────────────┘    │
└─────────────────┼───────────────────┘
                  │ SLY Protocol
┌─────────────────┼───────────────────┐
│                 ▼                   │
│           LISP IMAGE                │
│  ┌─────────────────────────────┐    │
│  │         agent-q             │    │  The Brain
│  │                             │    │
│  │  Context Manager            │    │  - Accumulate context
│  │  Conversation Manager       │    │  - Track history
│  │  Tool System                │    │  - Execute tools
│  │  Agent Loop                 │    │  - Drive iterations
│  │  Knowledge Base (SQLite)    │    │  - Persist learnings
│  │                             │    │
│  │         ┌───────────────┐   │    │
│  │         │cl-llm-provider│   │    │  LLM Communication
│  │         └───────┬───────┘   │    │
│  └─────────────────┼───────────┘    │
└────────────────────┼────────────────┘
                     │
                     ▼
              ┌─────────────┐
              │ LLM (Claude)│
              └─────────────┘
```

---

## Phase Summary

### Phase 1: Foundation (MVP) ✅ COMPLETE
- ✅ Basic elisp UI for context and conversation
- ✅ Core CL infrastructure (context, conversation, agent)
- ✅ Integration with cl-llm-provider
- ✅ Simple prompt → response flow
- **Outcome:** Working assistant that can help with simple coding tasks
- **Completed:** December 31, 2025

### Phase 2: REPL-Aware Agent 🔄 IN PROGRESS
- ✅ Tool system infrastructure (registry, protocol)
- ✅ Introspection tools (describe, apropos, who-calls, etc.)
- ✅ Execution tools (eval, compile, macroexpand)
- ✅ Buffer tools (read, write, search)
- ✅ Diff approval workflow
- ⏳ Autonomous tool use loop (in development)
- **Outcome:** Agent that understands and can modify the running image

### Phase 3: Autonomous Developer 🔄 PARTIAL
- ✅ Session management with SQLite persistence
- ✅ Rich markdown rendering in chat interface
- ✅ Streaming response display
- ⏳ Condition system integration (debugger awareness)
- ⏳ Testing framework integration (FiveAM, Parachute)
- ⏳ Tracing capabilities
- ⏳ Persistent knowledge base
- ⏳ Context summarization
- **Outcome:** Agent that handles complex multi-step tasks

### Phase 4: Intelligent Partner ⏳ PLANNED
- ⏳ Semantic index of codebase
- ⏳ Profiling integration
- ⏳ Safe multi-file refactoring
- ⏳ Pattern detection and suggestions
- ⏳ ASDF/project awareness
- **Outcome:** Deep codebase understanding and architectural insights

---

## Tool Categories

| Category | Phase | Examples |
|----------|-------|----------|
| **Buffer** | 1-2 | read-file, write-to-buffer, search-in-buffer |
| **Introspection** | 2 | describe-symbol, who-calls, class-hierarchy |
| **Execution** | 2 | eval-form, compile-form, get-last-error |
| **Debugging** | 3 | list-restarts, invoke-restart, get-backtrace |
| **Testing** | 3 | run-test, run-test-suite, list-tests |
| **Tracing** | 3 | trace-function, get-trace-output |
| **Knowledge** | 3 | search-knowledge-base, store-in-knowledge-base |
| **Semantic** | 4 | query-dependencies, rebuild-index |
| **Profiling** | 4 | profile-form, find-hot-spots |
| **Refactoring** | 4 | rename-symbol, extract-function |
| **Patterns** | 4 | find-code-duplication, detect-code-smells |
| **Project** | 4 | analyze-asdf-system, get-load-order |

---

## Key Dependencies

- **cl-llm-provider**: LLM API communication (your library)
- **closer-mop**: Portable MOP for introspection
- **cl-sqlite**: Knowledge base persistence
- **FiveAM/Parachute**: Test framework integration (optional)

---

## Configuration

```lisp
;; ~/.config/agent-q/config.lisp
(in-package #:agent-q)

(configure :provider :anthropic
           :model "claude-sonnet-4-20250514"
           :api-key (uiop:getenv "ANTHROPIC_API_KEY"))
```

Project-specific prompts:
```
project-root/.agent-q/system-prompt.md
```

---

## Keybindings (Default)

| Key | Command |
|-----|---------|
| `C-c q c r` | Add region to context |
| `C-c q c d` | Add defun to context |
| `C-c q c c` | Clear context |
| `C-c q s` | Send message to agent |
| `C-c q r` | Send region with instruction |
| `C-c q i` | Insert last response |
| `C-c q v` | Show conversation |
| `C-c q n` | New conversation |

---

## Example Workflows

### Simple Documentation
```
1. C-c q c d    (add current function to context)
2. C-c q s      "Document this function with examples"
3. C-c q i      (insert docstring)
```

### Debug and Fix
```
1. Error occurs in REPL
2. C-c q s      "There's a bug - help me fix it"
3. Agent uses: get-last-error, describe-symbol, who-calls
4. Agent proposes fix
5. Agent uses: eval-form to test
6. Agent iterates until working
7. C-c q i      (insert solution)
```

### Refactoring
```
1. C-c q s      "This function is too complex, help me split it"
2. Agent uses: describe-symbol, who-calls, extract-function
3. Agent shows preview of changes
4. You approve
5. Agent applies refactoring
6. Agent runs tests to verify
```

---

## Directory Structure

```
agent-q/
├── agent-q.asd
├── README.md
├── src/
│   ├── package.lisp
│   ├── config.lisp
│   ├── context.lisp
│   ├── conversation.lisp
│   ├── prompts.lisp
│   ├── agent.lisp
│   ├── sly-interface.lisp
│   ├── tools/
│   │   ├── protocol.lisp
│   │   ├── registry.lisp
│   │   ├── executor.lisp
│   │   ├── introspection.lisp
│   │   ├── execution.lisp
│   │   ├── buffer.lisp
│   │   ├── condition-tools.lisp
│   │   ├── testing-tools.lisp
│   │   ├── tracing-tools.lisp
│   │   └── knowledge-tools.lisp
│   ├── knowledge/
│   │   ├── schema.lisp
│   │   ├── storage.lisp
│   │   └── queries.lisp
│   ├── semantic/
│   │   ├── index.lisp
│   │   ├── definitions.lisp
│   │   ├── dependencies.lisp
│   │   └── change-tracker.lisp
│   ├── profiling/
│   │   └── profiler.lisp
│   ├── refactoring/
│   │   ├── engine.lisp
│   │   └── rename.lisp
│   └── patterns/
│       └── detector.lisp
└── contrib/
    └── sly-agent-q/
        ├── sly-agent-q.el
        └── sly-agent-q-tools.el
```

---

## Specification Documents

- [Phase 1: Foundation](./PHASE-1-SPEC.md)
- [Phase 2: REPL-Aware Agent](./PHASE-2-SPEC.md)
- [Phase 3: Autonomous Developer](./PHASE-3-SPEC.md)
- [Phase 4: Intelligent Partner](./PHASE-4-SPEC.md)
- [cl-llm-provider Requirements](./CL-LLM-PROVIDER-REQUIREMENTS.md)

---

## Design Principles

1. **Lisp-First**: Leverage CL's unique capabilities (introspection, conditions, REPL)
2. **Autonomous but Safe**: Agent acts independently but respects trust levels
3. **Learn and Remember**: Build up knowledge over time
4. **Composable**: System prompts, tools, and workflows are composable
5. **Incremental**: Each phase delivers value; later phases enhance

---

## Why This Is Different

| Traditional AI Assistant | Agent-Q |
|--------------------------|---------|
| Suggests code, hopes it works | Evaluates code, observes results |
| Treats code as text | Understands semantic structure |
| Stateless | Builds persistent knowledge |
| Generic | Deeply integrated with CL ecosystem |
| Reactive only | Can iterate autonomously |

Agent-Q isn't just an AI that happens to work with Lisp - it's an AI that thinks *with* Lisp.
