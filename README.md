# Agent-Q

**AI-powered agentic extension for Common Lisp development**

Agent-Q integrates with SLY/Emacs to provide REPL-aware assistance with Common Lisp development. Unlike generic AI assistants, Agent-Q can introspect your running Lisp image, execute code, observe results, and iterate autonomously.

## Quick Links

| I want to... | Go here |
|--------------|---------|
| **Get started in 10 minutes** | [Quickstart Guide](docs/user/quickstart.md) |
| **Learn all features** | [User Guide](docs/user/user-guide.md) |
| **Contribute to Agent-Q** | [AGENT.md](AGENT.md) |
| **Understand the architecture** | [Canon Specifications](canon/) |
| **Browse API reference** | [Reference Docs](docs/reference/) |

## What Makes Agent-Q Different?

Agent-Q is **REPL-aware**. It has 18 specialized tools for:

- **Introspection**: Inspect symbols, find callers, explore class hierarchies
- **Execution**: Eval and compile code, examine errors, check history
- **File Operations**: Read, search, and propose changes with reviewable diffs
- **Autonomous Iteration**: Test → Observe → Fix → Verify cycles

```
Traditional AI:
  "Fix this bug" → [Guesses solution]

Agent-Q:
  "Fix this bug" → [Inspects code] → [Tests in REPL] → [Sees error] →
  [Proposes fix] → [Verifies fix works] → "Fixed! Here's what I changed."
```

## Key Features

### ✅ Phase 1 & 2: Complete

- **Context Management** - Sliding window of 50 code snippets
- **Multi-turn Conversations** - Full message history
- **18 Powerful Tools** - Introspection, execution, file operations
- **Diff Review** - Per-hunk review with visual feedback
- **SLY Integration** - Seamless integration with your Lisp workflow
- **Rich Markdown** - Syntax highlighting in chat responses

### ✅ Phase 3: Stable

- **Session Management** - SQLite-backed persistence across restarts
- **@-Mention Completion** - Inline attachment of files, symbols, buffers
- **Context Pills** - Visual indicators with hover previews
- **Streaming Responses** - Real-time token-by-token display
- **Observability** - Logging, metrics, request statistics
- **Cost Estimation** - Budget enforcement and tracking

### 📋 Phase 4: Planned

- Semantic code indexing
- Profiling integration
- Automated refactoring suggestions
- Pattern detection and learning

## Installation (Quick Version)

### 1. Set API Key

```bash
export ANTHROPIC_API_KEY="sk-ant-your-key-here"
# Add to ~/.bashrc or ~/.zshrc
```

### 2. Install

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/yourusername/agent-q.git
```

### 3. Load in Lisp

```lisp
(ql:quickload "cl-llm-provider")
(ql:quickload :agent-q)
```

### 4. Configure Emacs

Add to `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(with-eval-after-load 'sly
  (add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")
  (require 'sly-agent-q)
  (sly-agent-q-setup))
```

**Full installation instructions**: [Quickstart Guide](docs/user/quickstart.md)

## Quick Example

```lisp
;; Write a function
(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

;; Add to context (C-c q c d), then ask Agent-Q (C-c q S):
;; "Make this tail-recursive with memoization"

;; Agent-Q will:
;; 1. Inspect the current implementation (describe_symbol)
;; 2. Test it to understand behavior (eval_form)
;; 3. Propose optimized version
;; 4. Show reviewable diff
;; 5. Verify the fix works after you accept
```

## Essential Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `C-c q s` | Send message |
| `C-c q S` | Send with context (capital S) |
| `C-c q c d` | Add current function to context |
| `C-c q i` | Insert response at cursor |
| `C-c q q d` | Quick: Document function |
| `C-c q q f` | Quick: Fix error |

**Full keyboard reference**: [User Guide - Keyboard Reference](docs/user/user-guide.md#keyboard-reference)

## Documentation Structure

### For End Users

| Document | Purpose |
|----------|---------|
| [Quickstart](docs/user/quickstart.md) | Get running in 10 minutes |
| [User Guide](docs/user/user-guide.md) | Complete feature tour and workflows |
| [Reference](docs/reference/) | Detailed API specifications |

### For Contributors (Agents)

| Document | Purpose |
|----------|---------|
| [AGENT.md](AGENT.md) | Build commands, conventions, architecture rules, invariants |
| [Canon Index](canon/INDEX.md) | Navigation to specifications |
| [Canon Features](canon/features/) | Contracts, properties, scenarios for each feature |
| [Design Decisions](canon/core/decisions/) | Architecture Decision Records (ADRs) |

### For External Agents (Reading as Library)

External agents building on Agent-Q should read the **Canon directly**:

- **Contracts**: `canon/features/*/contracts/` (JSON schemas, APIs, public only)
- **Vocabulary**: `canon/core/foundation/vocabulary.md` (domain terms)
- **Properties**: `canon/features/*/properties/` (invariants and constraints)

This avoids duplicate documentation and provides formal specifications.

## Architecture

```
┌─────────────┐
│    Emacs    │  ← User interacts here
│   (SLY UI)  │
└──────┬──────┘
       │ SLYNK Protocol (RPC)
       ▼
┌─────────────┐
│ Common Lisp │  ← Agent-Q runs here
│    REPL     │  ← 18 tools for introspection/execution
└──────┬──────┘
       │ HTTP/JSON (via cl-llm-provider)
       ▼
┌─────────────┐
│ LLM Provider│  ← Claude, GPT-4, Ollama, etc.
└─────────────┘
```

**Canon Version**: 0.5.0
**Status**: Phase 1-2 complete, Phase 3 stable

## Project Structure

```
agent-q/
├── README.md              # You are here
├── AGENT.md               # Contributor guide
├── canon/                 # Formal specifications
│   ├── INDEX.md           # Canon navigation
│   ├── core/              # Foundation (vocabulary, decisions)
│   └── features/          # Feature specs (contracts, properties, scenarios)
├── src/                   # Common Lisp source
│   ├── agent.lisp         # Core agent loop
│   ├── context.lisp       # Context management
│   ├── conversation.lisp  # Message history
│   ├── session.lisp       # SQLite persistence
│   └── tools/             # Tool system (18 tools)
├── contrib/
│   └── sly-agent-q/       # Emacs integration
│       ├── sly-agent-q.el              # Core mode
│       ├── sly-agent-q-chat.el         # Chat UI
│       ├── sly-agent-q-context.el      # Context management
│       ├── sly-agent-q-diff.el         # Diff review
│       └── test/                       # Test suite (165 tests)
└── docs/                  # User documentation
    ├── user/              # End-user guides
    └── reference/         # API reference
```

## Development Status

| Phase | Status | Confidence | Features |
|-------|--------|-----------|----------|
| **Phase 1** | ✅ Complete | 0.95 | Context, Conversation, Chat UI |
| **Phase 2** | ✅ Complete | 0.90 | Tools, Diff Approval, File System |
| **Phase 3** | 🟡 Stable | 0.80 | Sessions, Streaming, Completion |
| **Phase 4** | 📋 Planned | 0.30 | Semantic search, profiling, learning |

**Test Coverage**:
- Emacs Lisp: 165 tests, 100% passing ✅
- Common Lisp: 64 filesystem tests, 100% passing ✅

**Documentation Quality**: High (0.87 confidence, triangulated from code/docs/tests/git)

## Troubleshooting

### Authentication Failed

```bash
# Check API key is set
echo $ANTHROPIC_API_KEY

# Set it and restart Lisp REPL
export ANTHROPIC_API_KEY="sk-ant-..."
```

### Tools Not Working

```lisp
;; Verify tools loaded (should be 18)
(length (agent-q.tools:get-agent-q-tools))

;; Reload if needed
(ql:quickload :agent-q :force t)
```

**Full troubleshooting**: [User Guide - Troubleshooting](docs/user/user-guide.md#troubleshooting)

## Configuration

### Provider Selection

Create `~/.config/agent-q/config.lisp`:

```lisp
(in-package :agent-q)

;; Anthropic Claude (recommended)
(configure :provider :anthropic
           :model "claude-sonnet-4-20250514")

;; OR OpenAI GPT-4
(configure :provider :openai
           :model "gpt-4-turbo")

;; OR Local Ollama (free, private)
(configure :provider :ollama
           :model "llama2"
           :base-url "http://localhost:11434")
```

### Project-Specific Instructions

Create `.agent-q/system-prompt.md` in your project root:

```markdown
This project uses Alexandria for utilities and FiveAM for testing.

Naming convention: verb-noun (e.g., parse-data, emit-code)

When debugging:
1. Use describe_symbol to understand functions
2. Use eval_form to reproduce bugs
3. Use get_last_error to examine backtraces
```

Agent-Q automatically includes these when working in that project.

## Contributing

**For Users**: Report issues, suggest features, share workflows

**For Contributors**: Read [AGENT.md](AGENT.md) first

1. Review [Canon specifications](canon/) before making changes
2. Follow [architecture rules](AGENT.md#architecture-rules)
3. Run test suite (165 Elisp tests)
4. Update Canon after code changes (`canon-evolve` skill)

**For External Agents**: Read Canon contracts directly (`canon/features/*/contracts/`)

## Dependencies

- **Required**: Emacs, SLY, Common Lisp (SBCL recommended), Quicklisp
- **Required**: cl-llm-provider library (multi-provider LLM support)
- **Required**: API key for Anthropic/OpenAI or local Ollama instance
- **Optional**: closer-mop (for CLOS introspection tools)

## Credits

**Author**: quasi / quasiLabs

**Built With**:
- [cl-llm-provider](https://github.com/user/cl-llm-provider) - LLM integration
- [SLY](https://github.com/joaotavora/sly) - Superior Lisp Interaction Mode
- Claude (Sonnet 4.5) - Agent implementation partner

## License

MIT License - see LICENSE file

## Links

- **Issues**: https://github.com/yourusername/agent-q/issues
- **Canon**: [canon/INDEX.md](canon/INDEX.md)
- **User Docs**: [docs/user/](docs/user/)
- **Contributor Guide**: [AGENT.md](AGENT.md)

---

**Recent Updates** (2026-01-22):
- ✅ Complete documentation overhaul with Canon-based structure
- ✅ AGENT.md for contributing agents
- ✅ Canon INDEX.md for navigation
- ✅ User guides restructured in docs/user/
- ✅ Clear separation: end users vs contributors vs external agents
