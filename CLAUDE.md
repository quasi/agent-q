# CLAUDE.md

## Project Overview

**Agent-Q** is an AI-powered agentic extension for Common Lisp development. It integrates with SLY/Emacs to create an autonomous development partner that can introspect a running Lisp image, execute code, observe results, and iterate until tasks are complete.

---

## Canon-Driven Development

This project uses a **Canon** specification system. The Canon (`canonical-specification/`) is the authoritative source of truth for architecture, contracts, vocabulary, and design decisions.

### Before Working on Anything

```
1. Read canonical-specification/core/foundation/vocabulary.md   → Domain terms
2. Check canonical-specification/canon.yaml                     → Feature overview + confidence
3. Find your feature in canonical-specification/features/       → Contracts, properties, scenarios
4. Check canonical-specification/core/decisions/*.md            → Architectural rationale
```

### Canon Structure

```
canonical-specification/
├── canon.yaml              # Manifest with all features + confidence scores
├── README.md               # How to read/use the Canon
├── core/
│   ├── foundation/
│   │   └── vocabulary.md   # 31 domain terms (agents, context, tools, etc.)
│   └── decisions/          # ADRs (6 documented design decisions)
└── features/               # Feature specifications
    ├── context-management/ # Context accumulation, 50-item window
    ├── tool-system/        # 18 tools, safety levels, registry
    ├── diff-approval/      # Hunk-based review, state machine
    ├── chat-interface/     # Markdown rendering, streaming
    ├── context-completion/ # @-mention, pills, panel
    ├── session-management/ # SQLite persistence, CRUD
    ├── file-system-tools/  # Directory nav, file editing
    └── ...
```

### When Modifying Code

1. Find affected features in `canonical-specification/features/`
2. Read `contracts/` for API expectations
3. Check `properties/` for invariants to maintain
4. Update Canon after changes (use `canon-evolve` skill)

---

## External Dependencies

| Dependency | Purpose | Path |
|------------|---------|------|
| **cl-llm-provider** | Multi-provider LLM communication | `/Users/quasi/quasilabs/projects/cl-llm-provider/` |
| **SLY** | Lisp development environment, RPC protocol | System package |
| **closer-mop** | Portable MOP for introspection | Quicklisp |

---

## Quick Reference

### Loading

```lisp
(ql:quickload "agent-q")
(agent-q:configure :provider :anthropic
                   :model "claude-sonnet-4-20250514")
```

### Running Tests

```bash
# Emacs Lisp tests (165 tests)
cd contrib/sly-agent-q/test && emacs --batch -l run.el

# Common Lisp tests
(asdf:test-system "agent-q")
```

---

## Common Mistakes to Avoid

1. **Calling LLM APIs directly** → Always use `cl-llm-provider`
2. **Executing user code unsafely** → Tools have safety levels. Respect them.
3. **Losing context** → Context accumulation is stateful. Be careful with clearing.
4. **Blocking the REPL** → Tool execution runs in-image. Long operations block.

---

## Skill Usage

| Skill | When to Use |
|-------|-------------|
| **elisp-writer** | Writing `.el` files |
| **elisp-test-writer** | Writing Elisp tests |
| **elisp-analyzer** | Reviewing Elisp code |
| **cl-library-craft** | Writing Common Lisp code |
| **doc-writer-for-humans** | Human-facing documentation |
| **canon-specify** / **canon-evolve** | Updating Canon specifications |

---

## Issue Tracking

This project uses **bd (beads)** for issue tracking.
Run `bd prime` for workflow context, or install hooks (`bd hooks install`) for auto-injection.

**Quick reference:**
- `bd ready` - Find unblocked work
- `bd create "Title" --type task --priority 2` - Create issue
- `bd close <id>` - Complete work
- `bd sync` - Sync with git (run at session end)

For full workflow details: `bd prime`
