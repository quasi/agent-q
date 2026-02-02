# Agent-Q Canon

**Version:** 0.5.0
**Created:** 2026-01-17 (via canon-initiate)
**Method:** Multi-source triangulation (7-pass extraction)
**Overall Confidence:** 0.87

This is the **Canon** (authoritative specification) for Agent-Q, an AI-powered agentic extension for Common Lisp development.

---

## What is a Canon?

A Canon is a structured, machine-readable specification system that serves as the single source of truth for a software system's:

- **Vocabulary** - Domain terms and concepts
- **Contracts** - API surfaces and behavior guarantees
- **Properties** - Invariants and constraints
- **Scenarios** - Usage patterns and workflows
- **Decisions** - Architectural choices and rationale

Think of it as "executable documentation" - precise enough for LLMs to implement from, yet clear enough for humans to understand.

---

## Quick Start

### For Humans

1. **Start with vocabulary**: `core/foundation/vocabulary.md` (17 domain terms)
2. **Explore features**: `features/` (11 features documented)
3. **Understand decisions**: `core/decisions/` (6 ADRs)
4. **Check the manifest**: `canon.yaml` (overview + confidence scores)

### For LLMs / Canon Skills

1. **Read canon.yaml** for feature overview
2. **Load feature context** via `.context.yaml` manifests
3. **Verify contracts** in `features/*/contracts/`
4. **Check properties** in `features/*/properties/`

---

## Structure

```
canon/
├── canon.yaml                  # Manifest (features, metadata, quality)
├── README-CANON.md             # This file
├── .canon-meta.yaml            # Infrastructure tracking
│
├── core/
│   ├── foundation/
│   │   └── vocabulary.md       # 17 domain terms
│   └── decisions/              # 6 Architectural Decision Records
│       ├── 0001-session-conversation-unification.md
│       ├── 0002-streaming-tool-fallback.md
│       ├── 0003-elisp-first-testing.md
│       ├── 0004-fail-open-cost-estimation.md
│       ├── 0005-phased-chat-development.md
│       └── 0006-partial-feature-stabilization.md
│
└── features/                   # Feature specifications
    ├── chat-interface/         # Interactive chat UI (complete)
    ├── context-completion/     # @-mention completion (complete)
    ├── context-management/     # 50-item sliding window (complete)
    ├── conversation/           # Multi-turn messages (complete)
    ├── cost-estimation/        # Budget enforcement (complete, code-only)
    ├── diff-approval/          # Hunk-based review (complete)
    ├── file-system-tools/      # Directory nav + editing (stable)
    ├── observability/          # Logging hooks (complete, code-only)
    ├── session-management/     # SQLite persistence (complete)
    ├── streaming/              # Real-time display (complete, code-only)
    └── tool-system/            # 18 tools, registry (complete)
```

Each feature directory contains:

```
features/<feature-name>/
├── feature.yaml           # Metadata, contracts list, dependencies
├── .context.yaml          # Smart context loading manifest
├── contracts/             # API specifications (what the feature exposes)
├── properties/            # Invariants and constraints
├── scenarios/             # Usage patterns and workflows
└── decisions/             # Feature-specific ADRs (if any)
```

---

## Features by Phase

### Phase 1: Foundation (Complete, confidence: 0.95)
- **context-management** - Code snippet accumulation with 50-item window
- **conversation** - Multi-turn conversation with message history
- **chat-interface** - Interactive chat with markdown rendering

### Phase 2: Tool System (Complete, confidence: 0.90)
- **tool-system** - 18 tools across 4 categories (introspection, execution, buffer, diff)
- **diff-approval** - Hunk-based file edit review
- **file-system-tools** - Directory navigation, file editing (10 tools)

### Phase 3: Autonomous Developer (Partial, confidence: 0.80)
- **session-management** - SQLite-backed persistence
- **context-completion** - @-mention completion for files/symbols/buffers
- **streaming** - Real-time token-by-token display
- **observability** - Logging hooks and metrics
- **cost-estimation** - Pre-flight cost checks

### Phase 4: Intelligent Partner (Planned, confidence: 0.30)
- Knowledge base, test execution, condition handling (not yet implemented)

---

## Quality Metrics

| Metric | Value |
|--------|-------|
| **Spec Adherence** | 93% (18/19 tools, most contracts impl) |
| **Test Coverage (Elisp)** | Excellent (165 tests, 100% passing) |
| **Test Coverage (CL)** | Good (64 filesystem tests, 100% passing) |
| **Documentation Freshness** | High (most updated within 30 days) |
| **Overall Confidence** | 0.87 (triangulated from code + docs + tests) |

**Triangulation Results:**
- 74% convergent (code, docs, tests agree)
- 17% code-only (implemented but not spec'd)
- 7% docs-only (planned but not implemented)
- 3% conflicts (resolved during extraction)

---

## How This Canon Was Created

This Canon was **extracted from an existing codebase** using the `canon-initiate` skill with multi-source triangulation methodology:

1. **Pass 1: Vocabulary** - Extracted 17 domain terms from code, docs, tests
2. **Pass 2: Contracts** - Identified API surfaces from exports + docs
3. **Pass 3: Properties** - Found invariants in tests + assertions
4. **Pass 4: Scenarios** - Discovered workflows from tests + examples
5. **Pass 5: Decisions** - Recovered architectural choices from commits + docs
6. **Pass 6: Triangulation** - Cross-verified sources, resolved conflicts
7. **Pass 7: Finalization** - Generated manifest, confidence scores

**Result:** High-confidence Canon (0.87) that accurately reflects the working system.

---

## Known Gaps

### Specifications
- Session management RPC endpoints need formal spec
- Streaming/observability/cost-estimation need elevation from plans to specs

### Testing
- Common Lisp side has no automated tests (except filesystem)
- Tool system untested (18 tools with 0 tests)
- LLM integration untested

### Documentation
- `:debug` message role undocumented
- `:custom` context type undocumented
- Concurrency model (single-threaded) undocumented
- REPL history capacity undocumented

See `canon.yaml` for detailed gap analysis.

---

## Using This Canon

### Before Modifying Code

1. Find affected features in `features/`
2. Read contracts for API expectations
3. Check properties for invariants to maintain
4. Review decisions for architectural context

### After Modifying Code

1. Update corresponding Canon artifacts
2. Use `canon-evolve` skill for consistency maintenance
3. Document new decisions in `core/decisions/` as ADRs
4. Update `canon.yaml` if confidence/status changes

### Creating New Features

1. Use `canon-specify` or `canon-genesis` skill
2. Define contracts first (API surface)
3. Add properties (invariants)
4. Create scenarios (usage patterns)
5. Update `canon.yaml` to register the feature

---

## Canon Health

Run `/canon-check` periodically to:
- Validate structure and semantics
- Check for broken references
- Discover pending workflows
- Detect stale artifacts
- Generate fix plans

**Last check:** 2026-01-21T14:16:14Z

---

## External Dependencies

| Dependency | Purpose | Status |
|------------|---------|--------|
| **cl-llm-provider** | Multi-provider LLM communication | External |
| **SLY** | Lisp development environment, RPC | System package |
| **closer-mop** | Portable MOP for introspection | Quicklisp |

---

## Questions?

- **For humans**: Read `core/foundation/vocabulary.md` first, then explore features by confidence score
- **For LLMs**: Load `.context.yaml` manifests for efficient, targeted context loading
- **For contributors**: See project's main `CLAUDE.md` for development workflow

---

**Note:** This Canon is a living document. It evolves alongside the codebase. When in doubt, trust the Canon - it's the authoritative specification.
