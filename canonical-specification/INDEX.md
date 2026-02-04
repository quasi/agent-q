# Canon Index

**Project**: Agent-Q
**Canon Version**: 0.5.0
**Last Updated**: 2026-02-04

## Quick Navigation

| I want to... | Start here |
|--------------|------------|
| Understand what Agent-Q does | `core/foundation/vocabulary.md` |
| Learn the domain terms | `core/foundation/vocabulary.md` (17 terms) |
| Implement a feature | `features/{feature}/INDEX.md` |
| Understand an architecture decision | `core/decisions/` (6 ADRs) |
| Find a specific contract | Search `features/*/contracts/` |
| Check system-wide properties | `core/foundation/vocabulary.md` (invariants section) |
| See all features at a glance | This page (see Features section below) |

## Purpose

This Canon is the **formal specification** of Agent-Q, extracted from the existing codebase through multi-source triangulation. It represents the ground truth verified by comparing code, documentation, tests, and git history.

**Confidence Level**: High (0.87 average across 193 analyzed artifacts)

## Structure

```
canonical-specification/
├── canon.yaml                          # Manifest with confidence scores
├── INDEX.md                            # This file
├── core/
│   ├── foundation/
│   │   └── vocabulary.md               # 17 domain terms (confidence: 0.92)
│   └── decisions/                      # Architecture Decision Records
│       ├── 0001-session-conversation-unification.md
│       ├── 0002-streaming-tool-fallback.md
│       ├── 0003-elisp-first-testing.md
│       ├── 0004-fail-open-cost-estimation.md
│       ├── 0005-phased-chat-development.md
│       └── 0006-partial-feature-stabilization.md
└── features/                           # Feature specifications
    ├── context-management/             # Phase 1 (complete)
    ├── conversation/                   # Phase 1 (complete)
    ├── tool-system/                    # Phase 2 (complete)
    ├── diff-approval/                  # Phase 2 (complete)
    ├── file-system-tools/              # Phase 2 (stable, 6/10 contracts)
    ├── chat-interface/                 # Phase 1-3 (complete)
    ├── context-completion/             # Phase 3 (complete, 99 tests)
    ├── session-management/             # Phase 3 (complete, needs spec)
    ├── streaming/                      # Phase 3 (complete, needs spec)
    ├── observability/                  # Phase 3 (complete, needs spec)
    └── cost-estimation/                # Phase 3 (complete, needs spec)
```

## Features

### Phase 1: Foundation (Complete)

| Feature | Confidence | Status | Description |
|---------|-----------|--------|-------------|
| **context-management** | 0.95 | ✅ Complete | Code snippet accumulation with 50-item sliding window |
| **conversation** | 0.90 | ✅ Complete | Multi-turn conversation with message history |
| **chat-interface** | 0.95 | ✅ Complete | Interactive chat with markdown rendering |

**Entry Point**: `features/context-management/INDEX.md`

### Phase 2: Tool System (Complete)

| Feature | Confidence | Status | Description |
|---------|-----------|--------|-------------|
| **tool-system** | 0.85 | ✅ Complete | Extensible tool registry with 18 tools across 4 categories |
| **diff-approval** | 0.95 | ✅ Complete | Per-hunk diff review with state machine |
| **file-system-tools** | 0.95 | 🟡 Stable | 6/10 contracts implemented (navigation + editing) |

**Entry Point**: `features/tool-system/INDEX.md`

### Phase 3: Autonomous Developer (Partial)

| Feature | Confidence | Status | Description |
|---------|-----------|--------|-------------|
| **session-management** | 0.80 | ✅ Complete | SQLite-backed session persistence with 8 RPC endpoints |
| **context-completion** | 0.95 | ✅ Complete | @-mention completion for files, symbols, buffers |
| **streaming** | 0.75 | ⚠️ Needs Spec | Real-time token-by-token response display |
| **observability** | 0.70 | ⚠️ Needs Spec | Logging hooks and metrics collection |
| **cost-estimation** | 0.75 | ⚠️ Needs Spec | Pre-flight cost checks and budget enforcement |

**Entry Point**: `features/session-management/INDEX.md`

### Phase 4: Intelligent Partner (Planned)

Not yet implemented. See `canon.yaml` for planned features.

## Development Phases

| Phase | Status | Confidence | Features |
|-------|--------|-----------|----------|
| **Phase 1** | ✅ Complete | 0.95 | Context, Conversation, Chat UI |
| **Phase 2** | ✅ Complete | 0.90 | Tools, Diff Approval, File System |
| **Phase 3** | 🟡 Partial | 0.80 | Sessions, Streaming, Completion |
| **Phase 4** | 📋 Planned | 0.30 | Semantic search, profiling, advanced introspection |

## Core Foundation

### Vocabulary (31 Terms)

**File**: `core/foundation/vocabulary.md`
**Confidence**: 0.95
**Convergence**: 94% (29/31 terms documented + verified by code)

**Key Terms**:
- Agent, Context Item, Context Manager, Message, Conversation
- Session, Session Manager, Tool, Tool Registry, Tool Result
- Hunk, Hunk State, Diff Buffer
- @-Mention, Context Pill, Context Panel
- Streaming Callback, Budget

**Undocumented**: `:debug` message role, `:custom` context type

### Architecture Decisions (6 ADRs)

**Directory**: `core/decisions/`
**Coverage**: 13 design decisions recovered from git history, 6 formally documented

| ADR | Title | Status |
|-----|-------|--------|
| 0001 | Session-Conversation Unification | ✅ Documented |
| 0002 | Streaming Tool Fallback | ✅ Documented |
| 0003 | Elisp-First Testing | ✅ Documented |
| 0004 | Fail-Open Cost Estimation | ✅ Documented |
| 0005 | Phased Chat Development | ✅ Documented |
| 0006 | Partial Feature Stabilization | ✅ Documented |

## How to Use This Canon

### For New Contributors

1. **Start with vocabulary**: `core/foundation/vocabulary.md` (15 minutes)
2. **Read ADRs**: `core/decisions/*.md` (~30 minutes for all 6)
3. **Pick a feature**: Browse `features/` by confidence score
4. **Read contracts**: `features/{feature}/contracts/` for APIs
5. **Check properties**: `features/{feature}/properties/` for invariants

### For Implementing Features

1. Find feature in `canon.yaml` (check confidence score)
2. Read `features/{feature}/feature.yaml` for overview
3. Read all `contracts/*.md` for API specifications
4. Read all `properties/*.md` for invariants to maintain
5. Read `scenarios/*.md` for expected behavior
6. Update Canon after implementation (`canon-evolve` skill)

### For Modifying Code

1. Find affected features in `canon.yaml`
2. Read existing contracts to understand current API
3. Check properties for invariants that must hold
4. Make code changes
5. Update Canon to reflect changes (`canon-evolve` skill)
6. Verify tests still pass

## Quality Metrics

### Source Triangulation

**Method**: 7-pass extraction comparing code, docs, tests, git history

| Source | Artifacts | Pass Rate |
|--------|-----------|-----------|
| Documentation | 10 files | 93% (matches code) |
| Code | 29 files | 100% (analyzed) |
| Tests | 165 Elisp tests | 100% (passing) |
| Git History | 25 commits | 76% (rationale recovered) |

**Overall Convergence**: 74% (code matches docs)

### Confidence Distribution

| Score | Meaning | Count | % |
|-------|---------|-------|---|
| 1.00 | Perfect (verified by all sources) | 95 | 49% |
| 0.95 | High (minor gaps) | 47 | 24% |
| 0.90 | Good (some divergence) | 28 | 15% |
| 0.85 | Medium (needs review) | 14 | 7% |
| 0.80 | Acceptable (known gaps) | 6 | 3% |
| <0.80 | Low (requires work) | 3 | 2% |

**Average**: 0.87 (High confidence overall)

### Test Coverage

| Side | Framework | Tests | Status | Coverage |
|------|-----------|-------|--------|----------|
| Emacs Lisp | ERT | 165 | ✅ 100% passing | Excellent |
| Common Lisp | (None) | 64 | ✅ 100% passing | Filesystem only |

**Gap**: Common Lisp core logic has 0 unit tests (relies on integration testing)

## Known Gaps

### Specifications

- Session management: 8 RPC endpoints implemented but not formally specified
- Streaming/observability/cost: Implementation exists, needs elevation from plans to specs

### Testing

- Common Lisp side has no automated unit tests for core logic
- Tool system has 18 tools with 0 Common Lisp tests
- LLM integration is untested

### Documentation

- `:debug` message role is undocumented
- `:custom` context type is undocumented
- Concurrency model (single-threaded) is undocumented
- REPL history capacity is undocumented

### Rationale

- `:debug` role origin unknown
- Why Common Lisp tests were never implemented

## Next Steps

### Immediate (High Priority)

1. Create `specs/SESSION-MANAGEMENT-SPEC.md`
2. Document undocumented properties in `core/foundation/`
3. Elevate streaming/observability/cost plans to formal specs

### Medium Priority

4. Add Common Lisp test strategy document
5. Create remaining feature specs for Phase 3
6. Document all 8 session management RPC endpoints

### Low Priority

7. Investigate `:debug` role with original author
8. Document REPL history capacity
9. Add status badges to Phase 3-4 specs

## Reading Order Recommendations

### Quick Start (30 minutes)

1. This INDEX.md
2. `core/foundation/vocabulary.md`
3. `features/context-management/feature.yaml`

### Deep Dive (2-3 hours)

1. All of Quick Start
2. All 6 ADRs in `core/decisions/`
3. `features/tool-system/` (all artifacts)
4. `features/diff-approval/` (all artifacts)

### Comprehensive (1 day)

1. All of Deep Dive
2. Every feature in `features/` (contracts, properties, scenarios)
3. Triangulation report: `.canon-initiation/pass6-triangulation-report.md`

## References

### Source Material

- **Triangulation Report**: `.canon-initiation/pass6-triangulation-report.md`
- **Pass Reports**: `.canon-initiation/pass*.yaml`
- **Initiation State**: `.canon-initiation/initiation-state.yaml`

### Tools

- **canon-initiate**: Multi-source triangulation extractor (used to create this Canon)
- **canon-specify**: Ongoing Canon specification work
- **canon-evolve**: Consistency maintenance for existing Canon
- **canon-verify**: Conformance verification
- **canon-document**: Generate documentation from Canon (this skill)

### Documentation

- **Specs** (legacy): `specs/PHASE-*.md` (formal specifications, pre-Canon)
- **Plans**: `docs/plans/*.md` (implementation plans)
- **User Docs**: `docs/` (human-friendly guides)
- **Agent Instructions**: `CANON.md` (root level - contributor guide)

---

**Canon Status**: 🟢 Active (initialized and ready for ongoing development)
**Last Triangulation**: 2026-01-17
**Next Review**: When Phase 4 features begin development
