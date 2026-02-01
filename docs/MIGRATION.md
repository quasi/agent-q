# Documentation Migration Guide

**Date**: 2026-01-22
**Status**: Documentation overhaul complete

## What Changed

Agent-Q's documentation has been completely overhauled to follow Canon-based documentation principles with clear separation between:

1. **End Users** - People using Agent-Q for development
2. **Contributing Agents** - AI agents helping develop Agent-Q
3. **External Agents** - AI agents building on Agent-Q as a library

## New Documentation Structure

```
agent-q/
├── README.md                      # Main entry point (UPDATED)
├── AGENT.md                       # For contributing agents (NEW)
├── canon/
│   ├── INDEX.md                   # Canon navigation (NEW)
│   ├── core/                      # Foundation specs
│   │   ├── foundation/
│   │   │   └── vocabulary.md      # 17 domain terms
│   │   └── decisions/             # 6 ADRs
│   └── features/                  # Feature specifications
│       ├── context-management/
│       ├── tool-system/
│       ├── diff-approval/
│       └── ...
├── docs/
│   ├── user/                      # End-user documentation (NEW)
│   │   ├── quickstart.md          # Get running in 10 min
│   │   └── user-guide.md          # Complete feature tour
│   ├── reference/                 # API reference (PLANNED)
│   ├── guides/                    # Legacy guides (DEPRECATED)
│   ├── plans/                     # Implementation plans
│   └── MIGRATION.md               # This file
└── research/                      # Research dumps (IGNORE)
```

## Mapping: Old → New

### For End Users

| Old Location | New Location | Status |
|--------------|--------------|--------|
| `docs/QUICKSTART.md` | `docs/user/quickstart.md` | ✅ Replaced |
| `docs/USER-GUIDE.md` | `docs/user/user-guide.md` | ✅ Replaced |
| `docs/guides/context-management.md` | `docs/user/user-guide.md#context-management` | ✅ Consolidated |
| `docs/guides/DIFF-REVIEW-GUIDE.md` | `docs/user/user-guide.md#diff-review` | ✅ Consolidated |
| `docs/guides/TESTING.md` | `AGENT.md#testing-strategy` | ✅ Moved to contributors |
| `docs/guides/testing-chat.md` | `AGENT.md#testing-strategy` | ✅ Moved to contributors |

### For Contributing Agents

| Old Location | New Location | Status |
|--------------|--------------|--------|
| `CLAUDE.md` (project) | `AGENT.md` | ✅ Superseded |
| Scattered architecture docs | `AGENT.md#architecture-rules` | ✅ Consolidated |
| Build commands in README | `AGENT.md#build-commands` | ✅ Moved |
| `docs/guides/TESTING.md` | `AGENT.md#testing-strategy` | ✅ Moved |
| Invariants (scattered) | `AGENT.md#invariants` | ✅ Centralized |
| Canon README | `canon/INDEX.md` | ✅ Enhanced |

### For External Agents

| Information Needed | New Location |
|--------------------|--------------|
| API contracts | `canon/features/*/contracts/` |
| Domain vocabulary | `canon/core/foundation/vocabulary.md` |
| System properties | `canon/features/*/properties/` |
| Design rationale | `canon/core/decisions/` |
| Navigation | `canon/INDEX.md` |

## What to Use Now

### I'm an End User

**Start here**: `README.md` → Quick Links table

**Primary docs**:
1. `docs/user/quickstart.md` - Get started in 10 minutes
2. `docs/user/user-guide.md` - Complete feature reference

**Old docs to ignore**:
- ~~`docs/QUICKSTART.md`~~ (use `docs/user/quickstart.md` instead)
- ~~`docs/USER-GUIDE.md`~~ (use `docs/user/user-guide.md` instead)
- ~~`docs/guides/*.md`~~ (consolidated into user-guide.md)

### I'm Contributing to Agent-Q

**Start here**: `AGENT.md`

**Primary docs**:
1. `AGENT.md` - Build commands, conventions, rules, invariants
2. `canon/INDEX.md` - Navigate to specifications
3. `canon/core/foundation/vocabulary.md` - Learn domain terms (15 min)
4. `canon/core/decisions/*.md` - Understand architecture (6 ADRs)
5. `canon/features/*/` - Feature specifications

**Old docs to ignore**:
- ~~`CLAUDE.md`~~ (project) - superseded by AGENT.md
- ~~`docs/guides/TESTING.md`~~ (use AGENT.md instead)

### I'm an External Agent Building on Agent-Q

**Start here**: `README.md` → "For External Agents" section

**Primary docs**:
1. `canon/core/foundation/vocabulary.md` - Domain terms
2. `canon/features/*/contracts/` - Public API contracts
3. `canon/features/*/properties/` - Invariants and constraints

**Do NOT read**:
- Implementation files (`src/`)
- Internal tests (`contrib/sly-agent-q/test/`)
- Private contracts (marked `visibility: private`)

## What Was Deprecated

### Deprecated Files (Old Structure)

These files are now superseded by the new structure but **not yet deleted** (for reference):

```
docs/
├── QUICKSTART.md              → use docs/user/quickstart.md
├── USER-GUIDE.md              → use docs/user/user-guide.md
├── BUGFIX-SUMMARY.md          → useful reference, keep
├── DIFF-IMPLEMENTATION.AGENT.md → useful reference, keep
└── guides/
    ├── context-management.md  → consolidated into user-guide.md
    ├── DIFF-REVIEW-GUIDE.md   → consolidated into user-guide.md
    ├── TESTING.md             → moved to AGENT.md
    ├── testing-chat.md        → moved to AGENT.md
    └── MANUAL-TESTING-FILESYSTEM-TOOLS.md → moved to AGENT.md
```

**Action**: These will be archived to `docs/archive/` in a future commit after review.

### Deprecated Sections

**In old QUICKSTART.md**:
- Tool-by-tool examples → Consolidated into workflows in user-guide.md
- Manual testing checklist → Moved to AGENT.md

**In old USER-GUIDE.md**:
- Scattered keyboard shortcuts → Centralized table in user-guide.md
- Redundant installation instructions → Quickstart only

## Key Improvements

### 1. Clear Audience Separation

**Old**: Mixed user docs and contributor docs, unclear who should read what

**New**: Three distinct audiences with dedicated entry points:
- End users → `docs/user/`
- Contributors → `AGENT.md` + `canon/`
- External agents → `canon/` (public contracts only)

### 2. Single Source of Truth

**Old**: Canon + separate docs → duplication, drift

**New**: Canon is authoritative, docs are derived views:
- User docs: Canon → human-friendly prose
- Agent docs: Canon → instructions + rules
- External: Canon contracts directly (no duplication)

### 3. Navigation

**Old**: No clear index, hard to find relevant specs

**New**:
- `README.md` - Quick links table
- `canon/INDEX.md` - "I want to..." navigation
- `AGENT.md` - Jump to common patterns

### 4. Consolidation

**Old**:
- 5 separate guide files
- Scattered keyboard shortcuts
- Duplicate quickstart content

**New**:
- 2 main user docs (quickstart + guide)
- 1 contributor doc (AGENT.md)
- Single keyboard reference table

## Migration Checklist for Contributors

If you're updating documentation:

- [ ] End-user content → `docs/user/`
- [ ] Contributor content → `AGENT.md` or `canon/`
- [ ] Public APIs → `canon/features/*/contracts/`
- [ ] Architecture decisions → `canon/core/decisions/*.md`
- [ ] Don't duplicate Canon content
- [ ] Update `canon/INDEX.md` if adding features

## FAQ

### Why the split between docs/user/ and canon/?

**Canon** is the formal specification (what IS true about the code).
**docs/user/** is human-friendly documentation (how to USE it).

Canon is for contributors and external agents. User docs are for end users.

### What happened to CLAUDE.md (project)?

It's superseded by `AGENT.md`, which is a formal contributor guide following Canon documentation principles. The project CLAUDE.md had contributor instructions scattered across multiple files. AGENT.md consolidates everything contributors need.

### Can I still read the old docs?

Yes, they're still in the repo (not deleted yet). But use the new docs for up-to-date information. Old docs will be archived soon.

### Where do I report documentation issues?

- User docs: GitHub issues
- Contributor docs: GitHub issues + Canon updates via `canon-evolve`
- Canon specs: Use `canon-evolve` skill to maintain consistency

### How do I know which Canon artifacts are public?

Canon artifacts without `visibility: private` are public. External agents should only read public artifacts.

## Summary

**Old structure**: Mixed audiences, scattered docs, duplication
**New structure**: Clear separation, Canon-driven, single source of truth

**Action for end users**: Use `docs/user/quickstart.md` and `docs/user/user-guide.md`
**Action for contributors**: Read `AGENT.md` first, then explore `canon/`
**Action for external agents**: Read `canon/` public contracts only

---

**Questions?** See `README.md` Quick Links table for your audience.
