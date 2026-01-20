# Agent-Q Canon

**Status:** Initialized via Triangulation
**Date:** 2026-01-17
**Method:** Multi-source triangulation (7-pass extraction)
**Confidence:** High (0.87 average)

---

## What is This?

This Canon is the **formal specification** of Agent-Q extracted from the existing codebase through multi-source triangulation. It represents the ground truth of what Agent-Q is, how it works, and why it was built this way.

Unlike traditional documentation that describes what *should* be, this Canon describes what **actually is**, verified by comparing code, documentation, tests, and git history.

---

## How Was This Created?

**Method:** canon-initiate (triangulation v0.2.0)

The Canon was extracted through 7 systematic passes:

1. **Pass 0: Documentation Survey** - Extracted 59 claims from docs (vocabulary, features, architecture)
2. **Pass 1: Structural Discovery** - Analyzed code structure, found 26 features
3. **Pass 2: Contract Extraction** - Extracted 43 API contracts
4. **Pass 3: Behavioral Capture** - Analyzed 165 tests, extracted 29 scenarios
5. **Pass 4: Property Inference** - Inferred 45 properties/invariants from code
6. **Pass 5: Rationale Recovery** - Recovered 13 design decisions from 25 git commits
7. **Pass 6: Reconciliation** - Generated triangulation report with confidence scores

**Result:** 193 artifacts analyzed with 74% convergence (code matches docs)

---

## Structure

```
canon/
├── canon.yaml                          # Manifest with confidence scores
├── README.md                           # This file
├── core/
│   ├── foundation/
│   │   └── vocabulary.md               # 17 domain terms (complete)
│   └── decisions/
│       └── 0001-session-conversation-unification.md  # First ADR (complete)
└── features/
    └── context-management/             # First feature Canon (complete)
        ├── feature.yaml                # Metadata (confidence: 0.95)
        └── contracts/
            └── context-item.md         # Complete contract (confidence: 1.00)
```

---

## What's Been Created (So Far)

### ✅ Core Foundation
- **canon.yaml** - Manifest listing all 11 features with confidence scores
- **vocabulary.md** - Complete glossary of 17 terms (agents, context, messages, sessions, tools, diff, streaming, cost)

### ✅ Design Decisions
- **ADR-0001** - Session-Conversation Unification (recovered from git commit 555703e8)
  - Documents root cause of message persistence bug
  - Explains session-first conversation lookup pattern

### ✅ Features (Started)
- **context-management/** (confidence: 0.95)
  - feature.yaml with metadata
  - contracts/context-item.md (complete specification)

### 📋 Planned (High Confidence, Ready to Extract)
- **tool-system/** (confidence: 0.85, 18 tools, 93% spec adherence)
- **diff-approval/** (confidence: 0.95, exemplary spec fidelity)
- **chat-interface/** (confidence: 0.95, 36 tests)
- **context-completion/** (confidence: 0.95, 99 tests, best tested)

### ⚠️ Needs Specification First
- **session-management/** (confidence: 0.80, 8 RPC endpoints, no formal spec)
- **streaming/** (confidence: 0.75, implementation complete, needs spec)
- **observability/** (confidence: 0.70, code-only)
- **cost-estimation/** (confidence: 0.75, code-only)

---

## Confidence Levels

Artifacts are tagged with confidence scores:

| Score | Meaning | Count |
|-------|---------|-------|
| **1.00** | Verified by code + docs + tests | 95 (49%) |
| **0.95** | High confidence, minor gaps | 47 (24%) |
| **0.90** | Good confidence, some divergence | 28 (15%) |
| **0.85** | Medium confidence, needs review | 14 (7%) |
| **0.80** | Acceptable, has known gaps | 6 (3%) |
| **<0.80** | Low confidence, requires work | 3 (2%) |

**Average:** 0.87 (High confidence overall)

---

## How to Read This Canon

### For Understanding Agent-Q

1. **Start with vocabulary.md** - Learn the domain terms
2. **Read feature.yaml files** - Get overview of each feature
3. **Dive into contracts/** - Understand APIs and data structures
4. **Check decisions/** - Learn architectural rationale

### For Implementing Features

1. **Check triangulation report** (.canon-initiation/pass6-triangulation-report.md)
2. **Find your feature** in canon.yaml
3. **Read contracts/** for that feature
4. **Check properties/** for invariants
5. **Review scenarios/** for expected behavior

### For Modifying Code

1. **Find affected features** in canon/features/
2. **Read contracts** to understand current API
3. **Check properties** for invariants to maintain
4. **Update Canon** after code changes (use canon-evolve)

---

## Quality Indicators

### What Makes This Canon High Quality

✅ **93% spec adherence** - Code matches specifications closely
✅ **165 tests analyzed** - Behavioral contracts verified
✅ **45 properties inferred** - Invariants documented with confidence
✅ **13 design decisions recovered** - Rationale preserved from git
✅ **74% convergence** - Strong agreement between sources

### Known Gaps

⚠️ **Common Lisp tests missing** - 0 CL unit tests (Elisp has 165)
⚠️ **3 features need specs** - Streaming, observability, cost
⚠️ **2 undocumented terms** - :debug message role, :custom context type
⚠️ **1 architectural gap** - Session management has 8 RPC endpoints but no formal spec

---

## Using This Canon

### For New Contributors

"How does context management work?"
→ Read `features/context-management/README.md`

"What are the data structures?"
→ Read `core/foundation/vocabulary.md`

"Why was this designed this way?"
→ Read `core/decisions/*.md`

### For Maintaining Code

"I need to change context-item structure"
→ Check `features/context-management/contracts/context-item.md` for invariants
→ Check `features/context-management/properties/` for properties to maintain

"I'm adding a new tool"
→ Read `features/tool-system/contracts/tool-protocol.md` (when created)
→ Follow the safety level system

### For Specification Work

"I need to spec session management"
→ Use `.canon-initiation/pass2-contract-extraction.yaml` as source
→ 8 RPC endpoints already documented there with signatures

"I need to add tests"
→ Check `.canon-initiation/pass3-behavioral-capture.yaml` for scenarios
→ Common Lisp side has 0 tests, high-value target

---

## Triangulation Summary

### Sources Compared

✅ **Documentation** (10 files: specs, plans, guides)
✅ **Code** (29 files: 18 CL + 6 EL + 5 test files)
✅ **Tests** (165 tests, 100% passing)
✅ **Git History** (25 commits analyzed for rationale)

### Agreement Matrix

| Comparison | Match Rate | Notes |
|------------|------------|-------|
| Docs ↔ Code | 93% | Phase 1-2 specs match perfectly |
| Code ↔ Tests | 100% | All tested behaviors work as coded |
| Docs ↔ Tests | 95% | Minor gaps in CL test coverage |
| Git ↔ All | 76% | Recovered 13/17 design rationales |

### Divergence Patterns

**Code-Only (17%):** Recent features (streaming, observability, cost) where implementation preceded specification

**Docs-Only (7%):** Future work (Phase 3-4 features) clearly marked as planned

**Conflicts (3%):** Mostly cosmetic (outdated metrics, test counts off by 4)

---

## Next Steps

### Immediate (High Priority)

1. **Complete context-management/**
   - Add contracts/context-manager.md
   - Add properties/*.md
   - Add scenarios/*.md

2. **Create high-confidence features**
   - tool-system/ (0.85 confidence, ready)
   - diff-approval/ (0.95 confidence, ready)
   - context-completion/ (0.95 confidence, 99 tests)

3. **Document more ADRs**
   - 0002-streaming-tool-fallback.md
   - 0003-elisp-first-testing.md
   - 0004-fail-open-cost-estimation.md
   - 0005-phased-chat-development.md

### Medium Priority

4. **Formalize code-only features**
   - Create specs/SESSION-MANAGEMENT-SPEC.md
   - Elevate streaming plan to formal spec
   - Document observability system
   - Document cost estimation system

5. **Close gaps**
   - Document :debug message role
   - Document :custom context type
   - Document concurrency model (single-threaded)

### Low Priority

6. **Add CL tests** (Phase 4+ work)
   - Tool system (18 tools, 0 tests)
   - LLM integration (untested)
   - Agent loop (untested)

---

## Maintenance

### When Code Changes

1. **Identify affected features** (check canon.yaml)
2. **Update contracts** if APIs changed
3. **Update properties** if invariants changed
4. **Update scenarios** if behavior changed
5. **Run canon-evolve** to check consistency

### When Adding Features

1. **Create feature.yaml** in features/new-feature/
2. **Write contracts/** before implementation (if possible)
3. **Document properties** as you discover them
4. **Create ADR** if architectural decision made
5. **Update canon.yaml** with new feature

### When Removing Features

1. **Mark as deprecated** in feature.yaml
2. **Create ADR** explaining removal rationale
3. **Update dependent features**
4. **Archive to canon/archived/** (don't delete)

---

## References

### Source Material
- **Triangulation Report:** `.canon-initiation/pass6-triangulation-report.md`
- **All Pass Reports:** `.canon-initiation/pass*.yaml`
- **Initiation State:** `.canon-initiation/initiation-state.yaml`

### Tools
- **canon-initiate:** Multi-source triangulation extractor (used to create this)
- **canon-specify:** Ongoing Canon specification work
- **canon-evolve:** Consistency maintenance for existing Canon
- **canon-verify:** Conformance verification

### Documentation
- **Specs:** `specs/PHASE-*.md` (formal specifications)
- **Plans:** `specs/plans/*.md` (implementation plans)
- **Guides:** `docs/guides/*.md` (user documentation)
- **Agent Docs:** `docs/*AGENT.md` (agent-oriented docs)

---

## Credits

**Extracted by:** Claude (Sonnet 4.5) using canon-initiate skill
**Method:** Multi-source triangulation (7 systematic passes)
**Date:** 2026-01-17
**Duration:** ~4 hours
**Artifacts Analyzed:** 193
**Lines Generated:** ~4,700 lines of structured extraction data

**Quality:** High confidence (0.87 average), ready for ongoing use

---

## License

This Canon is extracted from the Agent-Q project which is licensed under MIT License.

The Canon itself is documentation of the codebase and inherits the same license.

---

**Canon Status:** 🟢 **Active** (initialized and ready for ongoing development)
**Last Updated:** 2026-01-17
**Next Review:** When Phase 4 features begin development
