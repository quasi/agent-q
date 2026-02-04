# Canon Changelog

All notable changes to the Agent-Q Canon specification will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [0.5.0] - 2026-01-21

### Added

#### Infrastructure
- Added `.canon-meta.yaml` infrastructure tracking file for health monitoring
- Added `README-CANON.md` comprehensive Canon introduction (8.1 KB)
- Added `.canon-check-plan.md` fix plan from initial health check

#### Feature Specifications (4 code-only features formalized)
- Added `conversation/feature.yaml` - Multi-turn conversation management (6.0 KB)
- Added `streaming/feature.yaml` - Real-time token-by-token response display (6.8 KB)
- Added `observability/feature.yaml` - Logging hooks and metrics collection (9.3 KB)
- Added `cost-estimation/feature.yaml` - Pre-flight cost checks and budget enforcement (9.0 KB)

#### Context Manifests (100% coverage across all 11 features)
- Added `chat-interface/.context.yaml` - Token estimate: 2,550
- Added `context-completion/.context.yaml` - Token estimate: 2,700
- Added `context-management/.context.yaml` - Token estimate: 2,800
- Added `conversation/.context.yaml` - Token estimate: 2,550
- Added `cost-estimation/.context.yaml` - Token estimate: 4,900
- Added `diff-approval/.context.yaml` - Token estimate: 2,600
- Added `file-system-tools/.context.yaml` - Token estimate: 4,850
- Added `observability/.context.yaml` - Token estimate: 4,750
- Added `session-management/.context.yaml` - Token estimate: 5,050
- Added `streaming/.context.yaml` - Token estimate: 3,050
- Added `tool-system/.context.yaml` - Token estimate: 4,000

### Changed
- Updated `.canon-meta.yaml` with feature formalization changelog entries
- Updated `.canon-meta.yaml` with health check results (chk-002: all clean)

### Fixed
- Resolved all 12 issues from initial canon-check (chk-001)
  - 2 infrastructure issues (missing .canon-meta.yaml, README-CANON.md)
  - 7 context manifest issues (missing .context.yaml files)
  - 4 structure issues (missing feature.yaml files)

## [0.4.0] - 2026-01-17

### Added
- Initial Canon created via `canon-initiate` with multi-source triangulation
- 7-pass extraction analyzed 193 artifacts with 0.87 overall confidence
- 11 features cataloged across 4 phases
- 17 core vocabulary terms defined
- 6 architectural decision records (ADRs)
- Core foundation structure (vocabulary, decisions)

### Features Extracted
- Phase 1: context-management, conversation, chat-interface
- Phase 2: tool-system, diff-approval, file-system-tools
- Phase 3: session-management, context-completion, streaming, observability, cost-estimation

### Quality Metrics
- Spec adherence: 93%
- Test coverage (Elisp): 165 tests, 100% passing
- Test coverage (CL): 64 filesystem tests, 100% passing
- Documentation freshness: High (most updated within 30 days)

---

## Summary Statistics (v0.5.0)

**Features:** 11 total
- Complete with specs: 11 (100%)
- Complete with context manifests: 11 (100%)
- Tested (Elisp): 7
- Tested (CL): 1 (file-system-tools)

**Canon Health:**
- Infrastructure: ✓ Current
- Structure: ✓ Complete (0 issues, 0 warnings)
- Semantics: ✓ Valid (0 issues, 0 warnings)
- Context Manifests: ✓ All present (11/11)
- Checkpoints: ✓ None pending
- Staleness: ✓ Recent (last updated today)

**Artifacts:**
- feature.yaml files: 11
- .context.yaml files: 11
- Vocabulary terms: 17
- Design decisions: 6
- Total specification size: ~80 KB (31 KB feature.yaml + 9 KB contexts + 40 KB supporting)

**Tools Used:**
- canon-initiate (v0.2.0) - Initial extraction
- canon-check - Health monitoring and fix planning
- canon-specify - Feature formalization

---

**Note:** This Canon achieved perfect health status (0 issues, 0 warnings) on 2026-01-21 after systematic formalization of all code-only features and generation of complete context manifests.
