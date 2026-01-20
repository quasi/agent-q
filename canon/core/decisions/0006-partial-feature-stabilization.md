# Decision Record: Partial Feature Stabilization

**ID:** DR-006
**Date:** 2026-01-20
**Status:** Accepted
**Type:** Process Decision

---

## Context

The file-system-tools feature was planned with 10 tools across 4 implementation phases:
- Phase 1: Core navigation (list_directory, get_file_info, get_project_root)
- Phase 2: Advanced navigation (directory_tree, search_files)
- Phase 3: Editing (edit_file, insert_at_line)
- Phase 4: Lifecycle (create_file, move_file, delete_file)

Implementation completed **Phase 1 (3 tools) and Phase 3 (1 tool)** with 27 comprehensive tests achieving 100% pass rate. Phases 2 and 4 remain unimplemented.

**Question:** Should we mark this feature as "stable" when only 40% of planned tools are implemented?

---

## Decision

**Yes, we will mark the file-system-tools feature as stable (version 1.0.0) while documenting it as a partial implementation.**

### Rationale

1. **Vertical Slice Value**
   - The 4 implemented tools provide real, production-ready value
   - Users can navigate projects and make targeted edits today
   - Waiting for all 10 tools delays delivering working functionality

2. **Test Coverage**
   - 27 tests with 100% pass rate demonstrate production quality
   - Security properties (path-safety) fully verified
   - All critical scenarios (navigation, targeted editing) covered

3. **Stability Definition**
   - "Stable" means "API won't change without major version bump"
   - "Stable" ≠ "Feature complete"
   - The 4 tools have stable APIs; adding more tools is non-breaking

4. **Versioning Communicates Intent**
   - Version 1.0.0 signals "production ready for implemented tools"
   - Remaining tools stay in "planned" state, not "draft"
   - Future additions increment minor version (1.1.0, 1.2.0, etc.)

5. **Precedent in Software Industry**
   - Many successful projects ship 1.0 with subset of planned features
   - Rust 1.0 didn't have all RFC-planned features
   - HTTP/1.1 was stable before all optional extensions existed

---

## Implementation Strategy

### Feature.yaml Structure

```yaml
feature:
  name: file-system-tools
  version: 1.0.0
  status: stable
  implementation_status: partial  # NEW FIELD
  confidence: 0.95

  implemented_contracts: 4
  planned_contracts: 6

contracts:
  # Implemented and stable
  - name: list_directory
    status: stable
    version: 1.0.0

  # Planned for future (not draft)
  - name: directory_tree
    status: planned
    version: null
```

### Contract Status Taxonomy

| Status | Meaning | API Changes |
|--------|---------|-------------|
| **planned** | Roadmap item, not yet started | N/A - doesn't exist |
| **draft** | In development, API unstable | No guarantees |
| **stable** | Production ready, API guaranteed | Semver: major bump for breaking changes |
| **deprecated** | Stable but phasing out | Works but discouraged |

---

## Alternatives Considered

### Alternative 1: Keep entire feature as "draft"
**Rejected because:**
- Users can't trust the quality of working tools
- No incentive to complete remaining phases (draft has no API guarantees)
- Hides the 27 tests and security validation effort

### Alternative 2: Split into multiple features
**Rejected because:**
- Artificial separation: "file-system-tools-phase1" vs "file-system-tools-phase3"
- More complex dependency management
- Fragments user mental model

### Alternative 3: Wait to stabilize until all 10 tools complete
**Rejected because:**
- Delays value delivery
- Might wait forever if phases 2/4 aren't prioritized
- Discourages incremental progress

---

## Consequences

### Positive
- ✅ Users can rely on list_directory, get_file_info, get_project_root, edit_file APIs
- ✅ Clear communication: stable = working, planned = future
- ✅ Encourages shipping quality vertical slices
- ✅ Test coverage becomes a feature differentiator

### Negative
- ⚠️ "Stable" feature with missing functionality might confuse users
- ⚠️ Need to clearly document what's implemented vs planned
- ⚠️ Planned tools might never be implemented (acceptable risk)

### Mitigations
- Add `implementation_status: partial` field to feature.yaml
- Document implemented vs planned contracts explicitly
- Use "planned" status (not "draft") for unimplemented tools

---

## Verification

This decision is successful if:
- [ ] Users can build against stable tools with confidence
- [ ] Adding new tools (e.g., directory_tree) is a minor version bump (1.1.0)
- [ ] No confusion about which tools exist vs planned
- [ ] Other features follow this pattern for partial implementations

---

## Related Decisions

- **DR-005:** Phased Chat Development (precedent for incremental delivery)
- **DR-003:** Elisp-First Testing (established test quality bar)

---

## References

- Impact Analysis: `.impact-analysis-file-system-tools.md`
- Implementation: `src/tools/filesystem.lisp`
- Tests: `tests/filesystem-tests.lisp` (27 tests)
- Spec: `specs/PHASE-2-SPEC.md` (tool system)
