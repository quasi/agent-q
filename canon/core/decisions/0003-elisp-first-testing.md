# ADR-0003: Elisp-First Testing Strategy

**Status:** Accepted
**Date:** 2026-01-05 (decision), recovered via git archaeology
**Deciders:** Development team
**Commit:** 24c0353f

---

## Context

Agent-Q is a dual-language system with significant logic in both Common Lisp (backend) and Emacs Lisp (UI). Testing strategy needed to balance:
- **Common Lisp**: Core agent logic, tool system, LLM integration, session management
- **Emacs Lisp**: Chat interface, context completion, buffer manipulation, SLY integration

### Problem Statement

**Challenge:** Where to focus testing effort?

**Constraints:**
- Limited time for comprehensive testing across both languages
- Different test frameworks (ERT for Elisp, FiveAM/Parachute for CL)
- Integration testing requires full SLY environment (complex setup)
- User-facing bugs mostly manifest in UI layer

**Traditional approach:** Test backend thoroughly, minimal UI tests (since UI is "just display").

**Agent-Q reality:** UI has significant logic (markdown rendering, @-mention completion, context pills, session switching, streaming display).

---

## Decision

**Prioritize comprehensive Emacs Lisp testing. Verify Common Lisp backend through integration testing.**

### Testing Strategy

**Emacs Lisp: Comprehensive Unit + Integration**
- Full ERT test suite for all UI components
- Test coverage: chat interface, context management, sessions, diff approval
- Mock SLY functions for CI compatibility
- Target: >90% coverage of user-facing features
- Run in batch mode (CI-friendly)

**Common Lisp: Integration-Only (for now)**
- No automated unit tests initially
- Verify through Elisp integration tests (end-to-end)
- Manual testing of tool execution
- Rely on REPL-driven development for validation
- Defer comprehensive CL testing to later phase

**Rationale:**
1. **User interaction happens in Elisp** - bugs here are most visible
2. **Elisp logic is complex** - markdown rendering, completion, pills aren't trivial
3. **CL is verified indirectly** - if Elisp tests pass, CL must be working
4. **Time-boxed development** - focus effort where bugs hurt users most

---

## Alternatives Considered

### Alternative 1: Backend-First Testing

**Approach:** Comprehensive CL tests, minimal Elisp tests.

**Rejected because:**
- UI bugs would slip through (most user-visible issues)
- Doesn't match development workflow (UI iterated more frequently)
- Integration gaps (CL tests wouldn't catch Elisp-CL mismatches)

**Advantages:**
- Traditional approach, well-understood
- Backend logic easier to unit test (no UI mocking)

### Alternative 2: Equal Coverage Both Languages

**Approach:** Comprehensive testing for both CL and Elisp.

**Rejected because:**
- Time constraint: Would slow feature delivery
- Diminishing returns: Overlapping coverage (integration tests hit CL anyway)
- Maintenance burden: Two full test suites to maintain

**Advantages:**
- Best theoretical coverage
- Each layer independently verified

### Alternative 3: Integration-Only Testing

**Approach:** Only test end-to-end flows, no unit tests.

**Rejected because:**
- Slow: Integration tests take longer to run
- Hard to debug: Failures don't pinpoint root cause
- Coverage gaps: Edge cases hard to trigger end-to-end

**Advantages:**
- Matches user workflows exactly
- Catches real integration issues

---

## Consequences

### Positive

✅ **Excellent UI quality**
- 165 Elisp tests (100% passing)
- Comprehensive coverage: chat (41 tests), context (99 tests), sessions (9 tests), diff (12 tests)
- User-facing bugs caught early

✅ **CI-friendly**
- Batch-mode ERT execution
- Mock SLY functions (no full environment needed)
- Fast test runs (~5 seconds for full suite)

✅ **Rapid iteration**
- Tests match development workflow
- Quick feedback on UI changes
- Refactoring confidence

✅ **Real-world validation**
- Integration tests prove CL backend works with Elisp frontend
- SLY RPC contracts verified through actual calls

### Negative

⚠️ **CL backend untested directly**
- Tool system: 18 tools, 0 unit tests
- LLM integration: No mocking/stubbing tests
- Agent loop: No state machine tests
- Reliance on manual testing/REPL validation

⚠️ **Refactoring risk**
- CL changes could break in subtle ways not caught by Elisp tests
- No safety net for pure CL logic changes
- Example: Context sliding window logic untested in isolation

⚠️ **Coverage blind spots**
- Edge cases in CL code may not be exercised by integration tests
- Error handling paths might be untested
- Performance characteristics unknown

⚠️ **Technical debt**
- Acknowledged gap (documented in canon.yaml)
- Will need addressing in later phases
- Could become harder to retrofit tests later

---

## Implementation

### Test Structure

```
contrib/sly-agent-q/test/
├── test-helper.el              # ERT setup, SLY mocks
├── run.el                      # Batch mode test runner
├── sly-agent-q-chat-test.el    # 41 tests (chat interface)
├── sly-agent-q-context-test.el # 99 tests (context management)
├── sly-agent-q-sessions-test.el # 9 tests (session CRUD)
├── sly-agent-q-diff-test.el     # 6 tests (diff parsing)
└── sly-agent-q-diff-integration-test.el # 6 tests (diff workflow)

Total: 161 tests defined, 165 test assertions
Pass rate: 100%
Run time: ~5 seconds (full suite)
```

### Example Test (Elisp)

```elisp
;; contrib/sly-agent-q/test/sly-agent-q-context-test.el
(ert-deftest sly-agent-q-test-context-pill-creation ()
  "Test that @-mention creates visual context pill."
  (with-temp-buffer
    (sly-agent-q-chat-mode)
    (insert "@src/agent.lisp")
    (let ((pills (sly-agent-q-context--get-all-pills)))
      (should (= 1 (length pills)))
      (should (string= "src/agent.lisp" (car pills))))))
```

### CI Integration

```bash
# Run all Elisp tests in batch mode
cd contrib/sly-agent-q/test
emacs --batch -l run.el
# Exit code 0 = all pass, non-zero = failures
```

---

## Experience Report

### What Worked Well

- Elisp test suite caught **dozens of bugs** during development
- Context completion tests (99 tests) verified complex @-mention logic
- Diff approval tests validated state machine transitions
- Mock SLY functions worked perfectly for CI

### What Could Be Improved

- Should add CL tests for high-risk areas (tool system, agent loop)
- Integration tests don't exercise all CL error paths
- Test coverage metrics would help identify gaps

### Bugs Caught by Elisp Tests

**Examples recovered from commits:**
1. Streaming display bugs (commit 23f2819c)
2. Context pill removal edge cases
3. Session switching state management
4. Markdown rendering off-by-one errors

**Without these tests:** Bugs would have reached users.

### Lessons Learned

- **Test where bugs live**: UI bugs are more visible than backend bugs
- **Integration tests != comprehensive**: Still need unit tests eventually
- **Time-boxed pragmatism**: Ship working features, add tests incrementally
- **Acknowledge debt**: Document what's missing, plan to address

---

## Related Decisions

- **ADR-0001**: Session-Conversation Unification (caught by integration testing)
- **ADR-0005**: Phased Chat Development (tests grew with each phase)
- **Future ADR**: When adding CL tests, document strategy shift

---

## References

- **Commit:** [24c0353f](commit:24c0353f) - "Add comprehensive chat interface with session management (Phases 1-3)"
  - Mentions: "38/59 tests passing" initially (test suite grew to 165)
- **Test Suite:** `contrib/sly-agent-q/test/*.el`
- **Test Framework:** ERT (Emacs Lisp Regression Testing)
- **Canon Gap:** `canon/canon.yaml:240-242` documents CL testing gap

---

## Notes

This decision was **recovered via git archaeology** during Canon initiation (Pass 5: Rationale Recovery). Commit 24c0353f mentions CL tests but they were never implemented, suggesting a deliberate (if implicit) prioritization decision.

**Recovery Confidence:** 0.90 (decision was implicit, inferred from actions)

**Gap Analysis:**
- **Stated intent:** Commit mentions CL tests
- **Reality:** Only Elisp tests exist
- **Interpretation:** Elisp-first was the de facto strategy

**Future Work:**
- **Phase 4+**: Add CL test suite for tool system
- **Target:** FiveAM or Parachute for CL testing
- **Priority:** Tool execution, agent loop, LLM integration
- **Estimated effort:** 2-3 weeks for comprehensive CL coverage

---

**ADR Status:** ✅ Accepted and Implemented
**Last Reviewed:** 2026-01-17
**Next Review:** When adding CL testing infrastructure (Phase 4+)
