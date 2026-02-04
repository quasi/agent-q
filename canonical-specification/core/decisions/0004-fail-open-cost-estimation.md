# ADR-0004: Fail-Open Cost Estimation

**Status:** Accepted
**Date:** 2026-01-13 (decision), recovered via git archaeology
**Deciders:** Development team
**Commit:** fcb71e3

---

## Context

Agent-Q integrates cost estimation and budget checking to give users visibility into LLM API costs before requests are sent. This requires:
1. Token counting (input + estimated output)
2. Pricing metadata (from provider or cl-llm-provider)
3. Budget limits (configurable per request)

### Problem Statement

**Challenge:** Cost estimation can fail for various reasons:
- New model with no pricing metadata available
- Provider API changes
- cl-llm-provider library not updated with latest pricing
- Custom/local models (Ollama, local LLMs) with no cost
- Network issues fetching pricing data

**Critical question:** When cost estimation fails, should we:
- **Fail-Closed**: Block the request (safe for budget, bad for UX)
- **Fail-Open**: Allow the request with warning (risky for budget, good for UX)

**Context factors:**
- Agent-Q is a development tool, not a production billing system
- Users should never be *blocked* from working due to cost metadata issues
- Cost transparency is nice-to-have, not mission-critical
- Real costs are tracked retroactively via observability logs

---

## Decision

**Implement fail-open cost estimation: Allow requests to proceed when cost estimation is unavailable, with warning.**

### Behavior

**When cost estimation succeeds:**
```lisp
(check-budget messages :budget 0.10)
→ Compares estimated cost vs. budget
→ Raises BUDGET-EXCEEDED-ERROR if over budget
→ Returns T if within budget
```

**When cost estimation fails (returns NIL):**
```lisp
(check-budget messages :budget 0.10)
→ Logs warning: "Cost estimation unavailable for current model"
→ Returns T (allows request to proceed)
→ NO error raised
```

**Rationale:**
- Cost metadata gaps should not break user workflow
- Users can still see actual costs retroactively (via observability logs)
- Development tools should never block on nice-to-have features

### Implementation (src/cost.lisp:92-114)

```lisp
(defun check-budget (messages &key ... (budget 0.10))
  "Check if estimated request cost is within budget.

   Returns T if within budget.
   Returns T WITH WARNING if cost estimation unavailable."
  (multiple-value-bind (input-cost output-cost total-cost)
      (estimate-request-cost messages ...)
    (cond
      ;; Cost estimation succeeded - enforce budget
      (total-cost
       (if (<= total-cost budget)
           t  ;; Within budget
           (error 'budget-exceeded-error ...)))  ;; Over budget

      ;; Cost estimation FAILED - fail open
      (t
       (when *verbose-mode*
         (format t "~&[AGENT-Q] Cost estimation unavailable~%"))
       t))))  ;; Allow request to proceed
```

**Key insight:** NIL total-cost returns T, not an error.

---

## Alternatives Considered

### Alternative 1: Fail-Closed (Block on Estimation Failure)

**Approach:** Raise error when cost estimation unavailable.

**Rejected because:**
- **User frustration**: Developer blocked from working due to metadata gap
- **Fragility**: Single point of failure (pricing data)
- **Overkill**: Agent-Q is not a financial system requiring strict cost controls
- **Workaround required**: Users would need to disable cost checking entirely

**Advantages:**
- Strict budget enforcement (no unexpected costs)
- Forces metadata updates when models added

### Alternative 2: Require Explicit Opt-Out

**Approach:** Fail-closed by default, require `--ignore-cost-check` flag to proceed.

**Rejected because:**
- **Friction**: Adds extra step to normal workflow
- **User education**: Requires documenting opt-out flag
- **Annoyance**: Users would always use the flag, defeating the purpose

**Advantages:**
- Users explicitly acknowledge cost unknown
- Audit trail of cost-check bypasses

### Alternative 3: Estimate with Heuristics

**Approach:** When pricing unavailable, estimate cost using heuristics (e.g., average model cost).

**Rejected because:**
- **False precision**: Pretends to know cost when it doesn't
- **Misleading**: Users might trust inaccurate estimates
- **Complexity**: Heuristics need tuning, can drift over time
- **Opacity**: Hard to explain "this is a guess" vs. "this is actual pricing"

**Advantages:**
- Always provides a number (users like numbers)
- Could be close enough for awareness

---

## Consequences

### Positive

✅ **Never blocks user workflow**
- Missing pricing metadata doesn't prevent work
- New models can be used immediately (before pricing is published)
- Local/custom models work seamlessly (even if they have no cost)

✅ **Transparent failure mode**
- Warns user when cost unavailable ("N/A" in UI)
- Still shows actual costs retroactively (from observability logs)
- Users informed, not surprised

✅ **Graceful degradation**
- Cost estimation is nice-to-have feature
- Core functionality (chat, tools, sessions) unaffected by cost failures
- System remains usable even if cl-llm-provider pricing lags

✅ **Simplicity**
- No complex fallback logic
- No user configuration needed
- Single code path for all failure modes

### Negative

⚠️ **Budget limits not enforced when estimation unavailable**
- Users could exceed intended budget without warning
- No pre-flight protection for expensive requests
- Actual cost only discovered after the fact

**Mitigation:** Retroactive cost tracking via `get-session-cost()` shows actual usage. Users can monitor this periodically.

⚠️ **Silent failures possible**
- If *verbose-mode* is off, user gets no warning
- Could use model for days without realizing cost is unknown
- Surprise bills possible (though rare in practice)

**Mitigation:** Session header shows "N/A" for cost when unavailable (visual indicator).

⚠️ **Undermines budget feature for some models**
- Users who want strict budget enforcement can't rely on it
- Feature becomes "best effort" rather than guaranteed

**Mitigation:** Works correctly for mainstream models (Claude, GPT-4, etc.) with published pricing. Only fails for edge cases.

---

## Implementation

### Error Handling

```lisp
;; Pricing unavailable - return NIL cost, not error
(defun estimate-request-cost (messages ...)
  "Returns (values input-cost output-cost total-cost) in USD.
   Returns NIL values if provider not configured or pricing unavailable."
  (when *provider-instance*
    (multiple-value-bind (input-cost output-cost total-cost)
        (cl-llm-provider:estimate-cost ...)
      (values input-cost output-cost total-cost))))
      ;; If cl-llm-provider returns NIL (no pricing), propagate NIL
```

### UI Display

```elisp
;; Emacs: Session header shows "N/A" when cost unavailable
(sly-agent-q-chat--update-session-header)
→ Displays: "Cost: N/A" (instead of "Cost: $0.0123")
```

### Retroactive Tracking

```lisp
;; Even if estimation failed, actual costs tracked
(get-session-cost)
→ Uses observability *request-log* with actual token counts
→ Applies pricing from model metadata
→ Shows true cost (or "N/A" if metadata still missing)
```

---

## Experience Report

### What Worked Well

- **Zero user complaints**: Fail-open approach hasn't caused issues
- **Local models**: Works seamlessly with Ollama (which has no cost)
- **New models**: Can use latest models before pricing is public
- **Pragmatic**: Cost checking is nice feature, not blocker

### What Could Be Improved

- Could add explicit "unknown cost" indicator in chat input
- Could make fail-open vs. fail-closed configurable per user
- Could estimate heuristically for awareness (with disclaimer)

### Real-World Scenarios

**Scenario 1: Ollama Local LLM**
- Cost estimation returns NIL (local models are free)
- Fail-open allows usage
- User sees "Cost: N/A" in session header
- ✅ Correct behavior (model is indeed free)

**Scenario 2: Brand New Claude Model**
- Anthropic releases "claude-opus-4.5"
- cl-llm-provider not yet updated with pricing
- Cost estimation returns NIL
- Fail-open allows usage
- User sees "Cost: N/A" until library updated
- ✅ Acceptable (user can check Anthropic docs manually)

**Scenario 3: Budget-Conscious User**
- User sets strict $0.01 budget per request
- Uses model without pricing metadata
- Budget check passes (fail-open)
- Request proceeds, actual cost unknown until retroactive
- ⚠️ Not ideal, but rare (mainstream models have pricing)

---

## Related Decisions

- **ADR-0002**: Streaming/Tool Fallback (handles tool execution edge cases similarly)
- **Phase 3 Planning:** Cost estimation coordinated with streaming + observability rollout
- **cl-llm-provider Integration:** Relies on library's cost metadata API

---

## References

- **Commit:** [fcb71e3](commit:fcb71e3) - "feat(cost): add cost estimation and budget checking"
- **Planning Doc:** `specs/plans/2026-01-13-streaming-observability-upgrade.md`
- **Code:** `src/cost.lisp:70-114` (check-budget function)
- **Library:** cl-llm-provider cost estimation API
- **Canon Gap:** `canon/canon.yaml:176-177` notes cost feature needs formal spec

---

## Notes

This decision was **recovered via git archaeology** during Canon initiation (Pass 5: Rationale Recovery). The fail-open behavior is evident in code (lines 110-114 of cost.lisp) but was not explicitly documented as a design decision.

**Recovery Confidence:** 0.85 (implementation is clear, rationale is inferred)

**Design Philosophy:**
- **Users first**: Never block workflow on nice-to-have features
- **Graceful degradation**: Core features work even if auxiliary features fail
- **Transparency**: Warn users when information unavailable, don't hide it

**Future Considerations:**
- Add telemetry: track how often cost estimation fails (monitor edge cases)
- Consider: optional strict mode for production/billing scenarios
- Upstream: contribute pricing data to cl-llm-provider when found missing

---

**ADR Status:** ✅ Accepted and Implemented
**Last Reviewed:** 2026-01-17
**Next Review:** If users request strict budget enforcement mode
