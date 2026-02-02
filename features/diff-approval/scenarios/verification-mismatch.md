# Scenario: Verification Mismatch

**Flow:** Original content doesn't match current file
**Confidence:** 0.90

---

## Context

LLM's original content is stale (file was modified since LLM read it).

---

## Flow

1. Tool generates diff from original → modified
2. Emacs attempts to apply to actual file
3. `diff-apply-hunk` may fail if context doesn't match

---

## Result

**If mismatch:** Diff application fails, user sees error
**Current behavior:** Error not explicitly handled in spec
**Gap:** No formal error recovery for verification mismatch

---

## Recommendation

Future: Add verification step comparing `original` to actual file content before showing diff.

---

**Status:** ⚠️ Known gap in specification (mentioned in feature.yaml)
