# Scenario: Accept All Hunks

**Flow:** User approves all proposed changes
**Confidence:** 1.00

---

## Context

Diff has 3 hunks, user wants to apply all.

---

## Actions

1. Press `n` to navigate to first hunk
2. Press `a` to apply first hunk
3. Press `n` to next hunk
4. Press `a` to apply second hunk
5. Press `n` to next hunk
6. Press `a` to apply third hunk
7. Press `q` to finish

---

## Result

**Decision:** `"accepted"`
**File:** All 3 hunks applied
**LLM sees:** "accepted: Applied 3 hunks to <path>"

---

**Status:** ✅ Common case, well-tested
