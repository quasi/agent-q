# Scenario: Reject All Hunks

**Flow:** User declines all proposed changes
**Confidence:** 1.00

---

## Actions

1. Press `r` on each hunk (or just press `q` without applying any)
2. Press `q`

---

## Result

**Decision:** `"rejected"`
**File:** Unchanged (no hunks applied)
**LLM sees:** "rejected: No changes applied to <path>"

---

**Note:** Pressing `q` without applying any hunks also results in "rejected"

---

**Status:** ✅ Tested, user control
