---
type: scenario
name: accept-selective-hunks
version: 1.0.0
feature: diff-approval
covers:
  - propose-file-edit
tags:
  - partial-approval
  - user-control
---

# Scenario: Accept Selective Hunks

**Flow:** User approves some changes, rejects others
**Confidence:** 1.00

---

## Context

Diff has 3 hunks:
1. Add docstring (user wants)
2. Refactor variable name (user doesn't want)
3. Fix typo (user wants)

---

## Actions

1. Press `a` on hunk 1 → applied
2. Press `r` on hunk 2 → rejected (no file change)
3. Press `a` on hunk 3 → applied
4. Press `q`

---

## Result

**Decision:** `"accepted"` (at least one applied)
**File:** Hunks 1 and 3 applied, hunk 2 not applied
**LLM sees:** "accepted: Applied 2 of 3 hunks to <path>"

---

**Status:** ✅ Common case, demonstrates granular control
