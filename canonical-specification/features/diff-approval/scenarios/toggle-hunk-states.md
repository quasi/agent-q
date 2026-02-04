---
type: scenario
name: toggle-hunk-states
version: 1.0.0
feature: diff-approval
covers:
  - propose-file-edit
tags:
  - edge-case
  - user-interaction
---

# Scenario: Toggle Hunk States

**Flow:** User changes mind, toggles hunk states
**Confidence:** 0.95

---

## Context

User applies hunk, then realizes mistake and wants to undo visually.

---

## Actions

1. Press `a` on hunk → applied (file modified)
2. Press `SPC` on same hunk → toggle to nil (visual only)
3. **CRITICAL:** File change remains (applied hunks can't be unapplied)

---

## Result

**Visual State:** Hunk shows as pending (nil)
**File State:** Hunk still applied (irreversible)
**Decision on exit:** Depends on final state count

---

## Property

See **irreversibility.md** - Applied hunks cannot be unapplied from UI

---

**Status:** ⚠️ Edge case, confusing UX (toggle doesn't undo file changes)
