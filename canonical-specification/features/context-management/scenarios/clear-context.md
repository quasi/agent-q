---
type: scenario
name: clear-context
version: 1.0.0
feature: context-management
covers:
  - context-manager
tags:
  - user-story
---

# Scenario: Clear Context

**Feature:** context-management
**User Story:** As a user, I want to clear all accumulated context when starting a new task so unrelated code doesn't confuse the LLM.
**Test Coverage:** agent-q-context/commands/clear-context (sly-agent-q-context-test.el:752)
**Confidence:** 1.00

---

## Context

During long sessions, users accumulate context from multiple tasks. When switching to a new, unrelated task, old context becomes noise. Users need a way to reset context to empty state.

---

## Actors

- **User**: Developer switching tasks
- **Emacs UI**: `sly-agent-q-clear-context` command
- **Context Manager**: Clears items vector

---

## Preconditions

1. Context manager contains N items (0 ≤ N ≤ 50)
2. User decides current context is no longer relevant

**Example Initial State:**
```
items = [ctx-15, ctx-16, ..., ctx-25]  # 11 items from previous task
```

---

## Main Flow

### Step 1: User Invokes Clear

**Action:** `M-x agent-q-clear-context` or keybinding

**Elisp Behavior:**
```elisp
(defun agent-q-clear-context ()
  "Clear all accumulated context."
  (interactive)
  (when (yes-or-no-p "Clear all context? ")
    (sly-eval '(cl:agent-q-clear-context))
    (message "Context cleared")))
```

**User Confirmation:** Prompts "Clear all context?" to prevent accidents

### Step 2: RPC to Lisp

**Call:** `(sly-eval '(cl:agent-q-clear-context))`

**Lisp Handler:**
```lisp
;; From src/sly-interface.lisp
(defun agent-q-clear-context ()
  "Clear all context items."
  (ensure-agent)
  (let ((conversation (or (and *session-manager*
                               (current-session *session-manager*)
                               (session-conversation (current-session *session-manager*)))
                          (agent-conversation *current-agent*))))
    (clear-context (conversation-context conversation)))
  t)
```

### Step 3: Context Manager Clears

**Method:**
```lisp
;; From src/context.lisp:80
(defmethod clear-context ((manager context-manager))
  "Clear all context items."
  (setf (context-items manager)
        (make-array 0 :adjustable t :fill-pointer 0)))
```

**Behavior:** Replaces existing vector with empty vector

---

## Postconditions

1. Manager contains 0 items
2. `(length (context-items manager))` = 0
3. `(get-context manager)` returns empty list
4. Next LLM prompt has no context section (or empty "## Context")
5. **All previous items are garbage collected** (no recovery possible)

---

## Alternative Flows

### Alt 1: User Cancels Confirmation

**Trigger:** User answers "no" to confirmation prompt

**Behavior:**
```elisp
(when (yes-or-no-p "Clear all context? ")
  ...)  ; Body not executed if no
```

**Result:** Context unchanged, no RPC call made

### Alt 2: Clear Empty Context

**Setup:** Manager already empty (0 items)

**Behavior:** `clear-context` still replaces vector (idempotent operation)

**Result:** Manager remains empty (no error)

---

## Error Flows

### No Errors Expected

**Rationale:**
- No validation needed (clearing is always valid)
- No network dependencies
- No disk I/O
- Simple array replacement

**Only Possible Failure:** Loss of Lisp connection during RPC
- **Result:** Elisp shows connection error
- **Recovery:** Context unchanged on Lisp side

---

## Verification

### Test: Clear Empties Context

**Test:** `agent-q-context/commands/clear-context` (sly-agent-q-context-test.el:752)

```elisp
(ert-deftest agent-q-context/commands/clear-context ()
  "Test that clear-context empties the context list."
  (with-temp-buffer
    (agent-q-chat-mode)
    (push (list :type :code :content "test") agent-q-context--items)
    (agent-q-clear-context)
    (should (= 0 (length agent-q-context--items)))))
```

**Status:** ✅ Pass

### Test: Clear is Interactive

**Test:** `agent-q-context/commands/clear-context-is-interactive` (sly-agent-q-context-test.el:761)

```elisp
(ert-deftest agent-q-context/commands/clear-context-is-interactive ()
  "Test that clear-context is an interactive command."
  (should (commandp 'agent-q-clear-context)))
```

**Status:** ✅ Pass

---

## Example: Complete Workflow

### Scenario: Task Switch

```
# Initial state: Debugging task
items = [ctx-20, ctx-21, ..., ctx-30]  # 11 items related to Bug #123

# User finishes debugging, starts new feature
User: M-x agent-q-clear-context
Prompt: "Clear all context? (yes or no)"
User: yes
Message: "Context cleared"

# New state: Clean slate
items = []  # Empty, ready for new task context
```

### Before Clear

```markdown
LLM Prompt:
## Context

### Code (from src/bug-fix.lisp:10-20)
```lisp
(defun problematic-function () ...)
```

### Error
```lisp
ERROR: Division by zero
```

<user>Start new feature...</user>
```

### After Clear

```markdown
LLM Prompt:
## Context

<user>Start new feature...</user>
```

**Impact:** LLM no longer sees old debugging context

---

## Use Cases

### Use Case 1: Task Boundaries

**When:** Switching from debugging to feature development
**Why:** Prevent old error messages from biasing LLM

### Use Case 2: Context Cleanup

**When:** Accumulated 50 items, many irrelevant
**Why:** Faster to clear and re-add essentials than manually manage

### Use Case 3: Privacy

**When:** About to share session or screenshot
**Why:** Remove sensitive code/data from context

### Use Case 4: Testing

**When:** Testing LLM behavior with specific context
**Why:** Ensure clean state for reproducible tests

---

## Related Scenarios

- **add-context-item.md**: Repopulating after clear
- **sliding-window-overflow.md**: Alternative to manual clearing (automatic eviction)

---

## Related Properties

- **sliding-window.md**: Clear is immediate alternative to gradual eviction

---

## Known Limitations

### L-001: No Undo

**Problem:** Clearing is irreversible
**Impact:** Accidental clear loses all context
**Mitigation:** Confirmation prompt reduces accidents
**Status:** Accepted (undo would require history tracking)

### L-002: All-or-Nothing

**Problem:** Cannot selectively remove items
**Impact:** Must clear everything or nothing
**Workaround:** Clear, then re-add desired items
**Recommendation:** Add selective removal in future

---

## UI Improvements (Future)

### Improvement 1: Visual Confirmation

Show count before clearing:
```
"Clear all context? (15 items will be removed)"
```

### Improvement 2: Context Snapshots

Allow saving context before clearing:
```elisp
(agent-q-save-context-snapshot "bug-123-context")
(agent-q-clear-context)
;; Later:
(agent-q-restore-context-snapshot "bug-123-context")
```

### Improvement 3: Selective Clear

Clear by type or age:
```elisp
(agent-q-clear-context-by-type :error)  ; Remove only errors
(agent-q-clear-context-older-than (* 60 60))  ; Remove items >1 hour old
```

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | Clear-context implemented | Phase 1 foundation |
| 2026-01-17 | Scenario documented | Canon extraction |

---

**Scenario Status:** ✅ Verified (tested)
**Confidence:** 1.00
**Test Coverage:** ✅ Explicit tests pass
