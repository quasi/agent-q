# ADR-0001: Session-Conversation Unification

**Status:** Accepted
**Date:** 2026-01-17 (decision), recovered via git archaeology
**Deciders:** Development team
**Commit:** 555703e8

---

## Context

Agent-Q initially had two separate conversation objects:
1. **Agent's conversation**: Stored in `(agent-conversation *current-agent*)`
2. **Session's conversation**: Stored in `(session-conversation session)`

When users sent messages, they were added to the agent's conversation. However, when sessions were saved to disk, only the session's conversation was persisted—which remained empty. This caused all messages to disappear after session save/reload.

### Problem Statement

**Root Cause:** Agent had its own conversation object, separate from the session's conversation. Messages were added to the agent's conversation but the session's conversation (which gets persisted) stayed empty.

**Impact:**
- High: Message history lost on session reload
- User trust: Persistence feature appeared broken
- Data integrity: Conversation state not actually persisted

**Discovery:** Found during integration testing of session management feature (Phase 3).

---

## Decision

**All conversation access now checks for an active session first and uses the session's conversation when available.**

Changed functions to implement session-first lookup:
- `send-to-agent` - Messages now added to session's conversation
- `agent-q-add-context` - Context added to session's conversation
- `agent-q-clear-context` - Clears session's conversation context
- `agent-q-get-context-summary` - Shows session's context
- `agent-q-get-conversation-history` - Returns session's messages
- `agent-q-new-conversation` - Creates new session, uses its conversation

### Decision Pattern

```lisp
;; Before: Always use agent's conversation
(add-context (conversation-context (agent-conversation *current-agent*))
             content)

;; After: Check for session first
(let ((conversation (or (and *session-manager*
                             (current-session *session-manager*)
                             (session-conversation (current-session *session-manager*)))
                        (agent-conversation *current-agent*))))
  (add-context (conversation-context conversation)
               content))
```

**Fallback:** If no session exists, fall back to agent's conversation for backward compatibility.

---

## Alternatives Considered

### Alternative 1: Sync Agent and Session Conversations

**Approach:** Keep both conversations, sync messages between them.

**Rejected because:**
- Complexity: Two sources of truth, sync logic error-prone
- Performance: Double storage, sync overhead
- Maintenance: Hard to reason about which is canonical

### Alternative 2: Remove Agent's Conversation Entirely

**Approach:** Only use session conversations, require session for all operations.

**Rejected because:**
- Breaking change: Legacy code assumes agent has conversation
- Edge case: What if session creation fails?
- Migration: Would require updating all consumers

### Alternative 3: Make Sessions Own Agents

**Approach:** Each session has its own agent instance.

**Rejected because:**
- Architectural shift: Too large a change
- Resource usage: Multiple agents = more memory
- Complexity: Agent lifecycle management per session

---

## Consequences

### Positive

✅ **Message persistence works correctly**
- Messages added to session's conversation get persisted
- Session reload restores full message history

✅ **Single source of truth**
- Session's conversation is canonical when session exists
- No sync logic needed

✅ **Backward compatible**
- Falls back to agent's conversation if no session
- Legacy code paths still work

✅ **Minimal code change**
- Only changed conversation lookup pattern
- No new APIs or data structures

### Negative

⚠️ **Conditional logic in hot paths**
- Session-first check on every conversation access
- Slightly more complex lookup pattern

⚠️ **Agent's conversation becomes vestigial**
- Still exists but rarely used (only when no session)
- Could be confusing for new contributors

⚠️ **Implicit coupling**
- Functions now implicitly depend on `*session-manager*` global
- Makes testing slightly harder (need to mock session state)

---

## Implementation

### Changed Files

- `src/agent.lisp` - `send-to-agent` uses session conversation
- `src/sly-interface.lisp` - All RPC endpoints updated with session-first pattern

### Code Example

```lisp
;; src/sly-interface.lisp:13
(defun agent-q-add-context (content &key (type :code) metadata)
  "Add CONTENT to the current context.
   Uses session's conversation if available."
  (ensure-agent)
  (let ((conversation (or (and *session-manager*
                               (current-session *session-manager*)
                               (session-conversation (current-session *session-manager*)))
                          (agent-conversation *current-agent*))))
    (add-context (conversation-context conversation)
                 content
                 :type type
                 :metadata metadata))
  t)
```

### Testing

**Manual Test:**
1. Create session
2. Send message
3. Save session
4. Switch to different session
5. Switch back to original session
6. ✅ Message history restored

**Test Result:** Messages persist correctly across session switches and Emacs restarts.

---

## Experience Report

### What Worked Well

- Bug fix was surgical—changed only conversation lookup
- Fallback to agent's conversation prevented breaking changes
- Pattern is consistent across all affected functions

### What Could Be Improved

- Could add explicit test for this behavior
- Could deprecate agent's conversation in future
- Could document the session-first pattern more prominently

### Lessons Learned

- **Dual state is dangerous**: Two conversation objects was confusing
- **Test persistence early**: Integration testing caught this before users did
- **Fallback patterns prevent breaks**: Backward compatibility saved legacy code

---

## Related Decisions

- **ADR-0003**: Elisp-First Testing Strategy (explains why caught in integration tests)
- **ADR-0005**: Phased Chat Development (session management was Phase 3)

---

## References

- **Commit:** [555703e8](commit:555703e8) - "fix(session): connect agent conversation to session for persistence"
- **Related Commits:**
  - [3f5acf4](commit:3f5acf4) - "docs: expand session persistence test with file verification"
- **Specification:** Session management not formally specified (see gap in triangulation report)
- **Code:** `src/sly-interface.lisp`, `src/agent.lisp`

---

## Notes

This decision was **recovered via git archaeology** during Canon initiation (Pass 5: Rationale Recovery). The original commit message provided excellent root cause analysis, making recovery straightforward.

**Recovery Confidence:** 1.00
**Decision Quality:** Pragmatic bug fix with minimal disruption

---

**ADR Status:** ✅ Accepted and Implemented
**Last Reviewed:** 2026-01-17
**Next Review:** When refactoring agent/session architecture (Phase 4+)
