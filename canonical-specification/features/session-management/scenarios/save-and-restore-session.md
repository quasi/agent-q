---
type: scenario
name: save-and-restore-session
version: 1.0.0
feature: session-management
covers:
  - session-serialization
tags:
  - persistence
  - happy-path
---

# Scenario: Save and Restore Session

**Feature:** session-management
**User Story:** As a user, I want my conversations to persist across Emacs restarts so I can resume work seamlessly.
**Test Coverage:** Integration tests verify serialization round-trips
**Confidence:** 0.90

---

## Context

Sessions must be serialized to disk and deserialized on load, preserving conversation history, context items, and metadata.

---

## Main Flow

### Step 1: User Closes Emacs (Auto-Save Triggered)

**Current Session State:**
```lisp
#<SESSION
  id: "session-20260203-143000-A4F2"
  name: "Bug fix session"
  messages: 5
  context-items: 3
  created: 2026-02-03T14:30:00
  updated: 2026-02-03T15:45:00>
```

### Step 2: Session Serialized to Disk

**Serialization Process:**
```lisp
(serialize-session session)
```

**Output (SQLite + JSON):**
- Session metadata stored in SQLite
- Conversation messages serialized to JSON
- Context items serialized to JSON
- File saved to `~/.agent-q/sessions/session-20260203-143000-A4F2.json`

### Step 3: User Restarts Emacs

**Emacs loads session list from SQLite**

### Step 4: User Switches to Saved Session

**Command:** `M-x agent-q-switch-session` → Select "Bug fix session"

**Deserialization:**
```lisp
(deserialize-session "session-20260203-143000-A4F2")
```

**Loaded Session:**
```lisp
#<SESSION
  id: "session-20260203-143000-A4F2"
  name: "Bug fix session"
  messages: 5  ; ← All messages restored
  context-items: 3  ; ← All context restored
  created: 2026-02-03T14:30:00
  updated: 2026-02-03T15:45:00>
```

---

## Postconditions

1. Session state identical before save and after load
2. All messages preserved
3. All context items preserved
4. Metadata (timestamps, token counts) preserved
5. User can continue conversation seamlessly

---

## Serialization Details

**Format:** JSON with ISO 8601 timestamps
**Timestamp Handling:** Elisp 4-tuple timestamps converted to Universal Time for Common Lisp
**Symbol Handling:** Keywords preserved, other symbols converted to strings
**Encoding:** UTF-8

---

**Scenario Status:** ✅ Verified (serialization round-trips tested)
**Confidence:** 0.90
