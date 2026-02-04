---
type: contract
name: dual-region-layout
version: 1.0.0
feature: chat-interface
depends_on: []
---

# Contract: Dual-Region Layout

**Confidence:** 1.00
**Source:** sly-agent-q-chat.el, Phase 1 foundation

---

## Purpose

Buffer divided into two regions: conversation (read-only) and input (editable).

---

## JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Dual-Region Layout",
  "description": "Chat buffer with read-only conversation and editable input regions",
  "properties": {
    "conversation_marker": {
      "type": "integer",
      "description": "Buffer position marking end of conversation region",
      "minimum": 1
    },
    "input_marker": {
      "type": "integer",
      "description": "Buffer position marking start of input region",
      "minimum": 1
    },
    "conversation_readonly": {
      "type": "boolean",
      "description": "Whether conversation region is read-only",
      "const": true
    },
    "input_editable": {
      "type": "boolean",
      "description": "Whether input region is editable",
      "const": true
    }
  },
  "required": ["conversation_marker", "input_marker"]
}
```

---

## Structure

```
Buffer:
  ├─ Conversation Region (1 → conversation-marker)
  │    - Read-only
  │    - Message history
  │    - Auto-scrolls on new messages
  └─ Input Region (input-marker → point-max)
       - Editable
       - User types here
       - Cleared after send
```

---

## Markers

- `sly-agent-q-chat--conversation-marker`: End of conversation, beginning of input
- `sly-agent-q-chat--input-marker`: Start of input region
- Both are buffer-local variables

---

## Invariants

1. Conversation region MUST be read-only
2. Input region MUST be editable
3. Markers MUST advance as content added
4. No overlap between regions

---

## Operations

**Insert Message:** Add to conversation region before marker
**Send Message:** Extract from input region, clear input
**Navigate:** Scroll conversation independently of input

---

## Error Conditions

| Error | Cause | Recovery |
|-------|-------|----------|
| Marker corruption | Markers become nil or invalid positions | Reinitialize buffer with `agent-q-chat-mode` |
| Region overlap | Markers point to same position | Reset markers to valid positions |
| Read-only violation | Attempt to edit conversation region | Emacs signals read-only error automatically |
| Buffer killed | Chat buffer closed unexpectedly | Session persists; reopen with `agent-q-switch-session` |

**Prevention:**
- Markers are buffer-local and automatically managed
- Read-only text properties enforced by Emacs
- Marker positions validated before operations

**Handling:**
- Most errors are prevented by Emacs text properties
- Marker corruption auto-recovers on mode re-initialization
- No user intervention typically required

---

**Status:** ✅ Tested (2 tests)
