# Contract: Dual-Region Layout

**Confidence:** 1.00
**Source:** sly-agent-q-chat.el, Phase 1 foundation

---

## Purpose

Buffer divided into two regions: conversation (read-only) and input (editable).

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

**Status:** ✅ Tested (2 tests)
