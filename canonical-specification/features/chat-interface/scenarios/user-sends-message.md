---
type: scenario
name: user-sends-message
version: 1.0.0
feature: chat-interface
covers:
  - dual-region-layout
tags:
  - happy-path
  - core-workflow
---

# Scenario: User Sends Message

**Flow:** Complete send workflow
**Confidence:** 1.00

---

## Preconditions

- Chat buffer open
- Session active
- User types message in input region

---

## Flow

1. **User Types:** "What does defun do?"
2. **User Presses:** `C-c RET` (or `RET` if configured)
3. **System:**
   - Extracts input text
   - Creates message in session
   - Clears input region
   - Displays user message in conversation
   - Sends to agent (RPC call)
4. **Agent Response:** Streams back
5. **Display:** Shows assistant response with markdown

---

## Postconditions

- Input region cleared
- User message visible in conversation
- Assistant response streaming/complete
- Session timestamp updated

---

## Timing

- Input extraction: < 1ms
- RPC call: 50-200ms
- First token: 500-2000ms
- Full response: 2-10 seconds

---

**Status:** ✅ Core workflow, tested
