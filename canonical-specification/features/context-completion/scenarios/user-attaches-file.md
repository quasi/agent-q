---
type: scenario
name: user-attaches-file
version: 1.0.0
feature: context-completion
covers:
  - pill-protocol
tags:
  - happy-path
  - user-workflow
---

# Scenario: User Attaches File Context

**Flow:** Complete @-mention workflow
**Confidence:** 1.00

---

## Preconditions

- Chat buffer open
- User wants to ask about a specific file

---

## Flow

1. **User Types:** "@src/ag" in input region
2. **Completion Triggers:** After 2 characters ("@s")
3. **Candidates Shown:** Files matching "src/ag"
   - src/agent.lisp
   - src/agent-q.asd
4. **User Selects:** "src/agent.lisp"
5. **Pill Created:** `[@src/agent.lisp]` inserted
6. **Context Added:** File path stored in buffer-local list
7. **User Types Message:** "Explain the process-instruction function"
8. **User Sends:** `C-c RET`
9. **Content Fetched:** File read (lazy, at send time)
10. **LLM Receives:**
    ```
    User message: Explain the process-instruction function

    <context type="file" path="src/agent.lisp">
    (in-package :agent-q)
    (defun process-instruction ...)
    </context>
    ```

---

## Postconditions

- Pill visible in input (until message sent)
- Context attached to message
- LLM has file content for reference
- Panel shows attached file

---

## Timing

- Completion: < 50ms
- Pill creation: < 5ms
- Content fetch (at send): 10-100ms depending on file size
- Total overhead: ~100ms

---

**Status:** ✅ Core workflow, 99 tests verify
