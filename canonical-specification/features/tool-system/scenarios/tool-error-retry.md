---
type: scenario
name: tool-error-retry
version: 1.0.0
feature: tool-system
covers:
  - tool-execution
tags:
  - error-recovery
  - llm-retry
---

# Scenario: Tool Error and LLM Retry

**Category:** Error Recovery
**Safety Level:** N/A (demonstrates error handling)
**Confidence:** 1.00
**Source:** Manual testing, error-recovery property

---

## Context

User asks about a function but provides incorrect name (typo). LLM calls tool, receives error, and automatically retries with corrected input.

---

## Preconditions

- Agent initialized
- User message: "What does the function `proces-instruction` do?" (typo)

---

## Flow

### 1. First LLM Attempt (Incorrect)

```json
{
  "tool_calls": [{
    "id": "call_1",
    "function": {
      "name": "describe_symbol",
      "arguments": "{\"symbol\": \"proces-instruction\", \"package\": \"agent-q\"}"
    }
  }]
}
```

### 2. Tool Execution

```lisp
(let ((pkg (find-package "agent-q"))
      (sym (find-symbol "PROCES-INSTRUCTION" pkg)))
  sym)
→ NIL  ; Symbol not found
```

### 3. Error Result

```lisp
(make-instance 'tool-result
  :id "call_1"
  :success nil
  :error "Symbol PROCES-INSTRUCTION not found in package AGENT-Q (status: NIL)")
```

### 4. LLM Receives Error

```json
{
  "role": "tool",
  "tool_call_id": "call_1",
  "content": "Symbol PROCES-INSTRUCTION not found in package AGENT-Q"
}
```

### 5. LLM Analysis

LLM recognizes:
- Symbol name looks like typo (similar to known function)
- Can retry with corrected name
- No need to bother user with error details

### 6. LLM Retry (Corrected)

```json
{
  "content": null,
  "tool_calls": [{
    "id": "call_2",
    "function": {
      "name": "describe_symbol",
      "arguments": "{\"symbol\": \"process-instruction\", \"package\": \"agent-q\"}"
    }
  }]
}
```

### 7. Successful Execution

```lisp
(let ((sym (find-symbol "PROCESS-INSTRUCTION" (find-package "agent-q"))))
  (with-output-to-string (s)
    (describe sym s)))
→ "PROCESS-INSTRUCTION [function]
Documentation: Process a user instruction by sending to the LLM agent...
Lambda list: (INSTRUCTION &KEY STREAMING)
..."
```

### 8. Success Result

```lisp
(make-instance 'tool-result
  :id "call_2"
  :success t
  :content "PROCESS-INSTRUCTION [function]\nDocumentation: ...")
```

### 9. LLM Response to User

```json
{
  "role": "assistant",
  "content": "The function `process-instruction` (I noticed a small typo in your question) is used to process user instructions by sending them to the LLM agent..."
}
```

---

## Postconditions

- ✅ Initial error handled gracefully
- ✅ LLM self-corrected without user intervention
- ✅ User received answer despite typo
- ✅ No system crash or degradation

---

## Error Types and Recovery

| Error Type | Cause | LLM Recovery Strategy |
|------------|-------|----------------------|
| Symbol not found | Typo, wrong package | Retry with corrected name or different package |
| Function not bound | Not a function | Try as variable or class |
| Package not found | Wrong package name | List available packages, suggest closest match |
| Xref unavailable | SLYNK not loaded | Explain limitation, use alternative approach |

---

## Multi-Tool Recovery

Sometimes LLM chains tools for recovery:

1. `describe_symbol` fails (not found)
2. `apropos_search` with similar pattern (find candidates)
3. `describe_symbol` again with correct name from apropos results

---

## Timing

- First tool call (error): 5-10ms
- LLM analysis + retry decision: 200-500ms
- Second tool call (success): 5-50ms
- **Total:** < 1 second for error recovery

---

## Consequences

**Positive:**
- ✅ Robust to user typos and mistakes
- ✅ LLM learns from errors (within conversation)
- ✅ No manual error handling needed
- ✅ Graceful degradation

**Negative:**
- ⚠️ Extra LLM call adds 1-3 seconds latency and ~500-2000 tokens. Maximum 1 automatic retry per tool call.
- ⚠️ May retry wrong approach repeatedly
- ⚠️ User doesn't see intermediate errors (less transparency)

---

## Alternative: LLM Asks User

In some cases, LLM can't self-correct and asks user:

```
Assistant: I couldn't find a function called `proces-instruction`.
Did you mean one of these?
- process-instruction
- process-input
- process-response

Or please provide the correct name.
```

User responds, conversation continues.

---

## References

- **error-recovery.md** - Error handling property
- **tool-execution.md** - Executor error handling
- **safe-tool-execution.md** - Successful execution flow

---

**Scenario Status:** ✅ Verified via manual testing, common occurrence
**Last Updated:** 2026-01-20
