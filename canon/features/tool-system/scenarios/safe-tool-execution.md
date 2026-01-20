# Scenario: Safe Tool Execution

**Category:** Introspection
**Safety Level:** Safe (auto-execute)
**Confidence:** 1.00
**Source:** Manual testing, PHASE-2-SPEC.md

---

## Context

User asks about a Lisp function. LLM decides to use the `describe_symbol` tool to get detailed information.

---

## Preconditions

- Agent initialized and connected
- Tool system registered (18 tools available)
- LLM has access to tool schema
- User message: "What does the function `defun` do?"

---

## Flow

### 1. LLM Decision

LLM determines it needs symbol information:
```json
{
  "role": "assistant",
  "content": null,
  "tool_calls": [{
    "id": "call_abc123",
    "type": "function",
    "function": {
      "name": "describe_symbol",
      "arguments": "{\"symbol\": \"defun\", \"package\": \"common-lisp\"}"
    }
  }]
}
```

### 2. Tool Extraction

Agent extracts tool call:
```lisp
(extract-tool-calls llm-response)
→ ((:id "call_abc123"
    :name "describe_symbol"
    :arguments (:symbol "defun" :package "common-lisp")))
```

### 3. Tool Lookup

```lisp
(get-tool "describe_symbol")
→ #<TOOL describe_symbol safety=:safe>
```

### 4. Safety Check

```lisp
(eq (tool-safety-level tool) :safe)
→ T  ; No approval needed, execute immediately
```

### 5. Handler Invocation

```lisp
(funcall (tool-handler tool)
         (make-hash-table-args :symbol "defun" :package "common-lisp"))
→ "DEFUN [macro]
Documentation: Define a function.
Lambda list: (NAME LAMBDA-LIST &BODY BODY)
..."
```

### 6. Result Formatting

```lisp
(make-instance 'tool-result
  :id "call_abc123"
  :success t
  :content "DEFUN [macro]\nDocumentation: Define a function...")
```

### 7. LLM Continuation

Tool result sent back to LLM:
```json
{
  "role": "tool",
  "tool_call_id": "call_abc123",
  "content": "DEFUN [macro]\nDocumentation: Define a function..."
}
```

### 8. LLM Response

```json
{
  "role": "assistant",
  "content": "DEFUN is a macro in Common Lisp used to define functions. It takes a function name, a lambda list (parameters), and a body of expressions..."
}
```

---

## Postconditions

- ✅ Tool executed successfully
- ✅ No user approval required
- ✅ Result returned to LLM
- ✅ User receives informative answer
- ✅ Conversation continues naturally

---

## Timing

- Tool lookup: < 1ms
- Handler execution: 5-50ms (depends on symbol complexity)
- Result formatting: < 1ms
- **Total:** < 100ms for typical introspection

---

## Variants

### Variant 1: Symbol Not Found

**Input:** `{:symbol "nonexistent"}`

**Result:**
```lisp
(make-instance 'tool-result
  :id "call_xyz"
  :success nil
  :error "Symbol NONEXISTENT not found in package COMMON-LISP")
```

**LLM Response:** "I couldn't find that symbol. Could you check the spelling?"

### Variant 2: Multiple Tools

LLM calls multiple tools in sequence:
1. `describe_symbol` → Get basic info
2. `function_arglist` → Get detailed parameters
3. `who_calls` → Find usage examples

Each executes independently, results combined by LLM.

---

## References

- **tool-protocol.md** - Tool definition
- **tool-execution.md** - Execution flow
- **PHASE-2-SPEC.md:277-310** - Introspection tools specification

---

**Scenario Status:** ✅ Verified via manual testing
**Last Updated:** 2026-01-20
