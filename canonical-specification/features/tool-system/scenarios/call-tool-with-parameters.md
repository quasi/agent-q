---
type: scenario
name: call-tool-with-parameters
version: 1.0.0
feature: tool-system
covers:
  - tool-protocol
tags:
  - happy-path
  - execution
---

# Scenario: Call Tool with Parameters

**Feature:** tool-system
**User Story:** As the LLM, I want to call tools with parameters so I can perform actions on behalf of the user.
**Test Coverage:** Integration tests verify tool execution
**Confidence:** 0.90

---

## Context

The tool protocol defines how LLM tool calls are translated into Lisp function calls and how results are formatted back to the LLM.

---

## Main Flow

### Step 1: LLM Generates Tool Call

**LLM Response:**
```json
{
  "tool_calls": [
    {
      "id": "call_abc123",
      "name": "describe_symbol",
      "arguments": {
        "symbol_name": "make-context-item",
        "package": "agent-q"
      }
    }
  ]
}
```

### Step 2: Tool Executor Validates and Executes

**Validation:**
1. Tool name exists in registry ✓
2. Required parameters present ✓
3. Parameter types valid ✓

**Execution:**
```lisp
(execute-tool "describe_symbol"
              '(("symbol_name" . "make-context-item")
                ("package" . "agent-q")))
```

### Step 3: Handler Returns Result

**Handler Output:**
```lisp
"Function MAKE-CONTEXT-ITEM in package AGENT-Q
Arguments: (content &key type metadata)
Returns: context-item
Creates a new context item with the given content..."
```

### Step 4: Result Formatted for LLM

**Tool Result:**
```json
{
  "id": "call_abc123",
  "success": true,
  "content": "Function MAKE-CONTEXT-ITEM in package AGENT-Q\\nArguments: (content &key type metadata)\\nReturns: context-item\\nCreates a new context item..."
}
```

---

## Postconditions

1. Tool executed successfully
2. Result returned to LLM
3. No side effects for safe tools
4. State modified appropriately for stateful tools

---

**Scenario Status:** ✅ Verified (integration tests pass)
**Confidence:** 0.90
