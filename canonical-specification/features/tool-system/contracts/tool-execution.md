---
type: contract
name: tool-execution
version: 0.2.0
feature: tool-system
depends_on: []
---

# Contract: Tool Execution

**Version:** 0.2.0
**Status:** Stable
**Confidence:** 0.85
**Source:** specs/PHASE-2-SPEC.md:214-273, src/tools/registry.lisp:execute-tool-calls

---

## Purpose

Executes tool calls from the LLM with safety enforcement, approval handling, and result formatting.

---

## JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Tool Execution",
  "description": "Schema for tool execution request and response",
  "definitions": {
    "tool_call": {
      "type": "object",
      "properties": {
        "id": {"type": "string", "description": "Unique call identifier"},
        "name": {"type": "string", "pattern": "^[a-z][a-z0-9_]*$"},
        "arguments": {"type": "object", "description": "Tool arguments"}
      },
      "required": ["id", "name", "arguments"]
    },
    "tool_result": {
      "type": "object",
      "properties": {
        "id": {"type": "string"},
        "success": {"type": "boolean"},
        "content": {"type": "string"},
        "metadata": {
          "type": "object",
          "properties": {
            "execution_time_ms": {"type": "number"},
            "safety_level": {"type": "string"},
            "approved": {"type": "boolean"}
          }
        }
      },
      "required": ["id", "success", "content"]
    }
  }
}
```

---

## Execution Flow

```
LLM Response (contains tool calls)
  ↓
execute-tool-calls (list of tool-call plists)
  ↓
For each tool-call:
  ├─→ Get tool from registry (by name)
  ├─→ Check safety level
  │     ├─→ :safe → execute immediately
  │     ├─→ :cautious → log, then execute
  │     └─→ :dangerous → request approval
  │           ├─→ approved → execute
  │           ├─→ denied → return error
  │           └─→ modified → execute with new params
  ├─→ Invoke handler with arguments
  ├─→ Format result for LLM
  └─→ Return tool-result object
```

---

## Core Function

### execute-tool-calls

```lisp
(execute-tool-calls tool-calls) → list of tool-results
```

**Parameters:**
- `tool-calls` - list of plists from LLM response

**Tool Call Format:**
```lisp
(:id "call_abc123"
 :name "describe_symbol"
 :arguments (:symbol "defun" :package "common-lisp"))
```

**Returns:** List of tool-result objects

---

## Tool Result Structure

```lisp
(defclass tool-result ()
  ((id      :initarg :id       ; Matches tool-call id
           :accessor tool-result-id)
   (success :initarg :success  ; t or nil
           :accessor tool-result-success)
   (content :initarg :content  ; Formatted result string
           :accessor tool-result-content)
   (error   :initarg :error    ; Error message if success=nil
           :accessor tool-result-error
           :initform nil)))
```

**Example Success:**
```lisp
(make-instance 'tool-result
  :id "call_abc123"
  :success t
  :content "DEFUN [function]
Documentation: Define a function...")
```

**Example Error:**
```lisp
(make-instance 'tool-result
  :id "call_abc123"
  :success nil
  :error "Symbol XYZ not found in package COMMON-LISP")
```

---

## Safety Enforcement

### Safe Tools (Immediate Execution)

```lisp
(when (eq (tool-safety-level tool) :safe)
  (execute-immediately tool args))
```

**Behavior:**
- No approval required
- Executed synchronously
- Result returned to agent

**Examples:** describe-symbol, apropos-search, function-arglist

### Cautious Tools (Logged Execution)

```lisp
(when (eq (tool-safety-level tool) :cautious)
  (log-tool-execution tool args)  ; Via observability hooks
  (execute-immediately tool args))
```

**Behavior:**
- No approval required
- Execution logged for auditing
- Result returned to agent

**Examples:** eval-form, compile-form

### Dangerous Tools (Approval Required)

```lisp
(when (and (eq (tool-safety-level tool) :dangerous)
           *approval-handler*)
  (let ((decision (funcall *approval-handler* tool args)))
    (case decision
      (:approved (execute-immediately tool args))
      (:denied (make-error-result "User denied tool execution"))
      ((:modified new-args) (execute-immediately tool new-args)))))
```

**Behavior:**
- Execution paused
- Approval handler called (Emacs UI prompt)
- User reviews and approves/denies/modifies
- Result returned after approval

**Examples:** write-file, propose-file-edit

---

## Approval Handler Protocol

### Signature

```lisp
*approval-handler* : (lambda (tool args) ...) → decision
```

**Parameters:**
- `tool` - tool object requesting approval
- `args` - hash table of arguments

**Returns:**
- `:approved` - Execute with original args
- `:denied` - Cancel execution, return error
- `(:modified new-args-hash)` - Execute with modified args

### Integration (Elisp Side)

```elisp
;; In sly-agent-q-tools.el
(defun sly-agent-q--approve-tool (tool-name tool-args)
  "Prompt user to approve dangerous tool execution."
  (let ((msg (format "Tool '%s' wants to execute with args: %S\nApprove?"
                     tool-name tool-args)))
    (if (y-or-n-p msg)
        :approved
      :denied)))
```

**Special Case - Diff Approval:**
For `propose_file_edit`, custom approval UI shows unified diff with per-hunk approval.

---

## Result Formatting

### format-tool-result

```lisp
(format-tool-result raw-result) → string
```

**Input Types → Output:**
- `string` → unchanged
- `nil` → "nil"
- `list` → pretty-printed s-expression
- `number` → `(prin1-to-string result)`
- `object` → `(prin1-to-string result)`

**Example:**
```lisp
(format-tool-result '(1 2 3))
→ "(1 2 3)"

(format-tool-result #<SYMBOL CAR>)
→ "#<SYMBOL COMMON-LISP:CAR>"
```

**LLM Context:**
Results sent back to LLM as tool response messages. LLM uses result to:
- Answer user's question
- Decide next action
- Request additional tools if needed

---

## Error Handling

### Handler Errors

```lisp
(handler-case
    (let ((result (funcall (tool-handler tool) args)))
      (make-success-result result))
  (error (e)
    (make-error-result (format nil "Tool error: ~A" e))))
```

**Behavior:**
- All errors caught (no unhandled conditions)
- Error message extracted and formatted
- LLM receives error as tool-result with `:success nil`
- Agent continues (doesn't crash on tool error)

### Unknown Tool

```lisp
(unless tool
  (make-error-result (format nil "Unknown tool: ~A" tool-name)))
```

**Behavior:**
- If tool not in registry, return error result
- LLM informed tool doesn't exist
- Can retry with correct tool name

---

## Observability Hooks

```lisp
(defvar *tool-execution-hooks* nil
  "List of hook functions: (lambda (phase tool args result) ...)")
```

**Phases:**
- `:before` - Before tool handler invoked
- `:after` - After successful completion
- `:error` - On handler error

**Hook Usage (Observability):**
```lisp
(push (lambda (phase tool args result)
        (when (eq phase :before)
          (log-info "Executing tool: ~A" (tool-name tool))))
      *tool-execution-hooks*)
```

**Integration:**
Hooks populated by `observability.lisp:setup-observability-hooks`

---

## Execution Metrics

**Not Currently Tracked:**
- Tool execution time
- Success/failure rates per tool
- Most frequently used tools
- Average result size

**Future Enhancement:** Add timing and analytics via hooks

---

## Invariants

1. **Result ID Match**: tool-result ID MUST match tool-call ID
2. **Success XOR Error**: Exactly one of `:success t` or `:error non-nil`
3. **Content Format**: Content MUST be string (for LLM consumption)
4. **Approval Blocking**: Dangerous tools MUST NOT execute without approval
5. **Error Recovery**: Errors MUST NOT propagate beyond executor (always caught)

---

## Integration with Agent Loop

```lisp
;; From src/agent.lisp
(when (tool-calls-p llm-response)
  (let ((tool-results (execute-tool-calls (extract-tool-calls llm-response))))
    ;; Send results back to LLM
    (continue-conversation-with-tool-results tool-results)))
```

---

## References

- **PHASE-2-SPEC.md:214-273** - Execution specification
- **src/tools/registry.lisp** - execute-tool-calls implementation
- **tool-protocol.md** - Tool handler contract
- **tool-registry.md** - Tool retrieval

---

**Contract Status:** ✅ Stable, handles 18 tools across 3 safety levels
**Last Updated:** 2026-01-20
