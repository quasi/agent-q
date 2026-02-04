# Property: Error Recovery

**Type:** Reliability / Fault Tolerance
**Confidence:** 1.00
**Source:** src/tools/registry.lisp:execute-tool-calls

---

## Statement

**Tool execution errors MUST NOT propagate beyond the executor. All errors are caught, formatted, and returned as error results to the LLM.**

**Metric:** 100% of tool handler errors caught by handler-case; zero unhandled conditions propagate to the agent loop.

---

## Invariant

```
∀ tool-call ∈ tool-calls:
  (execute-tool-calls (list tool-call)) signals no conditions
  ∧
  (returns list of tool-results)
  ∧
  (on handler error → result.success = nil ∧ result.error ≠ nil)
```

**In words:** Execution always returns results, never signals errors.

---

## Error Types

### 1. Tool Not Found

```lisp
(execute-tool-calls '((:id "1" :name "nonexistent" :arguments ())))
→ (#<TOOL-RESULT id="1" success=NIL error="Unknown tool: nonexistent">)
```

### 2. Handler Signals Error

```lisp
;; Tool handler raises condition
(define-tool "failing_tool" ...
  :handler (lambda (args) (error "Something went wrong")))

(execute-tool-calls '((:id "2" :name "failing_tool" :arguments ())))
→ (#<TOOL-RESULT id="2" success=NIL error="Tool error: Something went wrong">)
```

### 3. Invalid Arguments

```lisp
;; Missing required parameter
(execute-tool-calls '((:id "3" :name "describe_symbol" :arguments ())))
→ (#<TOOL-RESULT id="3" success=NIL error="Required parameter 'symbol' missing">)
```

---

## Recovery Mechanism

```lisp
(defun execute-tool-call (tool-call)
  (handler-case
      ;; Try to execute
      (let ((tool (get-tool (getf tool-call :name))))
        (unless tool
          (return-from execute-tool-call
            (make-error-result "Unknown tool")))

        (let ((result (funcall (tool-handler tool) args)))
          (make-success-result result)))

    ;; Catch ALL errors
    (error (e)
      (make-error-result (format nil "Tool error: ~A" e)))))
```

---

## Consequences

**Agent Loop Continues:**
```lisp
;; Even if tool fails, agent continues
(let ((tool-results (execute-tool-calls tool-calls)))
  ;; Results include errors, but no crash
  (send-results-to-llm tool-results)
  ;; LLM can retry or adjust approach
  (continue-conversation))
```

**LLM Sees Error Messages:**
- LLM receives error in tool-result
- Can understand what went wrong
- Can retry with corrected input
- Can ask user for clarification

---

## Test Case

```lisp
(test error-recovery
  "Verify that tool errors don't crash the system."

  ;; Tool that always fails
  (register-tool *test-registry*
    (make-instance 'tool
      :name "always_fails"
      :handler (lambda (args) (error "Intentional error"))))

  ;; Execute failing tool
  (let ((results (execute-tool-calls
                   '((:id "test" :name "always_fails" :arguments ())))))

    ;; Should return error result, not signal
    (is (= 1 (length results)))
    (let ((result (first results)))
      (is (not (tool-result-success result)))
      (is (search "Intentional error" (tool-result-error result))))))
```

---

## Real-World Example

**User asks:** "What does the function `process-request` do?"

**LLM calls:** `describe_symbol` with symbol="process-request"

**Handler finds:** Symbol not found (typo)

**Result:**
```lisp
{:success nil
 :error "Symbol PROCESS-REQUEST not found in package AGENT-Q"}
```

**LLM response:** "I couldn't find a function named `process-request`. Did you mean `process-instruction`? Let me check that instead."

**LLM retries:** `describe_symbol` with symbol="process-instruction"

**Success!** Returns documentation.

---

## Consequences

**Positive:**
- ✅ System never crashes on tool errors
- ✅ LLM can self-correct from errors
- ✅ User sees graceful degradation, not crashes

**Negative:**
- ⚠️ Errors swallowed (no debugger entry)
- ⚠️ Stack traces not preserved for debugging
- ⚠️ Silent failures possible if error message unclear

---

## Related Properties

- **safety-enforcement.md** - Prevents dangerous operations
- **tool-name-uniqueness.md** - Registry integrity

---

**Property Status:** ✅ Enforced by handler-case wrapper
**Last Updated:** 2026-01-20
