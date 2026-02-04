# Property: Safety Enforcement

**Type:** Security / Access Control
**Confidence:** 1.00
**Source:** specs/PHASE-2-SPEC.md:228-243, src/tools/registry.lisp:execute-tool-calls

---

## Statement

**Tools MUST NOT execute destructive operations without the safety checks mandated by their assigned safety level: :safe tools execute immediately, :cautious tools are logged before execution, :dangerous tools require explicit user approval via the approval handler.**

---

## Safety Levels

| Level | Auto-Execute | Approval Required | Logging | Examples |
|-------|--------------|-------------------|---------|----------|
| `:safe` | Yes | No | No | describe-symbol, apropos-search |
| `:cautious` | Yes | No | Yes | eval-form, compile-form |
| `:dangerous` | No | Yes | Yes | write-file, propose-file-edit |

---

## Invariant

```
∀ tool ∈ registry:
  (safety-level tool) = :dangerous
  ⇒
  execution requires (approval-handler tool args) = :approved
```

**In words:** Every tool marked `:dangerous` MUST obtain approval before execution.

---

## Verification

### Safe Tool (No Checks)

```lisp
(let ((tool (get-tool "describe_symbol")))
  (assert (eq (tool-safety-level tool) :safe))
  ;; Executes immediately, no approval
  (execute-tool-calls (list (:id "1" :name "describe_symbol" :arguments (...)))))
```

### Dangerous Tool (Approval Required)

```lisp
(let ((tool (get-tool "write_file")))
  (assert (eq (tool-safety-level tool) :dangerous))

  ;; Without approval handler → error
  (let ((*approval-handler* nil))
    (let ((result (execute-tool-calls ...)))
      (assert (not (tool-result-success result)))))

  ;; With approval → executes
  (let ((*approval-handler* (lambda (tool args) :approved)))
    (let ((result (execute-tool-calls ...)))
      (assert (tool-result-success result)))))
```

---

## Test Case

```lisp
(test dangerous-tool-requires-approval
  "Verify that dangerous tools cannot execute without approval."

  ;; Setup: Mock approval handler
  (let ((*approval-handler* nil)  ; No approval granted
        (tool-call '(:id "test-1"
                    :name "write_file"
                    :arguments (:path "/tmp/test.txt" :content "hello"))))

    ;; Execute tool call
    (let ((results (execute-tool-calls (list tool-call))))
      (is (= 1 (length results)))
      (let ((result (first results)))
        ;; Must fail without approval
        (is (not (tool-result-success result)))
        (is (search "approval" (tool-result-error result) :test #'char-equal))))))
```

---

## Consequences

**Positive:**
- ✅ Prevents accidental file overwrites
- ✅ User control over destructive operations
- ✅ Audit trail for approved actions

**Negative:**
- ⚠️ UX friction (user prompts)
- ⚠️ Cannot batch-approve multiple dangerous operations
- ⚠️ Approval handler must be implemented (cl-llm-provider doesn't provide one)

---

## Related Properties

- **error-recovery.md** - Errors don't crash system
- **tool-name-uniqueness.md** - Registry integrity

---

**Property Status:** ✅ Enforced, verified manually
**Last Updated:** 2026-01-20
