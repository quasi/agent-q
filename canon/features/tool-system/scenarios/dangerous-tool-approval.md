# Scenario: Dangerous Tool with Approval

**Category:** File Modification
**Safety Level:** Dangerous (requires approval)
**Confidence:** 0.95
**Source:** Manual testing, diff approval workflow

---

## Context

User asks agent to create a new configuration file. LLM decides to use `write_file` tool, which requires user approval.

---

## Preconditions

- Agent initialized
- Approval handler installed (`*approval-handler*` bound)
- User message: "Create a file called config.txt with the text 'debug=true'"

---

## Flow

### 1. LLM Decision

```json
{
  "tool_calls": [{
    "id": "call_write_1",
    "function": {
      "name": "write_file",
      "arguments": "{\"path\": \"/tmp/config.txt\", \"content\": \"debug=true\"}"
    }
  }]
}
```

### 2. Tool Lookup

```lisp
(get-tool "write_file")
→ #<TOOL write_file safety=:dangerous>
```

### 3. Safety Check

```lisp
(eq (tool-safety-level tool) :dangerous)
→ T  ; Approval required!
```

### 4. Approval Request (Elisp Side)

Agent calls approval handler:
```lisp
(funcall *approval-handler* tool args)
```

Which triggers Emacs UI:
```elisp
(sly-agent-q--approve-tool-execution
  "write_file"
  '(:path "/tmp/config.txt" :content "debug=true"))
```

### 5. User Prompt

```
Tool 'write_file' requests permission:

  Path: /tmp/config.txt
  Content: debug=true
  Size: 10 bytes

Approve? (y/n)
```

### 6a. User Approves

User presses `y`:
```elisp
→ :approved
```

Execution continues:
```lisp
(funcall (tool-handler tool) args)
→ "Wrote 10 bytes to /tmp/config.txt"

(make-instance 'tool-result
  :id "call_write_1"
  :success t
  :content "Wrote 10 bytes to /tmp/config.txt")
```

### 6b. User Denies

User presses `n`:
```elisp
→ :denied
```

Execution aborted:
```lisp
(make-instance 'tool-result
  :id "call_write_1"
  :success nil
  :error "Tool execution denied by user")
```

LLM sees denial, responds to user:
"I understand. I won't create the file. Would you like me to suggest an alternative?"

### 7. LLM Continuation

If approved, LLM receives success:
```json
{
  "role": "tool",
  "tool_call_id": "call_write_1",
  "content": "Wrote 10 bytes to /tmp/config.txt"
}
```

LLM response:
"I've created the file `/tmp/config.txt` with the content `debug=true`."

---

## Postconditions

**If Approved:**
- ✅ File written to disk
- ✅ User informed of success
- ✅ Tool result returned to LLM

**If Denied:**
- ✅ No file written
- ✅ LLM informed of denial
- ✅ Conversation continues (no crash)

---

## Timing

- Approval request → User response: 2-30 seconds (human interaction)
- File write (if approved): < 10ms
- Result formatting: < 1ms
- **Total:** Dominated by user approval time

---

## Special Case: Diff Approval

For `propose_file_edit`, approval is per-hunk:

1. LLM generates unified diff
2. Diff displayed in Emacs with hunks
3. User approves/rejects each hunk
4. Approved hunks applied to file
5. Result summary returned to LLM

See: `canon/features/diff-approval/` for full workflow.

---

## Variants

### Variant 1: No Approval Handler

If `*approval-handler*` is nil:
```lisp
(make-instance 'tool-result
  :success nil
  :error "Dangerous tool requires approval handler, but none configured")
```

### Variant 2: Modified Parameters

User modifies parameters in approval dialog:
```elisp
→ (:modified (:path "/tmp/config-backup.txt" :content "debug=true"))
```

Tool executes with modified params.

---

## Security Implications

**Why approval is critical:**
- Prevents accidental file overwrites
- User controls what agent can modify
- Audit trail (approvals logged via observability hooks)
- Defense against prompt injection attacks (LLM can't write files without user consent)

---

## References

- **tool-execution.md** - Approval handler protocol
- **safety-enforcement.md** - Safety level enforcement property
- **diff-approval/** - Special diff workflow

---

**Scenario Status:** ✅ Verified via manual testing and diff approval integration
**Last Updated:** 2026-01-20
