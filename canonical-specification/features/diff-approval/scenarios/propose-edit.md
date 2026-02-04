---
type: scenario
name: propose-edit
version: 1.0.0
feature: diff-approval
covers:
  - propose-file-edit
tags:
  - happy-path
  - core-workflow
---

# Scenario: Propose File Edit

**Flow:** Complete workflow from LLM tool call to user approval
**Confidence:** 1.00
**Source:** docs/DIFF-IMPLEMENTATION.AGENT.md, manual testing

---

## Preconditions

- File exists at target path
- LLM has `propose_file_edit` tool available
- User asks for file modification

---

## Flow

1. **LLM Decision**: Calls `propose_file_edit` tool
2. **Tool Execution**: Generates unified diff, opens Emacs UI
3. **User Reviews**: Navigates hunks, applies/rejects each
4. **Exit**: User presses `q`, decision returned
5. **Result**: LLM receives "accepted" or "rejected"

---

## Example

**User:** "Add docstrings to the `process-instruction` function"

**LLM Tool Call:**
```json
{
  "name": "propose_file_edit",
  "arguments": {
    "path": "/path/to/agent.lisp",
    "original": "(defun process-instruction ...)",
    "modified": "(defun process-instruction\n  \"Process user instruction...\"\n  ...)",
    "description": "Add docstring to process-instruction"
  }
}
```

**Diff UI:**
```diff
@@ -1,3 +1,4 @@
 (defun process-instruction (instruction &key streaming)
+  "Process a user instruction by sending to LLM agent."
   (ensure-agent)
```

**User Actions:**
- Press `a` on hunk → applied (file modified)
- Press `q` → exit

**Tool Result:** `"accepted: Applied 1 hunk to /path/to/agent.lisp"`

---

## Postconditions

- File modified if any hunks applied
- LLM receives success/rejection message
- Buffer closed

---

**Scenario Status:** ✅ Core workflow, tested in integration tests
