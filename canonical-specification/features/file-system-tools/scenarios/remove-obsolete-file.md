---
type: scenario
name: remove-obsolete-file
version: 1.0.0
feature: file-system-tools
covers:
  - delete-file
tags:
  - happy-path
  - cleanup
---

# Scenario: Remove Obsolete File

**Feature:** file-system-tools
**User Story:** As a developer, I want to delete files that are no longer needed so I can keep the project clean during refactoring.
**Test Coverage:** Planned
**Confidence:** 0.90

---

## Context

During cleanup and refactoring, obsolete files need to be removed. The `delete_file` tool provides safe deletion with confirmation prompts.

---

## Main Flow

### Step 1: LLM Requests File Deletion

**Tool Call:**
```json
{
  "name": "delete_file",
  "arguments": {
    "path": "src/obsolete-module.lisp",
    "reason": "Functionality moved to new-module.lisp"
  }
}
```

### Step 2: System Confirms Deletion (if interactive)

**Prompt:** "Delete src/obsolete-module.lisp? (Reason: Functionality moved to new-module.lisp)"

**User:** Confirms deletion

### Step 3: Tool Deletes File

**Behavior:**
1. Validate file exists
2. Check file is within project root
3. Request user confirmation (if interactive mode)
4. Delete file
5. Return success

**Result:**
```json
{
  "success": true,
  "path": "src/obsolete-module.lisp",
  "deleted": true
}
```

---

## Postconditions

1. File no longer exists
2. Deletion logged with reason (for audit trail)
3. No other files affected

---

## Safety Notes

- Deletion is irreversible (requires git to recover)
- User confirmation prevents accidental deletion
- Reason field provides audit trail

---

**Scenario Status:** 📝 Specified (implementation planned)
**Confidence:** 0.90
