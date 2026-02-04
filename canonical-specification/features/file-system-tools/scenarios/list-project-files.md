---
type: scenario
name: list-project-files
version: 1.0.0
feature: file-system-tools
covers:
  - list-directory
tags:
  - happy-path
  - navigation
---

# Scenario: List Project Files

**Feature:** file-system-tools
**User Story:** As a developer, I want to list files in a directory so I can understand project structure and locate relevant files.
**Test Coverage:** Verified by filesystem-tests.lisp
**Confidence:** 0.95

---

## Context

When exploring a codebase, developers need to see directory contents to understand project organization and find files of interest.

---

## Main Flow

### Step 1: LLM Requests Directory Listing

**Tool Call:**
```json
{
  "name": "list_directory",
  "arguments": {
    "path": "src/"
  }
}
```

### Step 2: Tool Returns File List

**Result:**
```json
{
  "path": "src/",
  "entries": [
    {"name": "agent.lisp", "type": "file", "size": 5120},
    {"name": "context.lisp", "type": "file", "size": 8192},
    {"name": "conversation.lisp", "type": "file", "size": 4096},
    {"name": "tools/", "type": "directory", "size": 0}
  ]
}
```

---

## Postconditions

1. Directory contents listed
2. Files and subdirectories distinguished
3. No filesystem modifications

---

**Scenario Status:** ✅ Verified
**Confidence:** 0.95
