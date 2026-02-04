---
type: scenario
name: relocate-file
version: 1.0.0
feature: file-system-tools
covers:
  - move-file
tags:
  - happy-path
  - refactoring
---

# Scenario: Relocate File

**Feature:** file-system-tools
**User Story:** As a developer, I want to move files to different locations so I can reorganize project structure during refactoring.
**Test Coverage:** Planned
**Confidence:** 0.90

---

## Context

During refactoring, files often need to be moved to different directories. The `move_file` tool handles relocation while maintaining file content and updating references.

---

## Main Flow

### Step 1: LLM Requests File Move

**Tool Call:**
```json
{
  "name": "move_file",
  "arguments": {
    "source": "src/old-location.lisp",
    "destination": "src/new-directory/renamed.lisp"
  }
}
```

### Step 2: Tool Moves File

**Behavior:**
1. Validate source exists
2. Check destination doesn't exist
3. Ensure destination parent directory exists
4. Move file (atomic operation if possible)
5. Return success

**Result:**
```json
{
  "success": true,
  "source": "src/old-location.lisp",
  "destination": "src/new-directory/renamed.lisp"
}
```

---

## Postconditions

1. File exists at destination
2. File no longer exists at source
3. File content unchanged
4. File permissions preserved

---

**Scenario Status:** 📝 Specified (implementation planned)
**Confidence:** 0.90
