---
type: scenario
name: inspect-file-metadata
version: 1.0.0
feature: file-system-tools
covers:
  - get-file-info
tags:
  - happy-path
  - introspection
---

# Scenario: Inspect File Metadata

**Feature:** file-system-tools
**User Story:** As a developer, I want to inspect file metadata (size, modification time, permissions) so I can understand file characteristics without opening the file.
**Test Coverage:** Verified by filesystem-tests.lisp
**Confidence:** 0.95

---

## Context

When working with files, developers often need metadata (size, timestamps, permissions) without reading the entire file content. The `get_file_info` tool provides this information efficiently.

---

## Actors

- **LLM**: Requests file information
- **Tool Executor**: Executes `get_file_info` tool
- **Filesystem**: Provides file metadata

---

## Preconditions

1. File exists at known path
2. User has read permissions on file
3. File is within project root boundary

---

## Main Flow

### Step 1: LLM Requests File Info

**Tool Call:**
```json
{
  "name": "get_file_info",
  "arguments": {
    "path": "src/context.lisp"
  }
}
```

### Step 2: Tool Executor Retrieves Metadata

**Lisp Handler:**
```lisp
(get-file-info "src/context.lisp")
```

**Returns:**
```lisp
(:path "src/context.lisp"
 :size 8192
 :modified-time 3900000000
 :exists t
 :readable t
 :writable t
 :directory nil)
```

### Step 3: Result Formatted for LLM

**Tool Result:**
```json
{
  "path": "src/context.lisp",
  "size": 8192,
  "modified": "2025-01-17T10:30:00Z",
  "exists": true,
  "readable": true,
  "writable": true,
  "is_directory": false
}
```

---

## Postconditions

1. Metadata returned without reading file content
2. No file modifications occurred
3. LLM has sufficient context to decide next action

---

## Verification

**Test:** filesystem-tests.lisp verifies get-file-info behavior
**Status:** ✅ Pass

---

**Scenario Status:** ✅ Verified
**Confidence:** 0.95
