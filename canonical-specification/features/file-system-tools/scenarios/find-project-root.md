---
type: scenario
name: find-project-root
version: 1.0.0
feature: file-system-tools
covers:
  - project-root
tags:
  - happy-path
  - security
---

# Scenario: Find Project Root

**Feature:** file-system-tools
**User Story:** As the system, I want to identify the project root directory so I can constrain file operations to the project boundary for security.
**Test Coverage:** Verified by filesystem-tests.lisp
**Confidence:** 1.00

---

## Context

All file operations must be constrained within the project root directory to prevent accessing files outside the project (security boundary).

---

## Main Flow

### Step 1: System Requests Project Root

**Tool Call:**
```json
{
  "name": "get_project_root",
  "arguments": {}
}
```

### Step 2: Tool Identifies Root Marker

**Lisp Handler:**
```lisp
(find-project-root) ; Searches for .git, *.asd, etc.
```

**Returns:**
```lisp
"/Users/quasi/quasilabs/projects/agent-q"
```

### Step 3: Root Used for Path Validation

**Security Check:**
```lisp
(defun validate-path (requested-path)
  (let ((root (get-project-root)))
    (unless (path-within-root-p requested-path root)
      (error "Path outside project boundary"))))
```

---

## Postconditions

1. Project root identified
2. All subsequent file operations constrained within root
3. Security boundary enforced

---

**Scenario Status:** ✅ Verified
**Confidence:** 1.00
