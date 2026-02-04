# Contract: Context Item

**Feature:** context-management
**Source:** src/context.lisp:14-40
**Specification:** specs/PHASE-1-SPEC.md:133-148
**Confidence:** 1.00

---

## Purpose

A `context-item` represents a piece of code or text with metadata that can be included in LLM prompts. Context items are the atomic units of context accumulation.

---

## Data Structure

### Class Definition

```lisp
(defclass context-item ()
  ((id :initarg :id
       :accessor context-item-id
       :initform (generate-id)
       :documentation "Unique identifier (ctx-N)")

   (item-type :initarg :type
              :accessor context-item-type
              :type (member :code :text :file :repl-history :error :custom)
              :documentation "Type of context item")

   (content :initarg :content
            :accessor context-item-content
            :type string
            :documentation "The actual content")

   (metadata :initarg :metadata
             :accessor context-item-metadata
             :initform nil
             :documentation "Plist of metadata: :filename, :start-line, :end-line, :package, etc.")

   (timestamp :initarg :timestamp
              :accessor context-item-timestamp
              :initform (get-universal-time)
              :documentation "Creation timestamp (immutable)")))
```

### Constructor

```lisp
(defun make-context-item (content &key (type :code) metadata)
  "Create a new context item.

   CONTENT - String content to store
   TYPE - One of the allowed types (default :code)
   METADATA - Optional plist with keys like :filename, :start-line, etc.

   Returns: New context-item instance"
  (make-instance 'context-item
                 :content content
                 :type type
                 :metadata metadata))
```

**Usage:**
```lisp
(make-context-item "(defun foo () 42)"
                   :type :code
                   :metadata '(:filename "example.lisp"
                              :start-line 10
                              :end-line 12))
```

---

## JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Context Item",
  "description": "A piece of code or text with metadata for LLM context",
  "properties": {
    "id": {
      "type": "string",
      "pattern": "^ctx-[0-9]+$",
      "description": "Unique identifier (e.g., ctx-1, ctx-42)",
      "examples": ["ctx-1", "ctx-42"]
    },
    "type": {
      "type": "string",
      "enum": ["code", "text", "file", "repl-history", "error", "custom"],
      "description": "Type of context item"
    },
    "content": {
      "type": "string",
      "description": "The actual content (code, text, error message, etc.)"
    },
    "metadata": {
      "type": ["object", "null"],
      "description": "Optional metadata plist",
      "properties": {
        "filename": {"type": "string"},
        "start_line": {"type": "integer", "minimum": 1},
        "end_line": {"type": "integer", "minimum": 1},
        "package": {"type": "string"}
      },
      "additionalProperties": true
    },
    "timestamp": {
      "type": "integer",
      "description": "Creation timestamp (Universal Time)",
      "minimum": 0
    }
  },
  "required": ["id", "type", "content", "timestamp"],
  "additionalProperties": false
}
```

---

## Type System

### Allowed Types

The `item-type` slot is constrained to a fixed set via CLOS type declaration:

| Type | Purpose | Example Use Case |
|------|---------|------------------|
| `:code` | Code snippets | Marked regions from Emacs buffers |
| `:text` | Plain text | Documentation, notes |
| `:file` | File contents | Entire file attached for context |
| `:repl-history` | REPL interactions | Previous evaluations |
| `:error` | Error messages | Compiler or runtime errors |
| `:custom` | Extension point | Future custom types (⚠️ undocumented) |

**Enforcement:** CLOS type system enforces at runtime
**Test:** `agent-q-context/struct/type-variants` (99-test suite)

---

## Metadata Schema

The `metadata` slot is an **unstructured plist**. Common keys by convention:

### Standard Keys

| Key | Type | Purpose | Example |
|-----|------|---------|---------|
| `:filename` | string | Source file path | `"src/agent.lisp"` |
| `:start-line` | integer | Beginning line number | `42` |
| `:end-line` | integer | Ending line number | `58` |
| `:package` | string/symbol | Lisp package | `:agent-q` |

### Example Metadata

```lisp
'(:filename "src/tools/introspection.lisp"
  :start-line 120
  :end-line 145
  :package :agent-q.tools)
```

**Note:** No schema enforcement—any keys allowed. Flexibility vs. type safety trade-off.

---

## ID Generation

### Format

IDs follow the pattern: `ctx-N` where N is a monotonically increasing counter.

**Examples:**
- `ctx-1`
- `ctx-2`
- `ctx-42`

### Implementation

```lisp
(defvar *context-id-counter* 0)

(defun generate-id ()
  "Generate unique ID for context items."
  (format nil "ctx-~D" (incf *context-id-counter*)))
```

**Properties:**
- Unique within a single Lisp image session
- Monotonically increasing (never decreases)
- Not thread-safe (assumes single-threaded use)
- Resets on image restart (not persistent)

---

## Invariants

### INV-001: Type Membership
`(member (context-item-type item) '(:code :text :file :repl-history :error :custom))`

**Enforcement:** CLOS type system
**Confidence:** 1.00

### INV-002: Content is String
`(stringp (context-item-content item))`

**Enforcement:** CLOS type system
**Confidence:** 1.00

### INV-003: ID is Unique
`∀ items i,j: i ≠ j ⇒ (context-item-id i) ≠ (context-item-id j)`

**Enforcement:** Monotonic counter
**Confidence:** 1.00 (within single session)

### INV-004: Timestamp is Immutable
`(context-item-timestamp item)` never changes after creation

**Enforcement:** No setter provided, only reader
**Confidence:** 1.00

### INV-005: Metadata is Optional
`metadata` can be `nil` (valid empty state)

**Enforcement:** `:initform nil`
**Confidence:** 1.00

---

## Operations

### Accessors (Auto-generated by CLOS)

```lisp
;; Readers
(context-item-id item)         ; → string
(context-item-type item)       ; → keyword (member set)
(context-item-content item)    ; → string
(context-item-metadata item)   ; → list (plist)
(context-item-timestamp item)  ; → universal-time

;; Writers (all except timestamp)
(setf (context-item-type item) new-type)
(setf (context-item-content item) new-content)
(setf (context-item-metadata item) new-metadata)
;; Note: No setter for id or timestamp
```

### Typical Usage Pattern

```lisp
;; Create context item
(let ((item (make-context-item
              "(defun add (a b) (+ a b))"
              :type :code
              :metadata '(:filename "math.lisp"
                         :start-line 5
                         :end-line 7))))

  ;; Access fields
  (context-item-id item)        ; → "ctx-1"
  (context-item-type item)      ; → :CODE
  (context-item-content item)   ; → "(defun add (a b) (+ a b))"
  (context-item-timestamp item) ; → 3910982400 (universal-time)

  ;; Metadata access
  (getf (context-item-metadata item) :filename) ; → "math.lisp"
  (getf (context-item-metadata item) :start-line) ; → 5
)
```

---

## Formatting for LLM

Context items are formatted as markdown when included in LLM prompts. See `context-to-string` in `context-manager.md` for details.

**Example Output:**
```markdown
### Code
#### math.lisp:5-7
```lisp
(defun add (a b) (+ a b))
```
```

---

## Test Coverage

### Verified Behaviors

| Test | Behavior | Status |
|------|----------|--------|
| `agent-q-context/struct/creates-item` | Item creation works | ✅ Pass |
| `agent-q-context/struct/type-variants` | All 6 types accepted | ✅ Pass |
| `agent-q-context/struct/default-values` | Handles nil defaults | ✅ Pass |
| `agent-q-context/struct/accessors-work` | All accessors function | ✅ Pass |

**Test File:** `contrib/sly-agent-q/test/sly-agent-q-context-test.el:21-60`
**Pass Rate:** 100%

---

## Related Contracts

- **context-manager.md** - Manages collections of context items
- **conversation.md** - Conversations contain context managers

---

## Known Limitations

### L-001: Non-Persistent IDs
Context item IDs reset on Lisp image restart. Not suitable for persistent references.

**Workaround:** Use metadata `:filename` + `:start-line` for stable references

### L-002: Non-Thread-Safe ID Generation
`*context-id-counter*` uses `incf` without locking. Concurrent item creation may produce duplicate IDs.

**Mitigation:** Agent-Q assumes single-threaded use
**Recommendation:** Document concurrency model explicitly

### L-003: Unstructured Metadata
No schema validation on metadata plist. Typos in keys go undetected.

**Mitigation:** Use consistent keys by convention
**Recommendation:** Consider schema validation in future

### L-004: :custom Type Undocumented
`:custom` type exists but has no documentation on intended use.

**Status:** Code-only discovery
**Recommendation:** Document use cases or deprecate

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | Initial implementation | Phase 1 foundation |
| 2026-01-17 | Canon extracted | Triangulation analysis |

---

**Contract Status:** ✅ Stable (Phase 1 complete)
**Verification:** 100% (spec + code + tests align)
**Confidence:** 1.00
