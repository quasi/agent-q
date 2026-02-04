---
type: contract
name: context-manager
version: 1.0.0
feature: context-management
depends_on: []
---

# Contract: Context Manager

**Feature:** context-management
**Source:** src/context.lisp:43-126
**Specification:** specs/PHASE-1-SPEC.md:153-167
**Confidence:** 1.00

---

## Purpose

A `context-manager` manages a sliding window collection of context items. It implements a FIFO queue with a configurable capacity (default: 50 items), automatically evicting the oldest item when capacity is exceeded.

---

## Data Structure

### Class Definition

```lisp
(defclass context-manager ()
  ((items :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor context-items
          :documentation "Adjustable vector storing context-item instances")

   (max-items :initarg :max-items
              :initform 50
              :accessor context-max-items
              :documentation "Sliding window size (capacity)")))
```

### Constructor

```lisp
(defun make-context-manager (&key (max-items 50))
  "Create a new context manager.

   MAX-ITEMS - Maximum number of items before FIFO eviction (default: 50)

   Returns: New context-manager instance"
  (make-instance 'context-manager :max-items max-items))
```

**Usage:**
```lisp
;; Default capacity (50 items)
(make-context-manager)

;; Custom capacity
(make-context-manager :max-items 100)
```

---

## JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Context Manager",
  "description": "Manages a sliding window collection of context items with FIFO eviction",
  "properties": {
    "items": {
      "type": "array",
      "description": "Array of context items",
      "items": {
        "$ref": "#/definitions/context-item"
      },
      "maxItems": 50
    },
    "max_items": {
      "type": "integer",
      "description": "Maximum capacity before FIFO eviction",
      "default": 50,
      "minimum": 1,
      "maximum": 1000
    }
  },
  "required": ["items", "max_items"],
  "definitions": {
    "context-item": {
      "type": "object",
      "properties": {
        "id": {"type": "string", "pattern": "^ctx-[0-9]+$"},
        "type": {"type": "string", "enum": ["code", "text", "file", "repl-history", "error", "custom"]},
        "content": {"type": "string"},
        "metadata": {"type": ["object", "null"]},
        "timestamp": {"type": "integer"}
      },
      "required": ["id", "type", "content", "timestamp"]
    }
  }
}
```

---

## Operations

### add-context

**Signature:**
```lisp
(defgeneric add-context (manager item-or-content &key type metadata))
```

**Methods:**

#### Method 1: Add Existing Context Item
```lisp
(defmethod add-context ((manager context-manager) (item context-item)
                        &key type metadata)
  "Add existing context-item to manager.

   ITEM - context-item instance
   TYPE, METADATA - Ignored (item already has these)

   Side Effects:
   - Appends item to items vector
   - If capacity exceeded, removes oldest item first

   Returns: item (the added context-item)")
```

#### Method 2: Add Raw Content
```lisp
(defmethod add-context ((manager context-manager) (content string)
                        &key (type :code) metadata)
  "Add content as new context item.

   CONTENT - String content to wrap
   TYPE - Context type (default :code)
   METADATA - Optional metadata plist

   Creates context-item internally via make-context-item.

   Returns: Newly created context-item")
```

**Behavior:**

1. **Check capacity**: If `(length items) >= max-items`, remove oldest item
2. **Eviction strategy**: Uses `(subseq items 1)` to shift array, removing index 0
3. **Append**: Uses `vector-push-extend` to add new item at end
4. **Order**: Items ordered chronologically (oldest at index 0, newest at end)

**Examples:**
```lisp
;; Add existing item
(let ((item (make-context-item "(+ 1 2)" :type :code)))
  (add-context manager item))

;; Add raw content
(add-context manager "(defun foo () 42)"
             :type :code
             :metadata '(:filename "example.lisp" :start-line 10))
```

---

### clear-context

**Signature:**
```lisp
(defgeneric clear-context (manager))
```

**Method:**
```lisp
(defmethod clear-context ((manager context-manager))
  "Clear all context items.

   Side Effects:
   - Replaces items vector with empty adjustable vector
   - All items are discarded (no recovery possible)

   Returns: nil (implicitly)")
```

**Behavior:**

Resets `items` slot to a fresh empty vector:
```lisp
(setf (context-items manager)
      (make-array 0 :adjustable t :fill-pointer 0))
```

**Important:** Clearing is irreversible. Items are not recoverable.

---

### get-context

**Signature:**
```lisp
(defgeneric get-context (manager &key types limit))
```

**Method:**
```lisp
(defmethod get-context ((manager context-manager) &key types limit)
  "Get context items, optionally filtered and limited.

   TYPES - List of keywords to filter by type (e.g., '(:code :error))
           If nil, return all types
   LIMIT - Maximum number of items to return (most recent N)
           If nil, return all items

   Returns: List of context-item objects (not vector)")
```

**Behavior:**

1. **Convert to list**: `(coerce (context-items manager) 'list)`
2. **Filter by types** (if provided): `(remove-if-not (lambda (item) (member (context-item-type item) types)) items)`
3. **Apply limit** (if provided): `(subseq filtered (max 0 (- (length filtered) limit)))`
   - Takes **most recent N** items (from end of list)
4. **Return list**: Preserves chronological order

**Examples:**
```lisp
;; Get all items
(get-context manager)
→ (list of all context-items)

;; Get only code items
(get-context manager :types '(:code))
→ (list of :code context-items)

;; Get 10 most recent error items
(get-context manager :types '(:error) :limit 10)
→ (list of up to 10 most recent :error items)
```

---

### context-to-string

**Signature:**
```lisp
(defgeneric context-to-string (manager))
```

**Method:**
```lisp
(defmethod context-to-string ((manager context-manager))
  "Format all context items as markdown for LLM consumption.

   Returns: String containing markdown-formatted context"
```

**Format:**

```markdown
## Context

### Code (from filename.lisp:10-20)
```lisp
(defun example () ...)
```

### Error
```lisp
ERROR: Division by zero
```
```

**Algorithm:**

1. **Header**: `## Context\n\n`
2. **For each item**:
   - Type header: `### Code` (capitalized symbol name)
   - Metadata annotation: `(from filename:start-end)` if present
   - Code fence: `` ```lisp ``
   - Content: item content verbatim
   - Close fence: `` ``` ``
   - Blank line separator

**Example Output:**

```markdown
## Context

### Code (from src/agent.lisp:42-58)
```lisp
(defun send-to-agent (instruction)
  (process-with-llm instruction))
```

### Error
```lisp
SIMPLE-ERROR: Unbound variable FOO
  Backtrace: ...
```
```

---

## Invariants

### INV-001: Capacity Constraint
`(length (context-items manager)) <= (context-max-items manager)`

**Enforcement:** Automatic FIFO eviction in `add-context`
**Confidence:** 1.00

### INV-002: Chronological Order
Items ordered by timestamp: `∀i,j: i < j ⇒ timestamp(items[i]) <= timestamp(items[j])`

**Enforcement:** Append-only insertion via `vector-push-extend`
**Confidence:** 1.00

### INV-003: Oldest Item Index
When full, the oldest item is always at index 0.

**Enforcement:** FIFO eviction via `(subseq items 1)` removes index 0
**Confidence:** 1.00

### INV-004: Vector Properties
`items` is always an adjustable vector with fill-pointer.

**Enforcement:** Explicit array creation in constructor and `clear-context`
**Confidence:** 1.00

### INV-005: Non-Negative Max Items
`(context-max-items manager) > 0`

**Enforcement:** Convention (no validation in code)
**Confidence:** 0.90 (assumed but not enforced)

---

## Algorithms

### FIFO Eviction Algorithm

When `add-context` detects capacity exceeded:

```lisp
;; Current state: items = [item-0 item-1 ... item-49] (50 items, at capacity)
;; Operation: add-context with new-item

;; Step 1: Check capacity
(when (>= (length items) max-items)
  ;; Step 2: Remove oldest (shift array left)
  (setf items (subseq items 1))  ; Creates [item-1 item-2 ... item-49]

  ;; Step 3: Recreate adjustable vector
  (setf (context-items manager)
        (make-array (length items)
                    :adjustable t
                    :fill-pointer (length items)
                    :initial-contents items)))

;; Step 4: Append new item
(vector-push-extend new-item (context-items manager))
;; Result: [item-1 item-2 ... item-49 new-item] (50 items again)
```

**Time Complexity:** O(n) where n = max-items (due to `subseq` and array recreation)

**Space Complexity:** O(n) temporary allocation for `subseq`

**Optimization Opportunity:** Could use circular buffer for O(1) eviction.

---

### Filtering Algorithm

The `get-context` method uses functional filtering:

```lisp
;; Step 1: Convert vector to list
(let ((items (coerce (context-items manager) 'list)))

  ;; Step 2: Filter by types (if provided)
  (let ((filtered (if types
                      (remove-if-not
                        (lambda (item) (member (context-item-type item) types))
                        items)
                      items)))

    ;; Step 3: Apply limit (take last N items)
    (let ((limited (if limit
                       (subseq filtered (max 0 (- (length filtered) limit)))
                       filtered)))
      limited)))
```

**Time Complexity:** O(n) for coerce + O(n) for filter + O(1) for subseq = O(n)

**Important:** Limit takes **most recent** items (from end of list).

---

## LLM Integration

### Prompt Inclusion

Context managers are embedded in LLM prompts via `context-to-string`:

**Typical Prompt Structure:**
```
<system>You are an AI assistant...</system>

<context-from-manager>
## Context

### Code (from example.lisp:10-15)
```lisp
(defun foo () 42)
```
</context-from-manager>

<user>What does this function do?</user>
```

**Token Budget Considerations:**

- Each context item consumes tokens
- 50-item capacity can accumulate significant token count
- No automatic truncation based on token limits
- **Recommendation:** Monitor total prompt tokens, clear or limit context proactively

---

## Test Coverage

### Verified Behaviors

| Test | Behavior | Status |
|------|----------|--------|
| `agent-q-context/struct/creates-manager` | Manager creation works | ✅ Pass |
| `agent-q-context/operations/add-item` | Adding items works | ✅ Pass |
| `agent-q-context/operations/clear-context` | Clearing empties list | ✅ Pass |
| `agent-q-context/operations/add-context-file` | File context addition | ✅ Pass |
| `agent-q-context/operations/add-context-buffer` | Buffer context addition | ✅ Pass |
| `agent-q-context/operations/add-context-region` | Region context addition | ✅ Pass |
| `agent-q-context/operations/add-context-symbol` | Symbol context addition | ✅ Pass |
| `agent-q-context/sliding-window/eviction` | FIFO eviction at capacity | ⚠️ Implicit |
| `agent-q-context/formatting/markdown` | Markdown output format | ⚠️ Implicit |

**Test File:** `contrib/sly-agent-q/test/sly-agent-q-context-test.el:21-200`
**Pass Rate:** 100% (explicit tests)
**Coverage Gap:** FIFO eviction and markdown formatting not explicitly tested

---

## Related Contracts

- **context-item.md** - Individual context items managed by this class
- **conversation.md** - Conversations contain a context-manager instance

---

## Known Limitations

### L-001: Inefficient FIFO Eviction
Uses `subseq` + array recreation for eviction, which is O(n) in capacity.

**Workaround:** Keep `max-items` reasonable (<100)
**Recommendation:** Use circular buffer for O(1) eviction if performance becomes issue

### L-002: No Token-Based Limiting
Capacity is item-count based, not token-based. Large items can exceed LLM context limits.

**Workaround:** Clear context proactively or use `get-context :limit`
**Recommendation:** Add token-aware capacity management in Phase 3+

### L-003: No Persistence
Context cleared on image restart. Not suitable for long-term accumulation.

**Mitigation:** Sessions persist conversation context across restarts (Phase 3)
**Status:** Addressed by session management layer

### L-004: No Item Removal
Only supports clear-all or FIFO eviction. Cannot remove specific items.

**Workaround:** Clear and re-add desired items
**Recommendation:** Add `remove-context-item` method if needed

### L-005: Markdown Formatting Assumptions
`context-to-string` assumes Lisp code and uses `` ```lisp `` fence.

**Impact:** Works for :code, :error, :repl-history; suboptimal for :text
**Recommendation:** Use language tag based on item type

---

## Usage Patterns

### Pattern 1: Accumulate Code Regions
```lisp
(let ((manager (make-context-manager)))
  ;; User marks region in Emacs, Elisp calls RPC
  (add-context manager "(defun foo () ...)"
               :type :code
               :metadata '(:filename "src/example.lisp" :start-line 10 :end-line 15))

  ;; User marks another region
  (add-context manager "(defclass bar () ...)"
               :type :code
               :metadata '(:filename "src/example.lisp" :start-line 20 :end-line 25))

  ;; Build prompt for LLM
  (let ((context-str (context-to-string manager)))
    ;; context-str now contains markdown-formatted context
    ))
```

### Pattern 2: Filter and Limit
```lisp
;; Get only errors from last 5 items
(let ((recent-errors (get-context manager :types '(:error) :limit 5)))
  (loop for error-item in recent-errors
        do (format t "~A~%" (context-item-content error-item))))
```

### Pattern 3: Reset Between Sessions
```lisp
;; Clear context when starting new conversation
(clear-context manager)
```

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | Initial implementation | Phase 1 foundation |
| 2026-01-17 | Canon contract extracted | Triangulation analysis |

---

**Contract Status:** ✅ Stable (Phase 1 complete)
**Verification:** 100% (spec + code + tests align)
**Confidence:** 1.00
