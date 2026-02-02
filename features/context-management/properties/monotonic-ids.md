# Property: Monotonic IDs

**Feature:** context-management
**Category:** State Management
**Source:** src/context.lisp:6-13
**Confidence:** 1.00

---

## Statement

**Context item IDs are monotonically increasing strings following the pattern `ctx-N`, where N is a global counter that increments on each context-item creation.**

---

## Formal Definition

### Monotonicity Invariant

```
∀ items i, j created at times t_i, t_j:
  t_i < t_j ⇒ N(id(i)) < N(id(j))

Where:
  N(id) extracts the numeric portion of "ctx-N"
  id(i) = (context-item-id i)
```

**In words:** If item i is created before item j, then i's ID number is less than j's ID number.

### Uniqueness Invariant

```
∀ items i, j:
  i ≠ j ⇒ id(i) ≠ id(j)
```

**In words:** Every context-item has a unique ID.

---

## Implementation

### Global Counter

```lisp
(defvar *context-id-counter* 0
  "Global counter for generating unique context IDs.
   Not thread-safe.")
```

### ID Generation Function

```lisp
(defun generate-id ()
  "Generate unique ID for context items.
   Format: ctx-N where N is monotonically increasing."
  (format nil "ctx-~D" (incf *context-id-counter*)))
```

**Algorithm:**
1. Atomically increment `*context-id-counter*` via `incf`
2. Format as string: `"ctx-" + counter-value`
3. Return string

### CLOS Integration

```lisp
(defclass context-item ()
  ((id :initarg :id
       :accessor context-item-id
       :initform (generate-id)    ; ← Automatic ID generation
       :documentation "Unique identifier (ctx-N)")))
```

---

## Enforcement Mechanism

### Automatic ID Assignment

**When:** During `make-context-item` or `make-instance 'context-item`
**How:** `:initform (generate-id)` calls generator at instance creation
**Result:** Every new context-item gets next ID in sequence

### Counter State

**Scope:** Global (one counter per Lisp image)
**Lifetime:** Reset on image restart (not persistent)
**Concurrency:** ⚠️ Not thread-safe (assumes single-threaded use)

---

## Verification

### Test Evidence

**Test:** Implicit in `agent-q-context/struct/creates-item` (sly-agent-q-context-test.el)
**Status:** ⚠️ Tests that IDs exist, but don't verify monotonicity
**Coverage Gap:** No test creates multiple items and verifies N increases

### Code Evidence

**ID Format Validation:**
```bash
$ grep -r "ctx-" src/ test/
# All references use "ctx-N" format
```

**Result:** ✅ Consistent ID format throughout codebase

---

## Rationale

### Why Monotonic IDs?

1. **Uniqueness guarantee**: No collisions within a session
2. **Chronological ordering**: ID order reflects creation order
3. **Simplicity**: No complex ID generation logic (UUID, timestamps, etc.)
4. **Debugging**: Sequential IDs are human-readable and easy to track

### Why "ctx-N" Format?

- **Prefix clarity**: "ctx" indicates context item (vs. other ID types)
- **Human-readable**: Simple format for logs and debugging
- **Parseable**: Easy to extract numeric portion if needed

### Why Global Counter?

- **Simplicity**: Single counter, no coordination needed
- **Performance**: `incf` is fast (O(1) operation)
- **Sufficient**: Agent-Q assumes single-threaded, so no race conditions

---

## Behavior Examples

### Example 1: Sequential ID Generation

```lisp
(let ((item1 (make-context-item "first" :type :code))
      (item2 (make-context-item "second" :type :code))
      (item3 (make-context-item "third" :type :code)))
  (list (context-item-id item1)
        (context-item-id item2)
        (context-item-id item3)))
; → ("ctx-1" "ctx-2" "ctx-3")
```

**Behavior:** IDs increment sequentially

### Example 2: Counter Persists Across Calls

```lisp
;; First session
(context-item-id (make-context-item "a" :type :code))  ; → "ctx-1"
(context-item-id (make-context-item "b" :type :code))  ; → "ctx-2"

;; ... later in same Lisp session ...
(context-item-id (make-context-item "c" :type :code))  ; → "ctx-3"
```

**Behavior:** Counter never resets (within session)

### Example 3: Counter Resets on Image Restart

```lisp
;; Session 1
(context-item-id (make-context-item "a" :type :code))  ; → "ctx-1"
; ... (restart Lisp) ...

;; Session 2 (new image)
(context-item-id (make-context-item "b" :type :code))  ; → "ctx-1" (reset!)
```

**Behavior:** ⚠️ Counter resets to 0 on image restart

---

## Consequences

### Positive

✅ **Unique within session**: No collisions during runtime
✅ **Chronological order**: ID order matches creation order
✅ **Human-readable**: Easy to reference in logs/debugging
✅ **Fast generation**: O(1) ID creation
✅ **Deterministic**: Predictable sequence for testing

### Negative

⚠️ **Non-persistent**: IDs reset on image restart
⚠️ **Not globally unique**: Different sessions can reuse IDs
⚠️ **Non-thread-safe**: Race conditions possible in concurrent scenarios
⚠️ **Counter overflow**: Extremely unlikely but theoretically possible (2^63 limit)

---

## Edge Cases

### Edge Case 1: Manual ID Assignment

```lisp
;; Can override automatic ID generation
(make-instance 'context-item
               :id "ctx-999"
               :content "custom ID"
               :type :code)
```

**Behavior:** ✅ Allowed via `:initarg :id`
**Risk:** ⚠️ Breaks monotonicity, may cause duplicate IDs
**Use Case:** Reconstructing items from saved session

### Edge Case 2: Counter State After Manual Override

```lisp
;; Manual ID doesn't affect counter
(make-instance 'context-item :id "ctx-999" :content "a" :type :code)
(context-item-id (make-context-item "b" :type :code))
; → "ctx-1" (counter unaffected by manual ID)
```

**Behavior:** Manual IDs bypass `generate-id`, counter unchanged

### Edge Case 3: Very Large Counter Values

```lisp
;; After 1,000,000 items created
(setf *context-id-counter* 1000000)
(context-item-id (make-context-item "a" :type :code))
; → "ctx-1000001"
```

**Behavior:** ✅ IDs scale to arbitrary size (limited by integer range)

---

## Related Properties

- **immutable-timestamps.md**: Timestamps also immutable like IDs
- **sliding-window.md**: Lowest ID = oldest item (monotonicity enables FIFO)
- **chronological-order.md**: IDs correlate with chronological order

---

## Known Limitations

### L-001: Non-Persistent IDs

**Problem:** IDs reset on Lisp image restart
**Impact:** Cannot use IDs as stable references across sessions
**Workaround:** Use metadata (`:filename` + `:start-line`) for stable references
**Status:** By design (IDs are session-scoped)

### L-002: Non-Thread-Safe Counter

**Problem:** `*context-id-counter*` uses `incf` without locking
**Impact:** Race conditions in concurrent item creation
**Proof:** `(incf *context-id-counter*)` is not atomic in multi-threaded Lisp
**Mitigation:** Agent-Q assumes single-threaded use (documented in specs)
**Recommendation:** Add `:documentation "Not thread-safe"` to `*context-id-counter*` defvar

### L-003: ID Collisions After Manual Override

**Problem:** Manual ID assignment can create duplicates
**Impact:** Breaks uniqueness invariant if user manually assigns duplicate ID
**Example:**
```lisp
(make-instance 'context-item :id "ctx-5" :content "a" :type :code)
;; ... (counter eventually reaches 5) ...
(make-context-item "b" :type :code)  ; → "ctx-5" (duplicate!)
```
**Status:** Accepted (manual override is advanced use case)

### L-004: No ID Reuse After Item Deletion

**Problem:** Deleted items' IDs never reused (counter only increments)
**Impact:** Counter grows unbounded (though unlikely to reach practical limit)
**Status:** Accepted (simplicity over optimization)

---

## Concurrency Analysis

### Single-Threaded Guarantee

**Agent-Q Assumption:** All operations run in single REPL thread

**Evidence:**
- Docs: "Not thread-safe" in vocabulary.md:59
- Code: No locking mechanisms in `generate-id`
- Spec: Single-threaded concurrency model implied

**Implication:** Monotonicity guaranteed in single-threaded execution

### Multi-Threaded Scenario (Not Supported)

If Agent-Q were used in multi-threaded context:

```lisp
;; Thread 1 calls generate-id → reads counter = 5 → incf to 6
;; Thread 2 calls generate-id → reads counter = 5 → incf to 6
;; Result: Both threads get "ctx-6" (duplicate!)
```

**Solution (if needed):** Add locking via `bordeaux-threads:with-lock-held`

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | Monotonic counter design | Phase 1 foundation |
| 2026-01-17 | Property documented | Canon extraction |

---

**Property Status:** ✅ Verified (code inspection)
**Confidence:** 1.00
**Test Coverage:** ⚠️ Partial (IDs tested, but not monotonicity explicitly)
**Thread Safety:** ❌ Not thread-safe (by design)
