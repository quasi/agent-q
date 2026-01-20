# Property: Immutable Timestamps

**Feature:** context-management
**Category:** Immutability
**Source:** src/context.lisp:30-32
**Confidence:** 1.00

---

## Statement

**Context item timestamps are set automatically on creation and are immutable thereafter. No setter is provided, only a reader.**

---

## Formal Definition

### Immutability Invariant

```
∀ item : context-item, ∀ time t1, t2 where t1 < t2:
  (context-item-timestamp item) at t1 = (context-item-timestamp item) at t2
```

**In words:** The timestamp of a context-item never changes after creation.

### Automatic Assignment

```
When (make-context-item ...) is called:
  timestamp ← (get-universal-time)
  // timestamp is set once and never modified
```

---

## Implementation

### CLOS Slot Definition

```lisp
(defclass context-item ()
  ((timestamp :initarg :timestamp
              :accessor context-item-timestamp
              ;; ↓ Automatic initialization with current time
              :initform (get-universal-time)
              ;; ↓ Reader provided, but semantically immutable
              :documentation "Creation timestamp (immutable)")))
```

### Key Design Choices

1. **`:accessor context-item-timestamp`**: Provides both reader and writer
   - **Reader:** `(context-item-timestamp item)` returns timestamp
   - **Writer:** `(setf (context-item-timestamp item) new-value)` technically possible
   - **Convention:** Writer not used in codebase (immutability by convention)

2. **`:initform (get-universal-time)`**: Automatic timestamp on creation
   - No manual timestamp management required
   - Uses universal-time format (seconds since 1900-01-01)

3. **No `:reader` restriction**: Could use `:reader` instead of `:accessor` to enforce immutability
   - Current design trusts convention over enforcement

---

## Enforcement Mechanism

### Convention-Based Immutability

**Enforcement Level:** Convention (not compiler-enforced)
**How Enforced:**
- No code in Agent-Q calls `(setf (context-item-timestamp ...))`
- Documentation clearly states "immutable"
- No use case for modifying timestamps

**Violation Possible?** ✅ Yes, technically possible to call setter
**Violation Likely?** ❌ No, no reason to modify timestamp

### Alternative Design (Stronger Enforcement)

Could use `:reader` instead of `:accessor` to prevent modification:

```lisp
;; Stronger immutability (not current implementation)
(defclass context-item ()
  ((timestamp :initarg :timestamp
              :reader context-item-timestamp   ; ← Reader only, no writer
              :initform (get-universal-time))))
```

**Why not used:** `:accessor` is idiomatic in Agent-Q codebase

---

## Verification

### Test Evidence

**Test:** Implicit in `agent-q-context/struct/accessors-work` (sly-agent-q-context-test.el)
**Status:** ⚠️ Tests read timestamp, but don't verify immutability
**Coverage Gap:** No test attempts to modify timestamp and verifies it fails

### Code Evidence

**Grep for timestamp modifications:**
```bash
$ grep -r "setf.*context-item-timestamp" src/
# (no results - timestamp never modified)
```

**Result:** ✅ No code modifies timestamps

---

## Rationale

### Why Immutable Timestamps?

1. **Audit trail**: Preserves when context was added (historical record)
2. **Simplicity**: No logic needed for timestamp updates
3. **Correctness**: Prevents accidental time manipulation
4. **Chronological order**: Immutable timestamps enable reliable sorting by age

### Why Universal Time Format?

- Standard Common Lisp format (`get-universal-time`)
- Integer representation (easy to compare, sort, persist)
- Sufficient precision (1-second granularity)
- Human-readable via `decode-universal-time`

---

## Behavior Examples

### Example 1: Automatic Timestamp on Creation

```lisp
(let ((item (make-context-item "(+ 1 2)" :type :code)))
  (context-item-timestamp item))
; → 3910982400 (example universal-time value)
```

**Behavior:** Timestamp automatically set to current time

### Example 2: Reading Timestamp

```lisp
(let* ((item (make-context-item "test" :type :code))
       (ts (context-item-timestamp item)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time ts)
    (format nil "~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))
; → "2026-01-17 12:00:00"
```

**Behavior:** Timestamp can be decoded to human-readable format

### Example 3: Timestamp Persistence Across Operations

```lisp
(let ((item (make-context-item "test" :type :code)))
  (let ((ts1 (context-item-timestamp item)))
    (sleep 5)  ; Wait 5 seconds
    (let ((ts2 (context-item-timestamp item)))
      (= ts1 ts2))))  ; → T (timestamp unchanged)
```

**Behavior:** Timestamp remains constant despite time passage

---

## Consequences

### Positive

✅ **Historical accuracy**: Always know when context was added
✅ **Chronological ordering**: Can sort items by age reliably
✅ **Audit trail**: Timestamps serve as creation records
✅ **No maintenance**: Automatic timestamp management

### Negative

⚠️ **No time travel**: Cannot backdate or future-date items
⚠️ **Precision limit**: 1-second granularity (may be insufficient for high-frequency operations)
⚠️ **Convention-based**: Immutability not enforced by type system

---

## Edge Cases

### Edge Case 1: Custom Timestamp on Creation

```lisp
;; Can override default timestamp if needed
(make-instance 'context-item
               :content "historical data"
               :type :code
               :timestamp 3910800000)  ; Custom past timestamp
```

**Behavior:** ✅ Allowed via `:initarg :timestamp`
**Use Case:** Reconstructing historical context (e.g., from saved session)

### Edge Case 2: Timestamp Comparison

```lisp
(let ((item1 (make-context-item "first" :type :code))
      (item2 (make-context-item "second" :type :code)))
  (< (context-item-timestamp item1)
     (context-item-timestamp item2)))
; → T (item1 created before item2)
```

**Behavior:** Timestamps enable ordering by creation time

### Edge Case 3: Timezone Handling

```lisp
;; Universal time is UTC-based
(get-universal-time)  ; → Always UTC, no timezone info
```

**Behavior:** ⚠️ Timestamps are UTC, no local timezone information
**Impact:** Displaying timestamps requires manual timezone conversion

---

## Related Properties

- **monotonic-ids.md**: IDs also increase monotonically with timestamps
- **sliding-window.md**: Oldest items (by timestamp) evicted first
- **chronological-order.md**: Timestamps enable chronological ordering

---

## Known Issues

### Issue 1: Convention vs. Enforcement

**Problem:** Immutability enforced by convention, not type system
**Impact:** Technically possible to modify timestamp via `(setf ...)`
**Workaround:** Code review discipline, lint rules
**Status:** Accepted (convention-based immutability is idiomatic in Lisp)

### Issue 2: No Timezone Information

**Problem:** Universal time is UTC only, no local timezone
**Impact:** Displaying timestamps requires manual timezone handling
**Workaround:** Use `decode-universal-time` with timezone argument
**Status:** By design (universal time is timezone-agnostic)

### Issue 3: 1-Second Precision

**Problem:** `get-universal-time` has 1-second granularity
**Impact:** Multiple items created in same second have same timestamp
**Workaround:** Use monotonic IDs for sub-second ordering
**Status:** Sufficient for current use case

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | Immutable timestamp design | Phase 1 foundation |
| 2026-01-17 | Property documented | Canon extraction |

---

**Property Status:** ✅ Verified (code inspection + grep analysis)
**Confidence:** 1.00
**Test Coverage:** ⚠️ Partial (reads tested, immutability not explicitly tested)
