# Property: Sliding Window

**Feature:** context-management
**Category:** Capacity Constraint
**Source:** src/context.lisp:47, 61-68
**Confidence:** 1.00

---

## Statement

**The context manager maintains a sliding window of at most 50 context items, automatically evicting the oldest item when capacity is exceeded.**

---

## Formal Definition

### Capacity Invariant

```
∀ manager : context-manager,
  (length (context-items manager)) ≤ (context-max-items manager)
```

Where `context-max-items` defaults to 50.

### FIFO Eviction Rule

```
When add-context is called and (length items) = max-items:
  1. Remove item at index 0 (oldest)
  2. Append new item at end (newest)

Post-condition: (length items) = max-items (unchanged)
```

---

## Implementation

### Declaration

```lisp
(defclass context-manager ()
  ((max-items :initarg :max-items
              :initform 50        ; ← Default capacity
              :accessor context-max-items)))
```

### Enforcement Mechanism

```lisp
(defmethod add-context ((manager context-manager) (item context-item) &key type metadata)
  "Add existing context-item to manager."
  (let ((items (context-items manager)))
    ;; Capacity check
    (when (>= (length items) (context-max-items manager))
      ;; FIFO eviction: remove oldest (index 0)
      (setf items (subseq items 1))  ; Shift left
      (setf (context-items manager)
            (make-array (length items)
                       :adjustable t
                       :fill-pointer (length items)
                       :initial-contents items)))
    ;; Append new item
    (vector-push-extend item (context-items manager))))
```

**Enforcement:** Automatic via `add-context` method
**Type:** Runtime enforcement (no compile-time check)

---

## Verification

### Test Evidence

**Test:** Implicit in `agent-q-context/operations/add-item` tests
**Status:** ⚠️ Not explicitly tested (coverage gap)
**Confidence:** 1.00 (verified by code inspection)

### Specification Evidence

**Source:** specs/PHASE-1-SPEC.md:158
```
When context exceeds 50 items, oldest item is automatically removed.
```

**Alignment:** ✅ Code matches specification exactly

---

## Rationale

### Why Sliding Window?

1. **Bounded Memory**: Prevents unbounded context accumulation
2. **LLM Token Limits**: 50 items ≈ reasonable prompt size
3. **Recency Bias**: Oldest context least relevant to current work
4. **Simplicity**: FIFO eviction requires no heuristics

### Why 50?

- Empirical choice balancing context richness vs. token budget
- Configurable via `:max-items` initarg for customization
- No formal justification in documentation

---

## Behavior Examples

### Example 1: Normal Addition (Below Capacity)

```lisp
;; Initial: 0 items
(let ((manager (make-context-manager)))
  (add-context manager "item-1")
  (add-context manager "item-2")
  ;; State: [item-1 item-2], length = 2
  )
```

**Behavior:** Items accumulate, no eviction

### Example 2: Eviction at Capacity

```lisp
;; Initial: 50 items (at capacity)
(let ((manager (make-context-manager)))
  ;; ... (add 50 items) ...
  ;; State: [item-1 item-2 ... item-50], length = 50

  (add-context manager "item-51")
  ;; State: [item-2 item-3 ... item-50 item-51], length = 50
  ;; item-1 evicted
  )
```

**Behavior:** Oldest item (item-1) removed, item-51 appended

### Example 3: Custom Capacity

```lisp
;; Create manager with 100-item capacity
(let ((manager (make-context-manager :max-items 100)))
  ;; ... can now accumulate 100 items before eviction ...
  )
```

**Behavior:** Capacity constraint respects custom value

---

## Consequences

### Positive

✅ **Memory bounded**: No risk of OOM from context accumulation
✅ **Automatic management**: Users don't manually manage capacity
✅ **Predictable behavior**: Always FIFO, no surprises

### Negative

⚠️ **Data loss**: Oldest items permanently discarded (no recovery)
⚠️ **Non-obvious threshold**: Users may not realize when eviction occurs
⚠️ **No semantic awareness**: Evicts based on age, not importance

---

## Related Properties

- **chronological-order.md**: Items ordered by age (enables FIFO)
- **monotonic-ids.md**: IDs increase monotonically with age

---

## Known Issues

### Issue 1: No Eviction Notification

**Problem:** Users not informed when items are evicted
**Impact:** May assume context is preserved indefinitely
**Workaround:** Check `(length (context-items manager))` periodically
**Status:** Open (no plan to address)

### Issue 2: Performance Cost

**Problem:** Eviction is O(n) due to `subseq` + array recreation
**Impact:** Adds latency to `add-context` when at capacity
**Workaround:** Keep `max-items` < 100 to minimize impact
**Status:** Accepted (not a bottleneck in practice)

### Issue 3: No Token-Based Capacity

**Problem:** 50-item limit doesn't account for item sizes
**Impact:** 50 large items may exceed LLM context limits
**Workaround:** Clear context or use `get-context :limit` proactively
**Status:** Future work (Phase 3+)

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | Initial 50-item capacity | Phase 1 design decision |
| 2026-01-17 | Property documented | Canon extraction |

---

**Property Status:** ✅ Verified (code + spec alignment)
**Confidence:** 1.00
**Test Coverage:** ⚠️ Implicit (not explicitly tested)
