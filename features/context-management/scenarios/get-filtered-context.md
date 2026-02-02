# Scenario: Get Filtered Context

**Feature:** context-management
**User Story:** As the agent system, I want to retrieve context items filtered by type and limited by count to optimize LLM prompts.
**Test Coverage:** Implicit (internal API, not user-facing)
**Confidence:** 1.00

---

## Context

The `get-context` method allows programmatic retrieval of context items with optional filtering by type and limiting by count. This is used internally when constructing LLM prompts or analyzing context composition.

---

## Actors

- **Agent System**: Constructs prompts and manages context
- **Context Manager**: Provides filtered access to items

---

## Preconditions

Context manager contains mixed items:
```
items = [
  ctx-1 (:code, "defun foo")
  ctx-2 (:error, "ERROR: ...")
  ctx-3 (:code, "defun bar")
  ctx-4 (:error, "WARNING: ...")
  ctx-5 (:code, "defun baz")
  ctx-6 (:text, "notes...")
]
```

---

## Main Flow

### Step 1: Retrieve All Items

**Call:** `(get-context manager)`

**Result:**
```lisp
(list ctx-1 ctx-2 ctx-3 ctx-4 ctx-5 ctx-6)
```

**Behavior:** Returns all items as list, preserving order

### Step 2: Filter by Type

**Call:** `(get-context manager :types '(:error))`

**Result:**
```lisp
(list ctx-2 ctx-4)  ; Only :error items
```

**Algorithm:**
```lisp
(remove-if-not (lambda (item)
                 (member (context-item-type item) '(:error)))
               all-items)
```

### Step 3: Limit Count (Most Recent)

**Call:** `(get-context manager :limit 3)`

**Result:**
```lisp
(list ctx-4 ctx-5 ctx-6)  ; Last 3 items
```

**Algorithm:**
```lisp
(subseq all-items (max 0 (- (length all-items) 3)))
```

**Important:** Takes **most recent** items (from end of list)

### Step 4: Combine Filters

**Call:** `(get-context manager :types '(:code) :limit 2)`

**Result:**
```lisp
(list ctx-3 ctx-5)  ; Last 2 :code items
```

**Algorithm:**
1. Filter by type → `(ctx-1 ctx-3 ctx-5)`
2. Limit to last 2 → `(ctx-3 ctx-5)`

---

## Postconditions

1. Manager state unchanged (read-only operation)
2. Returned list is fresh (not shared with internal vector)
3. Order preserved (chronological)
4. Items are references (not copies)

---

## Alternative Flows

### Alt 1: No Matches

**Call:** `(get-context manager :types '(:file))`

**Result:** `nil` (empty list)

**Behavior:** No error, just empty result

### Alt 2: Limit Exceeds Item Count

**Call:** `(get-context manager :limit 100)` (manager has 6 items)

**Result:** All 6 items returned

**Behavior:** `(max 0 (- 6 100))` = 0, so `(subseq items 0)` = all items

### Alt 3: Multiple Type Filters

**Call:** `(get-context manager :types '(:code :error))`

**Result:** All :code and :error items (excludes :text)

```lisp
(list ctx-1 ctx-2 ctx-3 ctx-4 ctx-5)  ; Excludes ctx-6 (:text)
```

---

## Implementation Details

### Method Signature

```lisp
(defmethod get-context ((manager context-manager) &key types limit)
  "Get context items, optionally filtered and limited."
  (let* ((items (coerce (context-items manager) 'list))
         (filtered (if types
                      (remove-if-not (lambda (item)
                                      (member (context-item-type item) types))
                                    items)
                      items))
         (limited (if limit
                     (subseq filtered (max 0 (- (length filtered) limit)))
                     filtered)))
    limited))
```

### Key Design Choices

1. **Convert to list first**: `(coerce ... 'list)` enables functional operations
2. **Filter before limit**: Ensures limit applies to filtered set, not original
3. **Take from end**: `(- (length ...) limit)` selects most recent items
4. **`max 0`**: Prevents negative indices if limit > length

---

## Use Cases

### Use Case 1: Error Summary

**Goal:** Show only recent errors in prompt

```lisp
(let ((errors (get-context manager :types '(:error) :limit 5)))
  (format nil "Recent errors: ~{~A~^, ~}"
          (mapcar #'context-item-content errors)))
```

### Use Case 2: Code-Only Context

**Goal:** Exclude non-code items from LLM prompt

```lisp
(defun build-code-only-prompt (manager)
  (let ((code-items (get-context manager :types '(:code))))
    (context-to-string-filtered manager code-items)))
```

### Use Case 3: Context Statistics

**Goal:** Analyze context composition

```lisp
(defun context-stats (manager)
  (list :total (length (get-context manager))
        :code (length (get-context manager :types '(:code)))
        :errors (length (get-context manager :types '(:error)))
        :recent-10 (length (get-context manager :limit 10))))
```

---

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Reason |
|-----------|------------|--------|
| `get-context` | O(n) | Convert vector to list |
| `get-context :types` | O(n) | Filter all items |
| `get-context :limit` | O(1) | Subseq from end |
| Combined | O(n) | Dominated by filtering |

Where n = number of items in manager (max 50)

### Space Complexity

**O(n)** - Creates new list (not view into original vector)

**Important:** Result is not shared with manager's internal vector
- Modifying result doesn't affect manager
- Manager changes don't affect returned list (snapshot)

---

## Verification

### Manual Test

```lisp
;; Test filtering and limiting
(let ((manager (make-context-manager)))
  (add-context manager "code1" :type :code)
  (add-context manager "error1" :type :error)
  (add-context manager "code2" :type :code)
  (add-context manager "error2" :type :error)

  ;; Test 1: Get all
  (assert (= 4 (length (get-context manager))))

  ;; Test 2: Filter by type
  (assert (= 2 (length (get-context manager :types '(:code)))))

  ;; Test 3: Limit
  (assert (= 2 (length (get-context manager :limit 2))))

  ;; Test 4: Combine
  (let ((result (get-context manager :types '(:error) :limit 1)))
    (assert (= 1 (length result)))
    (assert (string= "error2" (context-item-content (first result))))))
```

**Status:** ⚠️ Not in test suite (internal API)

---

## Related Scenarios

- **add-context-item.md**: Items retrieved here were added there
- **format-for-llm.md**: Uses `get-context` to build prompts

---

## Related Properties

- **sliding-window.md**: Limits total items available
- **type-safety.md**: Ensures valid types for filtering

---

## Known Limitations

### L-001: No Index-Based Access

**Problem:** Cannot get item by index (e.g., "get 5th item")
**Workaround:** `(nth 4 (get-context manager))`
**Status:** Accepted (not a common use case)

### L-002: No Reverse Order

**Problem:** Cannot easily get oldest N items
**Workaround:** `(subseq (get-context manager) 0 N)`
**Status:** Acceptable (recency bias is intentional)

### L-003: O(n) List Conversion

**Problem:** Always converts vector to list, even if not needed
**Impact:** Adds overhead for large contexts
**Optimization:** Could add `:as-vector` keyword for zero-copy access
**Status:** Not implemented (n ≤ 50, impact negligible)

---

## Future Enhancements

### Enhancement 1: Predicate-Based Filtering

Allow arbitrary predicates:
```lisp
(get-context manager :predicate (lambda (item)
                                  (> (length (context-item-content item)) 100)))
; → Items with content > 100 chars
```

### Enhancement 2: Sorting

Return sorted by timestamp, ID, or custom key:
```lisp
(get-context manager :sort-by :timestamp :order :descending)
```

### Enhancement 3: Pagination

Support offset + limit for pagination:
```lisp
(get-context manager :offset 10 :limit 10)
; → Items 11-20 (zero-indexed)
```

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | get-context implemented | Phase 1 foundation |
| 2026-01-17 | Scenario documented | Canon extraction |

---

**Scenario Status:** ✅ Verified (code inspection)
**Confidence:** 1.00
**Test Coverage:** ⚠️ Not explicitly tested (internal API)
