# Property: 50KB Content Limit

**Type:** Resource Management
**Confidence:** 1.00

---

## Statement

**Context items MUST NOT exceed 50KB to prevent LLM context window overflow.**

---

## Invariant

```
∀ item ∈ context:
  (length (fetch-content item)) ≤ 50 * 1024 bytes
```

---

## Behavior

### Content Fetching

```elisp
(defun sly-agent-q-context--fetch-content (item)
  (let ((content (read-file-or-buffer item)))
    (if (> (length content) 51200)  ; 50KB
        (substring content 0 51200)   ; Truncate
      content)))
```

### Truncation

- Files >50KB: First 50KB returned
- No warning to user (silent truncation)
- LLM receives partial content

---

## Rationale

**Why 50KB?**
- Prevents context overflow (LLM has token limits)
- Most source files < 50KB
- Balance between completeness and safety

**Why silent truncation?**
- User typically wants context, not error
- LLM can still work with partial content
- Asking user for confirmation would disrupt flow

---

## Test Case

```elisp
(ert-deftest context-50kb-limit
  (let ((large-content (make-string 100000 ?x)))
    (let ((result (sly-agent-q-context--fetch-content large-content)))
      (should (= 51200 (length result))))))
```

---

## Consequences

**Positive:**
- ✅ Prevents context overflow
- ✅ Protects LLM from excessive input
- ✅ System remains responsive

**Negative:**
- ⚠️ Large files silently truncated
- ⚠️ User doesn't know content is partial
- ⚠️ LLM might miss critical code at end of file

---

**Status:** ✅ Enforced, tested
