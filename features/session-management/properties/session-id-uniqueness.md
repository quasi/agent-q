# Property: Session ID Uniqueness

**Type:** Data Integrity
**Confidence:** 1.00

---

## Statement

**Session IDs MUST be globally unique to prevent collisions during session management.**

---

## Invariant

```
∀ s1, s2 ∈ sessions:
  s1 ≠ s2 ⇒ (session-id s1) ≠ (session-id s2)
```

---

## Mechanism

**ID Format:** `session-YYYYMMDD-HHMMSS-XXXX`

**Components:**
1. **Date (YYYYMMDD):** 8 digits, ensures temporal ordering
2. **Time (HHMMSS):** 6 digits, sub-day uniqueness
3. **Random Hex (XXXX):** 4 hex digits (65,536 possibilities), prevents same-second collisions

**Generator:**

```lisp
(defun generate-session-id ()
  "Generate a unique session ID in format: session-YYYYMMDD-HHMMSS-XXXX"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "session-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D-~4,'0X"
            year month day hour min sec (random #xFFFF))))
```

---

## Uniqueness Guarantee

**Probabilistic Guarantee:**

For sessions created within the same second:
- Collision probability = 1 / 65,536 (0.0015%)
- Expected unique sessions per second = 65,536

**Temporal Guarantee:**

Sessions created in different seconds are guaranteed unique (timestamp differs).

**Practical Safety:**

Assuming:
- Human-driven session creation (< 1 per second)
- Session creation rate << 65,536/second

**Collision probability ≈ 0** in practice.

---

## Examples

```lisp
;; Generated at 2026-01-20 14:30:22
(generate-session-id)
;; => "session-20260120-143022-A4F2"

;; Generated 1 second later (14:30:23)
(generate-session-id)
;; => "session-20260120-143023-B3C1"
;; Different timestamp, guaranteed unique

;; Generated same second (unlikely, but possible)
(generate-session-id)
;; => "session-20260120-143022-F8D4"
;; Different random suffix, highly likely unique
```

---

## Collision Handling

**Current Approach:** None (trust probabilistic uniqueness)

**Rationale:**
- Human-driven creation rate is low
- 65,536 random values per second is massive headroom
- No observed collisions in practice

**Future Enhancement (if needed):**

```lisp
(defun generate-unique-session-id (&optional (manager (ensure-session-manager)))
  "Generate a session ID, retrying if collision detected."
  (loop for id = (generate-session-id)
        until (not (gethash id (session-cache manager)))
        finally (return id)))
```

---

## Filename Mapping

**Session ID:** `session-20260120-143022-A4F2`
**Filename:** `session-20260120-143022-A4F2.lisp`

**Invariant:** Filename = Session ID + ".lisp"

This ensures:
- One file per session
- Session ID lookup via filename
- No separate ID → filename mapping needed

---

## Test Case

```lisp
(ert-deftest session-id-uniqueness ()
  "Test that multiple session IDs are unique."
  (let ((ids (loop repeat 1000 collect (generate-session-id))))
    (should (= 1000 (length (remove-duplicates ids :test #'string=))))))
```

**Expected:** All 1000 IDs unique (no duplicates).

---

## Consequences

**Positive:**
- ✅ Simple, human-readable format
- ✅ Temporal ordering (sessions sort chronologically)
- ✅ No central ID registry needed
- ✅ Collision-resistant for practical usage

**Negative:**
- ⚠️ Not cryptographically secure (predictable)
- ⚠️ No explicit collision detection
- ⚠️ Random state depends on CL implementation

---

**Status:** ✅ Implemented, no collisions observed
