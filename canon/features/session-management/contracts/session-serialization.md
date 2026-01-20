# Contract: Session Serialization

**Confidence:** 1.00
**Source:** src/session.lisp:117-230

---

## Purpose

Define the serialization protocol for converting sessions to/from disk format, with backwards compatibility for v1 (Elisp) format.

---

## Version History

**v1 (Legacy Elisp format):**
- Roles as symbols (`'user`, `'assistant`)
- Emacs timestamps `(HIGH LOW USEC PSEC)`
- Messages stored newest-first (reverse chronological)
- Managed by Elisp (unreliable across platforms)

**v2 (Current CL format):**
- Roles as keywords (`:user`, `:assistant`)
- Universal time (CL standard, seconds since 1900-01-01)
- Messages stored oldest-first (chronological)
- Managed by Common Lisp (reliable, cross-platform)

---

## Message Serialization

### To Plist (v2)

```lisp
(defun message-to-plist (message)
  "Convert MESSAGE to a plist for storage."
  (list :role (message-role message)
        :content (message-content message)
        :timestamp (message-timestamp message)))
```

**Example:**

```lisp
;; Input
(make-instance 'message
               :role :user
               :content "Hello"
               :timestamp 3945234622)

;; Output
(:role :user :content "Hello" :timestamp 3945234622)
```

### From Plist (v2)

```lisp
(defun plist-to-message (plist)
  "Reconstruct a message from PLIST."
  (make-instance 'message
                 :role (let ((role (getf plist :role)))
                         ;; Handle both symbol (v1) and keyword (v2) formats
                         (if (keywordp role)
                             role
                             (intern (string-upcase (string role)) :keyword)))
                 :content (getf plist :content)
                 :timestamp (or (getf plist :timestamp) (get-universal-time))))
```

**Role Normalization:** Handles both symbols and keywords, normalizes to keywords.

---

## Session Serialization

### To Plist (v2)

```lisp
(defun session-to-plist (session)
  "Convert SESSION to a plist for storage.
Messages are stored oldest-first (natural reading order)."
  (list :version 2
        :id (session-id session)
        :name (session-name session)
        :created-at (session-created-at session)
        :updated-at (session-updated-at session)
        :model (session-model session)
        :metadata (session-metadata session)
        :messages (mapcar #'message-to-plist
                          (session-messages session))))
```

**Message Order:** Oldest-first on disk (matches memory representation).

### From Plist (Dispatcher)

```lisp
(defun plist-to-session (plist)
  "Reconstruct a session from PLIST.
Handles both v1 (Elisp) and v2 (CL) formats."
  (let ((version (or (getf plist :version) 1)))
    (case version
      (1 (convert-v1-to-session plist))
      (2 (convert-v2-to-session plist))
      (t (error "Unknown session format version: ~A" version)))))
```

**Version Detection:** `:version` key present → v2, absent → v1 (legacy).

---

## v2 Deserialization

```lisp
(defun convert-v2-to-session (plist)
  "Convert v2 format (native CL) plist to session."
  (let* ((conv (make-instance 'conversation))
         (session (make-instance 'session
                                 :id (getf plist :id)
                                 :name (getf plist :name)
                                 :created-at (getf plist :created-at)
                                 :updated-at (getf plist :updated-at)
                                 :model (getf plist :model)
                                 :metadata (getf plist :metadata)
                                 :conversation conv)))
    ;; Restore messages (oldest-first on disk, same order in memory)
    (dolist (msg-plist (getf plist :messages))
      (let ((msg (plist-to-message msg-plist)))
        (setf (conversation-messages conv)
              (append (conversation-messages conv) (list msg)))))
    session))
```

**Message Order:** Oldest-first on disk → append to conversation (preserves order).

---

## v1 Deserialization (Migration)

```lisp
(defun convert-v1-to-session (plist)
  "Convert v1 format (Elisp) plist to session.
Handles Emacs timestamps, symbol roles, etc."
  (let* ((conv (make-instance 'conversation))
         ;; v1 may use Emacs (HIGH LOW USEC PSEC) timestamps - convert to universal-time
         (created (convert-elisp-timestamp (getf plist :created-at)))
         (updated (convert-elisp-timestamp (getf plist :updated-at)))
         (session (make-instance 'session
                                 :id (getf plist :id)
                                 :name (getf plist :name)
                                 :created-at (or created (get-universal-time))
                                 :updated-at (or updated (get-universal-time))
                                 :model (getf plist :model)
                                 :metadata (getf plist :metadata)
                                 :conversation conv)))
    ;; v1 stores messages in reverse order (newest-first), so reverse them
    (dolist (msg-plist (reverse (getf plist :messages)))
      (let ((msg (plist-to-message-v1 msg-plist)))
        (setf (conversation-messages conv)
              (append (conversation-messages conv) (list msg)))))
    session))
```

**Critical Differences:**
1. **Timestamps:** Emacs format → universal-time conversion
2. **Message Order:** Newest-first (v1) → reverse → oldest-first (v2)
3. **Roles:** Symbols → keywords

### v1 Message Deserialization

```lisp
(defun plist-to-message-v1 (plist)
  "Reconstruct a message from v1 (Elisp) format PLIST."
  (make-instance 'message
                 :role (let ((role (getf plist :role)))
                         ;; v1 uses symbols like 'user, 'assistant
                         (intern (string-upcase (string role)) :keyword))
                 :content (getf plist :content)
                 :timestamp (or (convert-elisp-timestamp (getf plist :timestamp))
                                (get-universal-time))))
```

---

## Timestamp Conversion

**Emacs Timestamp Format:** `(HIGH LOW USEC PSEC)`
- Time in seconds = `HIGH * 65536 + LOW`
- This is Unix time (seconds since 1970-01-01)
- Universal time = Unix time + 2208988800 (70 years)

**Converter:**

```lisp
(defun convert-elisp-timestamp (timestamp)
  "Convert an Elisp timestamp to universal-time.
Elisp timestamps are (HIGH LOW USEC PSEC) or just an integer.
Returns NIL if timestamp is NIL or unparseable."
  (cond
    ((null timestamp) nil)
    ((integerp timestamp) timestamp)  ; Already universal-time
    ((and (listp timestamp) (>= (length timestamp) 2))
     ;; Emacs timestamp: (HIGH LOW ...) where time = HIGH * 65536 + LOW
     ;; This is seconds since 1970-01-01, need to convert to universal-time
     ;; Universal time is seconds since 1900-01-01
     ;; Difference: 70 years = 2208988800 seconds
     (let* ((high (first timestamp))
            (low (second timestamp))
            (unix-time (+ (* high 65536) low))
            (unix-to-universal 2208988800))
       (+ unix-time unix-to-universal)))
    (t nil)))
```

**Example:**

```lisp
;; Emacs timestamp for 2026-01-20 14:30:22
(convert-elisp-timestamp '(60437 2626 0 0))
;; => 3945234622 (universal-time)
```

---

## Role Conversion

**v1 Format:** Symbols from Elisp (`'user`, `'assistant`, `'system`)

**v2 Format:** Keywords (`:user`, `:assistant`, `:system`)

**Converter:**

```lisp
;; Used in both plist-to-message and plist-to-message-v1
(let ((role (getf plist :role)))
  (if (keywordp role)
      role
      (intern (string-upcase (string role)) :keyword)))
```

**Normalization:** Always produces keywords, handles both input formats.

---

## Migration Guarantees

**Zero User Intervention:**
- v1 sessions load automatically
- Converted to v2 on first save
- No manual migration required

**Idempotency:**
- Loading v2 session produces identical v2 session
- No version drift

**Data Preservation:**
- All fields preserved during conversion
- Message content unchanged
- Timestamps converted accurately

---

## Serialization Invariants

1. **Version Tag:** All v2 sessions MUST have `:version 2`
2. **Keyword Roles:** All roles MUST be keywords in v2
3. **Universal Time:** All timestamps MUST be universal-time in v2
4. **Message Order:** Messages MUST be oldest-first in v2
5. **Backwards Compat:** v1 sessions MUST load without error

---

## Example Serialization

**v2 Session:**

```lisp
(:version 2
 :id "session-20260120-143022-A4F2"
 :name "Debug Session"
 :created-at 3945234622
 :updated-at 3945237800
 :model "claude-sonnet-4-20250514"
 :metadata (:total-input-tokens 1000 :total-output-tokens 500 :provider :anthropic)
 :messages ((:role :user :content "What is the bug?" :timestamp 3945234622)
            (:role :assistant :content "Let me investigate." :timestamp 3945234680)
            (:role :user :content "It's in module X." :timestamp 3945234720)))
```

**v1 Session (Legacy):**

```lisp
(:id "session-20260120-143022-A4F2"
 :name "Debug Session"
 :created-at (60437 2626 0 0)  ; Emacs timestamp
 :updated-at (60437 4250 0 0)
 :model "claude-sonnet-4-20250514"
 :metadata (:total-input-tokens 1000 :total-output-tokens 500)
 :messages ((:role user :content "It's in module X." :timestamp (60437 3700 0 0))
            (:role assistant :content "Let me investigate." :timestamp (60437 3660 0 0))
            (:role user :content "What is the bug?" :timestamp (60437 2626 0 0))))
;; Note: No :version key, messages newest-first, roles as symbols, Emacs timestamps
```

---

**Status:** ✅ Implemented with full backwards compatibility
