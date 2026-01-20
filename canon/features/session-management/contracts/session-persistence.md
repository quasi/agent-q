# Contract: Session Persistence

**Confidence:** 1.00
**Source:** src/session.lisp:236-403

---

## Purpose

Define the disk persistence protocol for saving, loading, deleting, listing, and searching sessions.

---

## Save Protocol

**Signature:**

```lisp
(defun save-session (session &optional (manager (ensure-session-manager)))
  "Save SESSION to disk as a readable S-expression file.
Returns filepath."
```

**Behavior:**

1. Ensure sessions directory exists
2. Update `session-updated-at` to current time
3. Generate filename: `{session-id}.lisp`
4. Write file with header comments + serialized plist
5. Update session cache
6. Return filepath

**File Format:**

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Agent-Q Session v2
;;; Created: 2026-01-20 14:30:22
;;; Name: Debug Session

(:version 2
 :id "session-20260120-143022-A4F2"
 :name "Debug Session"
 :created-at 3945234622
 :updated-at 3945237800
 :model "claude-sonnet-4-20250514"
 :metadata (:total-input-tokens 1000 :total-output-tokens 500)
 :messages ((:role :user :content "Hello" :timestamp 3945234622)
            (:role :assistant :content "Hi!" :timestamp 3945234650)))
```

**Pretty Printing:**

- `*print-pretty*` = `t`
- `*print-right-margin*` = 100
- `*print-case*` = `:downcase`

---

## Load Protocol

**Signature:**

```lisp
(defun load-session (session-id &optional (manager (ensure-session-manager)))
  "Load session by SESSION-ID from disk. Returns NIL if not found."
```

**Behavior:**

1. Check cache first (return if found)
2. Construct filepath from session-id
3. Check if file exists
4. Read file, skipping header comments
5. Parse plist (via `read`)
6. Deserialize to session object (handles v1/v2)
7. Update cache
8. Return session (or NIL if not found/error)

**Error Handling:**

```lisp
(handler-case
    (with-open-file (stream filepath :direction :input)
      ;; Skip comments, read plist, deserialize
      ...)
  (error (e)
    (warn "Failed to load session ~A: ~A" session-id e)
    nil))
```

**Comment Skipping:**

```lisp
;; Skip header comments starting with ';'
(loop for char = (peek-char t stream nil nil)
      while (and char (char= char #\;))
      do (read-line stream nil nil))
```

---

## Delete Protocol

**Signature:**

```lisp
(defun delete-session (session-id &optional (manager (ensure-session-manager)))
  "Delete session by SESSION-ID from disk and cache. Returns T if deleted."
```

**Behavior:**

1. Remove from cache (`remhash`)
2. Construct filepath
3. Check file exists
4. Delete file (`delete-file`)
5. Return `t` if deleted, `nil` otherwise

---

## List Protocol

**Signature:**

```lisp
(defun list-sessions (&optional (manager (ensure-session-manager)))
  "List all sessions as plists with :id, :name, :created-at.
Returns sessions sorted by created-at (most recent first)."
```

**Behavior:**

1. Find all `session-*.lisp` files in directory
2. For each file, extract metadata via `read-session-metadata-fast`
3. Sort by `:created-at` descending
4. Return list of plists

**Fast Metadata Extraction:**

```lisp
(defun read-session-metadata-fast (filepath)
  "Read just the metadata from a session file's header comments.
This is faster than loading the full session for listing purposes."
  (handler-case
      (with-open-file (stream filepath :direction :input)
        (let ((id (pathname-name filepath))
              (name nil)
              (created nil))
          ;; Parse header comments
          (loop for line = (read-line stream nil nil)
                while (and line
                           (> (length line) 0)
                           (char= (char line 0) #\;))
                do (cond
                     ((search "Name: " line)
                      (setf name (subseq line (+ (search "Name: " line) 6))))
                     ((search "Created: " line)
                      (setf created (parse-timestamp-string
                                     (subseq line (+ (search "Created: " line) 9)))))))
          (list :id id :name name :created-at created)))
    (error () nil)))
```

**Return Example:**

```lisp
((:id "session-20260120-143022-A4F2"
  :name "Debug Session"
  :created-at 3945234622)
 (:id "session-20260119-091500-B3C1"
  :name nil
  :created-at 3945148500))
```

---

## Search Protocol

**Signature:**

```lisp
(defun search-sessions (query &optional (manager (ensure-session-manager)))
  "Search sessions by name and message content.
QUERY is a string to search for (case-insensitive).
Returns list of session metadata plists for matching sessions."
```

**Algorithm:**

1. Convert query to lowercase
2. Get all sessions via `list-sessions`
3. For each session:
   - Check name first (fast, from metadata)
   - If no name match, load session and check message content
4. Return matching sessions

**Optimization:** Name check is fast (no load), content check requires full session load.

**Content Check:**

```lisp
(defun session-contains-query-p (session query-lower)
  "Check if SESSION contains QUERY-LOWER in any message content."
  (some (lambda (msg)
          (search query-lower (string-downcase (message-content msg))))
        (session-messages session)))
```

---

## Session Switching

**Signature:**

```lisp
(defun switch-session (session-id &optional (manager (ensure-session-manager)))
  "Switch to session by SESSION-ID, saving current session first.
Returns the new current session, or NIL if not found."
```

**Behavior:**

1. Save current session (if exists)
2. Load new session
3. Update `(current-session manager)` pointer
4. Return new session (or NIL if not found)

---

## Session Creation

**Signature:**

```lisp
(defun create-session (&key name model (manager (ensure-session-manager)))
  "Create a new session, save current first, and make it current.
Returns the new session."
```

**Behavior:**

1. Save current session (if exists)
2. Create new session via `make-session`
3. Set as current session
4. Add to cache
5. Return new session

---

## Session Lifecycle Guarantee

**Invariant:** Current session is ALWAYS saved before switching or creating new session.

```lisp
;; Pattern used in both switch-session and create-session
(when (current-session manager)
  (save-session (current-session manager) manager))
```

This prevents data loss during session transitions.

---

## Directory Management

**Ensure Directory:**

```lisp
(defun ensure-sessions-directory (&optional (manager (ensure-session-manager)))
  "Ensure the sessions directory exists."
  (let ((dir (sessions-directory manager)))
    (ensure-directories-exist (merge-pathnames "dummy.txt" dir))
    dir))
```

**Default Location:** `~/.emacs.d/agent-q-sessions/`

---

## Timestamp Utilities

**Format for Display:**

```lisp
(defun format-timestamp (universal-time)
  "Format UNIVERSAL-TIME as YYYY-MM-DD HH:MM:SS."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))
```

**Parse from String:**

```lisp
(defun parse-timestamp-string (str)
  "Parse a timestamp string YYYY-MM-DD HH:MM:SS to universal-time.
Returns NIL on parse failure."
  (handler-case
      (let ((year (parse-integer str :start 0 :end 4))
            (month (parse-integer str :start 5 :end 7))
            (day (parse-integer str :start 8 :end 10))
            (hour (parse-integer str :start 11 :end 13))
            (min (parse-integer str :start 14 :end 16))
            (sec (parse-integer str :start 17 :end 19)))
        (encode-universal-time sec min hour day month year))
    (error () nil)))
```

---

## Cache Management

**Cache Structure:** `(make-hash-table :test 'equal)`

**Cache Operations:**
- **Insert:** `(setf (gethash session-id cache) session)`
- **Lookup:** `(gethash session-id cache)`
- **Remove:** `(remhash session-id cache)`

**Cache Invalidation:** Only on explicit delete. Save operations update cache.

---

**Status:** ✅ Fully implemented with error handling
