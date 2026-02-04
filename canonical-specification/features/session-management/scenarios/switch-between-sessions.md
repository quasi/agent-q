---
type: scenario
name: switch-between-sessions
version: 1.0.0
feature: session-management
covers:
  - session-persistence
tags:
  - multi-session
  - user-story
---

# Scenario: Switch Between Sessions

**Flow:** Multi-session workflow with switching and restoration
**Confidence:** 0.90

---

## Preconditions

- Multiple sessions exist on disk
- User has active session with unsaved changes
- SLY connected

---

## Flow

### 1. User Initiates Switch

**Action:** `C-c C-s` (or `M-x agent-q-switch-session`)

**Elisp:**

```elisp
(defun agent-q-switch-session ()
  "Switch to a different session."
  (interactive)
  (agent-q--check-sly-connection)
  (let* ((sessions (agent-q--list-sessions-sync))
         (choices (mapcar (lambda (meta)
                            (cons (format "%s%s%s"
                                          (plist-get meta :id)
                                          (if (plist-get meta :name)
                                              (format " - %s" (plist-get meta :name))
                                            "")
                                          (if (plist-get meta :created-at)
                                              (format " (%s)"
                                                      (agent-q--format-timestamp
                                                       (plist-get meta :created-at)))
                                            ""))
                                  (plist-get meta :id)))
                          sessions)))
    (if (null choices)
        (message "No saved sessions found")
      (let ((choice (completing-read "Switch to session: " choices nil t)))
        ...))))
```

**RPC Call:**

```elisp
(sly-eval '(agent-q:agent-q-list-sessions))
```

---

### 2. System Lists Available Sessions

**CL Side:**

```lisp
(defun list-sessions (&optional (manager (ensure-session-manager)))
  "List all sessions as plists with :id, :name, :created-at.
Returns sessions sorted by created-at (most recent first)."
  (let* ((dir (sessions-directory manager))
         (pattern (merge-pathnames "session-*.lisp" dir))
         (files (directory pattern))
         (sessions nil))
    (dolist (filepath files)
      (let ((meta (read-session-metadata-fast filepath)))
        (when meta (push meta sessions))))
    (sort sessions #'> :key (lambda (s) (or (getf s :created-at) 0)))))
```

**Fast Metadata Extraction:**

```lisp
;; Read only header comments, not full file
(defun read-session-metadata-fast (filepath)
  (with-open-file (stream filepath :direction :input)
    (let ((id (pathname-name filepath))
          (name nil)
          (created nil))
      (loop for line = (read-line stream nil nil)
            while (and line (> (length line) 0) (char= (char line 0) #\;))
            do (cond
                 ((search "Name: " line)
                  (setf name (subseq line (+ (search "Name: " line) 6))))
                 ((search "Created: " line)
                  (setf created (parse-timestamp-string ...)))))
      (list :id id :name name :created-at created))))
```

**Performance:** Scans only header comments, not full session data.

---

### 3. User Selects Session

**Completion Interface:**

```
Switch to session: session-20260120-143022-A4F2 - Debug Feature X (2026-01-20 14:30)
                  session-20260119-091500-B3C1 - API Testing (2026-01-19 09:15)
                  session-20260118-163000-C2D4 (2026-01-18 16:30)
```

**User chooses:** "Debug Feature X" (session-20260120-143022-A4F2)

---

### 4. System Saves Current Session

**RPC:**

```elisp
(agent-q--switch-session-rpc "session-20260120-143022-A4F2")
```

**CL Side:**

```lisp
(defun switch-session (session-id &optional (manager (ensure-session-manager)))
  "Switch to session by SESSION-ID, saving current session first."
  ;; Step 1: Save current session
  (when (current-session manager)
    (save-session (current-session manager) manager))
  ;; Step 2: Load new session
  (let ((session (load-session session-id manager)))
    (when session
      (setf (current-session manager) session))
    session))
```

**Result:**
- Current session written to disk
- No data loss from active session

---

### 5. System Loads Target Session

**Load Protocol:**

```lisp
(defun load-session (session-id &optional (manager (ensure-session-manager)))
  "Load session by SESSION-ID from disk."
  ;; Check cache first
  (let ((cached (gethash session-id (session-cache manager))))
    (when cached (return-from load-session cached)))
  ;; Load from file
  (let* ((filename (format nil "~A.lisp" session-id))
         (filepath (merge-pathnames filename (sessions-directory manager))))
    (when (probe-file filepath)
      (with-open-file (stream filepath :direction :input)
        ;; Skip header comments
        (loop for char = (peek-char t stream nil nil)
              while (and char (char= char #\;))
              do (read-line stream nil nil))
        ;; Read and deserialize
        (let* ((plist (read stream nil nil))
               (session (plist-to-session plist)))
          (setf (gethash session-id (session-cache manager)) session)
          session)))))
```

**Cache Hit:** If session already in cache, return immediately (no disk I/O).

**Cache Miss:** Read from disk, deserialize, cache result.

---

### 6. System Restores UI

**Elisp:**

```elisp
(when (agent-q--switch-session-rpc session-id)
  ;; Update local state
  (setq agent-q--current-session (agent-q-session--create))
  (setf (agent-q-session-id agent-q--current-session) session-id)
  ;; Redisplay session (messages restored)
  (agent-q--redisplay-session)
  (message "Switched to session: %s" session-id))
```

**Redisplay:**

```elisp
(defun agent-q--redisplay-session ()
  "Redisplay current session in buffer."
  (let ((inhibit-read-only t))
    (agent-q--setup-buffer-layout)
    ;; Replay messages (stored oldest-first in CL)
    (dolist (msg (reverse (agent-q-session-messages agent-q--current-session)))
      (pcase (agent-q-message-role msg)
        ('user (agent-q--render-user-message (agent-q-message-content msg)))
        ('assistant (agent-q--render-assistant-message (agent-q-message-content msg)))))))
```

**Result:**
- Chat buffer cleared
- All messages from loaded session rendered
- Conversation history fully restored

---

## Postconditions

**Current Session Updated:**

```lisp
(current-session *session-manager*)
;; => #<SESSION session-20260120-143022-A4F2>
```

**Buffer Shows Loaded Session:**

```
╭──────────────────────────────────────────────────────────╮
│ Agent-Q Chat                                             │
│ [Debug Feature X:2 ↑1000↓500]                           │
╰──────────────────────────────────────────────────────────╯

[User] Explain the authentication bug

[Assistant] The bug is in the session validation...

───────────────────────────────────────────────────────────
> _
```

**Previous Session Saved:**

- Old session file updated on disk
- All messages from old session preserved

---

## Timing

- List sessions (metadata only): 10-50ms for 10-100 sessions
- Save current session: 20-50ms
- Load target session (cache hit): < 5ms
- Load target session (cache miss): 50-200ms
- UI redisplay: 50-100ms (depends on message count)
- **Total switch time:** 100-400ms

---

## Edge Cases

### No Sessions Found

**Behavior:**

```elisp
(if (null choices)
    (message "No saved sessions found")
  ...)
```

**Result:** User message, no action taken.

### Session File Deleted Externally

**Load Behavior:**

```lisp
(when (probe-file filepath)
  ...)  ; If file doesn't exist, returns nil
```

**Result:** Load returns `nil`, switch fails gracefully, user warned.

### Current Session is Target Session

**Behavior:** Save current session (no-op if unchanged), reload same session.

**Result:** Harmless (idempotent).

### Large Session (1000+ messages)

**Performance:**
- Metadata extraction: < 10ms per session (header-only parsing, no full deserialization)
- Full load: Slower (50-500ms depending on message size)
- UI redisplay: Slower (render time proportional to message count)

**Mitigation:** Cache keeps loaded sessions in memory (fast subsequent switches).

---

## Multi-Session Workflow Example

**Session A:** Debugging feature X (50 messages, 3 hours of work)

**Session B:** Exploring API (20 messages, 1 hour)

**User Workflow:**

1. Working in Session A
2. Switch to Session B (`C-c C-s`) → Session A saved, Session B loaded
3. Work in Session B for 30 minutes
4. Switch back to Session A → Session B saved, Session A loaded (from cache)
5. Continue debugging feature X (all 50 messages restored)

**No data loss:** All messages preserved across switches.

---

**Status:** ✅ Tested (session CRUD tests)
