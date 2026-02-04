---
type: scenario
name: create-and-save-session
version: 1.0.0
feature: session-management
covers:
  - session-persistence
  - session-storage
tags:
  - happy-path
  - lifecycle
---

# Scenario: Create and Save Session

**Flow:** Complete session lifecycle from creation to persistence
**Confidence:** 1.00

---

## Preconditions

- Agent-Q loaded in Emacs
- SLY connected to Lisp image
- No current session (or willing to switch)

---

## Flow

### 1. User Creates New Session

**Action:** `C-c C-n` (or `M-x agent-q-new-session`)

**Elisp:**

```elisp
(defun agent-q-new-session ()
  "Start a new session, saving the current one first."
  (interactive)
  (agent-q--check-sly-connection)
  (let ((session-id (agent-q--create-session-rpc)))
    (when session-id
      (setq agent-q--current-session (agent-q-session--create))
      (agent-q--setup-buffer-layout)
      (message "Started new session: %s" session-id))))
```

**RPC Call:**

```elisp
(sly-eval `(agent-q:agent-q-create-session :name ,name))
```

**CL Side:**

```lisp
(defun create-session (&key name model (manager (ensure-session-manager)))
  "Create a new session, save current first, and make it current."
  ;; Save current session
  (when (current-session manager)
    (save-session (current-session manager) manager))
  ;; Create new session
  (let ((session (make-session :name name :model model)))
    (setf (current-session manager) session)
    (setf (gethash (session-id session) (session-cache manager)) session)
    session))
```

**Result:**
- Old session saved to disk (if exists)
- New session created with ID: `session-20260120-143022-A4F2`
- Buffer cleared, ready for new conversation

---

### 2. User Names Session (Optional)

**Action:** `C-c C-r` (or `M-x agent-q-name-session`)

**Prompt:** `Session name: ` → User enters "Debug Feature X"

**RPC:**

```elisp
(sly-eval `(agent-q:agent-q-rename-session "Debug Feature X"))
```

**CL Side:**

```lisp
;; sly-interface.lisp RPC endpoint
(defslyfun agent-q-rename-session (name)
  (let ((session (current-session *session-manager*)))
    (when session
      (setf (session-name session) name)
      (save-session session)
      t)))
```

**Result:**
- Session name updated in memory
- Session immediately saved to disk with new name

---

### 3. User Sends Messages

**Actions:**
- Type message in input region: "Explain the authentication bug"
- Press `C-c RET` to send

**Result:**
- Message added to session's conversation
- LLM responds
- Both messages stored in session
- `updated-at` timestamp refreshed

---

### 4. Auto-Save (Background)

**Trigger:** 5 minutes elapsed (default interval)

**Timer:**

```elisp
(run-with-timer 300 300 #'agent-q--auto-save-if-needed)
```

**Save Call:**

```elisp
(sly-eval-async '(agent-q:agent-q-save-session)
  (lambda (result)
    (when result (message "Session saved"))))
```

**CL Side:**

```lisp
(defslyfun agent-q-save-session ()
  (let ((session (current-session *session-manager*)))
    (when session
      (save-session session)
      t)))
```

**Result:**
- Session written to `~/.emacs.d/agent-q-sessions/session-20260120-143022-A4F2.lisp`
- Optional "Session saved" message in Emacs

---

### 5. User Closes Buffer

**Action:** `C-x k` (kill buffer)

**Hook:**

```elisp
(add-hook 'kill-buffer-hook #'agent-q--on-chat-buffer-kill nil t)

(defun agent-q--on-chat-buffer-kill ()
  "Handle chat buffer kill - save session."
  (when agent-q-auto-save-sessions
    (agent-q--save-current-session)))
```

**Result:**
- Session saved before buffer destroyed
- No message loss

---

## Postconditions

**On Disk:**

File: `~/.emacs.d/agent-q-sessions/session-20260120-143022-A4F2.lisp`

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Agent-Q Session v2
;;; Created: 2026-01-20 14:30:22
;;; Name: Debug Feature X

(:version 2
 :id "session-20260120-143022-A4F2"
 :name "Debug Feature X"
 :created-at 3945234622
 :updated-at 3945237800
 :model "claude-sonnet-4-20250514"
 :metadata (:total-input-tokens 1000 :total-output-tokens 500)
 :messages ((:role :user :content "Explain the authentication bug" :timestamp 3945234622)
            (:role :assistant :content "The bug is in..." :timestamp 3945234680)))
```

**In Cache:**

```lisp
(gethash "session-20260120-143022-A4F2" (session-cache *session-manager*))
;; => #<SESSION session-20260120-143022-A4F2>
```

**Mode Line:**

```
*Agent-Q Chat* [Debug Feature X:2 ↑1000↓500]
```

---

## Timing

- Session creation: < 50ms
- First save: 50-200ms (directory creation + file write)
- Subsequent saves: 20-50ms (file overwrite)
- Auto-save (async): Non-blocking, ~50ms background
- Buffer kill save: 50-100ms (synchronous, but fast)

---

## Error Handling

**Directory Creation Fails:**

```lisp
(ensure-directories-exist (merge-pathnames "dummy.txt" dir))
;; Signals error if permissions insufficient
```

**File Write Fails:**

```lisp
(handler-case
    (with-open-file (stream filepath :direction :output
                            :if-exists :supersede ...)
      ...)
  (error (e)
    (warn "Failed to save session: ~A" e)))
```

**RPC Connection Lost:**

```elisp
(when (sly-connected-p)
  (agent-q--save-current-session))
;; No-op if disconnected (safe)
```

---

**Status:** ✅ Tested (20 UI tests)
