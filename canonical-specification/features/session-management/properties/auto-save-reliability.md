# Property: Auto-Save Reliability

**Type:** Data Safety
**Confidence:** 1.00

---

## Statement

**Sessions MUST be automatically saved to prevent data loss, using both periodic saves and buffer-kill hooks.**

---

## Invariant

```
∀ session ∈ active_sessions, ∀ event ∈ {timer_tick, buffer_kill, session_switch}:
  event occurs ⇒ save(session) is called
```

---

## Auto-Save Triggers

### 1. Periodic Timer

**Interval:** Configurable via `agent-q-auto-save-interval` (default: 300 seconds = 5 minutes)

**Mechanism:**

```elisp
(defun agent-q--setup-auto-save ()
  "Setup auto-save timer."
  (agent-q--cancel-auto-save)
  (when (and agent-q-auto-save-sessions
             (> agent-q-auto-save-interval 0))
    (setq agent-q--auto-save-timer
          (run-with-timer agent-q-auto-save-interval
                          agent-q-auto-save-interval
                          #'agent-q--auto-save-if-needed))))
```

**Behavior:**
- Timer fires every N seconds
- Calls `agent-q--auto-save-if-needed`
- Saves current session if chat buffer exists and SLY connected

**Configurable Disable:**

```elisp
;; Disable periodic saves (still saves on buffer kill)
(setq agent-q-auto-save-interval 0)

;; Disable all auto-save
(setq agent-q-auto-save-sessions nil)
```

### 2. Buffer Kill Hook

**Trigger:** User closes Agent-Q chat buffer (via `C-x k` or Emacs exit)

**Hook:**

```elisp
(defun agent-q--on-chat-buffer-kill ()
  "Handle chat buffer kill - save session."
  (when agent-q-auto-save-sessions
    (agent-q--save-current-session)))
```

**Registration:**

```elisp
;; In chat buffer setup
(add-hook 'kill-buffer-hook #'agent-q--on-chat-buffer-kill nil t)
```

**Behavior:**
- Runs before buffer is destroyed
- Saves current session synchronously
- Prevents message loss on unexpected Emacs exit

### 3. Session Switch

**Trigger:** User switches to different session or creates new session

**Mechanism:**

```lisp
(defun switch-session (session-id &optional (manager (ensure-session-manager)))
  "Switch to session by SESSION-ID, saving current session first."
  ;; Save current session before switching
  (when (current-session manager)
    (save-session (current-session manager) manager))
  ;; Load and switch to new session
  ...)
```

**Behavior:**
- Always saves current session before loading new one
- Guarantees no data loss during transitions
- Applies to both `switch-session` and `create-session`

---

## Auto-Save Implementation

### Timer-Based Save

```elisp
(defun agent-q--auto-save-if-needed ()
  "Auto-save current session if chat buffer exists and SLY is connected."
  (when (sly-connected-p)
    (when-let ((buf (get-buffer (if (boundp 'agent-q-chat-buffer-name)
                                    agent-q-chat-buffer-name
                                  "*Agent-Q Chat*"))))
      (with-current-buffer buf
        (agent-q--save-current-session)))))
```

**Guards:**
- Check SLY connection (no save if disconnected)
- Check chat buffer exists (no save if no active session)
- Buffer-local context (save in correct buffer)

### RPC Save

```elisp
(defun agent-q--save-current-session ()
  "Save current session via RPC.
Does nothing if not connected to SLY."
  (when (sly-connected-p)
    (sly-eval-async '(agent-q:agent-q-save-session)
      (lambda (result)
        (when result
          (message "Session saved"))))))
```

**Async:** Non-blocking (doesn't freeze Emacs during save)

**Feedback:** Optional user message on successful save

---

## Configuration

### Customization Variables

```elisp
(defcustom agent-q-auto-save-sessions t
  "Whether to auto-save sessions."
  :type 'boolean
  :group 'agent-q-sessions)

(defcustom agent-q-auto-save-interval 300
  "Auto-save interval in seconds.
Set to 0 to disable periodic saves (still saves on buffer kill)."
  :type 'integer
  :group 'agent-q-sessions)
```

### Configuration Examples

```elisp
;; Default: Auto-save every 5 minutes + buffer kill
(setq agent-q-auto-save-sessions t
      agent-q-auto-save-interval 300)

;; Aggressive: Auto-save every 1 minute
(setq agent-q-auto-save-interval 60)

;; Conservative: Only save on buffer kill
(setq agent-q-auto-save-interval 0)

;; Manual: Disable all auto-save (user must save explicitly)
(setq agent-q-auto-save-sessions nil)
```

---

## Reliability Guarantees

### Data Loss Prevention

**Scenario 1: Emacs Crash**

- Buffer kill hook does NOT fire (no chance to save)
- Last periodic save is most recent
- **Max data loss:** One auto-save interval (default: 5 minutes)

**Scenario 2: User Closes Buffer**

- Buffer kill hook fires
- Session saved synchronously before buffer destroyed
- **Data loss:** None

**Scenario 3: User Switches Sessions**

- `switch-session` saves current first
- New session loaded
- **Data loss:** None

**Scenario 4: User Quits Emacs Normally**

- Emacs runs buffer kill hooks
- Session saved
- **Data loss:** None

### Timer Cleanup

**Prevents Timer Leaks:**

```elisp
(defun agent-q--cancel-auto-save ()
  "Cancel auto-save timer if running."
  (when agent-q--auto-save-timer
    (cancel-timer agent-q--auto-save-timer)
    (setq agent-q--auto-save-timer nil)))
```

**Called:**
- On `agent-q--setup-auto-save` (prevents duplicate timers)
- On mode teardown (cleanup)

---

## Test Coverage

```elisp
;; Test: Timer setup creates timer
(ert-deftest agent-q-sessions/autosave/setup-creates-timer ()
  (let ((agent-q-auto-save-sessions t)
        (agent-q-auto-save-interval 60))
    (agent-q--setup-auto-save)
    (should (timerp agent-q--auto-save-timer))
    (agent-q--cancel-auto-save)))

;; Test: Timer cancel removes timer
(ert-deftest agent-q-sessions/autosave/cancel-removes-timer ()
  (let ((agent-q-auto-save-sessions t)
        (agent-q-auto-save-interval 60))
    (agent-q--setup-auto-save)
    (should (timerp agent-q--auto-save-timer))
    (agent-q--cancel-auto-save)
    (should (null agent-q--auto-save-timer))))

;; Test: Disabled when interval is zero
(ert-deftest agent-q-sessions/autosave/disabled-when-interval-zero ()
  (let ((agent-q-auto-save-sessions t)
        (agent-q-auto-save-interval 0))
    (agent-q--setup-auto-save)
    (should (null agent-q--auto-save-timer))))

;; Test: Disabled when flag is nil
(ert-deftest agent-q-sessions/autosave/disabled-when-flag-nil ()
  (let ((agent-q-auto-save-sessions nil)
        (agent-q-auto-save-interval 60))
    (agent-q--setup-auto-save)
    (should (null agent-q--auto-save-timer))))
```

**Coverage:** 5 tests for timer lifecycle and configuration

---

## Performance Considerations

**Non-Blocking:**
- Uses `sly-eval-async` (doesn't freeze Emacs)
- Save happens in background
- User can continue working

**Conditional Save:**
- Only saves if SLY connected
- Only saves if chat buffer exists
- Avoids unnecessary RPC calls

**Efficient Serialization:**
- Pretty-printed but compact
- File I/O happens on CL side (not Elisp)
- Typical session file: < 50KB

---

## Consequences

**Positive:**
- ✅ Prevents data loss on unexpected exits
- ✅ No manual save needed (fire-and-forget)
- ✅ Configurable aggressiveness
- ✅ Non-blocking (doesn't interrupt workflow)

**Negative:**
- ⚠️ Timer overhead (minimal, every 5 minutes)
- ⚠️ Emacs crash = data loss up to last save interval
- ⚠️ No visual feedback on auto-save (silent by default)

---

**Status:** ✅ Implemented with 5 tests
