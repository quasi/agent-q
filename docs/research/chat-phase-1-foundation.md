# Chat Interface Phase 1: Foundation

**Status:** Ready for Implementation
**Priority:** 1 (First)
**Depends On:** None
**Parent:** enhancement-4.md

---

## Goal

Replace the readonly `sly-agent-q-conversation-mode` buffer with an interactive chat interface featuring multi-line input, message history navigation, and streaming response handling.

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Base mode | `special-mode` | Not `comint-mode`; we only use ~25% of comint's features |
| Buffer layout | Marker-based regions | Simple, full control, no subprocess baggage |
| Existing code | Replace entirely | Clean break at v0.3.0 |

---

## Files to Create/Modify

### New Files

**`contrib/sly-agent-q/sly-agent-q-chat.el`**

Main chat interface implementation containing:
- `agent-q-chat-mode` major mode
- Buffer layout setup
- Input handling
- Message rendering
- History navigation
- Streaming response handling
- SLY integration bridge

### Modified Files

**`contrib/sly-agent-q/sly-agent-q.el`**

Update entry points:
- Replace `sly-agent-q-show-conversation` to use new chat buffer
- Update `sly-agent-q--append-to-conversation` to use new rendering
- Keep context commands working with new buffer

---

## Buffer Architecture

```
┌─────────────────────────────────────────┐
│           OUTPUT REGION                 │  ← Read-only (text property)
│                                         │
│  [USER] 14:30:22                        │
│  Fix the bug in auth.lisp               │
│                                         │
│  [AGENT-Q] 14:30:25                     │
│  Looking at auth.lisp...                │
│  The issue is in the jwt-decode...      │
│                                         │
├─────────────────────────────────────────┤  ← agent-q-output-end-marker
│  ─────────── Input ───────────          │  ← Separator (read-only)
├─────────────────────────────────────────┤  ← agent-q-input-start-marker
│           INPUT REGION                  │  ← Editable
│  > your message here                    │
│  > can be multiple lines█               │
└─────────────────────────────────────────┘
```

**Key markers:**
- `agent-q-output-end-marker`: Where new messages are inserted
- `agent-q-input-start-marker`: Start of editable input region

---

## Data Structures

### Message Structure

```elisp
(cl-defstruct agent-q-message
  "A single message in the conversation."
  (id (format "%s-%04x"
              (format-time-string "%Y%m%d%H%M%S")
              (random 65536)))
  (role nil)              ; 'user | 'assistant | 'system
  (content "")            ; Message text
  (timestamp (current-time))
  (context-snapshot nil)  ; Context items at time of message
  (metadata nil))         ; Plist: (:tool-calls ... :model ...)
```

### Session Structure

```elisp
(cl-defstruct agent-q-session
  "A conversation session containing messages."
  (id (format "session-%s" (format-time-string "%Y%m%d-%H%M%S")))
  (name nil)              ; Optional user-friendly name
  (created-at (current-time))
  (updated-at (current-time))
  (messages nil)          ; List of agent-q-message
  (model nil)             ; Model used (inherited from config if nil)
  (metadata nil))         ; Plist for extensibility
```

### Buffer-Local State

```elisp
(defvar-local agent-q-current-session nil
  "The active session in this chat buffer.")

(defvar-local agent-q-output-end-marker nil
  "Marker at end of output region, before separator.")

(defvar-local agent-q-input-start-marker nil
  "Marker at start of input region, after separator.")

(defvar-local agent-q-input-history nil
  "List of past inputs (most recent first).")

(defvar-local agent-q-history-index -1
  "Current position in input history. -1 = not browsing.")

(defvar-local agent-q-input-draft nil
  "Saved input when browsing history.")

(defvar-local agent-q-pending-response nil
  "Non-nil while waiting for agent response.")

(defvar-local agent-q--streaming-marker nil
  "Marker where streaming chunks are appended.")
```

---

## Mode Definition

```elisp
(defgroup agent-q-chat nil
  "Agent-Q interactive chat interface."
  :group 'agent-q
  :prefix "agent-q-")

(defcustom agent-q-input-history-max 100
  "Maximum number of inputs to remember in history."
  :type 'integer
  :group 'agent-q-chat)

(define-derived-mode agent-q-chat-mode special-mode "Agent-Q Chat"
  "Major mode for interactive chat with Agent-Q.

Key bindings:
\\{agent-q-chat-mode-map}"
  :group 'agent-q-chat

  ;; Buffer settings
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local cursor-type 'bar)
  (setq-local scroll-conservatively 101)

  ;; Initialize buffer layout
  (agent-q--setup-buffer-layout)

  ;; Initialize state
  (setq-local agent-q-current-session (make-agent-q-session))
  (setq-local agent-q-input-history nil)
  (setq-local agent-q-history-index -1)
  (setq-local agent-q-pending-response nil)

  ;; Hooks
  (add-hook 'kill-buffer-hook #'agent-q--on-buffer-kill nil t))
```

---

## Keybindings

```elisp
(defvar agent-q-chat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Input control
    (define-key map (kbd "RET") #'agent-q-send-or-newline)
    (define-key map (kbd "C-c C-c") #'agent-q-send-input)
    (define-key map (kbd "C-c C-k") #'agent-q-cancel-request)

    ;; History navigation
    (define-key map (kbd "M-p") #'agent-q-history-previous)
    (define-key map (kbd "M-n") #'agent-q-history-next)

    ;; Buffer operations
    (define-key map (kbd "C-c C-l") #'agent-q-clear-conversation)
    (define-key map (kbd "C-c C-o") #'agent-q-scroll-to-bottom)
    (define-key map (kbd "q") #'quit-window)

    ;; Session management (Phase 3 - define now, implement later)
    (define-key map (kbd "C-c C-s") #'agent-q-switch-session)
    (define-key map (kbd "C-c C-n") #'agent-q-new-session)

    ;; Context (Phase 4 - define now, implement later)
    (define-key map (kbd "C-c @") #'agent-q-add-context)
    (define-key map (kbd "C-c C-x") #'agent-q-clear-context)

    map)
  "Keymap for `agent-q-chat-mode'.")
```

---

## Core Functions

### Buffer Setup

```elisp
(defun agent-q--setup-buffer-layout ()
  "Initialize buffer with output region, separator, and input region."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Welcome message
    (insert (propertize "Agent-Q Chat\n"
                        'face 'agent-q-header-face
                        'read-only t))
    (insert (propertize (make-string 40 ?─)
                        'face 'agent-q-separator-face
                        'read-only t))
    (insert "\n\n")

    ;; Output end marker
    (setq agent-q-output-end-marker (point-marker))
    (set-marker-insertion-type agent-q-output-end-marker nil)

    ;; Separator line
    (insert (propertize (concat "\n" (make-string 40 ?─) " Input " (make-string 40 ?─) "\n")
                        'face 'agent-q-separator-face
                        'read-only t
                        'agent-q-separator t))

    ;; Input start marker
    (setq agent-q-input-start-marker (point-marker))
    (set-marker-insertion-type agent-q-input-start-marker t)

    ;; Initial prompt
    (insert (propertize "> "
                        'face 'agent-q-input-prompt-face
                        'read-only t
                        'rear-nonsticky t))))
```

### Input Handling

```elisp
(defun agent-q-send-or-newline ()
  "Send input if at end of buffer and input non-empty, else insert newline."
  (interactive)
  (if (and (eobp)
           (not (string-empty-p (string-trim (agent-q--get-input)))))
      (agent-q-send-input)
    (insert "\n> ")))

(defun agent-q-send-input ()
  "Send current input to Agent-Q."
  (interactive)
  (when agent-q-pending-response
    (user-error "Already waiting for response"))

  (let ((input (string-trim (agent-q--get-input))))
    (when (string-empty-p input)
      (user-error "Input is empty"))

    ;; Create and store message
    (let ((msg (make-agent-q-message :role 'user :content input)))
      (push msg (agent-q-session-messages agent-q-current-session))
      (setf (agent-q-session-updated-at agent-q-current-session) (current-time)))

    ;; Add to history
    (agent-q--add-to-history input)

    ;; Render user message
    (agent-q--render-user-message input)

    ;; Clear input
    (agent-q--clear-input)

    ;; Send to agent
    (agent-q--send-to-agent input)))

(defun agent-q--get-input ()
  "Get current input text."
  (buffer-substring-no-properties
   agent-q-input-start-marker
   (point-max)))

(defun agent-q--clear-input ()
  "Clear the input region."
  (let ((inhibit-read-only t))
    (delete-region agent-q-input-start-marker (point-max))
    (goto-char agent-q-input-start-marker)
    (insert (propertize "> "
                        'face 'agent-q-input-prompt-face
                        'read-only t
                        'rear-nonsticky t))))
```

### History Navigation

```elisp
(defun agent-q--add-to-history (input)
  "Add INPUT to history ring."
  (when (and input (not (string-empty-p input)))
    (push input agent-q-input-history)
    (when (> (length agent-q-input-history) agent-q-input-history-max)
      (setq agent-q-input-history
            (seq-take agent-q-input-history agent-q-input-history-max)))))

(defun agent-q-history-previous ()
  "Navigate to previous input in history (M-p)."
  (interactive)
  (unless agent-q-input-history
    (user-error "No history"))
  ;; Save draft on first navigation
  (when (= agent-q-history-index -1)
    (setq agent-q-input-draft (agent-q--get-input)))
  ;; Navigate
  (when (< agent-q-history-index (1- (length agent-q-input-history)))
    (cl-incf agent-q-history-index)
    (agent-q--replace-input (nth agent-q-history-index agent-q-input-history))))

(defun agent-q-history-next ()
  "Navigate to next input in history (M-n)."
  (interactive)
  (cond
   ((> agent-q-history-index 0)
    (cl-decf agent-q-history-index)
    (agent-q--replace-input (nth agent-q-history-index agent-q-input-history)))
   ((= agent-q-history-index 0)
    (setq agent-q-history-index -1)
    (agent-q--replace-input (or agent-q-input-draft "")))))

(defun agent-q--replace-input (text)
  "Replace current input with TEXT."
  (let ((inhibit-read-only t))
    (delete-region agent-q-input-start-marker (point-max))
    (goto-char agent-q-input-start-marker)
    (insert (propertize "> "
                        'face 'agent-q-input-prompt-face
                        'read-only t
                        'rear-nonsticky t))
    (insert text)
    (goto-char (point-max))))
```

### Message Rendering

```elisp
(defun agent-q--render-user-message (content)
  "Render user message with CONTENT in output region."
  (save-excursion
    (goto-char agent-q-output-end-marker)
    (let ((inhibit-read-only t)
          (start (point)))
      (insert (propertize "[USER]" 'face 'agent-q-user-header-face))
      (insert " ")
      (insert (propertize (format-time-string "%H:%M:%S")
                          'face 'agent-q-timestamp-face))
      (insert "\n")
      (insert content)
      (insert "\n\n")
      (put-text-property start (point) 'read-only t)
      (set-marker agent-q-output-end-marker (point)))))

(defun agent-q--begin-assistant-response ()
  "Prepare buffer for streaming assistant response."
  (setq agent-q-pending-response t)
  (save-excursion
    (goto-char agent-q-output-end-marker)
    (let ((inhibit-read-only t))
      (insert (propertize "[AGENT-Q]" 'face 'agent-q-assistant-header-face))
      (insert " ")
      (insert (propertize (format-time-string "%H:%M:%S")
                          'face 'agent-q-timestamp-face))
      (insert "\n")
      (setq agent-q--streaming-marker (point-marker))
      (set-marker-insertion-type agent-q--streaming-marker t)
      (set-marker agent-q-output-end-marker (point)))))

(defun agent-q--append-response-chunk (chunk)
  "Append CHUNK to current streaming response."
  (when agent-q--streaming-marker
    (save-excursion
      (goto-char agent-q--streaming-marker)
      (let ((inhibit-read-only t))
        (insert chunk)))))

(defun agent-q--finalize-response (full-content)
  "Finalize the assistant response."
  (let ((msg (make-agent-q-message :role 'assistant :content full-content)))
    (push msg (agent-q-session-messages agent-q-current-session))
    (setf (agent-q-session-updated-at agent-q-current-session) (current-time)))
  (save-excursion
    (goto-char agent-q-output-end-marker)
    (let ((inhibit-read-only t))
      (insert "\n\n")
      (set-marker agent-q-output-end-marker (point))))
  (setq agent-q-pending-response nil)
  (setq agent-q--streaming-marker nil))
```

### SLY Integration

```elisp
(defun agent-q--send-to-agent (content)
  "Send CONTENT to the Lisp agent via SLY."
  (agent-q--begin-assistant-response)
  ;; Call existing RPC endpoint
  (sly-eval-async
   `(agent-q:send-to-agent ,content)
   (lambda (result)
     (agent-q--finalize-response result))))

;; Callback for streaming (called from Lisp side)
(defun agent-q-chat-append-chunk (chunk)
  "Append streaming CHUNK from agent. Called via eval-in-emacs."
  (when-let ((buf (get-buffer "*Agent-Q Chat*")))
    (with-current-buffer buf
      (agent-q--append-response-chunk chunk))))

;; Callback for debug/tool messages (preserves existing behavior)
(defun agent-q-chat-debug-message (msg)
  "Display debug MSG from agent. Called via eval-in-emacs."
  (when-let ((buf (get-buffer "*Agent-Q Chat*")))
    (with-current-buffer buf
      (agent-q--append-response-chunk
       (propertize (format "[%s]\n" msg) 'face 'agent-q-debug-face)))))
```

---

## Faces

```elisp
(defface agent-q-header-face
  '((t :weight bold :height 1.2))
  "Face for chat buffer header."
  :group 'agent-q-chat)

(defface agent-q-user-header-face
  '((t :foreground "#61afef" :weight bold))
  "Face for [USER] message header."
  :group 'agent-q-chat)

(defface agent-q-assistant-header-face
  '((t :foreground "#98c379" :weight bold))
  "Face for [AGENT-Q] message header."
  :group 'agent-q-chat)

(defface agent-q-timestamp-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for message timestamps."
  :group 'agent-q-chat)

(defface agent-q-separator-face
  '((t :foreground "#3e4451"))
  "Face for separator lines."
  :group 'agent-q-chat)

(defface agent-q-input-prompt-face
  '((t :foreground "#c678dd" :weight bold))
  "Face for input prompt character."
  :group 'agent-q-chat)

(defface agent-q-debug-face
  '((t :foreground "#5c6370" :slant italic))
  "Face for debug/tool execution messages."
  :group 'agent-q-chat)
```

---

## Entry Point

```elisp
(defun agent-q-chat ()
  "Open or switch to Agent-Q chat buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Agent-Q Chat*")))
    (with-current-buffer buf
      (unless (eq major-mode 'agent-q-chat-mode)
        (agent-q-chat-mode)))
    (pop-to-buffer buf)))
```

---

## Integration with Existing Code

### Changes to sly-agent-q.el

1. Update `sly-agent-q-show-conversation` to call `agent-q-chat` instead
2. Update `sly-agent-q--append-to-conversation` to use new buffer:
   ```elisp
   (defun sly-agent-q--append-to-conversation (text &optional face)
     "Append TEXT to conversation. For backward compatibility."
     (when-let ((buf (get-buffer "*Agent-Q Chat*")))
       (with-current-buffer buf
         (agent-q--append-response-chunk
          (if face (propertize text 'face face) text)))))
   ```
3. Keep context commands working - they should add to `agent-q-context-items`

### Lisp-side Changes (minimal)

Update `agent.lisp` to use new Emacs callbacks:
```lisp
;; Instead of:
(slynk:eval-in-emacs '(sly-agent-q--append-to-conversation "..."))

;; Use:
(slynk:eval-in-emacs '(agent-q-chat-append-chunk "..."))
```

---

## Verification Checklist

- [ ] `M-x agent-q-chat` opens chat buffer
- [ ] Buffer has correct layout (output region, separator, input region)
- [ ] Can type multi-line input in input region
- [ ] Cannot edit output region (read-only)
- [ ] `RET` at end sends message, mid-line inserts newline
- [ ] `C-c C-c` always sends
- [ ] `M-p` / `M-n` navigates input history
- [ ] History saves draft when navigating
- [ ] User messages render with [USER] header and timestamp
- [ ] Agent responses render with [AGENT-Q] header
- [ ] Streaming chunks appear incrementally
- [ ] Debug/tool messages appear during agent iteration
- [ ] Diff tool still triggers existing diff UI
- [ ] `C-c C-l` clears conversation
- [ ] `q` quits window

---

## Success Metrics

- Multi-line input works smoothly
- No lag during streaming responses
- History navigation feels like shell-mode
- Existing context commands still work
- Existing diff approval workflow unchanged
