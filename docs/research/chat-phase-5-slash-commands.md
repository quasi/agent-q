# Chat Interface Phase 5: Slash Commands

**Status:** Ready for Implementation
**Priority:** 5 (was 6 in original research)
**Depends On:** Phase 1 (Foundation)
**Parent:** enhancement-4.md

---

## Goal

Implement a slash command system for quick access to common operations, making the chat interface more efficient for power users.

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Trigger | `/` at start of input | Industry standard (Discord, Slack, Claude Code) |
| Execution | Immediate (don't send to LLM) | Commands are local operations |
| Extensibility | Registry pattern | Users can add custom commands |
| Completion | `completing-read` | Consistent with @-mentions |

---

## Files to Modify

**`contrib/sly-agent-q/sly-agent-q-chat.el`**

Add slash command infrastructure:
- Command registry
- Input interception for `/` prefix
- Built-in commands
- Completion support

---

## Command Registry

```elisp
(defvar agent-q-slash-commands (make-hash-table :test 'equal)
  "Registry of slash commands.
Keys are command names (strings).
Values are plists: (:handler FN :description STR :args SPEC)")

(defun agent-q-register-command (name handler &rest props)
  "Register slash command NAME with HANDLER function.
PROPS can include :description and :args."
  (puthash name
           (list :handler handler
                 :description (plist-get props :description)
                 :args (plist-get props :args))
           agent-q-slash-commands))

(defun agent-q-unregister-command (name)
  "Remove slash command NAME from registry."
  (remhash name agent-q-slash-commands))
```

---

## Input Interception

```elisp
(defun agent-q-send-or-newline ()
  "Send input if at end of buffer and input non-empty, else insert newline.
Intercepts slash commands."
  (interactive)
  (if (and (eobp)
           (not (string-empty-p (string-trim (agent-q--get-input)))))
      (let ((input (string-trim (agent-q--get-input))))
        (if (string-prefix-p "/" input)
            (agent-q--handle-slash-command input)
          (agent-q-send-input)))
    (insert "\n> ")))

(defun agent-q--handle-slash-command (input)
  "Parse and execute slash command from INPUT."
  (let* ((parts (split-string input))
         (cmd-name (substring (car parts) 1))  ; Remove leading /
         (args (cdr parts))
         (cmd (gethash cmd-name agent-q-slash-commands)))
    (if cmd
        (progn
          (agent-q--clear-input)
          (condition-case err
              (apply (plist-get cmd :handler) args)
            (error
             (message "Command error: %s" (error-message-string err)))))
      (message "Unknown command: /%s (try /help)" cmd-name))))
```

---

## Built-in Commands

### /help - List Available Commands

```elisp
(agent-q-register-command
 "help"
 (lambda (&optional cmd)
   "Show help for commands."
   (if cmd
       ;; Help for specific command
       (if-let ((command (gethash cmd agent-q-slash-commands)))
           (message "/%s: %s"
                    cmd
                    (or (plist-get command :description) "No description"))
         (message "Unknown command: /%s" cmd))
     ;; List all commands
     (let ((commands nil))
       (maphash (lambda (name props)
                  (push (format "/%s - %s"
                                name
                                (or (plist-get props :description) ""))
                        commands))
                agent-q-slash-commands)
       (message "Commands:\n%s" (string-join (sort commands #'string<) "\n")))))
 :description "Show available commands"
 :args "[[command]]")
```

### /clear - Clear Conversation

```elisp
(agent-q-register-command
 "clear"
 (lambda ()
   "Clear current conversation."
   (when (yes-or-no-p "Clear conversation? ")
     (agent-q-clear-conversation)
     (message "Conversation cleared")))
 :description "Clear current conversation")
```

### /new - New Session

```elisp
(agent-q-register-command
 "new"
 (lambda (&optional name)
   "Start new session, optionally with NAME."
   (agent-q-new-session)
   (when name
     (setf (agent-q-session-name agent-q-current-session)
           (string-join (list name) " ")))
   (message "Started new session%s"
            (if name (format ": %s" name) "")))
 :description "Start a new session"
 :args "[[name]]")
```

### /session - Session Operations

```elisp
(agent-q-register-command
 "session"
 (lambda (&optional subcommand &rest args)
   "Session management. Subcommands: list, switch, name, save, delete"
   (pcase subcommand
     ((or "list" "ls")
      (agent-q-switch-session))
     ("switch"
      (agent-q-switch-session))
     ("name"
      (if args
          (agent-q-name-session (string-join args " "))
        (call-interactively #'agent-q-name-session)))
     ("save"
      (agent-q--save-current-session)
      (message "Session saved"))
     ("delete"
      (message "Use /session list and delete from there"))
     (_
      (message "Usage: /session [list|switch|name|save]"))))
 :description "Session management (list, switch, name, save)"
 :args "[list|switch|name|save]")
```

### /model - Switch Model

```elisp
(agent-q-register-command
 "model"
 (lambda (&optional model)
   "Show or set the LLM model."
   (if model
       (progn
         (setf (agent-q-session-model agent-q-current-session) model)
         (message "Model set to: %s" model))
     (message "Current model: %s"
              (or (agent-q-session-model agent-q-current-session)
                  "default"))))
 :description "Show or set the LLM model"
 :args "[[model-name]]")
```

### /context - Context Operations

```elisp
(agent-q-register-command
 "context"
 (lambda (&optional subcommand &rest args)
   "Context management. Subcommands: list, add, clear, panel"
   (pcase subcommand
     ((or "list" "ls")
      (if agent-q-context-items
          (message "Context:\n%s"
                   (mapconcat (lambda (item)
                                (format "  [%s] %s"
                                        (agent-q-context-item-type item)
                                        (agent-q-context-item-display-name item)))
                              agent-q-context-items
                              "\n"))
        (message "No context attached")))
     ("add"
      (call-interactively #'agent-q-add-context))
     ("clear"
      (agent-q-clear-context))
     ("panel"
      (agent-q-toggle-context-panel))
     (_
      (message "Usage: /context [list|add|clear|panel]"))))
 :description "Context management (list, add, clear, panel)"
 :args "[list|add|clear|panel]")
```

### /search - Search Sessions

```elisp
(agent-q-register-command
 "search"
 (lambda (&rest query-parts)
   "Search past sessions."
   (if query-parts
       (agent-q-search-sessions (string-join query-parts " "))
     (call-interactively #'agent-q-search-sessions)))
 :description "Search past sessions"
 :args "<query>")
```

### /retry - Retry Last Message

```elisp
(agent-q-register-command
 "retry"
 (lambda ()
   "Retry last user message."
   (if-let ((last-user-msg (seq-find (lambda (msg)
                                       (eq (agent-q-message-role msg) 'user))
                                     (agent-q-session-messages agent-q-current-session))))
       (let ((content (agent-q-message-content last-user-msg)))
         (agent-q--send-to-agent content)
         (message "Retrying: %s" (truncate-string-to-width content 50)))
     (message "No previous message to retry")))
 :description "Retry the last user message")
```

### /copy - Copy Last Response

```elisp
(agent-q-register-command
 "copy"
 (lambda ()
   "Copy last assistant response to clipboard."
   (if-let ((last-msg (seq-find (lambda (msg)
                                  (eq (agent-q-message-role msg) 'assistant))
                                (agent-q-session-messages agent-q-current-session))))
       (progn
         (kill-new (agent-q-message-content last-msg))
         (message "Response copied to clipboard"))
     (message "No response to copy")))
 :description "Copy last response to clipboard")
```

### /export - Export Conversation

```elisp
(agent-q-register-command
 "export"
 (lambda (&optional format)
   "Export conversation to file. Format: md (default), org, txt"
   (let* ((fmt (or format "md"))
          (ext (pcase fmt
                 ("org" "org")
                 ("txt" "txt")
                 (_ "md")))
          (file (read-file-name
                 (format "Export to .%s file: " ext)
                 nil nil nil
                 (format "%s.%s"
                         (agent-q-session-id agent-q-current-session)
                         ext))))
     (with-temp-file file
       (insert (agent-q--export-conversation fmt)))
     (message "Exported to: %s" file)))
 :description "Export conversation (md, org, txt)"
 :args "[[format]]")

(defun agent-q--export-conversation (format)
  "Export current conversation in FORMAT."
  (let ((messages (reverse (agent-q-session-messages agent-q-current-session))))
    (mapconcat
     (lambda (msg)
       (let ((role (if (eq (agent-q-message-role msg) 'user)
                       "User" "Agent-Q"))
             (content (agent-q-message-content msg)))
         (pcase format
           ("org"
            (format "* %s\n%s\n" role content))
           ("txt"
            (format "[%s]\n%s\n\n" role content))
           (_  ; markdown
            (format "## %s\n\n%s\n\n" role content)))))
     messages
     "")))
```

---

## Command Completion

```elisp
(defun agent-q-slash-complete-at-point ()
  "Completion-at-point for slash commands."
  (when (and (eq (char-before) ?/)
             (or (bobp)
                 (eq (char-before (1- (point))) ?\n)
                 (eq (char-before (1- (point))) ?\ )))
    (let ((start (1- (point)))
          (end (point)))
      (list start end
            (hash-table-keys agent-q-slash-commands)
            :exclusive 'no
            :annotation-function
            (lambda (cmd)
              (when-let ((props (gethash cmd agent-q-slash-commands)))
                (format " - %s" (or (plist-get props :description) ""))))))))

;; Add to completion-at-point-functions
(add-hook 'agent-q-chat-mode-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions
                         #'agent-q-slash-complete-at-point)))
```

---

## Custom Command Example

Users can add custom commands in their config:

```elisp
;; Example: /summarize command
(agent-q-register-command
 "summarize"
 (lambda ()
   "Ask agent to summarize the conversation."
   (agent-q--send-to-agent
    "Please provide a brief summary of our conversation so far."))
 :description "Ask for conversation summary")

;; Example: /fix command
(agent-q-register-command
 "fix"
 (lambda ()
   "Ask agent to fix the last suggested code."
   (agent-q--send-to-agent
    "The previous code had issues. Please fix it."))
 :description "Ask to fix last code suggestion")
```

---

## Help Display

When user types `/` and pauses, show available commands:

```elisp
(defun agent-q--show-command-hints ()
  "Show command hints in echo area."
  (when (and (looking-back "^> /" (line-beginning-position))
             (not (minibufferp)))
    (let ((commands (hash-table-keys agent-q-slash-commands)))
      (message "Commands: %s"
               (string-join (sort commands #'string<) ", ")))))

;; Optionally bind to post-self-insert-hook or use idle timer
(defvar agent-q--command-hint-timer nil)

(defun agent-q--schedule-command-hint ()
  "Schedule showing command hints."
  (when agent-q--command-hint-timer
    (cancel-timer agent-q--command-hint-timer))
  (setq agent-q--command-hint-timer
        (run-with-idle-timer 0.5 nil #'agent-q--show-command-hints)))
```

---

## Keybindings

Slash commands work via text input, but we can add shortcuts:

```elisp
;; Quick access to common commands
(define-key agent-q-chat-mode-map (kbd "C-c /") #'agent-q-run-command)

(defun agent-q-run-command ()
  "Run a slash command via completion."
  (interactive)
  (let* ((cmd (completing-read "Command: "
                               (hash-table-keys agent-q-slash-commands)
                               nil t))
         (props (gethash cmd agent-q-slash-commands))
         (handler (plist-get props :handler)))
    (when handler
      (call-interactively handler))))
```

---

## Verification Checklist

- [ ] `/help` lists all available commands
- [ ] `/help <cmd>` shows specific command help
- [ ] `/clear` clears conversation (with confirmation)
- [ ] `/new` starts new session
- [ ] `/new <name>` starts named session
- [ ] `/session list` shows session switcher
- [ ] `/session name <name>` renames session
- [ ] `/model` shows current model
- [ ] `/model <name>` sets model
- [ ] `/context list` shows attached context
- [ ] `/context clear` clears context
- [ ] `/search <query>` searches sessions
- [ ] `/retry` resends last user message
- [ ] `/copy` copies last response
- [ ] `/export` exports conversation to file
- [ ] Tab completion works for command names
- [ ] Unknown commands show helpful error

---

## Success Metrics

- Commands execute immediately (<100ms)
- Tab completion works smoothly
- Error messages are helpful
- Custom commands easy to add
- Commands reduce keyboard shortcuts needed
