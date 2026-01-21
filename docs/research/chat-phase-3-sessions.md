# Chat Interface Phase 3: Session Management

**Status:** Ready for Implementation
**Priority:** 3 (was 5 in original research)
**Depends On:** Phase 1 (Foundation)
**Parent:** enhancement-4.md

---

## Goal

Implement session persistence and search, allowing users to save conversations, switch between them, and find past discussions.

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Storage format | Lisp files | Readable, grep-able, Emacs-native |
| Storage location | `~/.emacs.d/agent-q-sessions/` | Standard Emacs data directory |
| Threading | Not included | Simplicity; add later if needed |
| Search | Grep-based initially | Simple; abstraction allows future vector DB |

---

## Files to Create/Modify

### New Files

**`contrib/sly-agent-q/sly-agent-q-sessions.el`**

Session management module containing:
- Storage backend protocol
- File-based storage implementation
- Session listing and switching
- Search functionality
- Auto-save logic

### Modified Files

**`contrib/sly-agent-q/sly-agent-q-chat.el`**

- Integrate session save/load on buffer lifecycle
- Add session info to mode line
- Call session functions from keybindings

---

## Storage Backend Protocol

Abstraction layer for future backend swapping (e.g., vector DB):

```elisp
(cl-defgeneric agent-q-storage-save (backend session)
  "Save SESSION using BACKEND.")

(cl-defgeneric agent-q-storage-load (backend session-id)
  "Load session with SESSION-ID using BACKEND.")

(cl-defgeneric agent-q-storage-delete (backend session-id)
  "Delete session with SESSION-ID using BACKEND.")

(cl-defgeneric agent-q-storage-list (backend)
  "List all sessions using BACKEND. Returns list of session metadata.")

(cl-defgeneric agent-q-storage-search (backend query)
  "Search sessions matching QUERY using BACKEND.")
```

---

## File-Based Storage Implementation

### Directory Structure

```
~/.emacs.d/agent-q-sessions/
├── session-20260111-143022.el
├── session-20260111-152045.el
├── session-20260112-091530.el
└── index.el  ; Optional: cached metadata for fast listing
```

### Session File Format

```elisp
;; ~/.emacs.d/agent-q-sessions/session-20260111-143022.el
;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;; Agent-Q Session
;; Created: 2026-01-11 14:30:22
;; Name: Auth bug investigation

(:version 1
 :id "session-20260111-143022"
 :name "Auth bug investigation"
 :created-at (25765 12345 0 0)
 :updated-at (25765 23456 0 0)
 :model "claude-sonnet-4-5"
 :messages
 ((:id "20260111143022-a1b2"
   :role user
   :content "Fix the bug in auth.lisp"
   :timestamp (25765 12345 0 0)
   :context-snapshot nil
   :metadata nil)
  (:id "20260111143025-c3d4"
   :role assistant
   :content "Looking at auth.lisp, I see the issue..."
   :timestamp (25765 12348 0 0)
   :context-snapshot nil
   :metadata (:tool-calls (...)))))
```

### Backend Implementation

```elisp
(defclass agent-q-storage-file-backend ()
  ((directory :initarg :directory
              :initform (expand-file-name "agent-q-sessions"
                                          user-emacs-directory)
              :accessor agent-q-storage-directory))
  "File-based session storage backend.")

(defvar agent-q-storage-backend
  (make-instance 'agent-q-storage-file-backend)
  "The active storage backend.")

(cl-defmethod agent-q-storage-save ((backend agent-q-storage-file-backend)
                                    session)
  "Save SESSION to a Lisp file."
  (let* ((dir (agent-q-storage-directory backend))
         (id (agent-q-session-id session))
         (file (expand-file-name (concat id ".el") dir)))
    ;; Ensure directory exists
    (unless (file-directory-p dir)
      (make-directory dir t))
    ;; Write session
    (with-temp-file file
      (insert ";; -*- mode: emacs-lisp; lexical-binding: t; -*-\n")
      (insert ";; Agent-Q Session\n")
      (insert (format ";; Created: %s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S"
                                          (agent-q-session-created-at session))))
      (when (agent-q-session-name session)
        (insert (format ";; Name: %s\n" (agent-q-session-name session))))
      (insert "\n")
      (pp (agent-q--session-to-plist session) (current-buffer)))))

(cl-defmethod agent-q-storage-load ((backend agent-q-storage-file-backend)
                                    session-id)
  "Load session with SESSION-ID from file."
  (let* ((dir (agent-q-storage-directory backend))
         (file (expand-file-name (concat session-id ".el") dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Skip comments
        (while (looking-at "^;")
          (forward-line 1))
        (agent-q--plist-to-session (read (current-buffer)))))))

(cl-defmethod agent-q-storage-delete ((backend agent-q-storage-file-backend)
                                      session-id)
  "Delete session file for SESSION-ID."
  (let* ((dir (agent-q-storage-directory backend))
         (file (expand-file-name (concat session-id ".el") dir)))
    (when (file-exists-p file)
      (delete-file file))))

(cl-defmethod agent-q-storage-list ((backend agent-q-storage-file-backend))
  "List all sessions with metadata."
  (let* ((dir (agent-q-storage-directory backend))
         (files (and (file-directory-p dir)
                     (directory-files dir t "\\.el$"))))
    (mapcar (lambda (file)
              (agent-q--read-session-metadata file))
            files)))

(cl-defmethod agent-q-storage-search ((backend agent-q-storage-file-backend)
                                      query)
  "Search sessions for QUERY using grep."
  (let* ((dir (agent-q-storage-directory backend))
         (default-directory dir))
    (when (file-directory-p dir)
      (let ((matches (split-string
                      (shell-command-to-string
                       (format "grep -l -i %s *.el 2>/dev/null"
                               (shell-quote-argument query)))
                      "\n" t)))
        (mapcar (lambda (file)
                  (agent-q-storage-load backend (file-name-base file)))
                matches)))))
```

### Serialization Helpers

```elisp
(defun agent-q--session-to-plist (session)
  "Convert SESSION struct to plist for storage."
  (list :version 1
        :id (agent-q-session-id session)
        :name (agent-q-session-name session)
        :created-at (agent-q-session-created-at session)
        :updated-at (agent-q-session-updated-at session)
        :model (agent-q-session-model session)
        :messages (mapcar #'agent-q--message-to-plist
                          (reverse (agent-q-session-messages session)))
        :metadata (agent-q-session-metadata session)))

(defun agent-q--plist-to-session (plist)
  "Convert PLIST to session struct."
  (make-agent-q-session
   :id (plist-get plist :id)
   :name (plist-get plist :name)
   :created-at (plist-get plist :created-at)
   :updated-at (plist-get plist :updated-at)
   :model (plist-get plist :model)
   :messages (mapcar #'agent-q--plist-to-message
                     (reverse (plist-get plist :messages)))
   :metadata (plist-get plist :metadata)))

(defun agent-q--message-to-plist (msg)
  "Convert MSG struct to plist."
  (list :id (agent-q-message-id msg)
        :role (agent-q-message-role msg)
        :content (agent-q-message-content msg)
        :timestamp (agent-q-message-timestamp msg)
        :context-snapshot (agent-q-message-context-snapshot msg)
        :metadata (agent-q-message-metadata msg)))

(defun agent-q--plist-to-message (plist)
  "Convert PLIST to message struct."
  (make-agent-q-message
   :id (plist-get plist :id)
   :role (plist-get plist :role)
   :content (plist-get plist :content)
   :timestamp (plist-get plist :timestamp)
   :context-snapshot (plist-get plist :context-snapshot)
   :metadata (plist-get plist :metadata)))

(defun agent-q--read-session-metadata (file)
  "Read just the metadata from session FILE (for fast listing)."
  (with-temp-buffer
    (insert-file-contents file nil 0 1000)  ; Read first 1KB
    (goto-char (point-min))
    (let ((id (file-name-base file))
          (name nil)
          (created nil))
      ;; Parse comments for metadata
      (while (looking-at "^;; \\(.*\\)")
        (let ((line (match-string 1)))
          (cond
           ((string-match "^Name: \\(.*\\)" line)
            (setq name (match-string 1 line)))
           ((string-match "^Created: \\(.*\\)" line)
            (setq created (match-string 1 line)))))
        (forward-line 1))
      (list :id id :name name :created created))))
```

---

## User Interface

### Session Switching

```elisp
(defun agent-q-switch-session ()
  "Switch to a different session."
  (interactive)
  (let* ((sessions (agent-q-storage-list agent-q-storage-backend))
         (choices (mapcar (lambda (meta)
                            (cons (format "%s - %s%s"
                                          (plist-get meta :id)
                                          (or (plist-get meta :name) "")
                                          (if (plist-get meta :created)
                                              (format " (%s)" (plist-get meta :created))
                                            ""))
                                  (plist-get meta :id)))
                          sessions))
         (choice (completing-read "Switch to session: " choices nil t)))
    (when choice
      (let ((session-id (cdr (assoc choice choices))))
        (agent-q--load-session session-id)))))

(defun agent-q--load-session (session-id)
  "Load and display SESSION-ID."
  (let ((session (agent-q-storage-load agent-q-storage-backend session-id)))
    (if session
        (progn
          (setq agent-q-current-session session)
          (agent-q--redisplay-session))
      (user-error "Session not found: %s" session-id))))

(defun agent-q--redisplay-session ()
  "Redisplay current session in buffer."
  (let ((inhibit-read-only t))
    (agent-q--setup-buffer-layout)
    ;; Replay messages
    (dolist (msg (reverse (agent-q-session-messages agent-q-current-session)))
      (if (eq (agent-q-message-role msg) 'user)
          (agent-q--render-user-message (agent-q-message-content msg))
        (agent-q--render-assistant-message (agent-q-message-content msg))))))
```

### New Session

```elisp
(defun agent-q-new-session ()
  "Start a new session."
  (interactive)
  (when (and agent-q-current-session
             (agent-q-session-messages agent-q-current-session))
    ;; Save current session first
    (agent-q--save-current-session))
  (setq agent-q-current-session (make-agent-q-session))
  (agent-q--setup-buffer-layout)
  (message "Started new session: %s" (agent-q-session-id agent-q-current-session)))
```

### Session Search

```elisp
(defun agent-q-search-sessions (query)
  "Search past sessions for QUERY."
  (interactive "sSearch sessions: ")
  (let ((matches (agent-q-storage-search agent-q-storage-backend query)))
    (if matches
        (let* ((choices (mapcar (lambda (session)
                                  (cons (format "%s - %s (%d messages)"
                                                (agent-q-session-id session)
                                                (or (agent-q-session-name session)
                                                    "unnamed")
                                                (length (agent-q-session-messages session)))
                                        session))
                                matches))
               (choice (completing-read
                        (format "Found %d matches: " (length matches))
                        choices nil t)))
          (when choice
            (let ((session (cdr (assoc choice choices))))
              (setq agent-q-current-session session)
              (agent-q--redisplay-session))))
      (message "No sessions found matching: %s" query))))
```

### Session Naming

```elisp
(defun agent-q-name-session (name)
  "Give current session a NAME for easier identification."
  (interactive "sSession name: ")
  (setf (agent-q-session-name agent-q-current-session) name)
  (agent-q--save-current-session)
  (message "Session named: %s" name))
```

---

## Auto-Save

```elisp
(defcustom agent-q-auto-save-sessions t
  "Whether to auto-save sessions."
  :type 'boolean
  :group 'agent-q-chat)

(defcustom agent-q-auto-save-interval 300
  "Auto-save interval in seconds (0 to disable interval saves)."
  :type 'integer
  :group 'agent-q-chat)

(defvar agent-q--auto-save-timer nil
  "Timer for periodic auto-save.")

(defun agent-q--save-current-session ()
  "Save current session if it has messages."
  (when (and agent-q-current-session
             (agent-q-session-messages agent-q-current-session))
    (agent-q-storage-save agent-q-storage-backend agent-q-current-session)))

(defun agent-q--on-buffer-kill ()
  "Handle buffer kill - save session."
  (when agent-q-auto-save-sessions
    (agent-q--save-current-session)))

(defun agent-q--setup-auto-save ()
  "Setup auto-save timer."
  (when (and agent-q-auto-save-sessions
             (> agent-q-auto-save-interval 0))
    (setq agent-q--auto-save-timer
          (run-with-timer agent-q-auto-save-interval
                          agent-q-auto-save-interval
                          #'agent-q--auto-save-if-needed))))

(defun agent-q--auto-save-if-needed ()
  "Auto-save current session if buffer exists."
  (when-let ((buf (get-buffer "*Agent-Q Chat*")))
    (with-current-buffer buf
      (agent-q--save-current-session))))
```

---

## Mode Line Integration

```elisp
(defun agent-q--mode-line-session-info ()
  "Return mode line string for current session."
  (when agent-q-current-session
    (format " [%s%s]"
            (if (agent-q-session-name agent-q-current-session)
                (agent-q-session-name agent-q-current-session)
              (substring (agent-q-session-id agent-q-current-session) 8 14))
            (if (agent-q-session-messages agent-q-current-session)
                (format ":%d"
                        (length (agent-q-session-messages agent-q-current-session)))
              ""))))

;; Add to mode definition
(setq-local mode-line-format
            (append mode-line-format
                    '((:eval (agent-q--mode-line-session-info)))))
```

---

## Keybindings (already defined in Phase 1)

```elisp
(define-key agent-q-chat-mode-map (kbd "C-c C-s") #'agent-q-switch-session)
(define-key agent-q-chat-mode-map (kbd "C-c C-n") #'agent-q-new-session)
;; Additional
(define-key agent-q-chat-mode-map (kbd "C-c C-f") #'agent-q-search-sessions)
(define-key agent-q-chat-mode-map (kbd "C-c C-r") #'agent-q-name-session)
```

---

## Verification Checklist

- [ ] Sessions saved to `~/.emacs.d/agent-q-sessions/`
- [ ] Session files are readable Lisp
- [ ] `C-c C-s` lists all sessions with completion
- [ ] Selecting session loads and displays it
- [ ] `C-c C-n` creates new session (saves current first)
- [ ] `C-c C-f` searches sessions by content
- [ ] `C-c C-r` names current session
- [ ] Session auto-saves on buffer kill
- [ ] Session auto-saves periodically
- [ ] Mode line shows session info
- [ ] Messages preserved across Emacs restart

---

## Future Enhancements (Not in This Phase)

- Vector DB backend for semantic search
- Session tagging and filtering
- Session export (markdown, org-mode)
- Session archiving/cleanup
- Conversation threading
