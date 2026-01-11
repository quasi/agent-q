;;; sly-agent-q-sessions.el --- Session persistence for Agent-Q -*- lexical-binding: t; -*-

;; ABOUTME: Session management module for Agent-Q chat interface.
;; ABOUTME: Provides save, load, switch, and search functionality for chat sessions.

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (sly "1.0"))
;; Keywords: lisp, ai, chat, sessions
;; URL: https://github.com/quasilabs/agent-q

;;; Commentary:

;; This module provides session persistence for the Agent-Q chat interface.
;; Sessions are stored as Lisp files in ~/.emacs.d/agent-q-sessions/,
;; making them human-readable and grep-searchable.
;;
;; Features:
;; - Storage backend protocol for future extensibility (e.g., vector DB)
;; - File-based storage with readable Lisp format
;; - Session switching with completion
;; - Grep-based search across sessions
;; - Auto-save on buffer kill and periodic timer
;; - Mode line session indicator
;;
;; Usage:
;;   C-c C-s   - Switch to a different session
;;   C-c C-n   - Start a new session
;;   C-c C-f   - Search sessions by content
;;   C-c C-r   - Rename current session

;;; Code:

(require 'cl-lib)
(require 'eieio)

;; IMPORTANT: This module MUST be loaded via sly-agent-q-chat.el's require,
;; which happens AFTER the struct definitions. Loading this file directly
;; or from a stale .elc will cause errors. Delete any .elc files if you
;; see "void-function make-agent-q-message" errors.

;; Forward declarations for chat module symbols
(declare-function agent-q-message-id "sly-agent-q-chat")
(declare-function agent-q-message-role "sly-agent-q-chat")
(declare-function agent-q-message-content "sly-agent-q-chat")
(declare-function agent-q-message-timestamp "sly-agent-q-chat")
(declare-function agent-q-message-context-snapshot "sly-agent-q-chat")
(declare-function agent-q-message-metadata "sly-agent-q-chat")
(declare-function make-agent-q-message "sly-agent-q-chat")
(declare-function agent-q-session-id "sly-agent-q-chat")
(declare-function agent-q-session-name "sly-agent-q-chat")
(declare-function agent-q-session-created-at "sly-agent-q-chat")
(declare-function agent-q-session-updated-at "sly-agent-q-chat")
(declare-function agent-q-session-messages "sly-agent-q-chat")
(declare-function agent-q-session-model "sly-agent-q-chat")
(declare-function agent-q-session-metadata "sly-agent-q-chat")
(declare-function make-agent-q-session "sly-agent-q-chat")
(declare-function agent-q-session--create "sly-agent-q-chat")
(declare-function agent-q--setup-buffer-layout "sly-agent-q-chat")
(declare-function agent-q--render-user-message "sly-agent-q-chat")
(declare-function agent-q--render-assistant-message "sly-agent-q-chat")
(declare-function agent-q-session-set-name "sly-agent-q-chat")
(declare-function agent-q-session-set-model "sly-agent-q-chat")
(declare-function agent-q-session-update-metadata "sly-agent-q-chat")
(declare-function agent-q-session-add-tokens "sly-agent-q-chat")

(defvar agent-q--current-session)
(defvar agent-q-chat-buffer-name)

;;; Customization

(defgroup agent-q-sessions nil
  "Agent-Q session management."
  :group 'agent-q-chat
  :prefix "agent-q-")

(defcustom agent-q-sessions-directory
  (expand-file-name "agent-q-sessions" user-emacs-directory)
  "Directory where Agent-Q sessions are stored."
  :type 'directory
  :group 'agent-q-sessions)

(defcustom agent-q-auto-save-sessions t
  "Whether to auto-save sessions."
  :type 'boolean
  :group 'agent-q-sessions)

(defcustom agent-q-auto-save-interval 300
  "Auto-save interval in seconds.
Set to 0 to disable periodic saves (still saves on buffer kill)."
  :type 'integer
  :group 'agent-q-sessions)

;;; Storage Backend Protocol

(cl-defgeneric agent-q-storage-save (backend session)
  "Save SESSION using BACKEND.
SESSION is an `agent-q-session' struct.")

(cl-defgeneric agent-q-storage-load (backend session-id)
  "Load session with SESSION-ID using BACKEND.
Return an `agent-q-session' struct or nil if not found.")

(cl-defgeneric agent-q-storage-delete (backend session-id)
  "Delete session with SESSION-ID using BACKEND.")

(cl-defgeneric agent-q-storage-list (backend)
  "List all sessions using BACKEND.
Return a list of plists with :id, :name, :created keys.")

(cl-defgeneric agent-q-storage-search (backend query)
  "Search sessions matching QUERY using BACKEND.
Return a list of `agent-q-session' structs.")

;;; File-Based Storage Backend

(defclass agent-q-storage-file-backend ()
  ((directory :initarg :directory
              :initform (expand-file-name "agent-q-sessions"
                                          user-emacs-directory)
              :accessor agent-q-storage-directory
              :documentation "Directory for session files."))
  "File-based session storage backend.
Stores sessions as readable Lisp files.")

(defvar agent-q-storage-backend nil
  "The active storage backend.
Initialized lazily to respect customization.")

(defun agent-q--get-storage-backend ()
  "Get or create the storage backend."
  (unless agent-q-storage-backend
    (setq agent-q-storage-backend
          (make-instance 'agent-q-storage-file-backend
                         :directory agent-q-sessions-directory)))
  agent-q-storage-backend)

;;; Serialization Helpers

(defun agent-q--message-to-plist (msg)
  "Convert MSG struct to plist for storage."
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

(defun agent-q--read-session-metadata (file)
  "Read just the metadata from session FILE for fast listing.
Parses header comments without reading the full session data."
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

;;; File Backend Implementation

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
      (delete-file file)
      t)))

(cl-defmethod agent-q-storage-list ((backend agent-q-storage-file-backend))
  "List all sessions with metadata."
  (let* ((dir (agent-q-storage-directory backend))
         (files (and (file-directory-p dir)
                     (directory-files dir t "^session-.*\\.el$"))))
    (mapcar #'agent-q--read-session-metadata
            (sort files #'file-newer-than-file-p))))

(cl-defmethod agent-q-storage-search ((backend agent-q-storage-file-backend)
                                      query)
  "Search sessions for QUERY using grep."
  (let* ((dir (agent-q-storage-directory backend))
         (default-directory dir))
    (when (file-directory-p dir)
      (let ((matches (split-string
                      (shell-command-to-string
                       (format "grep -l -i %s session-*.el 2>/dev/null"
                               (shell-quote-argument query)))
                      "\n" t)))
        (mapcar (lambda (file)
                  (agent-q-storage-load backend (file-name-base file)))
                matches)))))

;;; Session Save/Load

(defun agent-q--save-current-session ()
  "Save current session if it has messages."
  (when (and (boundp 'agent-q--current-session)
             agent-q--current-session
             (agent-q-session-messages agent-q--current-session))
    (agent-q-storage-save (agent-q--get-storage-backend)
                          agent-q--current-session)))

(defun agent-q--load-session (session-id)
  "Load and display SESSION-ID in the chat buffer."
  (let ((session (agent-q-storage-load (agent-q--get-storage-backend)
                                       session-id)))
    (if session
        (progn
          (setq agent-q--current-session session)
          (agent-q--redisplay-session))
      (user-error "Session not found: %s" session-id))))

(defun agent-q--redisplay-session ()
  "Redisplay current session in buffer."
  (let ((inhibit-read-only t))
    (agent-q--setup-buffer-layout)
    ;; Replay messages (messages are stored most-recent-first)
    (dolist (msg (reverse (agent-q-session-messages agent-q--current-session)))
      (pcase (agent-q-message-role msg)
        ('user (agent-q--render-user-message (agent-q-message-content msg)))
        ('assistant (agent-q--render-assistant-message (agent-q-message-content msg)))))))

;;; User Interface

;;;###autoload
(defun agent-q-switch-session ()
  "Switch to a different session."
  (interactive)
  (let* ((sessions (agent-q-storage-list (agent-q--get-storage-backend)))
         (choices (mapcar (lambda (meta)
                            (cons (format "%s%s%s"
                                          (plist-get meta :id)
                                          (if (plist-get meta :name)
                                              (format " - %s" (plist-get meta :name))
                                            "")
                                          (if (plist-get meta :created)
                                              (format " (%s)" (plist-get meta :created))
                                            ""))
                                  (plist-get meta :id)))
                          sessions)))
    (if (null choices)
        (message "No saved sessions found")
      (let ((choice (completing-read "Switch to session: " choices nil t)))
        (when choice
          ;; Save current session first
          (agent-q--save-current-session)
          (let ((session-id (cdr (assoc choice choices))))
            (agent-q--load-session session-id)))))))

;;;###autoload
(defun agent-q-new-session ()
  "Start a new session, saving the current one first."
  (interactive)
  (when (and (boundp 'agent-q--current-session)
             agent-q--current-session
             (agent-q-session-messages agent-q--current-session))
    ;; Save current session first
    (agent-q--save-current-session))
  (setq agent-q--current-session (agent-q-session--create))
  (agent-q--setup-buffer-layout)
  (message "Started new session: %s" (agent-q-session-id agent-q--current-session)))

;;;###autoload
(defun agent-q-search-sessions (query)
  "Search past sessions for QUERY."
  (interactive "sSearch sessions: ")
  (let ((matches (agent-q-storage-search (agent-q--get-storage-backend) query)))
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
            ;; Save current session first
            (agent-q--save-current-session)
            (let ((session (cdr (assoc choice choices))))
              (setq agent-q--current-session session)
              (agent-q--redisplay-session))))
      (message "No sessions found matching: %s" query))))

;;;###autoload
(defun agent-q-name-session (name)
  "Give current session a NAME for easier identification."
  (interactive "sSession name: ")
  (unless (and (boundp 'agent-q--current-session) agent-q--current-session)
    (user-error "No active session"))
  (agent-q-session-set-name agent-q--current-session name)
  (agent-q--save-current-session)
  (message "Session named: %s" name))

;;;###autoload
(defun agent-q-delete-session (session-id)
  "Delete the session with SESSION-ID."
  (interactive
   (let* ((sessions (agent-q-storage-list (agent-q--get-storage-backend)))
          (choices (mapcar (lambda (meta)
                             (cons (format "%s%s"
                                           (plist-get meta :id)
                                           (if (plist-get meta :name)
                                               (format " - %s" (plist-get meta :name))
                                             ""))
                                   (plist-get meta :id)))
                           sessions)))
     (if (null choices)
         (user-error "No saved sessions")
       (list (cdr (assoc (completing-read "Delete session: " choices nil t)
                         choices))))))
  (when (and session-id
             (yes-or-no-p (format "Delete session %s? " session-id)))
    (agent-q-storage-delete (agent-q--get-storage-backend) session-id)
    (message "Deleted session: %s" session-id)))

;;; Auto-Save

(defvar agent-q--auto-save-timer nil
  "Timer for periodic auto-save.")

(defun agent-q--auto-save-if-needed ()
  "Auto-save current session if chat buffer exists."
  (when-let ((buf (get-buffer (if (boundp 'agent-q-chat-buffer-name)
                                  agent-q-chat-buffer-name
                                "*Agent-Q Chat*"))))
    (with-current-buffer buf
      (agent-q--save-current-session))))

(defun agent-q--setup-auto-save ()
  "Setup auto-save timer."
  (agent-q--cancel-auto-save)
  (when (and agent-q-auto-save-sessions
             (> agent-q-auto-save-interval 0))
    (setq agent-q--auto-save-timer
          (run-with-timer agent-q-auto-save-interval
                          agent-q-auto-save-interval
                          #'agent-q--auto-save-if-needed))))

(defun agent-q--cancel-auto-save ()
  "Cancel auto-save timer if running."
  (when agent-q--auto-save-timer
    (cancel-timer agent-q--auto-save-timer)
    (setq agent-q--auto-save-timer nil)))

(defun agent-q--on-chat-buffer-kill ()
  "Handle chat buffer kill - save session."
  (when agent-q-auto-save-sessions
    (agent-q--save-current-session)))

;;; Mode Line Integration

(defun agent-q--mode-line-session-info ()
  "Return mode line string for current session."
  (when (and (boundp 'agent-q--current-session) agent-q--current-session)
    (let* ((session agent-q--current-session)
           (meta (agent-q-session-metadata session))
           (in-tokens (plist-get meta :total-input-tokens))
           (out-tokens (plist-get meta :total-output-tokens))
           (name-or-id (if (agent-q-session-name session)
                           (agent-q-session-name session)
                         ;; Show abbreviated session ID (date portion)
                         (let ((id (agent-q-session-id session)))
                           (if (> (length id) 14)
                               (substring id 8 14)
                             id))))
           (msg-count (length (agent-q-session-messages session))))
      (format " [%s%s%s]"
              name-or-id
              (if (> msg-count 0) (format ":%d" msg-count) "")
              (if (and in-tokens out-tokens (> (+ in-tokens out-tokens) 0))
                  (format " ↑%d↓%d" in-tokens out-tokens)
                "")))))

;;; Initialization

(defun agent-q-sessions-initialize ()
  "Initialize session management.
Call this from chat mode setup."
  (agent-q--setup-auto-save)
  ;; Add session info to mode line
  (setq-local mode-line-misc-info
              (append mode-line-misc-info
                      '((:eval (agent-q--mode-line-session-info))))))

(provide 'sly-agent-q-sessions)
;;; sly-agent-q-sessions.el ends here
