;;; sly-agent-q-sessions.el --- Session persistence for Agent-Q -*- lexical-binding: t; -*-

;; ABOUTME: Session management UI for Agent-Q chat interface.
;; ABOUTME: Provides UI for session operations; persistence handled by Common Lisp.

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (sly "1.0"))
;; Keywords: lisp, ai, chat, sessions
;; URL: https://github.com/quasilabs/agent-q

;;; Commentary:

;; This module provides session management UI for the Agent-Q chat interface.
;; Session persistence is handled by Common Lisp via RPC calls.
;;
;; Architecture:
;; - Elisp: UI layer (commands, completion, mode line, keybindings)
;; - Common Lisp: Persistence layer (save, load, search, serialization)
;;
;; Usage:
;;   C-c C-s   - Switch to a different session
;;   C-c C-n   - Start a new session
;;   C-c C-f   - Search sessions by content
;;   C-c C-r   - Rename current session

;;; Code:

(require 'cl-lib)
(require 'sly)

;; Forward declarations for chat module symbols
(declare-function agent-q-message-role "sly-agent-q-chat")
(declare-function agent-q-message-content "sly-agent-q-chat")
(declare-function agent-q-session-id "sly-agent-q-chat")
(declare-function agent-q-session-name "sly-agent-q-chat")
(declare-function agent-q-session-messages "sly-agent-q-chat")
(declare-function agent-q-session-metadata "sly-agent-q-chat")
(declare-function agent-q-session--create "sly-agent-q-chat")
(declare-function agent-q--setup-buffer-layout "sly-agent-q-chat")
(declare-function agent-q--render-user-message "sly-agent-q-chat")
(declare-function agent-q--render-assistant-message "sly-agent-q-chat")
(declare-function agent-q-session-set-name "sly-agent-q-chat")

(defvar agent-q--current-session)
(defvar agent-q-chat-buffer-name)

;;; Customization

(defgroup agent-q-sessions nil
  "Agent-Q session management."
  :group 'agent-q-chat
  :prefix "agent-q-")

(defcustom agent-q-auto-save-sessions t
  "Whether to auto-save sessions."
  :type 'boolean
  :group 'agent-q-sessions)

(defcustom agent-q-auto-save-interval 300
  "Auto-save interval in seconds.
Set to 0 to disable periodic saves (still saves on buffer kill)."
  :type 'integer
  :group 'agent-q-sessions)

;;; RPC Helpers

(defun agent-q--check-sly-connection ()
  "Check if SLY is connected and signal error if not."
  (unless (sly-connected-p)
    (user-error "Not connected to Lisp. Start SLY first with M-x sly")))

;;; Session Operations (RPC-based)

(defun agent-q--save-current-session ()
  "Save current session via RPC.
Does nothing if not connected to SLY."
  (when (sly-connected-p)
    (sly-eval-async '(agent-q:agent-q-save-session)
      (lambda (result)
        (when result
          (message "Session saved"))))))

(defun agent-q--list-sessions-sync ()
  "List sessions synchronously via RPC.
Returns list of plists with :id, :name, :created-at."
  (agent-q--check-sly-connection)
  (sly-eval '(agent-q:agent-q-list-sessions)))

(defun agent-q--search-sessions-sync (query)
  "Search sessions for QUERY synchronously via RPC.
Returns list of matching session plists."
  (agent-q--check-sly-connection)
  (sly-eval `(agent-q:agent-q-search-sessions ,query)))

(defun agent-q--switch-session-rpc (session-id)
  "Switch to SESSION-ID via RPC."
  (agent-q--check-sly-connection)
  (sly-eval `(agent-q:agent-q-switch-session ,session-id)))

(defun agent-q--create-session-rpc (&optional name)
  "Create new session via RPC, optionally with NAME.
Returns new session ID."
  (agent-q--check-sly-connection)
  (sly-eval `(agent-q:agent-q-create-session :name ,name)))

(defun agent-q--delete-session-rpc (session-id)
  "Delete SESSION-ID via RPC."
  (agent-q--check-sly-connection)
  (sly-eval `(agent-q:agent-q-delete-session ,session-id)))

(defun agent-q--rename-session-rpc (name)
  "Rename current session to NAME via RPC."
  (agent-q--check-sly-connection)
  (sly-eval `(agent-q:agent-q-rename-session ,name)))

(defun agent-q--get-session-info-rpc ()
  "Get current session info via RPC.
Returns plist with :id, :name, :message-count, :total-input-tokens, etc."
  (when (sly-connected-p)
    (sly-eval '(agent-q:agent-q-get-session-info))))

;;; Session Display

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
        (when choice
          (let ((session-id (cdr (assoc choice choices))))
            (when (agent-q--switch-session-rpc session-id)
              ;; Update local state - create new session struct
              ;; The actual data is managed by CL, we just need a placeholder
              (setq agent-q--current-session (agent-q-session--create))
              (setf (agent-q-session-id agent-q--current-session) session-id)
              (message "Switched to session: %s" session-id))))))))

(defun agent-q--format-timestamp (universal-time)
  "Format UNIVERSAL-TIME (CL universal time) as readable date."
  (if (numberp universal-time)
      ;; Convert from CL universal time (seconds since 1900) to Emacs time
      ;; CL epoch is 1900-01-01, Unix epoch is 1970-01-01
      ;; Difference: 2208988800 seconds
      (let ((unix-time (- universal-time 2208988800)))
        (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time unix-time)))
    ""))

;;;###autoload
(defun agent-q-new-session ()
  "Start a new session, saving the current one first."
  (interactive)
  (agent-q--check-sly-connection)
  (let ((session-id (agent-q--create-session-rpc)))
    (when session-id
      ;; Update local state
      (setq agent-q--current-session (agent-q-session--create))
      (agent-q--setup-buffer-layout)
      (message "Started new session: %s" session-id))))

;;;###autoload
(defun agent-q-search-sessions (query)
  "Search past sessions for QUERY."
  (interactive "sSearch sessions: ")
  (agent-q--check-sly-connection)
  (let ((matches (agent-q--search-sessions-sync query)))
    (if matches
        (let* ((choices (mapcar (lambda (meta)
                                  (cons (format "%s - %s"
                                                (plist-get meta :id)
                                                (or (plist-get meta :name) "unnamed"))
                                        (plist-get meta :id)))
                                matches))
               (choice (completing-read
                        (format "Found %d matches: " (length matches))
                        choices nil t)))
          (when choice
            (let ((session-id (cdr (assoc choice choices))))
              (when (agent-q--switch-session-rpc session-id)
                (setq agent-q--current-session (agent-q-session--create))
                (message "Switched to session: %s" session-id)))))
      (message "No sessions found matching: %s" query))))

;;;###autoload
(defun agent-q-name-session (name)
  "Give current session a NAME for easier identification."
  (interactive "sSession name: ")
  (agent-q--check-sly-connection)
  (unless (and (boundp 'agent-q--current-session) agent-q--current-session)
    (user-error "No active session"))
  (when (agent-q--rename-session-rpc name)
    (agent-q-session-set-name agent-q--current-session name)
    (message "Session named: %s" name)))

;;;###autoload
(defun agent-q-delete-session (session-id)
  "Delete the session with SESSION-ID."
  (interactive
   (progn
     (agent-q--check-sly-connection)
     (let* ((sessions (agent-q--list-sessions-sync))
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
                           choices)))))))
  (when (and session-id
             (yes-or-no-p (format "Delete session %s? " session-id)))
    (when (agent-q--delete-session-rpc session-id)
      (message "Deleted session: %s" session-id))))

;;; Auto-Save

(defvar agent-q--auto-save-timer nil
  "Timer for periodic auto-save.")

(defun agent-q--auto-save-if-needed ()
  "Auto-save current session if chat buffer exists and SLY is connected."
  (when (sly-connected-p)
    (when-let ((buf (get-buffer (if (boundp 'agent-q-chat-buffer-name)
                                    agent-q-chat-buffer-name
                                  "*Agent-Q Chat*"))))
      (with-current-buffer buf
        (agent-q--save-current-session)))))

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
  "Return mode line string for current session.
Fetches info from CL if connected, otherwise uses local state."
  (when (and (boundp 'agent-q--current-session) agent-q--current-session)
    (let* ((info (agent-q--get-session-info-rpc))
           ;; Use RPC info if available, fall back to local state
           (name-or-id (or (plist-get info :name)
                           (when (agent-q-session-name agent-q--current-session)
                             (agent-q-session-name agent-q--current-session))
                           (let ((id (or (plist-get info :id)
                                         (agent-q-session-id agent-q--current-session))))
                             (if (and id (> (length id) 14))
                                 (substring id 8 14)
                               id))))
           (msg-count (or (plist-get info :message-count)
                          (length (agent-q-session-messages agent-q--current-session))))
           (in-tokens (or (plist-get info :total-input-tokens)
                          (plist-get (agent-q-session-metadata agent-q--current-session)
                                     :total-input-tokens)))
           (out-tokens (or (plist-get info :total-output-tokens)
                           (plist-get (agent-q-session-metadata agent-q--current-session)
                                      :total-output-tokens))))
      (format " [%s%s%s]"
              (or name-or-id "?")
              (if (and msg-count (> msg-count 0)) (format ":%d" msg-count) "")
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
