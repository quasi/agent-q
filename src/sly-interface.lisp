;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; SLY RPC endpoints - callable from Elisp

(defun agent-q-send (message &key include-context)
  "Send MESSAGE to the agent. Returns response string.
   If INCLUDE-CONTEXT is true, accumulated context is included."
  (ensure-agent)
  (send-to-agent *current-agent* message :include-context include-context))

(defun agent-q-add-context (content &key (type :code) metadata)
  "Add CONTENT to the current context.
   TYPE is one of: :code :text :file :repl-history :error :custom
   METADATA is a plist with keys like :filename :start-line :end-line :package
   Uses session's conversation if available."
  (ensure-agent)
  (let ((conversation (or (and *session-manager*
                               (current-session *session-manager*)
                               (session-conversation (current-session *session-manager*)))
                          (agent-conversation *current-agent*))))
    (add-context (conversation-context conversation)
                 content
                 :type type
                 :metadata metadata))
  t)

(defun agent-q-clear-context ()
  "Clear all accumulated context.
   Uses session's conversation if available."
  (let ((conversation (or (and *session-manager*
                               (current-session *session-manager*)
                               (session-conversation (current-session *session-manager*)))
                          (and *current-agent*
                               (agent-conversation *current-agent*)))))
    (when conversation
      (clear-context (conversation-context conversation))
      t)))

(defun agent-q-get-context-summary ()
  "Return a summary of current context for display.
   Uses session's conversation if available.
   Returns plist: (:count N :types (list of types) :preview STRING)"
  (let ((conversation (or (and *session-manager*
                               (current-session *session-manager*)
                               (session-conversation (current-session *session-manager*)))
                          (and *current-agent*
                               (agent-conversation *current-agent*)))))
    (if conversation
        (let* ((ctx-mgr (conversation-context conversation))
               (items (coerce (context-items ctx-mgr) 'list))
               (count (length items))
               (types (remove-duplicates (mapcar #'context-item-type items)))
               (preview (if (> count 0)
                           (format nil "~D item~:P: ~{~A~^, ~}"
                                  count
                                  (mapcar (lambda (type)
                                           (string-capitalize (symbol-name type)))
                                         types))
                           "No context")))
          (list :count count :types types :preview preview))
        (list :count 0 :types nil :preview "No agent initialized"))))

(defun agent-q-new-conversation (&key project)
  "Start a new conversation, optionally associated with PROJECT.
   Creates a new session and uses its conversation."
  (ensure-agent)
  ;; Create a new session (this saves the current one first)
  (let ((session (create-session :name project)))
    ;; Also update agent's conversation for fallback compatibility
    (setf (agent-conversation *current-agent*)
          (session-conversation session)))
  t)

(defun agent-q-configure (&key provider model api-key)
  "Configure the agent. Returns T on success, error message on failure."
  (configure :provider provider :model model :api-key api-key))

(defun agent-q-get-conversation-history (&key limit)
  "Return recent conversation history for display.
   Uses session's conversation if available, otherwise agent's.
   Returns list of plists (:role ROLE :content CONTENT :timestamp TIME)"
  (let ((conversation (or (and *session-manager*
                               (current-session *session-manager*)
                               (session-conversation (current-session *session-manager*)))
                          (and *current-agent*
                               (agent-conversation *current-agent*)))))
    (if conversation
        (let ((messages (get-messages conversation :limit limit)))
          (mapcar (lambda (msg)
                    (list :role (message-role msg)
                          :content (message-content msg)
                          :timestamp (message-timestamp msg)))
                  messages))
        nil)))

;;; Phase 2: Diff workflow RPC endpoint

(defun agent-q-show-diff (path original modified description)
  "Show diff in Emacs and wait for user decision.
   Returns 'ACCEPTED or 'REJECTED."
  (agent-q.tools:eval-in-emacs
   `(sly-agent-q-show-diff-and-wait ,path ,original ,modified ,description)))

;;; ============================================================================
;;; Session Management RPC Endpoints
;;; ============================================================================

(defun agent-q-create-session (&key name)
  "Create a new session, saving current session first.
   Returns the new session ID."
  (let ((session (create-session :name name
                                 :model (when *current-agent*
                                          (agent-model *current-agent*)))))
    (session-id session)))

(defun agent-q-switch-session (session-id)
  "Switch to session by SESSION-ID, saving current first.
   Returns T on success, NIL if session not found."
  (not (null (switch-session session-id))))

(defun agent-q-save-session ()
  "Save the current session to disk.
   Returns the filepath on success, NIL if no current session."
  (let ((session (current-session (ensure-session-manager))))
    (when session
      (namestring (save-session session)))))

(defun agent-q-delete-session (session-id)
  "Delete session by SESSION-ID.
   Returns T if deleted, NIL if not found."
  (delete-session session-id))

(defun agent-q-rename-session (name)
  "Rename the current session.
   Returns T on success, NIL if no current session."
  (let ((session (current-session (ensure-session-manager))))
    (when session
      (setf (session-name session) name)
      (save-session session)
      t)))

(defun agent-q-list-sessions ()
  "List all sessions for completion UI.
   Returns list of plists: (:id ID :name NAME :created-at TIME :message-count N)"
  (list-sessions))

(defun agent-q-search-sessions (query)
  "Search sessions by name and message content.
   Returns list of matching session plists."
  (search-sessions query))

(defun agent-q-get-session-info ()
  "Get current session info for mode line display.
   Returns plist: (:id ID :name NAME :message-count N
                  :total-input-tokens N :total-output-tokens N
                  :model MODEL :provider PROVIDER)"
  (let ((session (current-session (ensure-session-manager))))
    (when session
      (let ((meta (session-metadata session)))
        (list :id (session-id session)
              :name (session-name session)
              :message-count (session-message-count session)
              :total-input-tokens (getf meta :total-input-tokens)
              :total-output-tokens (getf meta :total-output-tokens)
              :model (session-model session)
              :provider (getf meta :provider))))))
