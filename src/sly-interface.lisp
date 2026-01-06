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
   METADATA is a plist with keys like :filename :start-line :end-line :package"
  (ensure-agent)
  (add-context (conversation-context (agent-conversation *current-agent*))
               content
               :type type
               :metadata metadata)
  t)

(defun agent-q-clear-context ()
  "Clear all accumulated context."
  (when *current-agent*
    (clear-context (conversation-context (agent-conversation *current-agent*)))
    t))

(defun agent-q-get-context-summary ()
  "Return a summary of current context for display.
   Returns plist: (:count N :types (list of types) :preview STRING)"
  (if *current-agent*
      (let* ((ctx-mgr (conversation-context (agent-conversation *current-agent*)))
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
      (list :count 0 :types nil :preview "No agent initialized")))

(defun agent-q-new-conversation (&key project)
  "Start a new conversation, optionally associated with PROJECT."
  (ensure-agent)
  (setf (agent-conversation *current-agent*)
        (new-conversation :project project))
  t)

(defun agent-q-configure (&key provider model api-key)
  "Configure the agent. Returns T on success, error message on failure."
  (configure :provider provider :model model :api-key api-key))

(defun agent-q-get-conversation-history (&key limit)
  "Return recent conversation history for display.
   Returns list of plists (:role ROLE :content CONTENT :timestamp TIME)"
  (if *current-agent*
      (let ((messages (get-messages (agent-conversation *current-agent*)
                                   :limit limit)))
        (mapcar (lambda (msg)
                  (list :role (message-role msg)
                        :content (message-content msg)
                        :timestamp (message-timestamp msg)))
                messages))
      nil))

;;; Phase 2: Diff workflow RPC endpoint

(defun agent-q-show-diff (path original modified description)
  "Show diff in Emacs and wait for user decision.
   Returns 'ACCEPTED or 'REJECTED."
  (agent-q.tools:eval-in-emacs
   `(sly-agent-q-show-diff-and-wait ,path ,original ,modified ,description)))
