;;; sly-agent-q.el --- Agent-Q: AI-powered assistant for Common Lisp development -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (sly "1.0"))
;; Keywords: lisp, ai, assistant, tools
;; URL: https://github.com/quasilabs/agent-q

;;; Commentary:

;; Agent-Q is an AI-powered agentic extension for Common Lisp development.
;; It integrates with SLY to provide context-aware assistance, code generation,
;; debugging help, and more.

;;; Code:

(require 'sly)
(require 'sly-agent-q-diff)
(require 'sly-agent-q-tools)

;; Enable eval-in-emacs for Agent-Q to send debug messages in real-time
(setq sly-enable-evaluate-in-emacs t)

;;; Customization

(defgroup sly-agent-q nil
  "Agent-Q: AI-powered assistant for Common Lisp development."
  :group 'sly
  :prefix "sly-agent-q-")

(defcustom sly-agent-q-keymap-prefix (kbd "C-c q")
  "Prefix for Agent-Q keybindings."
  :type 'key-sequence
  :group 'sly-agent-q)

(defcustom sly-agent-q-conversation-buffer-name "*Agent-Q*"
  "Name of the conversation buffer."
  :type 'string
  :group 'sly-agent-q)

(defcustom sly-agent-q-insert-response-at-point t
  "If non-nil, insert agent responses at point.
Otherwise show in conversation buffer only."
  :type 'boolean
  :group 'sly-agent-q)

;;; Faces

(defface sly-agent-q-user-face
  '((t :foreground "cyan" :weight bold))
  "Face for user messages in conversation buffer."
  :group 'sly-agent-q)

(defface sly-agent-q-assistant-face
  '((t :foreground "green"))
  "Face for assistant messages in conversation buffer."
  :group 'sly-agent-q)

(defface sly-agent-q-context-face
  '((t :foreground "yellow" :slant italic))
  "Face for context indicators."
  :group 'sly-agent-q)

(defface sly-agent-q-debug-face
  '((t :foreground "gray50" :slant italic))
  "Face for debug/tool execution messages in conversation buffer."
  :group 'sly-agent-q)

;;; State

(defvar sly-agent-q--last-response nil
  "The last response from the agent.")

;;; Agent-Q availability check

(defun sly-agent-q--agent-loaded-p ()
  "Check if agent-q is loaded in the connected Lisp."
  (and (sly-connected-p)
       (ignore-errors
         ;; Use (if ... t nil) to ensure we get T or NIL back
         ;; SBCL 2.5+ returns the function object from fboundp which can't
         ;; be serialized over the SLY protocol
         (sly-eval '(cl:if (cl:and (cl:find-package :agent-q)
                                   (cl:fboundp 'agent-q:agent-q-send))
                           t
                           nil)))))

(defun sly-agent-q--check-loaded ()
  "Check if agent-q is loaded, show helpful message if not.
Returns t if loaded, nil otherwise."
  (if (sly-agent-q--agent-loaded-p)
      t
    (message "Agent-Q not loaded. Load it with: (asdf:load-system :agent-q)")
    nil))

;;; Conversation Buffer Mode

(defvar sly-agent-q-conversation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'sly-agent-q-new-conversation)
    (define-key map (kbd "C-c C-c") #'sly-agent-q-send)
    map)
  "Keymap for Agent-Q conversation buffer.")

(define-derived-mode sly-agent-q-conversation-mode special-mode "Agent-Q"
  "Major mode for Agent-Q conversation buffer.

\\{sly-agent-q-conversation-mode-map}"
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun sly-agent-q--get-conversation-buffer ()
  "Get or create the Agent-Q conversation buffer."
  (let ((buffer (get-buffer-create sly-agent-q-conversation-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'sly-agent-q-conversation-mode)
        (sly-agent-q-conversation-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "=== Agent-Q Conversation ===\n"
                             'face 'bold)
                  (propertize "Press 'q' to hide, 'n' for new conversation\n\n"
                             'face 'font-lock-comment-face)))))
    buffer))

(defun sly-agent-q--display-conversation-buffer ()
  "Display the Agent-Q conversation buffer."
  (let ((buffer (sly-agent-q--get-conversation-buffer)))
    (display-buffer buffer)
    buffer))

(defun sly-agent-q--append-to-conversation (role content)
  "Append a message with ROLE and CONTENT to the conversation buffer."
  (let ((buffer (sly-agent-q--get-conversation-buffer))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((face (pcase role
                    ('user 'sly-agent-q-user-face)
                    ('assistant 'sly-agent-q-assistant-face)
                    ('debug 'sly-agent-q-debug-face)
                    (_ 'default))))
        ;; For debug messages, don't show the [DEBUG] header, just indent the content
        (if (eq role 'debug)
            (insert (propertize (format "  %s\n" content)
                               'face face))
          (progn
            ;; Show [AGENT-Q] instead of [ASSISTANT] for assistant role
            (let ((role-label (if (eq role 'assistant) "AGENT-Q" (upcase (symbol-name role)))))
              (insert (propertize (format "\n[%s]\n" role-label)
                                 'face face)))
            (insert content "\n"))))
      (goto-char (point-max)))))

;;; Context Commands

;;;###autoload
(defun sly-agent-q-add-region-to-context (start end)
  "Add the selected region to Agent-Q context."
  (interactive "r")
  (when (sly-agent-q--check-loaded)
    (let* ((content (buffer-substring-no-properties start end))
           (filename (buffer-file-name))
           (start-line (line-number-at-pos start))
           (end-line (line-number-at-pos end))
           (metadata (list :filename filename
                          :start-line start-line
                          :end-line end-line)))
      (sly-eval-async
          `(agent-q:agent-q-add-context ,content
                                        :type :code
                                        :metadata ',metadata)
        (lambda (result)
          (message "Added %d lines to Agent-Q context" (- end-line start-line -1)))))))

;;;###autoload
(defun sly-agent-q-add-buffer-to-context ()
  "Add the entire current buffer to Agent-Q context."
  (interactive)
  (sly-agent-q-add-region-to-context (point-min) (point-max)))

;;;###autoload
(defun sly-agent-q-add-defun-to-context ()
  "Add the current top-level form to Agent-Q context."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (sly-agent-q-add-region-to-context start (point)))))

;;;###autoload
(defun sly-agent-q-clear-context ()
  "Clear all accumulated context."
  (interactive)
  (when (sly-agent-q--check-loaded)
    (sly-eval-async
        '(agent-q:agent-q-clear-context)
      (lambda (result)
        (message "Agent-Q context cleared")))))

;;;###autoload
(defun sly-agent-q-show-context ()
  "Display current context summary in minibuffer."
  (interactive)
  (when (sly-agent-q--check-loaded)
    (sly-eval-async
        '(agent-q:agent-q-get-context-summary)
      (lambda (result)
        (let ((preview (plist-get result :preview)))
          (message "Agent-Q context: %s" preview))))))

;;; Conversation Commands

;;;###autoload
(defun sly-agent-q-send (message)
  "Send MESSAGE to the agent."
  (interactive "sMessage to Agent-Q: ")
  (when (sly-agent-q--check-loaded)
    (sly-agent-q--append-to-conversation 'user message)
    (sly-agent-q--display-conversation-buffer)
    (message "Sending to Agent-Q...")
    (sly-eval-async
        `(agent-q:agent-q-send ,message :include-context nil)
      (lambda (response)
        (if (stringp response)
            (progn
              (setq sly-agent-q--last-response response)
              ;; Debug messages appear in real-time during execution
              ;; Just append the final response
              (sly-agent-q--append-to-conversation 'assistant response)
              (message "Agent-Q response received"))
          ;; Debug: unexpected response type
          (message "ERROR: Expected string, got %s: %S" (type-of response) response)
          (sly-agent-q--append-to-conversation
           'assistant
           (format "ERROR: Unexpected response type: %s\nValue: %S"
                   (type-of response) response)))))))

;;;###autoload
(defun sly-agent-q-send-with-context (message)
  "Send MESSAGE to the agent with accumulated context."
  (interactive "sMessage to Agent-Q (with context): ")
  (when (sly-agent-q--check-loaded)
    (sly-agent-q--append-to-conversation 'user message)
    (sly-agent-q--display-conversation-buffer)
    (message "Sending to Agent-Q with context...")
    (sly-eval-async
        `(agent-q:agent-q-send ,message :include-context t)
      (lambda (response)
        (if (stringp response)
            (progn
              (setq sly-agent-q--last-response response)
              (sly-agent-q--append-to-conversation 'assistant response)
              (message "Agent-Q response received"))
          ;; Debug: unexpected response type
          (message "ERROR: Expected string, got %s: %S" (type-of response) response)
          (sly-agent-q--append-to-conversation
           'assistant
           (format "ERROR: Unexpected response type: %s\nValue: %S"
                   (type-of response) response)))))))

;;;###autoload
(defun sly-agent-q-send-region-with-instruction (start end instruction)
  "Send region with an instruction. Adds region to context first."
  (interactive "r\nsInstruction: ")
  (when (sly-agent-q--check-loaded)
    (let* ((content (buffer-substring-no-properties start end))
           (filename (buffer-file-name))
           (start-line (line-number-at-pos start))
           (end-line (line-number-at-pos end))
           (metadata (list :filename filename
                          :start-line start-line
                          :end-line end-line)))
      (sly-agent-q--append-to-conversation 'user instruction)
      (sly-agent-q--display-conversation-buffer)
      (message "Adding context and sending to Agent-Q...")
      ;; Add to context, then send
      (sly-eval-async
          `(agent-q:agent-q-add-context ,content
                                        :type :code
                                        :metadata ',metadata)
        (lambda (result)
          (sly-eval-async
              `(agent-q:agent-q-send ,instruction :include-context t)
            (lambda (response)
              (setq sly-agent-q--last-response response)
              (sly-agent-q--append-to-conversation 'assistant response)
              (message "Agent-Q response received"))))))))

;;;###autoload
(defun sly-agent-q-new-conversation ()
  "Start a new conversation."
  (interactive)
  (when (sly-agent-q--check-loaded)
    (sly-eval-async
        '(agent-q:agent-q-new-conversation)
      (lambda (result)
        (let ((buffer (sly-agent-q--get-conversation-buffer))
              (inhibit-read-only t))
          (with-current-buffer buffer
            (erase-buffer)
            (insert (propertize "=== New Agent-Q Conversation ===\n"
                               'face 'bold)
                    (propertize "Press 'q' to hide, 'n' for new conversation\n\n"
                               'face 'font-lock-comment-face)))
          (message "Started new Agent-Q conversation"))))))

;;;###autoload
(defun sly-agent-q-show-conversation ()
  "Show/switch to the conversation buffer."
  (interactive)
  (switch-to-buffer (sly-agent-q--get-conversation-buffer)))

;;; Response Handling

;;;###autoload
(defun sly-agent-q-insert-last-response ()
  "Insert the last agent response at point."
  (interactive)
  (if sly-agent-q--last-response
      (insert sly-agent-q--last-response)
    (message "No Agent-Q response to insert")))

;;;###autoload
(defun sly-agent-q-copy-last-response ()
  "Copy the last agent response to kill ring."
  (interactive)
  (if sly-agent-q--last-response
      (progn
        (kill-new sly-agent-q--last-response)
        (message "Agent-Q response copied to kill ring"))
    (message "No Agent-Q response to copy")))

;;; Quick Actions

;;;###autoload
(defun sly-agent-q-document-defun ()
  "Ask agent to document the current function."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (sly-agent-q-send-region-with-instruction
       start (point)
       "Add a comprehensive docstring to this function."))))

;;;###autoload
(defun sly-agent-q-explain-region (start end)
  "Ask agent to explain the selected code."
  (interactive "r")
  (sly-agent-q-send-region-with-instruction
   start end
   "Explain what this code does."))

;;;###autoload
(defun sly-agent-q-fix-error ()
  "Send recent error to agent and ask for fix."
  (interactive)
  (when (sly-agent-q--check-loaded)
    (let ((error-msg (sly-eval '(cl:princ-to-string (swank:backtrace 0 10)))))
      (sly-eval-async
          `(agent-q:agent-q-add-context ,error-msg :type :error)
        (lambda (result)
          (sly-agent-q-send-with-context
           "I encountered this error. What's wrong and how do I fix it?"))))))

;;; Keymap

(defvar sly-agent-q-command-map
  (let ((map (make-sparse-keymap)))
    ;; Context
    (define-key map (kbd "c r") #'sly-agent-q-add-region-to-context)
    (define-key map (kbd "c b") #'sly-agent-q-add-buffer-to-context)
    (define-key map (kbd "c d") #'sly-agent-q-add-defun-to-context)
    (define-key map (kbd "c c") #'sly-agent-q-clear-context)
    (define-key map (kbd "c s") #'sly-agent-q-show-context)

    ;; Conversation
    (define-key map (kbd "s") #'sly-agent-q-send)
    (define-key map (kbd "S") #'sly-agent-q-send-with-context)
    (define-key map (kbd "r") #'sly-agent-q-send-region-with-instruction)
    (define-key map (kbd "n") #'sly-agent-q-new-conversation)
    (define-key map (kbd "v") #'sly-agent-q-show-conversation)

    ;; Response
    (define-key map (kbd "i") #'sly-agent-q-insert-last-response)
    (define-key map (kbd "w") #'sly-agent-q-copy-last-response)

    ;; Quick actions
    (define-key map (kbd "q d") #'sly-agent-q-document-defun)
    (define-key map (kbd "q e") #'sly-agent-q-explain-region)
    (define-key map (kbd "q f") #'sly-agent-q-fix-error)

    map)
  "Keymap for Agent-Q commands, used as a prefix map under C-c q.")

(defvar sly-agent-q-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c q") sly-agent-q-command-map)
    map)
  "Keymap for sly-agent-q-mode minor mode.")

;;; Minor Mode

;;;###autoload
(define-minor-mode sly-agent-q-mode
  "Minor mode for Agent-Q integration with SLY."
  :lighter " AgentQ"
  :keymap sly-agent-q-mode-map
  :group 'sly-agent-q)

;;; Auto-load agent-q on connection

(defun sly-agent-q--auto-load ()
  "Automatically load agent-q when SLY connects."
  (sly-eval-async
      '(cl:progn
        (cl:handler-case
            (cl:progn
              ;; Load agent-q (which depends on cl-llm-provider)
              (ql:quickload :agent-q :silent cl:t)
              ;; Now load configuration using runtime symbol resolution
              (cl:let ((pkg (cl:find-package :cl-llm-provider)))
                (cl:when pkg
                  (cl:let ((load-config-sym (cl:find-symbol "LOAD-CONFIGURATION-FROM-FILE" pkg)))
                    (cl:when (cl:and load-config-sym (cl:fboundp load-config-sym))
                      (cl:funcall load-config-sym)))))
              "Agent-Q loaded successfully")
          (cl:error (e)
            (cl:format cl:nil "Agent-Q auto-load failed: ~A" e))))
    (lambda (result)
      (message "Agent-Q: %s" result))))

;;;###autoload
(defun sly-agent-q-setup ()
  "Set up Agent-Q integration with SLY.
Add this to your init file:
  (with-eval-after-load 'sly
    (add-to-list 'load-path \"path/to/agent-q/contrib/sly-agent-q/\")
    (require 'sly-agent-q)
    (sly-agent-q-setup))"
  (add-hook 'sly-mode-hook #'sly-agent-q-mode)
  (add-hook 'sly-connected-hook #'sly-agent-q--auto-load))

(provide 'sly-agent-q)

;;; sly-agent-q.el ends here
