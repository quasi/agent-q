;;; sly-agent-q-chat.el --- Interactive chat interface for Agent-Q -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (sly "1.0"))
;; Keywords: lisp, ai, chat
;; URL: https://github.com/quasilabs/agent-q

;;; Commentary:

;; This file implements an interactive chat interface for Agent-Q.
;; It provides a buffer with separate output and input regions,
;; multi-line input support, message history navigation, and
;; streaming response handling.
;;
;; The buffer layout:
;;   - Output region (read-only): displays conversation messages
;;   - Separator line
;;   - Input region (editable): where user types messages
;;
;; Usage:
;;   M-x agent-q-chat   - Open the chat buffer
;;   RET                - Send message (if at end) or insert newline
;;   C-c C-c            - Send message
;;   M-p / M-n          - Navigate input history

;;; Code:

(require 'cl-lib)
(require 'sly)
(require 'sly-agent-q-context)
;; Note: sly-agent-q-sessions is required AFTER struct definitions below

;;; Customization

(defgroup agent-q-chat nil
  "Agent-Q interactive chat interface."
  :group 'sly
  :prefix "agent-q-")

(defcustom agent-q-input-history-max 100
  "Maximum number of inputs to remember in history."
  :type 'integer
  :group 'agent-q-chat)

(defcustom agent-q-chat-buffer-name "*Agent-Q Chat*"
  "Name of the Agent-Q chat buffer."
  :type 'string
  :group 'agent-q-chat)

(defcustom agent-q-render-markdown t
  "Whether to render markdown in assistant responses.
When non-nil, bold, italic, inline code, links, and fenced code
blocks will be rendered with appropriate faces and highlighting."
  :type 'boolean
  :group 'agent-q-chat)

(defcustom agent-q-show-message-separators t
  "Whether to show separators between messages.
When non-nil, a subtle dotted line appears between each message
for visual clarity."
  :type 'boolean
  :group 'agent-q-chat)

;;; Data Structures

(cl-defstruct (agent-q-message (:constructor agent-q-message--create))
  "A single message in the conversation."
  (id (format "%s-%04x"
              (format-time-string "%Y%m%d%H%M%S")
              (random 65536))
      :documentation "Unique identifier for this message.")
  (role nil :documentation "Message role: `user', `assistant', or `system'.")
  (content "" :documentation "Message text content.")
  (timestamp (current-time) :documentation "When the message was created.")
  (context-snapshot nil :documentation "Context items at time of message.")
  (metadata nil :documentation "Plist for additional data: (:tool-calls ... :model ...)."))

(cl-defstruct (agent-q-session (:constructor agent-q-session--create))
  "A conversation session containing messages."
  (id (format "session-%s" (format-time-string "%Y%m%d-%H%M%S"))
      :documentation "Unique session identifier.")
  (name nil :documentation "Optional user-friendly name.")
  (created-at (current-time) :documentation "When the session was created.")
  (updated-at (current-time) :documentation "When the session was last modified.")
  (messages nil :documentation "List of `agent-q-message' structs (most recent first).")
  (model nil :documentation "Model used (inherited from config if nil).")
  (metadata nil :documentation "Plist for extensibility."))

;; Aliases for serialization (sessions module uses make-* names)
(defalias 'make-agent-q-message #'agent-q-message--create)
(defalias 'make-agent-q-session #'agent-q-session--create)

;; Session setter - needed because sessions module loads before struct is defined
(defun agent-q-session-set-name (session name)
  "Set the NAME of SESSION."
  (setf (agent-q-session-name session) name))

(defun agent-q-session-set-model (session model)
  "Set the MODEL of SESSION."
  (setf (agent-q-session-model session) model))

(defun agent-q-session-update-metadata (session key value)
  "Update SESSION metadata with KEY set to VALUE."
  (let ((meta (agent-q-session-metadata session)))
    (setf (agent-q-session-metadata session)
          (plist-put meta key value))))

(defun agent-q-session-add-tokens (session input-tokens output-tokens)
  "Add INPUT-TOKENS and OUTPUT-TOKENS to SESSION's running totals."
  (let* ((meta (agent-q-session-metadata session))
         (current-in (or (plist-get meta :total-input-tokens) 0))
         (current-out (or (plist-get meta :total-output-tokens) 0)))
    (setf (agent-q-session-metadata session)
          (plist-put (plist-put meta
                                :total-input-tokens (+ current-in input-tokens))
                     :total-output-tokens (+ current-out output-tokens)))))

;; Now load sessions module - structs are defined above
(require 'sly-agent-q-sessions)

;;; Buffer-Local State

(defvar-local agent-q--current-session nil
  "The active session in this chat buffer.")

(defvar-local agent-q--output-end-marker nil
  "Marker at end of output region, before separator.")

(defvar-local agent-q--input-start-marker nil
  "Marker at start of input region, after separator.")

(defvar-local agent-q--input-history nil
  "List of past inputs (most recent first).")

(defvar-local agent-q--history-index -1
  "Current position in input history.
-1 means not currently browsing history.")

(defvar-local agent-q--input-draft nil
  "Saved input text when browsing history.")

(defvar-local agent-q--pending-response nil
  "Non-nil while waiting for agent response.")

(defvar-local agent-q--streaming-marker nil
  "Marker where streaming chunks are appended.")

;; Forward declaration - defined in sly-agent-q.el which loads after us
;; This allows the insert-last-response command to work with chat responses
(defvar sly-agent-q--last-response nil
  "The last response from the agent.")

;;; Faces

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

(defface agent-q-system-message-face
  '((t :foreground "#56b6c2" :slant italic))
  "Face for system informational messages."
  :group 'agent-q-chat)

(defface agent-q-code-block-face
  '((((background dark))
     :background "#2c323c" :extend t)
    (((background light))
     :background "#f0f0f0" :extend t))
  "Face for fenced code block background.
The `:extend t' property ensures the background color extends
to the edge of the window, not just the text width."
  :group 'agent-q-chat)

(defface agent-q-inline-code-face
  '((((background dark))
     :background "#3e4451" :foreground "#e5c07b")
    (((background light))
     :background "#e8e8e8" :foreground "#986801"))
  "Face for inline `code` snippets."
  :group 'agent-q-chat)

(defface agent-q-code-lang-face
  '((t :foreground "#5c6370" :slant italic :height 0.9))
  "Face for code block language identifier (e.g., lisp, python)."
  :group 'agent-q-chat)

(defface agent-q-message-separator-face
  '((t :foreground "#3e4451" :height 0.8))
  "Face for message separators between chat messages."
  :group 'agent-q-chat)

;;; Rendering Constants

(defconst agent-q--code-block-regexp
  "^```\\([a-zA-Z0-9+-]*\\)\n\\(\\(?:.*\n\\)*?\\)```$"
  "Regexp matching fenced code blocks in markdown.
Group 1: language identifier (may be empty)
Group 2: code content (without the fence markers)")

;;; Keymap

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
    (define-key map (kbd "q") #'agent-q--quit-or-self-insert)

    ;; Session management (Phase 3)
    (define-key map (kbd "C-c C-s") #'agent-q-switch-session)
    (define-key map (kbd "C-c C-n") #'agent-q-new-session)
    (define-key map (kbd "C-c C-f") #'agent-q-search-sessions)
    (define-key map (kbd "C-c C-r") #'agent-q-name-session)

    ;; Context (Phase 4 - define now, implement later)
    (define-key map (kbd "C-c @") #'agent-q-add-context)
    (define-key map (kbd "C-c C-x") #'agent-q-clear-context)

    map)
  "Keymap for `agent-q-chat-mode'.")

;;; Menu

(easy-menu-define agent-q-chat-menu agent-q-chat-mode-map
  "Menu for Agent-Q Chat mode."
  '("Agent-Q"
    ["Send Message" agent-q-send-input
     :help "Send current input to Agent-Q"]
    ["Cancel Request" agent-q-cancel-request
     :help "Cancel pending request"]
    "---"
    ("History"
     ["Previous Input" agent-q-history-previous
      :help "Navigate to previous input in history"]
     ["Next Input" agent-q-history-next
      :help "Navigate to next input in history"])
    ("Conversation"
     ["Clear Conversation" agent-q-clear-conversation
      :help "Clear the conversation and start fresh"]
     ["Scroll to Bottom" agent-q-scroll-to-bottom
      :help "Scroll to the bottom of the chat"])
    ("Sessions"
     ["New Session" agent-q-new-session
      :help "Start a new conversation session"]
     ["Switch Session" agent-q-switch-session
      :help "Switch to a different session"]
     ["Search Sessions" agent-q-search-sessions
      :help "Search past sessions by content"]
     ["Name Session" agent-q-name-session
      :help "Give current session a name"]
     ["Delete Session" agent-q-delete-session
      :help "Delete a saved session"])
    "---"
    ["Quit" quit-window
     :help "Close the chat window"]))

;;; Major Mode

;; Note: We derive from `text-mode' (not `special-mode') because:
;; - `special-mode' makes ALL keys undefined (no self-insert)
;; - We need typing to work in the input region
;; - Read-only behavior is enforced via text properties instead

(define-derived-mode agent-q-chat-mode text-mode "Agent-Q Chat"
  "Major mode for interactive chat with Agent-Q.

The buffer is divided into two regions:
- Output region (above separator): Read-only, displays conversation
- Input region (below separator): Editable, for typing messages

\\{agent-q-chat-mode-map}"
  :group 'agent-q-chat

  ;; Buffer settings
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local cursor-type 'bar)
  (setq-local scroll-conservatively 101)

  ;; Don't use global buffer-read-only; we use text properties instead
  (setq-local buffer-read-only nil)

  ;; Initialize buffer layout
  (agent-q--setup-buffer-layout)

  ;; Initialize state
  (setq-local agent-q--current-session (agent-q-session--create))
  (setq-local agent-q--input-history nil)
  (setq-local agent-q--history-index -1)
  (setq-local agent-q--pending-response nil)

  ;; Hooks
  (add-hook 'kill-buffer-hook #'agent-q--on-buffer-kill nil t)

  ;; Advice quit-window to save session (catches C-x w q, menu quit, etc.)
  (advice-add 'quit-window :before #'agent-q--before-quit-window)

  ;; Initialize session management (auto-save, mode line)
  (agent-q-sessions-initialize)

  ;; Set up context completion for @-mentions
  (agent-q-context-setup)

  ;; Display current provider/model configuration
  (agent-q--display-config-info)

  ;; Position menu before Help by adding to menu-bar-final-items
  ;; easy-menu-define converts "Agent-Q" to symbol 'agent-q
  (unless (memq 'agent-q menu-bar-final-items)
    (setq menu-bar-final-items
          (cons 'agent-q (delq 'agent-q menu-bar-final-items)))))

;;; Buffer Setup

(defun agent-q--setup-buffer-layout ()
  "Initialize buffer with output region, separator, and input region."
  (let ((inhibit-read-only t))
    (erase-buffer)

    ;; Welcome header
    (insert (propertize "Agent-Q Chat\n"
                        'face 'agent-q-header-face
                        'read-only t))
    (insert (propertize (make-string 50 ?─)
                        'face 'agent-q-separator-face
                        'read-only t))
    (insert (propertize "\n\n" 'read-only t))

    ;; Output end marker (where new messages get inserted)
    (setq agent-q--output-end-marker (point-marker))
    (set-marker-insertion-type agent-q--output-end-marker nil)

    ;; Separator block between output and input (3 lines)
    (insert (propertize (concat "\n"
                                (make-string 50 ?─) "\n"
                                "                       Input\n"
                                (make-string 50 ?─) "\n")
                        'face 'agent-q-separator-face
                        'read-only t
                        'agent-q-separator t))

    ;; Initial prompt (insert BEFORE setting marker)
    (insert (propertize "> "
                        'face 'agent-q-input-prompt-face
                        'read-only t
                        'rear-nonsticky t))

    ;; Input start marker - set AFTER prompt, with insertion-type nil
    ;; so it stays at the START of user input as they type
    (setq agent-q--input-start-marker (point-marker))
    (set-marker-insertion-type agent-q--input-start-marker nil)))

(defun agent-q--render-system-message (message)
  "Render a system MESSAGE in the output region."
  (save-excursion
    (goto-char agent-q--output-end-marker)
    (let ((inhibit-read-only t)
          (start (point)))
      (insert (propertize message 'face 'agent-q-system-message-face))
      (insert "\n\n")
      (put-text-property start (point) 'read-only t)
      (set-marker agent-q--output-end-marker (point)))))

(defun agent-q--display-config-info ()
  "Fetch and display the current provider/model configuration."
  (when (sly-connected-p)
    (sly-eval-async '(agent-q:get-config-info)
      (lambda (config)
        (when config
          (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
            (with-current-buffer buf
              (if-let ((error-msg (plist-get config :error)))
                  ;; Configuration failed - display error
                  (agent-q--render-system-message
                   (propertize (format "⚠ Configuration Error: %s" error-msg)
                               'face '(:foreground "orange")))
                ;; Configuration succeeded - display provider info
                (let ((provider-name (or (plist-get config :provider-name) "Unknown"))
                      (model (or (plist-get config :model) "unknown")))
                  (agent-q--render-system-message
                   (format "Using %s %s" provider-name model)))))))))))

;;; Input Handling

(defun agent-q--get-input ()
  "Return the current input text from the input region."
  (buffer-substring-no-properties
   agent-q--input-start-marker
   (point-max)))

(defun agent-q--clear-input ()
  "Clear the input region and reset prompt."
  (let ((inhibit-read-only t))
    ;; Delete everything from marker to end (user text only, prompt is before marker)
    (delete-region agent-q--input-start-marker (point-max))
    (goto-char agent-q--input-start-marker)))

(defun agent-q--replace-input (text)
  "Replace current input with TEXT."
  (let ((inhibit-read-only t))
    ;; Delete user text only (prompt is before marker)
    (delete-region agent-q--input-start-marker (point-max))
    (goto-char agent-q--input-start-marker)
    (insert text)
    (goto-char (point-max))))

(defun agent-q--in-input-region-p ()
  "Return non-nil if point is in the input region."
  (and agent-q--input-start-marker
       (>= (point) agent-q--input-start-marker)))

(defun agent-q-send-or-newline ()
  "Send input if at end of buffer with non-empty input, else insert newline.
This provides a natural typing experience where RET sends at the end
but allows multi-line input when typing in the middle."
  (interactive)
  (if (and (eobp)
           (not (string-empty-p (string-trim (agent-q--get-input)))))
      (agent-q-send-input)
    (insert "\n> ")))

(defun agent-q-send-input ()
  "Send current input to Agent-Q."
  (interactive)
  (when agent-q--pending-response
    (user-error "Already waiting for response"))

  (let ((input (string-trim (agent-q--get-input))))
    (when (string-empty-p input)
      (user-error "Input is empty"))

    ;; Create and store message
    (let ((msg (agent-q-message--create :role 'user :content input)))
      (push msg (agent-q-session-messages agent-q--current-session))
      (setf (agent-q-session-updated-at agent-q--current-session) (current-time)))

    ;; Add to history
    (agent-q--add-to-history input)

    ;; Render user message in output region
    (agent-q--render-user-message input)

    ;; Clear input
    (agent-q--clear-input)

    ;; Send to agent
    (agent-q--send-to-agent input)))

(defun agent-q-cancel-request ()
  "Cancel the current pending request."
  (interactive)
  (if agent-q--pending-response
      (progn
        (setq agent-q--pending-response nil)
        (setq agent-q--streaming-marker nil)
        (message "Request cancelled"))
    (message "No pending request")))

;;; History Navigation

(defun agent-q--add-to-history (input)
  "Add INPUT to the history ring."
  (when (and input (not (string-empty-p input)))
    ;; Don't add duplicates of the most recent entry
    (unless (equal input (car agent-q--input-history))
      (push input agent-q--input-history)
      (when (> (length agent-q--input-history) agent-q-input-history-max)
        (setq agent-q--input-history
              (seq-take agent-q--input-history agent-q-input-history-max))))))

(defun agent-q-history-previous ()
  "Navigate to previous input in history (M-p)."
  (interactive)
  (unless agent-q--input-history
    (user-error "No history"))

  ;; Save draft on first navigation
  (when (= agent-q--history-index -1)
    (setq agent-q--input-draft (agent-q--get-input)))

  ;; Navigate backwards in history
  (when (< agent-q--history-index (1- (length agent-q--input-history)))
    (cl-incf agent-q--history-index)
    (agent-q--replace-input (nth agent-q--history-index agent-q--input-history))))

(defun agent-q-history-next ()
  "Navigate to next input in history (M-n)."
  (interactive)
  (cond
   ;; More history to go through
   ((> agent-q--history-index 0)
    (cl-decf agent-q--history-index)
    (agent-q--replace-input (nth agent-q--history-index agent-q--input-history)))
   ;; At beginning of history, restore draft
   ((= agent-q--history-index 0)
    (setq agent-q--history-index -1)
    (agent-q--replace-input (or agent-q--input-draft "")))))

;;; Message Rendering

(defun agent-q--render-user-message (content)
  "Render user message with CONTENT in the output region."
  (save-excursion
    (goto-char agent-q--output-end-marker)
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
      (set-marker agent-q--output-end-marker (point)))))

(defun agent-q--begin-assistant-response ()
  "Prepare buffer for streaming assistant response.
Inserts the [AGENT-Q] header and sets up the streaming marker."
  (setq agent-q--pending-response t)
  (save-excursion
    (goto-char agent-q--output-end-marker)
    (let ((inhibit-read-only t))
      (insert (propertize "[AGENT-Q]" 'face 'agent-q-assistant-header-face))
      (insert " ")
      (insert (propertize (format-time-string "%H:%M:%S")
                          'face 'agent-q-timestamp-face))
      (insert "\n")
      ;; Set streaming marker for chunk insertion
      (setq agent-q--streaming-marker (point-marker))
      (set-marker-insertion-type agent-q--streaming-marker t)
      (set-marker agent-q--output-end-marker (point)))))

(defun agent-q--append-response-chunk (chunk)
  "Append CHUNK to the current streaming response."
  (when agent-q--streaming-marker
    (save-excursion
      (goto-char agent-q--streaming-marker)
      (let ((inhibit-read-only t))
        (insert chunk)))
    ;; Force immediate display update for streaming
    (redisplay t)))

(defun agent-q--finalize-response (full-content)
  "Finalize the assistant response with FULL-CONTENT.
Stores the message in the session and cleans up streaming state.

If FULL-CONTENT is non-empty and nothing was streamed, the content
is inserted now (handles synchronous responses from the agent).

After content is finalized, applies markdown rendering (bold, italic,
code blocks with syntax highlighting, clickable links) and optionally
inserts a message separator.

Also sets `sly-agent-q--last-response' so users can insert the
response into other buffers with `sly-agent-q-insert-last-response'."
  ;; Store in session
  (let ((msg (agent-q-message--create :role 'assistant :content full-content)))
    (push msg (agent-q-session-messages agent-q--current-session))
    (setf (agent-q-session-updated-at agent-q--current-session) (current-time)))

  ;; Store as last response for insert/copy commands
  (when (and full-content (not (string-empty-p full-content)))
    (setq sly-agent-q--last-response full-content))

  ;; Capture message start position before any modifications
  (let ((msg-start (and agent-q--streaming-marker
                        (marker-position agent-q--streaming-marker))))

    ;; Insert content if it wasn't streamed
    ;; (streaming-marker points to where content should appear, has insertion-type t)
    (when (and agent-q--streaming-marker
               full-content
               (not (string-empty-p full-content)))
      (save-excursion
        (goto-char agent-q--streaming-marker)
        (let ((inhibit-read-only t))
          (insert full-content)
          ;; streaming-marker advanced past content (insertion-type t)
          ;; Update output-end-marker to track it
          (set-marker agent-q--output-end-marker (point)))))

    ;; Finalize buffer display (add trailing newlines after content)
    (let ((msg-end nil))
      (save-excursion
        (goto-char agent-q--output-end-marker)
        (let ((inhibit-read-only t))
          (insert "\n\n")
          (setq msg-end (point))
          (set-marker agent-q--output-end-marker (point))))

      ;; Apply markdown rendering to the message content
      (when msg-start
        (agent-q--render-markdown msg-start msg-end))

      ;; Insert message separator
      (save-excursion
        (goto-char agent-q--output-end-marker)
        (agent-q--insert-message-separator)
        (set-marker agent-q--output-end-marker (point)))))

  ;; Clean up state
  (setq agent-q--pending-response nil)
  (setq agent-q--streaming-marker nil)

  ;; Force display update after async response
  (redisplay t))

;;; Markdown Rendering

(defun agent-q--lang-to-mode (lang)
  "Map LANG string to an Emacs major mode function.
Returns nil if no matching mode is found or if the mode isn't available."
  (let ((mode (cond
               ((member lang '("lisp" "common-lisp" "cl")) 'lisp-mode)
               ((member lang '("elisp" "emacs-lisp")) 'emacs-lisp-mode)
               ((string= lang "python") 'python-mode)
               ((member lang '("js" "javascript")) 'js-mode)
               ((member lang '("ts" "typescript")) 'typescript-mode)
               ((string= lang "rust") 'rust-mode)
               ((string= lang "go") 'go-mode)
               ((member lang '("sh" "bash" "shell")) 'sh-mode)
               ((string= lang "json") 'json-mode)
               ((string= lang "yaml") 'yaml-mode)
               ((string= lang "sql") 'sql-mode)
               ((member lang '("md" "markdown")) 'markdown-mode)
               ((string= lang "html") 'html-mode)
               ((string= lang "css") 'css-mode)
               ((string= lang "c") 'c-mode)
               ((member lang '("c++" "cpp")) 'c++-mode)
               ((string= lang "java") 'java-mode)
               ((string= lang "ruby") 'ruby-mode)
               (t nil))))
    ;; Only return the mode if it's actually available
    (when (and mode (fboundp mode))
      mode)))

(defun agent-q--fontify-code (code mode)
  "Return CODE string fontified using MODE.
Uses a temporary buffer to apply the mode's font-lock rules."
  (with-temp-buffer
    (insert code)
    (delay-mode-hooks (funcall mode))
    (font-lock-ensure)
    (buffer-string)))

(defun agent-q--highlight-code-region (start end lang)
  "Apply syntax highlighting to code from START to END for LANG.
The region is replaced with fontified text while preserving position."
  (let ((mode (agent-q--lang-to-mode lang)))
    (when mode
      (let ((highlighted (agent-q--fontify-code
                          (buffer-substring-no-properties start end)
                          mode)))
        (when highlighted
          (delete-region start end)
          (goto-char start)
          (insert highlighted))))))

(defun agent-q--render-code-blocks (start end)
  "Render fenced code blocks in region from START to END.
Applies background face and syntax highlighting based on language."
  (save-excursion
    (goto-char start)
    (while (re-search-forward agent-q--code-block-regexp end t)
      (let* ((lang (match-string 1))
             (block-start (match-beginning 0))
             (block-end (match-end 0))
             (code-start (match-beginning 2))
             (code-end (match-end 2)))

        ;; Apply background to entire block
        (put-text-property block-start block-end
                           'face 'agent-q-code-block-face)

        ;; Apply syntax highlighting to code content
        (when (and lang (not (string-empty-p lang)))
          (agent-q--highlight-code-region code-start code-end lang))

        ;; Style the language identifier on the first line
        (save-excursion
          (goto-char block-start)
          (when (looking-at "```\\([a-zA-Z0-9+-]+\\)")
            (put-text-property (match-beginning 1) (match-end 1)
                               'face 'agent-q-code-lang-face)))))))

(defun agent-q--render-markdown (start end)
  "Apply markdown rendering to region from START to END.
Processes bold, italic, inline code, links, and fenced code blocks."
  (when agent-q-render-markdown
    (save-excursion
      (let ((inhibit-read-only t))
        ;; Bold: **text** or __text__
        (goto-char start)
        (while (re-search-forward "\\*\\*\\(.+?\\)\\*\\*\\|__\\(.+?\\)__" end t)
          (let ((text (or (match-string 1) (match-string 2)))
                (beg (match-beginning 0))
                (fin (match-end 0)))
            (delete-region beg fin)
            (goto-char beg)
            (insert (propertize text 'face 'bold))
            ;; Adjust end marker for deleted characters
            (setq end (+ end (- (length text) (- fin beg))))))

        ;; Italic: *text* or _text_ (not inside words/code)
        (goto-char start)
        (while (re-search-forward "\\(?:^\\|[^*_\\\\]\\)\\([*_]\\)\\([^*_\n]+?\\)\\1" end t)
          (let ((text (match-string 2))
                (beg (match-beginning 1))
                (fin (match-end 0)))
            (delete-region beg fin)
            (goto-char beg)
            (insert (propertize text 'face 'italic))
            (setq end (+ end (- (length text) (- fin beg))))))

        ;; Inline code: `code`
        (goto-char start)
        (while (re-search-forward "`\\([^`\n]+\\)`" end t)
          (let ((code (match-string 1)))
            (replace-match
             (propertize code 'face 'agent-q-inline-code-face)
             t t)))

        ;; Links: [text](url)
        (goto-char start)
        (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" end t)
          (let ((text (match-string 1))
                (url (match-string 2))
                (beg (match-beginning 0))
                (fin (match-end 0)))
            (delete-region beg fin)
            (goto-char beg)
            (insert-button text
                           'action (lambda (_) (browse-url url))
                           'face 'link
                           'help-echo url
                           'follow-link t)))

        ;; Render code blocks last (they span multiple lines)
        (agent-q--render-code-blocks start end)))))

(defun agent-q--insert-message-separator ()
  "Insert a subtle separator after a message.
Only inserts if `agent-q-show-message-separators' is non-nil."
  (when agent-q-show-message-separators
    (let ((inhibit-read-only t))
      (insert (propertize (concat "\n" (make-string 60 ?·) "\n")
                          'face 'agent-q-message-separator-face
                          'read-only t)))))

(defun agent-q--render-assistant-message (content)
  "Render assistant message with CONTENT for session replay.
Unlike streaming, this renders the complete message at once."
  (save-excursion
    (goto-char agent-q--output-end-marker)
    (let ((inhibit-read-only t)
          (start (point)))
      (insert (propertize "[AGENT-Q]" 'face 'agent-q-assistant-header-face))
      (insert " ")
      (insert (propertize (format-time-string "%H:%M:%S")
                          'face 'agent-q-timestamp-face))
      (insert "\n")
      (let ((content-start (point)))
        (insert content)
        (insert "\n\n")
        ;; Apply markdown rendering
        (agent-q--render-markdown content-start (point)))
      ;; Insert separator
      (agent-q--insert-message-separator)
      (put-text-property start (point) 'read-only t)
      (set-marker agent-q--output-end-marker (point)))))

;;; Streaming Callbacks (called from CL via eval-in-emacs)

(defun agent-q--start-streaming ()
  "Called when streaming response begins.
Prepares the chat buffer for incremental text.
This function is called from Common Lisp via `slynk:eval-in-emacs'."
  (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
    (with-current-buffer buf
      ;; Initialize streaming state
      (setq agent-q--pending-response t)
      ;; Insert assistant header at output end
      (save-excursion
        (goto-char agent-q--output-end-marker)
        (let ((inhibit-read-only t))
          (insert (propertize "[AGENT-Q]" 'face 'agent-q-assistant-header-face))
          (insert " ")
          (insert (propertize (format-time-string "%H:%M:%S")
                              'face 'agent-q-timestamp-face))
          (insert "\n")
          ;; Set streaming marker for chunk insertion
          (setq agent-q--streaming-marker (point-marker))
          (set-marker-insertion-type agent-q--streaming-marker t)
          (set-marker agent-q--output-end-marker (point))))
      ;; Force immediate display update
      (redisplay t))))

(defun agent-q--update-token-usage (prompt-tokens completion-tokens)
  "Called with token counts from streaming response.
PROMPT-TOKENS is the number of input tokens.
COMPLETION-TOKENS is the number of output tokens.
This function is called from Common Lisp via `slynk:eval-in-emacs'."
  (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (when agent-q--current-session
        ;; Accumulate token usage in session
        (when (and prompt-tokens completion-tokens)
          (agent-q-session-add-tokens agent-q--current-session
                                      prompt-tokens completion-tokens))))))

(defun agent-q--streaming-error (error-message)
  "Called when streaming encounters an error.
ERROR-MESSAGE is the error description string.
This function is called from Common Lisp via `slynk:eval-in-emacs'."
  (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (or agent-q--streaming-marker agent-q--output-end-marker))
        (let ((inhibit-read-only t))
          (insert "\n")
          (insert (propertize (format "[Error: %s]" error-message)
                              'face 'error))
          (insert "\n\n")
          (set-marker agent-q--output-end-marker (point))))
      ;; Clean up streaming state
      (setq agent-q--pending-response nil)
      (when agent-q--streaming-marker
        (set-marker agent-q--streaming-marker nil)
        (setq agent-q--streaming-marker nil))
      ;; Force display update
      (redisplay t))))

;;; SLY Integration

(defun agent-q--send-to-agent (content)
  "Send CONTENT to the Lisp agent via SLY.
Appends formatted context if any context items are present in
`agent-q-context-items'."
  (let ((full-content (concat content (agent-q--format-context-for-llm))))
    (agent-q--begin-assistant-response)
    ;; Call existing RPC endpoint
    (sly-eval-async
        `(agent-q:agent-q-send ,full-content :include-context t)
      (lambda (result)
        (agent-q--finalize-response result)))))

(defun agent-q-chat-append-chunk (chunk)
  "Append streaming CHUNK from agent.
This function is called via `slynk:eval-in-emacs' from the Lisp side."
  (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (agent-q--append-response-chunk chunk))))

(defun agent-q-chat-debug-message (msg)
  "Display debug MSG from agent.
Called via `slynk:eval-in-emacs' for tool execution feedback."
  (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (agent-q--append-response-chunk
       (propertize (format "[%s]\n" msg) 'face 'agent-q-debug-face)))))

(defun agent-q-chat-set-session-info (model provider input-tokens output-tokens)
  "Update current session with MODEL, PROVIDER, and token counts.
Called from Lisp side via `slynk:eval-in-emacs' after receiving LLM response."
  (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (when agent-q--current-session
        ;; Set model if not already set
        (unless (agent-q-session-model agent-q--current-session)
          (agent-q-session-set-model agent-q--current-session model))
        ;; Store provider in metadata
        (when provider
          (agent-q-session-update-metadata agent-q--current-session :provider provider))
        ;; Accumulate token usage
        (when (and input-tokens output-tokens)
          (agent-q-session-add-tokens agent-q--current-session
                                      input-tokens output-tokens))))))

(defun agent-q--insert-tool-message (msg)
  "Insert tool MSG into the output region.
Works whether or not streaming is in progress."
  (let ((text (propertize msg 'face 'agent-q-debug-face)))
    (if agent-q--streaming-marker
        ;; Streaming in progress - append to current response
        (agent-q--append-response-chunk text)
      ;; No streaming - insert directly at output end
      (save-excursion
        (goto-char agent-q--output-end-marker)
        (let ((inhibit-read-only t))
          (insert text)
          (set-marker agent-q--output-end-marker (point))))
      ;; Force display update
      (redisplay t))))

(defun agent-q-chat-tool-start (tool-name &optional details)
  "Notify chat that TOOL-NAME is starting.
Optional DETAILS provides additional context.
Called from diff and other tool modules."
  (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (let ((msg (if details
                     (format "[TOOL: %s] %s\n" tool-name details)
                   (format "[TOOL: %s] executing...\n" tool-name))))
        (agent-q--insert-tool-message msg)))))

(defun agent-q-chat-tool-result (tool-name result)
  "Notify chat that TOOL-NAME completed with RESULT.
Called from diff and other tool modules."
  (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (agent-q--insert-tool-message
       (format "[TOOL: %s] → %s\n" tool-name result)))))

;;; Buffer Operations

(defun agent-q-clear-conversation ()
  "Clear the conversation and start fresh."
  (interactive)
  (when (yes-or-no-p "Clear conversation? ")
    (setq agent-q--current-session (agent-q-session--create))
    (agent-q--setup-buffer-layout)
    (message "Conversation cleared")))

(defun agent-q-scroll-to-bottom ()
  "Scroll to the bottom of the chat buffer."
  (interactive)
  (goto-char (point-max)))

(defun agent-q--before-quit-window (&optional kill window)
  "Advice for `quit-window' to save session when quitting chat buffer.
KILL and WINDOW are the arguments to `quit-window'."
  (when (and (eq major-mode 'agent-q-chat-mode)
             (boundp 'agent-q--current-session)
             agent-q--current-session)
    (agent-q--save-current-session)))

(defun agent-q--quit-or-self-insert ()
  "Quit window if in output region, otherwise insert `q'."
  (interactive)
  (if (agent-q--in-input-region-p)
      (self-insert-command 1)
    ;; quit-window will trigger our advice which saves the session
    (quit-window)))

(defun agent-q--on-buffer-kill ()
  "Clean up when the chat buffer is killed."
  ;; Save session before cleanup
  (agent-q--on-chat-buffer-kill)
  ;; Cancel any pending requests
  (when agent-q--pending-response
    (setq agent-q--pending-response nil))
  ;; Clean up markers
  (when agent-q--output-end-marker
    (set-marker agent-q--output-end-marker nil))
  (when agent-q--input-start-marker
    (set-marker agent-q--input-start-marker nil))
  (when agent-q--streaming-marker
    (set-marker agent-q--streaming-marker nil)))

;;; Entry Point

;;;###autoload
(defun agent-q-chat ()
  "Open or switch to the Agent-Q chat buffer."
  (interactive)
  (let ((buf (get-buffer-create agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'agent-q-chat-mode)
        (agent-q-chat-mode)))
    (pop-to-buffer buf)
    ;; Position cursor in input region
    (goto-char agent-q--input-start-marker)))

(defun agent-q-chat-send-message (message &optional include-context)
  "Send MESSAGE through the chat interface programmatically.
If INCLUDE-CONTEXT is non-nil, include accumulated context.
This is the entry point for external commands that want to use the chat."
  (let ((buf (get-buffer-create agent-q-chat-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'agent-q-chat-mode)
        (agent-q-chat-mode)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      ;; Check if already waiting
      (when agent-q--pending-response
        (user-error "Already waiting for response"))
      ;; Create and store message
      (let ((msg (agent-q-message--create :role 'user :content message)))
        (push msg (agent-q-session-messages agent-q--current-session))
        (setf (agent-q-session-updated-at agent-q--current-session) (current-time)))
      ;; Add to history
      (agent-q--add-to-history message)
      ;; Render user message
      (agent-q--render-user-message message)
      ;; Clear any existing input
      (agent-q--clear-input)
      ;; Send to agent
      (agent-q--begin-assistant-response)
      (sly-eval-async
          `(agent-q:agent-q-send ,message :include-context ,(if include-context t nil))
        (lambda (result)
          (when-let ((buf (get-buffer agent-q-chat-buffer-name)))
            (with-current-buffer buf
              (agent-q--finalize-response result))))))))

(provide 'sly-agent-q-chat)
;;; sly-agent-q-chat.el ends here
