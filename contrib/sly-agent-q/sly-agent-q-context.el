;;; sly-agent-q-context.el --- Context management for Agent-Q chat -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: lisp, ai, context
;; URL: https://github.com/quasilabs/agent-q

;; ABOUTME: Context management module for Agent-Q chat interface.
;; Provides @-mention completion, context pills, and context panel.

;;; Commentary:

;; Context management for Agent-Q chat interface.
;; Provides @-mention completion, context pills, and a context panel.
;;
;; Context items can be:
;; - Files from the project
;; - Symbols (functions, variables) from Lisp buffers
;; - Buffer contents
;; - Selected regions
;; - URLs

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'imenu)

;;; Customization Group

(defgroup agent-q-context nil
  "Context management for Agent-Q chat."
  :group 'agent-q
  :prefix "agent-q-context-")

;;; Faces

(defface agent-q-context-pill-face
  '((t :foreground "#61afef"
       :background "#3e4451"
       :box (:line-width -1 :color "#61afef")
       :weight bold))
  "Face for context pills in input.
Used to highlight [@name] pills that represent attached context items."
  :group 'agent-q-context)

(defface agent-q-context-button-face
  '((t :foreground "#98c379" :weight bold))
  "Face for clickable buttons in context panel."
  :group 'agent-q-context)

;;; Constants

(defconst agent-q-context-max-size 50000
  "Maximum size in bytes for context content.
Content larger than this will be truncated to prevent LLM context
window overflow.")

;;; Buffer-Local State

(defvar-local agent-q-context-items nil
  "List of context items attached to current message.
Each item is an `agent-q-context-item' struct.")

;;; Context Panel

(defvar agent-q-context-panel-buffer "*Agent-Q Context*"
  "Buffer name for context panel sidebar.")

;;; Data Structures

(cl-defstruct (agent-q-context-item (:constructor make-agent-q-context-item))
  "A context item attached to a message.
Context items represent pieces of code, files, or other content
that provide context to the LLM during conversation."
  (type nil :documentation "Type: :file | :symbol | :buffer | :region | :url")
  (display-name nil :documentation "Short name shown in pill")
  (data nil :documentation "Type-specific data (plist)")
  (content nil :documentation "Actual content to send to LLM"))

;;; Candidate Gathering

(defun agent-q--file-candidates (prefix)
  "Return file candidates matching PREFIX.
Searches project files if `project-current' returns a project.
Each candidate has `agent-q-context-type' and `agent-q-context-data'
text properties for use by the completion framework."
  (when-let ((project (project-current)))
    (let ((files (project-files project)))
      (mapcar (lambda (file)
                (propertize (file-name-nondirectory file)
                            'agent-q-context-type :file
                            'agent-q-context-data (list :path file)))
              (seq-filter (lambda (f)
                            (string-prefix-p prefix (file-name-nondirectory f) t))
                          files)))))

(defun agent-q--flatten-imenu (index prefix)
  "Flatten imenu INDEX, filtering by PREFIX.
Returns list of propertized candidate strings with
`agent-q-context-type' set to :symbol and `agent-q-context-data'
containing :name and :position."
  (let ((result nil))
    (dolist (item index)
      (cond
       ;; Nested submenu - recurse
       ((imenu--subalist-p item)
        (setq result (nconc result (agent-q--flatten-imenu (cdr item) prefix))))
       ;; Regular item - check prefix match
       ((and (consp item)
             (stringp (car item))
             (string-prefix-p prefix (car item) t))
        (push (propertize (car item)
                          'agent-q-context-type :symbol
                          'agent-q-context-data (list :name (car item)
                                                      :position (cdr item)))
              result))))
    result))

(defun agent-q--symbol-candidates (prefix)
  "Return symbol candidates matching PREFIX.
Gathers symbols from imenu of all open Lisp buffers (lisp-mode
and emacs-lisp-mode). Results are limited to 50 to prevent
overwhelming completion interfaces."
  (let ((symbols nil))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'lisp-mode 'emacs-lisp-mode)
          (ignore-errors
            (let ((index (imenu--make-index-alist t)))
              (setq symbols (nconc symbols
                                   (agent-q--flatten-imenu index prefix))))))))
    (seq-take symbols 50)))

(defun agent-q--buffer-candidates (prefix)
  "Return buffer candidates matching PREFIX.
Excludes internal buffers (those starting with space).
Each candidate has `agent-q-context-type' set to :buffer and
`agent-q-context-data' containing :buffer-name for later content fetching."
  (mapcar (lambda (buf)
            (propertize (buffer-name buf)
                        'agent-q-context-type :buffer
                        'agent-q-context-data (list :buffer-name (buffer-name buf))))
          (seq-filter (lambda (buf)
                        (let ((name (buffer-name buf)))
                          (and (not (string-prefix-p " " name))
                               (string-prefix-p prefix name t))))
                      (buffer-list))))

;;; Completion at Point

(defun agent-q--context-candidates (prefix)
  "Return completion candidates matching PREFIX.
Combines files, symbols, and buffers. Strips leading @ from PREFIX
if present."
  (let ((prefix-no-at (if (string-prefix-p "@" prefix)
                          (substring prefix 1)
                        prefix)))
    (append
     (agent-q--file-candidates prefix-no-at)
     (agent-q--symbol-candidates prefix-no-at)
     (agent-q--buffer-candidates prefix-no-at))))

(defun agent-q--context-annotation (candidate)
  "Return annotation for CANDIDATE showing its type.
Returns a string like \" [file]\" for display in completion UI."
  (let ((type (get-text-property 0 'agent-q-context-type candidate)))
    (pcase type
      (:file " [file]")
      (:symbol " [symbol]")
      (:buffer " [buffer]")
      (:region " [region]")
      (:url " [url]")
      (_ ""))))

(defun agent-q-context-complete-at-point ()
  "Completion-at-point function for @-mentions.
Returns completion data when point is after an @-mention, nil otherwise.
Works with any completion framework (vertico, ivy, helm, default)."
  (when-let ((bounds (agent-q--context-mention-bounds)))
    (list (car bounds)
          (cdr bounds)
          (completion-table-dynamic #'agent-q--context-candidates)
          :exclusive 'no
          :annotation-function #'agent-q--context-annotation)))

;;; @-Mention Detection

(defun agent-q--context-mention-bounds ()
  "Return bounds of @-mention at point, or nil.
Returns (START . END) where START is position of @ and END is position
after the mention text. Only returns bounds if point is within or
immediately after the matched @-mention. Only detects @-mentions on
the current line."
  (save-excursion
    (let ((original-point (point)))
      (when (re-search-backward "@\\([^ \t\n]*\\)" (line-beginning-position) t)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0)))
          ;; Only return bounds if point is within or immediately after the match
          (when (<= original-point match-end)
            (cons match-start match-end)))))))

;;; Content Fetching

(defun agent-q--fetch-context-content (type data)
  "Fetch actual content for context item.
TYPE is the context type (:file, :buffer, :symbol, :region).
DATA is the type-specific data plist.

Returns the content string limited to `agent-q-context-max-size',
or nil if the source cannot be read."
  (pcase type
    (:file
     (let ((path (plist-get data :path)))
       (when (and path (file-readable-p path))
         (with-temp-buffer
           (insert-file-contents path nil 0 agent-q-context-max-size)
           (buffer-string)))))
    (:symbol
     (when-let* ((pos (plist-get data :position))
                 (buf (and (markerp pos) (marker-buffer pos))))
       (with-current-buffer buf
         (save-excursion
           (goto-char pos)
           (let ((bounds (bounds-of-thing-at-point 'defun)))
             (when bounds
               (buffer-substring-no-properties
                (car bounds)
                (min (cdr bounds)
                     (+ (car bounds) agent-q-context-max-size)))))))))
    (:buffer
     (when-let ((buf (get-buffer (plist-get data :buffer-name))))
       (with-current-buffer buf
         (buffer-substring-no-properties
          (point-min)
          (min (point-max)
               (+ (point-min) agent-q-context-max-size))))))
    (:region
     (let ((buf (get-buffer (plist-get data :buffer)))
           (start (plist-get data :start))
           (end (plist-get data :end)))
       (when (and buf start end)
         (with-current-buffer buf
           (buffer-substring-no-properties
            start
            (min end (+ start agent-q-context-max-size)))))))
    (_ nil)))

;;; Context Pill Rendering

(defvar agent-q-context-pill-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'agent-q-context-pill-click)
    (define-key map (kbd "RET") #'agent-q-context-pill-visit)
    (define-key map (kbd "DEL") #'agent-q-context-pill-remove)
    (define-key map (kbd "?") #'agent-q-context-pill-describe)
    map)
  "Keymap for context pills.
Provides bindings for interacting with context items displayed as pills.")

(defun agent-q--context-pill-tooltip (item)
  "Generate tooltip string for context ITEM.
Returns a human-readable description including type and key data."
  (let ((type (agent-q-context-item-type item))
        (data (agent-q-context-item-data item)))
    (pcase type
      (:file (format "File: %s" (or (plist-get data :path) "unknown")))
      (:symbol (format "Symbol: %s" (or (plist-get data :name) "unknown")))
      (:buffer (format "Buffer: %s" (or (plist-get data :buffer-name) "unknown")))
      (:region (format "Region in %s" (or (plist-get data :buffer) "unknown")))
      (:url (format "URL: %s" (or (plist-get data :url) "unknown")))
      (_ "Context item"))))

(defun agent-q--insert-context-pill (item)
  "Insert visual pill for context ITEM at point.
The pill is formatted as [@display-name] and has text properties for:
- face: `agent-q-context-pill-face'
- keymap: `agent-q-context-pill-map'
- help-echo: tooltip from `agent-q--context-pill-tooltip'
- agent-q-context-item: the ITEM struct for later access"
  (let ((display-name (agent-q-context-item-display-name item)))
    (insert (propertize (format "[@%s]" display-name)
                        'face 'agent-q-context-pill-face
                        'agent-q-context-item item
                        'read-only t
                        'rear-nonsticky t
                        'mouse-face 'highlight
                        'keymap agent-q-context-pill-map
                        'help-echo (agent-q--context-pill-tooltip item)))
    (insert " ")))

;; Placeholder pill commands (to be fully implemented in later tasks)

(defun agent-q-context-pill-click (event)
  "Handle click on context pill.
EVENT is the mouse event."
  (interactive "e")
  (let* ((pos (posn-point (event-end event)))
         (item (get-text-property pos 'agent-q-context-item)))
    (if item
        (message "Context: %s" (agent-q--context-pill-tooltip item))
      (message "No context item at click position"))))

(defun agent-q-context-pill-visit ()
  "Visit source of context pill at point."
  (interactive)
  (if-let ((item (get-text-property (point) 'agent-q-context-item)))
      (let ((type (agent-q-context-item-type item))
            (data (agent-q-context-item-data item)))
        (pcase type
          (:file (when-let ((path (plist-get data :path)))
                   (find-file path)))
          (:buffer (when-let ((buf (plist-get data :buffer-name)))
                     (switch-to-buffer buf)))
          (:symbol (when-let ((pos (plist-get data :position)))
                     (when (markerp pos)
                       (switch-to-buffer (marker-buffer pos))
                       (goto-char pos))))
          (_ (message "Cannot visit this context type"))))
    (message "No context item at point")))

(defun agent-q-context-pill-remove ()
  "Remove context pill at point."
  (interactive)
  (message "Remove not yet implemented"))

(defun agent-q-context-pill-describe ()
  "Describe context pill at point."
  (interactive)
  (if-let ((item (get-text-property (point) 'agent-q-context-item)))
      (message "%s" (agent-q--context-pill-tooltip item))
    (message "No context item at point")))

;;; Context Panel Mode

(define-derived-mode agent-q-context-panel-mode special-mode "Context"
  "Major mode for Agent-Q context panel.
Displays attached context items with actions."
  :group 'agent-q-context
  (setq-local revert-buffer-function #'agent-q--refresh-context-panel))

(defun agent-q--refresh-context-panel (&rest _args)
  "Refresh context panel contents.
ARGS are ignored; this function can be used as `revert-buffer-function'."
  (let ((inhibit-read-only t)
        (items (when-let ((chat-buf (get-buffer "*Agent-Q Chat*")))
                 (buffer-local-value 'agent-q-context-items chat-buf))))
    (erase-buffer)
    (insert (propertize "Context Items\n" 'face 'bold))
    (insert (propertize (make-string 30 ?─) 'face 'shadow))
    (insert "\n\n")

    (if items
        (dolist (item items)
          (agent-q--insert-context-panel-item item))
      (insert (propertize "No context attached.\n\n" 'face 'shadow)
              "Use @filename to add files,\n"
              "or C-c @ to add interactively."))

    (insert "\n\n")
    (insert-button "[Clear All]"
                   'action (lambda (_btn)
                             (agent-q--clear-all-context)
                             (agent-q--refresh-context-panel))
                   'face 'agent-q-context-button-face
                   'help-echo "Remove all context items")))

(defun agent-q--insert-context-panel-item (item)
  "Insert ITEM in context panel with visit and remove buttons."
  (let ((type (agent-q-context-item-type item))
        (name (agent-q-context-item-display-name item)))
    ;; Type label
    (insert (propertize (format "[%s] "
                                (pcase type
                                  (:file "FILE")
                                  (:symbol "SYM")
                                  (:buffer "BUF")
                                  (:region "REG")
                                  (:url "URL")
                                  (_ "?")))
                        'face 'font-lock-type-face))
    ;; Clickable name
    (insert-button name
                   'action (lambda (_btn) (agent-q--visit-context-item item))
                   'face 'link
                   'help-echo "Click to visit source")
    (insert "  ")
    ;; Remove button
    (insert-button "×"
                   'action (lambda (_btn)
                             (agent-q--remove-context-item item)
                             (agent-q--refresh-context-panel))
                   'face 'error
                   'help-echo "Remove from context")
    (insert "\n")))

(defun agent-q--visit-context-item (item)
  "Visit the source of context ITEM."
  (let ((type (agent-q-context-item-type item))
        (data (agent-q-context-item-data item)))
    (pcase type
      (:file (when-let ((path (plist-get data :path)))
               (find-file path)))
      (:buffer (when-let ((buf (plist-get data :buffer-name)))
                 (switch-to-buffer buf)))
      (:symbol (when-let ((pos (plist-get data :position)))
                 (when (markerp pos)
                   (switch-to-buffer (marker-buffer pos))
                   (goto-char pos))))
      (:region (when-let ((buf (get-buffer (plist-get data :buffer))))
                 (switch-to-buffer buf)
                 (when-let ((start (plist-get data :start)))
                   (goto-char start))))
      (_ (message "Cannot visit this context type")))))

(defun agent-q--remove-context-item (item)
  "Remove ITEM from context list in the chat buffer."
  (when-let ((chat-buf (get-buffer "*Agent-Q Chat*")))
    (with-current-buffer chat-buf
      (setq agent-q-context-items
            (delete item agent-q-context-items)))))

(defun agent-q--clear-all-context ()
  "Clear all context items from the chat buffer."
  (when-let ((chat-buf (get-buffer "*Agent-Q Chat*")))
    (with-current-buffer chat-buf
      (setq agent-q-context-items nil))))

(defun agent-q-show-context-panel ()
  "Show context panel in side window."
  (interactive)
  (let ((buf (get-buffer-create agent-q-context-panel-buffer)))
    (with-current-buffer buf
      (agent-q-context-panel-mode)
      (agent-q--refresh-context-panel))
    (display-buffer buf
                    '(display-buffer-in-side-window
                      (side . right)
                      (window-width . 35)
                      (preserve-size . (t . nil))))))

(defun agent-q-toggle-context-panel ()
  "Toggle context panel sidebar visibility."
  (interactive)
  (let ((win (get-buffer-window agent-q-context-panel-buffer)))
    (if win
        (delete-window win)
      (agent-q-show-context-panel))))

(provide 'sly-agent-q-context)
;;; sly-agent-q-context.el ends here
