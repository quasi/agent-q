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
  "Return file candidates matching PREFIX (substring match, case-insensitive).
Searches project files if `project-current' returns a project.
When PREFIX is empty, returns up to 50 recent project files.
Each candidate has `agent-q-context-type' and `agent-q-context-data'
text properties for use by the completion framework."
  (when-let ((project (project-current)))
    (let* ((case-fold-search t)
           (files (project-files project))
           (matching (if (string-empty-p prefix)
                         (seq-take files 50)  ; Show some files when no prefix
                       (seq-filter (lambda (f)
                                     (string-match-p (regexp-quote prefix)
                                                     (file-name-nondirectory f)))
                                   files))))
      (mapcar (lambda (file)
                (propertize (file-name-nondirectory file)
                            'agent-q-context-type :file
                            'agent-q-context-data (list :path file)))
              (seq-take matching 50)))))

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
  "Return buffer candidates matching PREFIX (substring match, case-insensitive).
Excludes internal buffers (those starting with space).
When PREFIX is empty, returns all non-internal buffers.
Each candidate has `agent-q-context-type' set to :buffer and
`agent-q-context-data' containing :buffer-name and :file-path (if visiting a file)."
  (let* ((case-fold-search t)  ; case-insensitive matching
         (buffers (seq-filter (lambda (buf)
                                (let ((name (buffer-name buf)))
                                  (and (not (string-prefix-p " " name))
                                       (or (string-empty-p prefix)
                                           (string-match-p (regexp-quote prefix) name)))))
                              (buffer-list))))
    (mapcar (lambda (buf)
              (let ((file-path (buffer-file-name buf)))
                (propertize (buffer-name buf)
                            'agent-q-context-type :buffer
                            'agent-q-context-data (if file-path
                                                      (list :buffer-name (buffer-name buf)
                                                            :file-path file-path)
                                                    (list :buffer-name (buffer-name buf))))))
            buffers)))

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

(defun agent-q--context-exit-function (candidate status)
  "Handle completion exit for CANDIDATE with STATUS.
When STATUS is 'finished, creates context item and inserts pill.
This function is called by the completion framework after the user
selects a candidate."
  (when (eq status 'finished)
    (let* ((type (get-text-property 0 'agent-q-context-type candidate))
           (data (get-text-property 0 'agent-q-context-data candidate))
           (item (make-agent-q-context-item
                  :type type
                  :display-name candidate
                  :data data
                  :content (agent-q--fetch-context-content type data))))
      ;; Add to context list
      (push item agent-q-context-items)
      ;; Replace @mention text with pill
      (let ((bounds (agent-q--context-mention-bounds)))
        (when bounds
          (delete-region (car bounds) (cdr bounds))
          (agent-q--insert-context-pill item)))
      ;; Refresh panel if visible
      (when (get-buffer-window agent-q-context-panel-buffer)
        (with-current-buffer agent-q-context-panel-buffer
          (agent-q--refresh-context-panel))))))

(defun agent-q-context-complete-at-point ()
  "Completion-at-point function for @-mentions.
Returns completion data when point is after an @-mention, nil otherwise.
Works with any completion framework (vertico, ivy, helm, default)."
  (when-let ((bounds (agent-q--context-mention-bounds)))
    (list (car bounds)
          (cdr bounds)
          (completion-table-dynamic #'agent-q--context-candidates)
          :exclusive 'no
          :annotation-function #'agent-q--context-annotation
          :exit-function #'agent-q--context-exit-function)))

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

(defun agent-q--pill-bounds-at-point ()
  "Return (START . END) bounds of pill at point, or nil."
  (let ((item (get-text-property (point) 'agent-q-context-item)))
    (when item
      (let ((start (previous-single-property-change (1+ (point)) 'agent-q-context-item))
            (end (next-single-property-change (point) 'agent-q-context-item)))
        (cons (or start (point-min))
              (or end (point-max)))))))

(defun agent-q-context-pill-remove ()
  "Remove context pill at point.
Deletes the pill from the buffer and removes the associated
context item from `agent-q-context-items'."
  (interactive)
  (if-let ((item (get-text-property (point) 'agent-q-context-item)))
      (let ((bounds (agent-q--pill-bounds-at-point)))
        ;; Remove from context list
        (setq agent-q-context-items (delete item agent-q-context-items))
        ;; Delete the pill text (including trailing space)
        (when bounds
          (let ((inhibit-read-only t))
            (delete-region (car bounds)
                           (min (1+ (cdr bounds)) (point-max)))))
        ;; Refresh panel if visible
        (when (get-buffer-window agent-q-context-panel-buffer)
          (with-current-buffer agent-q-context-panel-buffer
            (agent-q--refresh-context-panel)))
        (message "Removed %s from context"
                 (agent-q-context-item-display-name item)))
    (message "No context pill at point")))

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

;;; Interactive Commands

(defun agent-q-clear-context ()
  "Clear all context items from the current buffer.
Also refreshes the context panel if visible."
  (interactive)
  (setq agent-q-context-items nil)
  (when (get-buffer agent-q-context-panel-buffer)
    (with-current-buffer agent-q-context-panel-buffer
      (agent-q--refresh-context-panel)))
  (message "Context cleared"))

(defun agent-q--add-context-file ()
  "Add file to context interactively.
Prompts user to select a file and returns the created context item."
  (let* ((file (read-file-name "Add file: "))
         (expanded (expand-file-name file))
         (content (agent-q--fetch-context-content :file (list :path expanded))))
    (make-agent-q-context-item
     :type :file
     :display-name (file-name-nondirectory file)
     :data (list :path expanded)
     :content content)))

(defun agent-q--add-context-buffer ()
  "Add buffer to context interactively.
Prompts user to select a buffer and returns the created context item."
  (let* ((buf-name (read-buffer "Add buffer: " nil t))
         (content (agent-q--fetch-context-content :buffer (list :buffer-name buf-name))))
    (make-agent-q-context-item
     :type :buffer
     :display-name buf-name
     :data (list :buffer-name buf-name)
     :content content)))

(defun agent-q--add-context-symbol ()
  "Add symbol to context interactively.
Prompts user to select a symbol from open Lisp buffers and returns
the created context item."
  (let* ((sym (completing-read "Add symbol: "
                               (agent-q--symbol-candidates "")))
         (data (get-text-property 0 'agent-q-context-data sym))
         (content (agent-q--fetch-context-content :symbol data)))
    (make-agent-q-context-item
     :type :symbol
     :display-name sym
     :data data
     :content content)))

(defun agent-q--add-context-region ()
  "Add current region to context.
Requires an active region. Returns the created context item.
Signals `user-error' if no region is active."
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((start (region-beginning))
         (end (region-end))
         (buf-name (buffer-name))
         (content (buffer-substring-no-properties start end))
         (display (format "%s:%d-%d"
                          buf-name
                          (line-number-at-pos start)
                          (line-number-at-pos end))))
    (make-agent-q-context-item
     :type :region
     :display-name display
     :data (list :buffer buf-name :start start :end end)
     :content content)))

(defun agent-q-add-context ()
  "Interactively add a context item.
Prompts for context type (file, symbol, buffer, region) and then
prompts for the specific item. Adds the created item to
`agent-q-context-items' and refreshes the context panel if visible."
  (interactive)
  (let* ((type (completing-read "Context type: "
                                '("file" "symbol" "buffer" "region")
                                nil t))
         (item (pcase type
                 ("file" (agent-q--add-context-file))
                 ("symbol" (agent-q--add-context-symbol))
                 ("buffer" (agent-q--add-context-buffer))
                 ("region" (agent-q--add-context-region)))))
    (when item
      (push item agent-q-context-items)
      (when (get-buffer agent-q-context-panel-buffer)
        (with-current-buffer agent-q-context-panel-buffer
          (agent-q--refresh-context-panel)))
      (message "Added %s to context" (agent-q-context-item-display-name item)))))

;;; LLM Integration

(defun agent-q--context-item-file-path (item)
  "Get file path for context ITEM if available.
Returns the file path from :file-path in data for buffers,
or :path for files. Returns nil if no file path is available."
  (let ((data (agent-q-context-item-data item))
        (type (agent-q-context-item-type item)))
    (pcase type
      (:file (plist-get data :path))
      (:buffer (plist-get data :file-path))
      (:symbol (when-let ((pos (plist-get data :position)))
                 (when (markerp pos)
                   (buffer-file-name (marker-buffer pos)))))
      (:region (when-let ((buf (get-buffer (plist-get data :buffer))))
                 (buffer-file-name buf)))
      (_ nil))))

(defun agent-q--format-context-for-llm ()
  "Format current context items for LLM prompt.
Returns a string containing all context items formatted as a <context>
block with markdown-style headers and code fences, or nil if no context
items are present.

The format is:
  <context>
  ### display-name (type)
  File: /full/path/to/file.ext
  ```language
  content
  ```
  </context>

The file path is included when available to help the LLM generate
accurate diffs. Context items are presented in reverse order (oldest
first) since they were accumulated with `push'."
  (when agent-q-context-items
    (concat
     "\n\n<context>\n"
     (mapconcat
      (lambda (item)
        (let* ((file-path (agent-q--context-item-file-path item))
               (lang (when file-path
                       (pcase (file-name-extension file-path)
                         ("lisp" "lisp")
                         ("cl" "lisp")
                         ("el" "emacs-lisp")
                         ("py" "python")
                         ("js" "javascript")
                         ("ts" "typescript")
                         (_ "")))))
          (format "### %s (%s)%s\n```%s\n%s\n```\n"
                  (agent-q-context-item-display-name item)
                  (agent-q-context-item-type item)
                  (if file-path (format "\nFile: %s" file-path) "")
                  (or lang "")
                  (or (agent-q-context-item-content item)
                      "(content unavailable)"))))
      (reverse agent-q-context-items)
      "\n")
     "</context>\n")))

;;; Mode Setup

(defcustom agent-q-context-auto-trigger t
  "Whether to automatically trigger completion after @-mention prefix.
When non-nil, completion will trigger after typing the minimum
prefix length (see `agent-q-context-trigger-threshold')."
  :type 'boolean
  :group 'agent-q-context)

(defcustom agent-q-context-trigger-threshold 2
  "Minimum characters after @ before auto-triggering completion.
For example, with value 2, typing `@ab' will trigger completion."
  :type 'integer
  :group 'agent-q-context)

(defun agent-q--at-mention-prefix-length ()
  "Return length of text after @ in current @-mention, or nil if not in one."
  (save-excursion
    (let ((original-point (point)))
      (when (re-search-backward "@\\([^ \t\n]*\\)" (line-beginning-position) t)
        (let ((match-end (match-end 0))
              (at-pos (match-beginning 0)))
          (when (<= original-point match-end)
            (- match-end at-pos 1)))))))  ; -1 for the @ itself

(defun agent-q--maybe-trigger-context-completion ()
  "Trigger completion if @-mention prefix meets threshold.
This function is intended for use with `post-self-insert-hook'."
  (when (and agent-q-context-auto-trigger
             (not buffer-read-only))
    (let ((prefix-len (agent-q--at-mention-prefix-length)))
      (when (and prefix-len
                 (>= prefix-len agent-q-context-trigger-threshold))
        ;; Use run-at-time to avoid issues with nested command loops
        (run-at-time 0 nil #'agent-q--trigger-context-completion)))))

(defun agent-q--trigger-context-completion ()
  "Trigger context completion using completing-read.
This works with any completion framework (Helm, Vertico, Ivy, default)."
  (when-let ((completion-data (agent-q-context-complete-at-point)))
    (let* ((start (nth 0 completion-data))
           (end (nth 1 completion-data))
           (props (nthcdr 3 completion-data))
           (prefix (buffer-substring-no-properties start end))
           (prefix-no-at (if (string-prefix-p "@" prefix)
                             (substring prefix 1)
                           prefix))
           ;; Get candidates directly from our function
           (candidates (agent-q--context-candidates prefix))
           (annotate-fn (plist-get props :annotation-function)))
      (when candidates
        ;; Build completion table with annotations
        (let ((selection (completing-read
                          "Context @: "
                          (lambda (string pred action)
                            (if (eq action 'metadata)
                                `(metadata
                                  (annotation-function . ,annotate-fn)
                                  (category . agent-q-context))
                              (complete-with-action action candidates string pred)))
                          nil nil prefix-no-at)))
          (when (and selection (not (string-empty-p selection)))
            ;; Find the candidate with text properties preserved
            (let ((actual (seq-find (lambda (c) (string= c selection)) candidates)))
              (when actual
                ;; Get properties from the candidate
                (let* ((type (get-text-property 0 'agent-q-context-type actual))
                       (data (get-text-property 0 'agent-q-context-data actual))
                       (item (make-agent-q-context-item
                              :type type
                              :display-name actual
                              :data data
                              :content (agent-q--fetch-context-content type data))))
                  ;; Add to context list
                  (push item agent-q-context-items)
                  ;; Delete @-mention and insert pill
                  (delete-region start end)
                  (agent-q--insert-context-pill item)
                  ;; Refresh panel if visible
                  (when (get-buffer-window agent-q-context-panel-buffer)
                    (with-current-buffer agent-q-context-panel-buffer
                      (agent-q--refresh-context-panel))))))))))))

(defun agent-q-context-setup ()
  "Set up context completion for current buffer.
Adds `agent-q-context-complete-at-point' to `completion-at-point-functions'.
When `agent-q-context-auto-trigger' is non-nil, also adds a hook to
automatically trigger completion when @ is typed.
Call this from `agent-q-chat-mode-hook' or similar initialization."
  ;; Make completion-at-point-functions buffer-local and add our CAPF
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               #'agent-q-context-complete-at-point)
  ;; Set up auto-trigger on @
  (when agent-q-context-auto-trigger
    (add-hook 'post-self-insert-hook
              #'agent-q--maybe-trigger-context-completion
              nil t)))

(provide 'sly-agent-q-context)
;;; sly-agent-q-context.el ends here
