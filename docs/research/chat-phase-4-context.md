# Chat Interface Phase 4: Context Management

**Status:** Ready for Implementation
**Priority:** 4 (was 3 in original research)
**Depends On:** Phase 1 (Foundation)
**Parent:** enhancement-4.md

---

## Goal

Implement @-mention completion and a context panel, making it easy to attach files, symbols, and buffers to conversations.

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Trigger character | `@` | Industry standard (Claude Code, Cursor, etc.) |
| Completion framework | `completing-read` | Works with any completion UI (vertico, ivy, helm) |
| Visual representation | Inline pills `[@file.lisp]` | Visible in input, clickable |
| Context panel | Optional sidebar | Toggle with `C-c C-p` |

---

## Files to Create/Modify

### New Files

**`contrib/sly-agent-q/sly-agent-q-context.el`**

Context management UI containing:
- @-mention detection and completion
- Context pill rendering
- Context panel buffer
- Integration with Lisp-side context manager

### Modified Files

**`contrib/sly-agent-q/sly-agent-q-chat.el`**

- Add `@` binding for completion trigger
- Include context items in message submission
- Display context pills in rendered messages

---

## Context Item Structure

```elisp
(cl-defstruct agent-q-context-item
  "A context item attached to a message."
  (type nil)          ; :file | :symbol | :buffer | :region | :url
  (display-name nil)  ; Short name shown in pill
  (data nil)          ; Type-specific data (plist)
  (content nil))      ; Actual content to send to LLM

;; Type-specific data:
;; :file   -> (:path "/path/to/file")
;; :symbol -> (:name "function-name" :file "/path" :position 123)
;; :buffer -> (:buffer-name "*scratch*")
;; :region -> (:buffer "foo.lisp" :start 100 :end 200)
;; :url    -> (:url "https://...")
```

---

## @-Mention Completion

### Trigger Detection

```elisp
(defun agent-q-context-complete-at-point ()
  "Completion-at-point function for @-mentions."
  (when-let ((bounds (agent-q--context-mention-bounds)))
    (list (car bounds)
          (cdr bounds)
          (completion-table-dynamic #'agent-q--context-candidates)
          :exclusive 'no
          :annotation-function #'agent-q--context-annotation
          :exit-function #'agent-q--context-insert-item)))

(defun agent-q--context-mention-bounds ()
  "Return bounds of @-mention at point, or nil."
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward "@\\([^ \t\n]*\\)" (line-beginning-position) t)
        (cons (match-beginning 0) end)))))

;; Add to completion-at-point-functions in chat mode
(add-hook 'agent-q-chat-mode-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions
                         #'agent-q-context-complete-at-point)))
```

### Candidate Gathering

```elisp
(defun agent-q--context-candidates (prefix)
  "Return completion candidates matching PREFIX."
  (let ((prefix-no-at (if (string-prefix-p "@" prefix)
                          (substring prefix 1)
                        prefix)))
    (append
     ;; Files in project
     (agent-q--file-candidates prefix-no-at)
     ;; Symbols in current/recent buffers
     (agent-q--symbol-candidates prefix-no-at)
     ;; Open buffers
     (agent-q--buffer-candidates prefix-no-at))))

(defun agent-q--file-candidates (prefix)
  "Return file candidates matching PREFIX."
  (when-let ((project (project-current)))
    (let ((files (project-files project)))
      (mapcar (lambda (file)
                (propertize (file-name-nondirectory file)
                            'agent-q-context-type :file
                            'agent-q-context-data (list :path file)))
              (seq-filter (lambda (f)
                            (string-prefix-p prefix (file-name-nondirectory f) t))
                          files)))))

(defun agent-q--symbol-candidates (prefix)
  "Return symbol candidates matching PREFIX."
  (let ((symbols nil))
    ;; Gather from imenu of recent Lisp buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'lisp-mode 'emacs-lisp-mode)
          (ignore-errors
            (let ((index (imenu--make-index-alist t)))
              (agent-q--flatten-imenu index prefix symbols))))))
    (seq-take symbols 50)))  ; Limit results

(defun agent-q--flatten-imenu (index prefix result)
  "Flatten imenu INDEX, filtering by PREFIX, into RESULT."
  (dolist (item index)
    (cond
     ((imenu--subalist-p item)
      (agent-q--flatten-imenu (cdr item) prefix result))
     ((and (stringp (car item))
           (string-prefix-p prefix (car item) t))
      (push (propertize (car item)
                        'agent-q-context-type :symbol
                        'agent-q-context-data (list :name (car item)
                                                    :position (cdr item)))
            result)))))

(defun agent-q--buffer-candidates (prefix)
  "Return buffer candidates matching PREFIX."
  (mapcar (lambda (buf)
            (propertize (buffer-name buf)
                        'agent-q-context-type :buffer
                        'agent-q-context-data (list :buffer-name (buffer-name buf))))
          (seq-filter (lambda (buf)
                        (and (not (string-prefix-p " " (buffer-name buf)))
                             (string-prefix-p prefix (buffer-name buf) t)))
                      (buffer-list))))

(defun agent-q--context-annotation (candidate)
  "Return annotation for CANDIDATE."
  (let ((type (get-text-property 0 'agent-q-context-type candidate)))
    (pcase type
      (:file " [file]")
      (:symbol " [symbol]")
      (:buffer " [buffer]")
      (_ ""))))
```

### Context Item Insertion

```elisp
(defun agent-q--context-insert-item (candidate status)
  "Insert context pill for CANDIDATE after completion."
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
      ;; Replace text with pill
      (let ((bounds (agent-q--context-mention-bounds)))
        (when bounds
          (delete-region (car bounds) (cdr bounds))
          (agent-q--insert-context-pill item))))))

(defun agent-q--insert-context-pill (item)
  "Insert visual pill for context ITEM."
  (let ((start (point)))
    (insert (propertize (format "[@%s]" (agent-q-context-item-display-name item))
                        'face 'agent-q-context-pill-face
                        'agent-q-context-item item
                        'read-only t
                        'rear-nonsticky t
                        'mouse-face 'highlight
                        'keymap agent-q-context-pill-map
                        'help-echo (agent-q--context-pill-tooltip item)))
    (insert " ")))

(defvar agent-q-context-pill-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'agent-q-context-pill-click)
    (define-key map (kbd "RET") #'agent-q-context-pill-visit)
    (define-key map (kbd "DEL") #'agent-q-context-pill-remove)
    (define-key map (kbd "?") #'agent-q-context-pill-describe)
    map)
  "Keymap for context pills.")
```

### Content Fetching

```elisp
(defun agent-q--fetch-context-content (type data)
  "Fetch actual content for context item."
  (pcase type
    (:file
     (let ((path (plist-get data :path)))
       (when (file-readable-p path)
         (with-temp-buffer
           (insert-file-contents path nil 0 50000)  ; Limit to 50KB
           (buffer-string)))))
    (:symbol
     (when-let* ((pos (plist-get data :position))
                 (buf (marker-buffer pos)))
       (with-current-buffer buf
         (save-excursion
           (goto-char pos)
           (let ((bounds (bounds-of-thing-at-point 'defun)))
             (when bounds
               (buffer-substring-no-properties (car bounds) (cdr bounds))))))))
    (:buffer
     (when-let ((buf (get-buffer (plist-get data :buffer-name))))
       (with-current-buffer buf
         (buffer-substring-no-properties
          (point-min)
          (min (point-max) 50000)))))  ; Limit to 50KB
    (_ nil)))
```

---

## Context Panel (Sidebar)

```elisp
(defvar agent-q-context-panel-buffer "*Agent-Q Context*"
  "Buffer name for context panel.")

(defun agent-q-toggle-context-panel ()
  "Toggle context panel sidebar."
  (interactive)
  (let ((win (get-buffer-window agent-q-context-panel-buffer)))
    (if win
        (delete-window win)
      (agent-q-show-context-panel))))

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

(define-derived-mode agent-q-context-panel-mode special-mode "Context"
  "Major mode for Agent-Q context panel."
  (setq-local revert-buffer-function #'agent-q--refresh-context-panel))

(defun agent-q--refresh-context-panel (&rest _)
  "Refresh context panel contents."
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
                   'action (lambda (_)
                             (agent-q-clear-context)
                             (agent-q--refresh-context-panel))
                   'face 'agent-q-button-face)))

(defun agent-q--insert-context-panel-item (item)
  "Insert ITEM in context panel."
  (let ((type (agent-q-context-item-type item))
        (name (agent-q-context-item-display-name item)))
    (insert (propertize (format "[%s] "
                                (pcase type
                                  (:file "FILE")
                                  (:symbol "SYM")
                                  (:buffer "BUF")
                                  (_ "?")))
                        'face 'font-lock-type-face))
    (insert-button name
                   'action (lambda (_) (agent-q--visit-context-item item))
                   'face 'link
                   'help-echo "Click to visit")
    (insert "  ")
    (insert-button "✕"
                   'action (lambda (_)
                             (agent-q--remove-context-item item)
                             (agent-q--refresh-context-panel))
                   'face 'error
                   'help-echo "Remove from context")
    (insert "\n")))
```

---

## Integration with LLM Prompt

```elisp
(defun agent-q--format-context-for-llm ()
  "Format current context items for LLM prompt."
  (when agent-q-context-items
    (concat
     "\n\n<context>\n"
     (mapconcat
      (lambda (item)
        (format "### %s (%s)\n```\n%s\n```\n"
                (agent-q-context-item-display-name item)
                (agent-q-context-item-type item)
                (or (agent-q-context-item-content item)
                    "(content unavailable)")))
      (reverse agent-q-context-items)
      "\n")
     "</context>\n")))

;; Modify send-to-agent to include context
(defun agent-q--send-to-agent (content)
  "Send CONTENT with context to the Lisp agent via SLY."
  (let ((full-content (concat content (agent-q--format-context-for-llm))))
    (agent-q--begin-assistant-response)
    (sly-eval-async
     `(agent-q:send-to-agent ,full-content)
     (lambda (result)
       (agent-q--finalize-response result)))))
```

---

## Additional Faces

```elisp
(defface agent-q-context-pill-face
  '((t :foreground "#61afef"
       :background "#3e4451"
       :box (:line-width -1 :color "#61afef")
       :weight bold))
  "Face for context pills in input."
  :group 'agent-q-chat)

(defface agent-q-button-face
  '((t :foreground "#98c379" :weight bold))
  "Face for clickable buttons."
  :group 'agent-q-chat)
```

---

## Commands

```elisp
(defun agent-q-add-context ()
  "Interactively add context item."
  (interactive)
  (let* ((type (completing-read "Context type: "
                                '("file" "symbol" "buffer" "region")))
         (item (pcase type
                 ("file" (agent-q--add-context-file))
                 ("symbol" (agent-q--add-context-symbol))
                 ("buffer" (agent-q--add-context-buffer))
                 ("region" (agent-q--add-context-region)))))
    (when item
      (push item agent-q-context-items)
      (agent-q--refresh-context-panel)
      (message "Added %s to context" (agent-q-context-item-display-name item)))))

(defun agent-q--add-context-file ()
  "Add file to context interactively."
  (let ((file (read-file-name "Add file: ")))
    (make-agent-q-context-item
     :type :file
     :display-name (file-name-nondirectory file)
     :data (list :path file)
     :content (agent-q--fetch-context-content :file (list :path file)))))

(defun agent-q-clear-context ()
  "Clear all context items."
  (interactive)
  (setq agent-q-context-items nil)
  (message "Context cleared"))
```

---

## Keybindings

```elisp
;; In chat mode
(define-key agent-q-chat-mode-map (kbd "C-c @") #'agent-q-add-context)
(define-key agent-q-chat-mode-map (kbd "C-c C-x") #'agent-q-clear-context)
(define-key agent-q-chat-mode-map (kbd "C-c C-p") #'agent-q-toggle-context-panel)

;; For @ completion, uses completion-at-point (M-TAB or TAB depending on config)
```

---

## Verification Checklist

- [ ] Typing `@` followed by chars triggers completion
- [ ] File completion shows project files
- [ ] Symbol completion shows function/variable names
- [ ] Buffer completion shows open buffers
- [ ] Selecting completion inserts pill `[@name]`
- [ ] Pill is clickable (visits source)
- [ ] Pill can be deleted (DEL key)
- [ ] `C-c @` adds context interactively
- [ ] `C-c C-p` toggles context panel
- [ ] Context panel shows all items with remove buttons
- [ ] Context included in LLM prompt
- [ ] Context snapshot saved with messages

---

## Success Metrics

- @-completion feels fast (<100ms)
- Context pills clearly visible
- Context panel accurately reflects state
- LLM receives properly formatted context
