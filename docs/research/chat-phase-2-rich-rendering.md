# Chat Interface Phase 2: Rich Rendering

**Status:** Ready for Implementation
**Priority:** 2
**Depends On:** Phase 1 (Foundation)
**Parent:** enhancement-4.md

---

## Goal

Add markdown rendering and syntax-highlighted code blocks to the chat interface, making agent responses more readable and visually appealing.

---

## Files to Modify

**`contrib/sly-agent-q/sly-agent-q-chat.el`**

Add rendering functions for:
- Markdown text formatting (bold, italic, lists)
- Fenced code block detection and highlighting
- Clickable links
- Visual message separators

---

## Dependencies

**Required package:** `markdown-mode`

```elisp
(require 'markdown-mode)
```

---

## Core Rendering Approach

Markdown rendering is applied **after** message content is inserted, using font-lock and text properties. This avoids interfering with streaming.

```
Message inserted (plain text)
    ↓
agent-q--finalize-response called
    ↓
agent-q--render-markdown applied to message region
    ↓
Syntax highlighting applied to code blocks
```

---

## Code Block Detection

### Regex Pattern

```elisp
(defconst agent-q-code-block-regexp
  "^```\\([a-zA-Z0-9+-]*\\)\n\\(\\(?:.*\n\\)*?\\)```$"
  "Regexp matching fenced code blocks.
Group 1: language identifier
Group 2: code content")
```

### Code Block Structure

```elisp
(cl-defstruct agent-q-code-block
  "A detected code block in a message."
  language       ; String: "lisp", "elisp", "python", etc.
  content        ; String: the code inside
  start          ; Buffer position: start of block
  end)           ; Buffer position: end of block
```

---

## Implementation

### Markdown Rendering

```elisp
(defcustom agent-q-render-markdown t
  "Whether to render markdown in assistant responses."
  :type 'boolean
  :group 'agent-q-chat)

(defun agent-q--render-markdown (start end)
  "Apply markdown rendering to region from START to END."
  (when agent-q-render-markdown
    (save-excursion
      (goto-char start)

      ;; Bold: **text** or __text__
      (while (re-search-forward "\\*\\*\\(.+?\\)\\*\\*\\|__\\(.+?\\)__" end t)
        (let ((text (or (match-string 1) (match-string 2)))
              (beg (match-beginning 0))
              (fin (match-end 0)))
          (delete-region beg fin)
          (goto-char beg)
          (insert (propertize text 'face 'bold))
          (setq end (+ end (- (length text) (- fin beg))))))

      ;; Italic: *text* or _text_ (not inside code)
      (goto-char start)
      (while (re-search-forward "\\(?:^\\|[^*_]\\)\\([*_]\\)\\([^*_]+?\\)\\1" end t)
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
      (agent-q--render-code-blocks start end))))
```

### Code Block Rendering

```elisp
(defun agent-q--render-code-blocks (start end)
  "Render fenced code blocks in region from START to END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward agent-q-code-block-regexp end t)
      (let* ((lang (match-string 1))
             (code (match-string 2))
             (block-start (match-beginning 0))
             (block-end (match-end 0)))

        ;; Apply background to entire block
        (put-text-property block-start block-end
                           'face 'agent-q-code-block-face)

        ;; Apply syntax highlighting to code content
        (when (and lang (not (string-empty-p lang)))
          (agent-q--highlight-code-region
           (match-beginning 2)
           (match-end 2)
           lang))

        ;; Mark language on first line
        (save-excursion
          (goto-char block-start)
          (when (looking-at "```\\([a-z]+\\)")
            (put-text-property (match-beginning 1) (match-end 1)
                               'face 'agent-q-code-lang-face)))))))

(defun agent-q--highlight-code-region (start end lang)
  "Apply syntax highlighting to code from START to END for LANG."
  (let ((mode (agent-q--lang-to-mode lang)))
    (when (and mode (fboundp mode))
      (let ((highlighted (agent-q--fontify-code
                          (buffer-substring-no-properties start end)
                          mode)))
        (when highlighted
          (delete-region start end)
          (goto-char start)
          (insert highlighted))))))

(defun agent-q--fontify-code (code mode)
  "Return CODE fontified using MODE."
  (with-temp-buffer
    (insert code)
    (delay-mode-hooks (funcall mode))
    (font-lock-ensure)
    (buffer-string)))

(defun agent-q--lang-to-mode (lang)
  "Map LANG string to Emacs major mode."
  (cond
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
   (t nil)))
```

### Update Finalize Response

```elisp
(defun agent-q--finalize-response (full-content)
  "Finalize the assistant response with rendering."
  (let ((msg (make-agent-q-message :role 'assistant :content full-content)))
    (push msg (agent-q-session-messages agent-q-current-session))
    (setf (agent-q-session-updated-at agent-q-current-session) (current-time)))

  ;; Find message boundaries for rendering
  (let ((msg-start (marker-position agent-q--streaming-marker))
        (msg-end nil))
    (save-excursion
      (goto-char agent-q-output-end-marker)
      (let ((inhibit-read-only t))
        (insert "\n\n")
        (setq msg-end (point))
        (set-marker agent-q-output-end-marker (point))))

    ;; Apply markdown rendering
    (let ((inhibit-read-only t))
      (agent-q--render-markdown msg-start msg-end)))

  (setq agent-q-pending-response nil)
  (setq agent-q--streaming-marker nil))
```

---

## Additional Faces

```elisp
(defface agent-q-code-block-face
  '((((background dark))
     :background "#2c323c" :extend t)
    (((background light))
     :background "#f0f0f0" :extend t))
  "Face for code block background."
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
  "Face for code block language identifier."
  :group 'agent-q-chat)
```

---

## Message Separators

```elisp
(defcustom agent-q-show-message-separators t
  "Whether to show separators between messages."
  :type 'boolean
  :group 'agent-q-chat)

(defun agent-q--insert-message-separator ()
  "Insert a subtle separator between messages."
  (when agent-q-show-message-separators
    (let ((inhibit-read-only t))
      (insert (propertize (concat "\n" (make-string 60 ?·) "\n\n")
                          'face 'agent-q-message-separator-face
                          'read-only t)))))

(defface agent-q-message-separator-face
  '((t :foreground "#3e4451" :height 0.8))
  "Face for message separators."
  :group 'agent-q-chat)
```

---

## Verification Checklist

- [ ] **text** renders as bold
- [ ] *text* renders as italic
- [ ] `code` renders with inline code face
- [ ] [link](url) renders as clickable button
- [ ] Fenced code blocks have background color
- [ ] Code blocks show language identifier
- [ ] Lisp code has syntax highlighting
- [ ] Python code has syntax highlighting
- [ ] Unknown languages still render (just no highlighting)
- [ ] Message separators appear between messages
- [ ] Rendering doesn't break streaming (applied after finalize)
- [ ] Read-only property preserved after rendering

---

## Success Metrics

- Code blocks are immediately recognizable
- Syntax highlighting matches major mode colors
- Links are clickable and open in browser
- Rendering doesn't cause noticeable lag
- Long messages with multiple code blocks render correctly
