# Chat Interface Phase 6: Action Buttons

**Status:** Ready for Implementation
**Priority:** 6 (was 4 in original research)
**Depends On:** Phase 1 (Foundation), Phase 2 (Rich Rendering)
**Parent:** enhancement-4.md

---

## Goal

Add convenience action buttons to assistant messages for common operations like Copy, Retry, and Apply (for non-tool code suggestions).

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Priority | Last | Existing diff UI handles main Apply workflow |
| Buttons | Copy, Retry, Apply | Core operations; keep minimal |
| Rendering | After message, inline | Visible, accessible |
| Navigation | Tab/S-Tab | Standard Emacs button navigation |

---

## Files to Modify

**`contrib/sly-agent-q/sly-agent-q-chat.el`**

Add button infrastructure to message rendering:
- Button insertion after assistant messages
- Button action handlers
- Keyboard navigation

---

## Button Design

### Visual Layout

```
[AGENT-Q] 14:30:25
Here's how you could implement that:
```lisp
(defun my-function ()
  (do-something))
```

Let me know if you need changes.

[Copy] [Retry] [Apply]
────────────────────────────────────────────────
```

### Button States

```elisp
(defvar agent-q-button-states '(active disabled loading)
  "Possible button states.")

;; Faces for states
(defface agent-q-button-active-face
  '((t :foreground "#98c379"
       :background "#3e4451"
       :box (:line-width -1 :color "#98c379")
       :weight bold))
  "Face for active buttons."
  :group 'agent-q-chat)

(defface agent-q-button-disabled-face
  '((t :foreground "#5c6370"
       :background "#2c323c"
       :box (:line-width -1 :color "#5c6370")))
  "Face for disabled buttons."
  :group 'agent-q-chat)

(defface agent-q-button-loading-face
  '((t :foreground "#e5c07b"
       :background "#3e4451"
       :box (:line-width -1 :color "#e5c07b")))
  "Face for loading buttons."
  :group 'agent-q-chat)

(defface agent-q-button-success-face
  '((t :foreground "#282c34"
       :background "#98c379"
       :weight bold))
  "Face for success state."
  :group 'agent-q-chat)
```

---

## Button Insertion

```elisp
(defcustom agent-q-show-action-buttons t
  "Whether to show action buttons after assistant messages."
  :type 'boolean
  :group 'agent-q-chat)

(defun agent-q--insert-action-buttons (message)
  "Insert action buttons for MESSAGE."
  (when agent-q-show-action-buttons
    (let ((inhibit-read-only t)
          (msg-id (agent-q-message-id message)))
      (insert "\n")

      ;; Copy button
      (agent-q--insert-button "Copy"
                              #'agent-q--button-copy
                              msg-id
                              "Copy response to clipboard")
      (insert " ")

      ;; Retry button
      (agent-q--insert-button "Retry"
                              #'agent-q--button-retry
                              msg-id
                              "Retry with same prompt")
      (insert " ")

      ;; Apply button (only if message contains code blocks)
      (when (agent-q--message-has-code-blocks-p message)
        (agent-q--insert-button "Apply"
                                #'agent-q--button-apply
                                msg-id
                                "Apply code to buffer"))

      (insert "\n"))))

(defun agent-q--insert-button (label action msg-id tooltip)
  "Insert a button with LABEL, ACTION, MSG-ID, and TOOLTIP."
  (insert-button (format "[%s]" label)
                 'action action
                 'agent-q-message-id msg-id
                 'face 'agent-q-button-active-face
                 'mouse-face 'highlight
                 'help-echo tooltip
                 'follow-link t
                 'keymap agent-q-button-keymap))

(defvar agent-q-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'push-button)
    (define-key map (kbd "SPC") #'push-button)
    (define-key map [mouse-1] #'push-button)
    map)
  "Keymap for action buttons.")
```

---

## Button Actions

### Copy Button

```elisp
(defun agent-q--button-copy (button)
  "Copy message content to clipboard."
  (let* ((msg-id (button-get button 'agent-q-message-id))
         (msg (agent-q--find-message-by-id msg-id)))
    (if msg
        (progn
          (kill-new (agent-q-message-content msg))
          (agent-q--flash-button button "Copied!" 'agent-q-button-success-face))
      (message "Message not found"))))

(defun agent-q--flash-button (button text face)
  "Temporarily change BUTTON to show TEXT with FACE."
  (let ((original-label (button-label button))
        (original-face (button-get button 'face)))
    (button-put button 'display (format "[%s]" text))
    (button-put button 'face face)
    (run-with-timer 1.5 nil
                    (lambda ()
                      (when (button-valid-p button)
                        (button-put button 'display nil)
                        (button-put button 'face original-face))))))
```

### Retry Button

```elisp
(defun agent-q--button-retry (_button)
  "Retry by resending the last user message."
  (if-let ((last-user-msg (seq-find (lambda (msg)
                                      (eq (agent-q-message-role msg) 'user))
                                    (agent-q-session-messages agent-q-current-session))))
      (let ((content (agent-q-message-content last-user-msg)))
        (message "Retrying...")
        (agent-q--send-to-agent content))
    (message "No previous message to retry")))
```

### Apply Button

```elisp
(defun agent-q--button-apply (button)
  "Apply code blocks from message to a buffer."
  (let* ((msg-id (button-get button 'agent-q-message-id))
         (msg (agent-q--find-message-by-id msg-id))
         (content (agent-q-message-content msg))
         (code-blocks (agent-q--extract-code-blocks content)))
    (if code-blocks
        (agent-q--apply-code-blocks code-blocks button)
      (message "No code blocks found in message"))))

(defun agent-q--extract-code-blocks (content)
  "Extract code blocks from CONTENT.
Returns list of plists: (:language :code :start :end)"
  (let ((blocks nil))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward "```\\([a-zA-Z0-9+-]*\\)\n\\(\\(?:.*\n\\)*?\\)```" nil t)
        (push (list :language (match-string 1)
                    :code (match-string 2)
                    :start (match-beginning 0)
                    :end (match-end 0))
              blocks)))
    (nreverse blocks)))

(defun agent-q--apply-code-blocks (blocks button)
  "Apply code BLOCKS to target buffer."
  (if (= (length blocks) 1)
      ;; Single block: insert at point in last visited buffer
      (agent-q--apply-single-block (car blocks) button)
    ;; Multiple blocks: let user choose
    (agent-q--apply-choose-block blocks button)))

(defun agent-q--apply-single-block (block button)
  "Apply single code BLOCK."
  (let* ((code (plist-get block :code))
         (lang (plist-get block :language))
         (target-buf (completing-read
                      "Insert in buffer: "
                      (mapcar #'buffer-name (buffer-list))
                      nil t nil nil
                      (buffer-name (other-buffer)))))
    (with-current-buffer target-buf
      (let ((pos (point)))
        (insert code)
        (message "Inserted %d chars at position %d in %s"
                 (length code) pos target-buf)))
    (agent-q--flash-button button "Applied!" 'agent-q-button-success-face)))

(defun agent-q--apply-choose-block (blocks button)
  "Let user choose which BLOCK to apply."
  (let* ((choices (mapcar (lambda (block)
                            (let ((lang (plist-get block :language))
                                  (code (plist-get block :code)))
                              (cons (format "[%s] %s..."
                                            (or lang "code")
                                            (truncate-string-to-width
                                             (replace-regexp-in-string "\n" " " code)
                                             40))
                                    block)))
                          blocks))
         (choice (completing-read "Apply which block? " choices nil t)))
    (when choice
      (agent-q--apply-single-block (cdr (assoc choice choices)) button))))

(defun agent-q--message-has-code-blocks-p (message)
  "Return non-nil if MESSAGE contains code blocks."
  (string-match-p "```" (agent-q-message-content message)))
```

---

## Button Navigation

```elisp
(defun agent-q-next-button ()
  "Move to next action button."
  (interactive)
  (let ((pos (next-button (point))))
    (if pos
        (goto-char pos)
      ;; Wrap to beginning
      (goto-char (point-min))
      (when-let ((first (next-button (point))))
        (goto-char first)))))

(defun agent-q-previous-button ()
  "Move to previous action button."
  (interactive)
  (let ((pos (previous-button (point))))
    (if pos
        (goto-char pos)
      ;; Wrap to end
      (goto-char (point-max))
      (when-let ((last (previous-button (point))))
        (goto-char last)))))

;; Add to mode keymap
(define-key agent-q-chat-mode-map (kbd "TAB") #'agent-q-next-button)
(define-key agent-q-chat-mode-map (kbd "<backtab>") #'agent-q-previous-button)
(define-key agent-q-chat-mode-map (kbd "S-TAB") #'agent-q-previous-button)
```

---

## Integration with Rendering

Update `agent-q--finalize-response` to insert buttons:

```elisp
(defun agent-q--finalize-response (full-content)
  "Finalize the assistant response with rendering and buttons."
  (let ((msg (make-agent-q-message :role 'assistant :content full-content)))
    (push msg (agent-q-session-messages agent-q-current-session))
    (setf (agent-q-session-updated-at agent-q-current-session) (current-time))

    ;; Find message boundaries
    (let ((msg-start (marker-position agent-q--streaming-marker))
          (msg-end nil))
      (save-excursion
        (goto-char agent-q-output-end-marker)
        (let ((inhibit-read-only t))
          ;; Apply markdown rendering
          (agent-q--render-markdown msg-start (point))
          (setq msg-end (point))

          ;; Insert action buttons
          (agent-q--insert-action-buttons msg)

          (insert "\n")
          (set-marker agent-q-output-end-marker (point))))))

  (setq agent-q-pending-response nil)
  (setq agent-q--streaming-marker nil))
```

---

## Helper Functions

```elisp
(defun agent-q--find-message-by-id (id)
  "Find message with ID in current session."
  (seq-find (lambda (msg)
              (equal (agent-q-message-id msg) id))
            (agent-q-session-messages agent-q-current-session)))

(defun button-valid-p (button)
  "Return non-nil if BUTTON is still valid (in buffer)."
  (and (markerp (button-start button))
       (marker-buffer (button-start button))))
```

---

## Copy Code Block Feature

For convenience, add ability to copy just a code block:

```elisp
(defun agent-q-copy-code-at-point ()
  "Copy code block at point to clipboard."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      ;; Find code block boundaries
      (when (re-search-backward "```" nil t)
        (let ((start (point)))
          (forward-line 1)
          (let ((code-start (point)))
            (when (re-search-forward "```" nil t)
              (let ((code (buffer-substring-no-properties
                           code-start
                           (match-beginning 0))))
                (kill-new (string-trim code))
                (message "Code block copied")))))))))

(define-key agent-q-chat-mode-map (kbd "C-c C-w") #'agent-q-copy-code-at-point)
```

---

## Verification Checklist

- [ ] Buttons appear after assistant messages
- [ ] [Copy] copies full response to clipboard
- [ ] [Copy] flashes "Copied!" briefly
- [ ] [Retry] resends last user message
- [ ] [Apply] only appears when message has code blocks
- [ ] [Apply] lets user choose target buffer
- [ ] [Apply] with multiple blocks lets user choose which
- [ ] [Apply] flashes "Applied!" briefly
- [ ] Tab moves to next button
- [ ] S-Tab moves to previous button
- [ ] RET/Space activates button
- [ ] Mouse click activates button
- [ ] `C-c C-w` copies code block at point
- [ ] Buttons don't interfere with text selection

---

## Success Metrics

- Buttons clearly visible but not intrusive
- Click/key activation feels responsive (<100ms)
- Flash feedback confirms action
- Navigation is intuitive
- Apply workflow faster than manual copy-paste
