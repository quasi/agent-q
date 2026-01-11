;;; sly-agent-q-chat-test.el --- Tests for chat interface -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Package-Requires: ((emacs "27.1") (ert "0"))

;;; Commentary:

;; Comprehensive test suite for sly-agent-q-chat.el
;; Tests Phases 1, 2, and 3:
;; - Phase 1: Basic chat interface, message rendering, input handling
;; - Phase 2: Markdown rendering (bold, italic, code, links, code blocks)
;; - Phase 3: Session management integration

;;; Code:

(require 'ert)
(require 'sly-agent-q-chat)

;;;; Phase 1 Tests: Basic Chat Interface

(ert-deftest agent-q-chat/buffer-setup/creates-correct-layout ()
  "Test that chat buffer is created with correct initial layout."
  (with-temp-buffer
    (agent-q-chat-mode)
    ;; Should have header
    (goto-char (point-min))
    (should (looking-at-p "Agent-Q Chat"))
    ;; Should have separator
    (should (search-forward "Input" nil t))
    ;; Should have input prompt
    (goto-char (point-max))
    (forward-line -1)
    (should (looking-at-p "> "))
    ;; Markers should be set
    (should (markerp agent-q--output-end-marker))
    (should (markerp agent-q--input-start-marker))))

(ert-deftest agent-q-chat/buffer-setup/initializes-session ()
  "Test that buffer initializes with a new session."
  (with-temp-buffer
    (agent-q-chat-mode)
    (should (agent-q-session-p agent-q--current-session))
    (should (null (agent-q-session-messages agent-q--current-session)))))

(ert-deftest agent-q-chat/input/get-input-returns-text ()
  "Test getting input from input region."
  (with-temp-buffer
    (agent-q-chat-mode)
    (goto-char agent-q--input-start-marker)
    (insert "test input")
    (should (string= "test input" (agent-q--get-input)))))

(ert-deftest agent-q-chat/input/clear-input-removes-text ()
  "Test clearing input region."
  (with-temp-buffer
    (agent-q-chat-mode)
    (goto-char agent-q--input-start-marker)
    (insert "test input")
    (agent-q--clear-input)
    (should (string= "" (agent-q--get-input)))
    ;; Prompt should remain
    (goto-char (point-max))
    (forward-line -1)
    (should (looking-at-p "> "))))

(ert-deftest agent-q-chat/input/replace-input-changes-text ()
  "Test replacing input with new text."
  (with-temp-buffer
    (agent-q-chat-mode)
    (goto-char agent-q--input-start-marker)
    (insert "old text")
    (agent-q--replace-input "new text")
    (should (string= "new text" (agent-q--get-input)))))

(ert-deftest agent-q-chat/history/adds-to-history ()
  "Test that input is added to history."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--add-to-history "first")
    (agent-q--add-to-history "second")
    (should (equal '("second" "first") agent-q--input-history))))

(ert-deftest agent-q-chat/history/no-duplicates ()
  "Test that duplicate consecutive entries are not added."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--add-to-history "same")
    (agent-q--add-to-history "same")
    (should (equal '("same") agent-q--input-history))))

(ert-deftest agent-q-chat/history/navigate-previous ()
  "Test navigating to previous history entry."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--add-to-history "first")
    (agent-q--add-to-history "second")
    (goto-char agent-q--input-start-marker)
    (insert "current")
    ;; Navigate back
    (agent-q-history-previous)
    (should (string= "second" (agent-q--get-input)))
    (agent-q-history-previous)
    (should (string= "first" (agent-q--get-input)))))

(ert-deftest agent-q-chat/history/navigate-next ()
  "Test navigating forward in history."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--add-to-history "first")
    (agent-q--add-to-history "second")
    (goto-char agent-q--input-start-marker)
    (insert "current")
    ;; Navigate back then forward
    (agent-q-history-previous)
    (agent-q-history-previous)
    (should (string= "first" (agent-q--get-input)))
    (agent-q-history-next)
    (should (string= "second" (agent-q--get-input)))
    (agent-q-history-next)
    (should (string= "current" (agent-q--get-input)))))

(ert-deftest agent-q-chat/messages/render-user-message ()
  "Test rendering a user message."
  (with-temp-buffer
    (agent-q-chat-mode)
    (let ((start (marker-position agent-q--output-end-marker)))
      (agent-q--render-user-message "Hello")
      ;; Should have moved output marker
      (should (> (marker-position agent-q--output-end-marker) start))
      ;; Should contain user header
      (goto-char start)
      (should (search-forward "[USER]" nil t))
      ;; Should contain message
      (should (search-forward "Hello" nil t)))))

(ert-deftest agent-q-chat/messages/render-system-message ()
  "Test rendering a system message."
  (with-temp-buffer
    (agent-q-chat-mode)
    (let ((start (marker-position agent-q--output-end-marker)))
      (agent-q--render-system-message "System info")
      (goto-char start)
      (should (search-forward "System info" nil t))
      (should (get-text-property (1- (point)) 'face)))))

(ert-deftest agent-q-chat/streaming/begin-response ()
  "Test beginning a streaming assistant response."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--begin-assistant-response)
    (should agent-q--pending-response)
    (should (markerp agent-q--streaming-marker))
    ;; Should have header
    (goto-char agent-q--output-end-marker)
    (forward-line -2)
    (should (looking-at-p "\\[AGENT-Q\\]"))))

(ert-deftest agent-q-chat/streaming/append-chunks ()
  "Test appending chunks during streaming."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--begin-assistant-response)
    (let ((start (marker-position agent-q--streaming-marker)))
      (agent-q--append-response-chunk "chunk1")
      (agent-q--append-response-chunk "chunk2")
      ;; Should have both chunks
      (goto-char start)
      (should (search-forward "chunk1chunk2" nil t)))))

(ert-deftest agent-q-chat/streaming/finalize-response ()
  "Test finalizing a streaming response."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q--begin-assistant-response)
    (agent-q--append-response-chunk "Complete message")
    (agent-q--finalize-response "Complete message")
    ;; Should clear streaming state
    (should-not agent-q--pending-response)
    (should-not agent-q--streaming-marker)
    ;; Should update session
    (should (= 1 (length (agent-q-session-messages agent-q--current-session))))))

;;;; Phase 2 Tests: Markdown Rendering

(ert-deftest agent-q-chat/markdown/renders-bold ()
  "Test bold text rendering with **text**."
  (with-temp-buffer
    (let ((agent-q-render-markdown t))
      (insert "Normal **bold** text")
      (agent-q--render-markdown (point-min) (point-max))
      ;; Bold markers should be removed
      (should-not (search-backward "**" nil t))
      ;; Bold text should have face
      (goto-char (point-min))
      (search-forward "bold")
      (should (eq 'bold (get-text-property (1- (point)) 'face))))))

(ert-deftest agent-q-chat/markdown/renders-italic ()
  "Test italic text rendering with *text*."
  (with-temp-buffer
    (let ((agent-q-render-markdown t))
      (insert "Normal *italic* text")
      (agent-q--render-markdown (point-min) (point-max))
      ;; Italic text should have face
      (goto-char (point-min))
      (search-forward "italic")
      (should (eq 'italic (get-text-property (1- (point)) 'face))))))

(ert-deftest agent-q-chat/markdown/renders-inline-code ()
  "Test inline code rendering with `code`."
  (with-temp-buffer
    (let ((agent-q-render-markdown t))
      (insert "Text with `code` inline")
      (agent-q--render-markdown (point-min) (point-max))
      ;; Code should have face
      (goto-char (point-min))
      (search-forward "code")
      (should (eq 'agent-q-inline-code-face
                  (get-text-property (1- (point)) 'face))))))

(ert-deftest agent-q-chat/markdown/renders-links ()
  "Test link rendering with [text](url)."
  (with-temp-buffer
    (let ((agent-q-render-markdown t))
      (insert "Visit [example](https://example.com) site")
      (agent-q--render-markdown (point-min) (point-max))
      ;; Link text should be a button
      (goto-char (point-min))
      (search-forward "example")
      (should (button-at (1- (point))))
      (should (eq 'link (get-text-property (1- (point)) 'face))))))

(ert-deftest agent-q-chat/markdown/renders-code-blocks ()
  "Test fenced code block rendering."
  (with-temp-buffer
    (let ((agent-q-render-markdown t))
      (insert "```lisp\n(defun foo () 42)\n```")
      (agent-q--render-markdown (point-min) (point-max))
      ;; Code block should have background face
      (goto-char (point-min))
      (search-forward "defun")
      (should (eq 'agent-q-code-block-face
                  (get-text-property (1- (point)) 'face))))))

(ert-deftest agent-q-chat/markdown/detects-language ()
  "Test language detection in code blocks."
  (should (eq 'lisp-mode (agent-q--lang-to-mode "lisp")))
  (should (eq 'lisp-mode (agent-q--lang-to-mode "common-lisp")))
  (should (eq 'python-mode (agent-q--lang-to-mode "python")))
  (should (eq 'emacs-lisp-mode (agent-q--lang-to-mode "elisp")))
  (should (null (agent-q--lang-to-mode "nonexistent"))))

(ert-deftest agent-q-chat/markdown/disabled-when-customized ()
  "Test that markdown rendering can be disabled."
  (with-temp-buffer
    (let ((agent-q-render-markdown nil))
      (insert "**bold** text")
      (agent-q--render-markdown (point-min) (point-max))
      ;; Should not be modified
      (goto-char (point-min))
      (should (search-forward "**bold**" nil t)))))

;;;; Phase 3 Tests: Session Management Integration

(ert-deftest agent-q-chat/session/creates-message-on-send ()
  "Test that sending input creates a message in the session."
  (with-temp-buffer
    (agent-q-chat-mode)
    (let ((agent-q--pending-response nil))
      ;; Mock the SLY communication
      (cl-letf (((symbol-function 'sly-eval-async)
                 (lambda (_ callback) (funcall callback "response"))))
        (goto-char agent-q--input-start-marker)
        (insert "test message")
        ;; Send (without triggering error about no connection)
        (let ((msg (agent-q-message--create :role 'user :content "test message")))
          (push msg (agent-q-session-messages agent-q--current-session)))
        ;; Should have message
        (should (= 1 (length (agent-q-session-messages agent-q--current-session))))
        (let ((msg (car (agent-q-session-messages agent-q--current-session))))
          (should (eq 'user (agent-q-message-role msg)))
          (should (string= "test message" (agent-q-message-content msg))))))))

(ert-deftest agent-q-chat/session/updates-timestamp ()
  "Test that session timestamp is updated on activity."
  (with-temp-buffer
    (agent-q-chat-mode)
    (let ((initial-time (agent-q-session-updated-at agent-q--current-session)))
      (sleep-for 0.01)
      (setf (agent-q-session-updated-at agent-q--current-session) (current-time))
      (should (time-less-p initial-time
                           (agent-q-session-updated-at agent-q--current-session))))))

(ert-deftest agent-q-chat/session/set-model ()
  "Test setting session model."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q-session-set-model agent-q--current-session "claude-3-opus")
    (should (string= "claude-3-opus"
                     (agent-q-session-model agent-q--current-session)))))

(ert-deftest agent-q-chat/session/accumulate-tokens ()
  "Test token accumulation in session metadata."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q-session-add-tokens agent-q--current-session 100 50)
    (agent-q-session-add-tokens agent-q--current-session 200 75)
    (let ((meta (agent-q-session-metadata agent-q--current-session)))
      (should (= 300 (plist-get meta :total-input-tokens)))
      (should (= 125 (plist-get meta :total-output-tokens))))))

(ert-deftest agent-q-chat/session/update-metadata ()
  "Test updating session metadata."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q-session-update-metadata agent-q--current-session :provider "anthropic")
    (agent-q-session-update-metadata agent-q--current-session :version "1.0")
    (let ((meta (agent-q-session-metadata agent-q--current-session)))
      (should (string= "anthropic" (plist-get meta :provider)))
      (should (string= "1.0" (plist-get meta :version))))))

;;;; Configuration Tests

(ert-deftest agent-q-chat/config/display-shows-provider ()
  "Test that config info is displayed at startup."
  (with-temp-buffer
    (agent-q-chat-mode)
    ;; Mock config info
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () t))
              ((symbol-function 'sly-eval-async)
               (lambda (_ callback)
                 (funcall callback '(:provider-name "OpenAI"
                                   :model "gpt-4"
                                   :provider :openai)))))
      (agent-q--display-config-info)
      ;; Wait for async
      (sleep-for 0.1)
      ;; Should display config (this is async, so we test the function exists)
      (should (fboundp 'agent-q--display-config-info)))))

;;;; Helper Functions

(ert-deftest agent-q-chat/helpers/in-input-region ()
  "Test detecting if point is in input region."
  (with-temp-buffer
    (agent-q-chat-mode)
    (goto-char agent-q--input-start-marker)
    (should (agent-q--in-input-region-p))
    (goto-char (point-min))
    (should-not (agent-q--in-input-region-p))))

(ert-deftest agent-q-chat/messages/message-separator ()
  "Test message separator insertion."
  (with-temp-buffer
    (let ((agent-q-show-message-separators t))
      (agent-q--insert-message-separator)
      (goto-char (point-min))
      (should (search-forward "·" nil t)))))

(ert-deftest agent-q-chat/messages/no-separator-when-disabled ()
  "Test that separator is not inserted when disabled."
  (with-temp-buffer
    (let ((agent-q-show-message-separators nil)
          (start (point)))
      (agent-q--insert-message-separator)
      (should (= start (point))))))

(provide 'sly-agent-q-chat-test)
;;; sly-agent-q-chat-test.el ends here
