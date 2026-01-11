;;; sly-agent-q-tools.el --- Tool execution display for Agent-Q -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (sly "1.0.0"))
;; Keywords: lisp, ai, tools
;; URL: https://github.com/quasilabs/agent-q

;;; Commentary:

;; This file provides UI for displaying tool execution in Agent-Q.
;; When the agent uses tools (introspection, execution, buffer operations),
;; this displays them in the conversation buffer for transparency.
;;
;; Usage:
;;   (sly-agent-q-tools--log-execution "eval_form" "(+ 1 2)" "3")

;;; Code:

;;; Customization

(defgroup sly-agent-q-tools nil
  "Tool execution display for Agent-Q."
  :group 'sly
  :prefix "sly-agent-q-tools-")

(defface sly-agent-q-tools-tool-name
  '((t :foreground "magenta" :weight bold))
  "Face for tool execution indicators."
  :group 'sly-agent-q-tools)

(defface sly-agent-q-tools-result
  '((t :inherit font-lock-comment-face))
  "Face for tool execution results."
  :group 'sly-agent-q-tools)

(defcustom sly-agent-q-tools-show-execution t
  "Whether to display tool executions in conversation buffer."
  :type 'boolean
  :group 'sly-agent-q-tools)

;;; Helper functions

(defun sly-agent-q-tools--get-conversation-buffer ()
  "Return the Agent-Q conversation buffer, or nil if not found.
Uses the new chat buffer name."
  (get-buffer "*Agent-Q Chat*"))

;;; Logging functions

(defun sly-agent-q-tools--log-execution (tool-name args result)
  "Log tool execution to conversation buffer.

TOOL-NAME is the name of the tool executed.
ARGS is a string representation of the arguments.
RESULT is the tool execution result."
  (when sly-agent-q-tools-show-execution
    (let ((buffer (sly-agent-q-tools--get-conversation-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; Use the streaming marker if available, otherwise append to output region
          (let ((text (concat
                       (propertize (format "[TOOL: %s]\n" tool-name)
                                   'face 'sly-agent-q-tools-tool-name)
                       (when args
                         (propertize (format "Args: %s\n" args)
                                     'face 'sly-agent-q-tools-result))
                       (propertize (format "→ %s\n" result)
                                   'face 'sly-agent-q-tools-result))))
            (if (and (boundp 'agent-q--streaming-marker) agent-q--streaming-marker)
                (agent-q--append-response-chunk text)
              ;; Fallback: insert at output end marker
              (when (and (boundp 'agent-q--output-end-marker) agent-q--output-end-marker)
                (save-excursion
                  (goto-char agent-q--output-end-marker)
                  (let ((inhibit-read-only t))
                    (insert text)))))))))))

(defun sly-agent-q-tools--log-tool-call (tool-name)
  "Log that TOOL-NAME is being executed."
  (when sly-agent-q-tools-show-execution
    (let ((buffer (sly-agent-q-tools--get-conversation-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((text (propertize (format "[TOOL: %s] executing...\n" tool-name)
                                  'face 'sly-agent-q-tools-tool-name)))
            (if (and (boundp 'agent-q--streaming-marker) agent-q--streaming-marker)
                (agent-q--append-response-chunk text)
              (when (and (boundp 'agent-q--output-end-marker) agent-q--output-end-marker)
                (save-excursion
                  (goto-char agent-q--output-end-marker)
                  (let ((inhibit-read-only t))
                    (insert text)))))))))))

(defun sly-agent-q-tools--log-tool-result (result)
  "Log tool RESULT to conversation buffer."
  (when sly-agent-q-tools-show-execution
    (let ((buffer (sly-agent-q-tools--get-conversation-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((text (propertize (format "→ %s\n" result)
                                  'face 'sly-agent-q-tools-result)))
            (if (and (boundp 'agent-q--streaming-marker) agent-q--streaming-marker)
                (agent-q--append-response-chunk text)
              (when (and (boundp 'agent-q--output-end-marker) agent-q--output-end-marker)
                (save-excursion
                  (goto-char agent-q--output-end-marker)
                  (let ((inhibit-read-only t))
                    (insert text)))))))))))

(provide 'sly-agent-q-tools)
;;; sly-agent-q-tools.el ends here
