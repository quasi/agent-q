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

(provide 'sly-agent-q-context)
;;; sly-agent-q-context.el ends here
