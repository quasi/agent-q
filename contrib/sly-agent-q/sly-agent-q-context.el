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

(provide 'sly-agent-q-context)
;;; sly-agent-q-context.el ends here
