;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q.TOOLS; Base: 10 -*-

(in-package :agent-q.tools)

;;; Diff Tool
;;;
;;; The key Phase 2 feature: propose file edits as diffs for user review.
;;; This tool blocks until the user accepts or rejects the proposed changes.

;;; propose-file-edit

(let ((tool (define-tool
              "propose_file_edit"
              "Propose changes to a file as a diff for user review. Shows before/after diff in Emacs. User can accept (apply changes) or reject. This is the preferred way to edit files - always use this instead of write_file for code changes."
              '((:name "path" :type :string :description "File path to edit (absolute or relative)")
                (:name "original" :type :string :description "Expected original content (for verification)")
                (:name "modified" :type :string :description "Proposed new content")
                (:name "description" :type :string :description "Clear explanation of what changed and why"))
              :required '("path" "original" "modified" "description")
              :safety-level :moderate
              :categories '(:buffer :editing)
              :handler (lambda (args)
                         (let ((path (gethash "path" args))
                               (original (gethash "original" args))
                               (modified (gethash "modified" args))
                               (description (gethash "description" args)))
                           (handler-case
                               (progn
                                 ;; Call Emacs to show diff and wait for user decision
                                 ;; This blocks until user accepts or rejects
                                 (let ((decision (eval-in-emacs
                                                 `(sly-agent-q-show-diff-and-wait
                                                   ,path
                                                   ,original
                                                   ,modified
                                                   ,description))))
                                   (cond
                                     ((string= decision "accepted")
                                      (format nil "✓ Changes accepted and applied to ~A~%~%~
                                                   The buffer has been updated with your proposed changes. ~
                                                   Save the buffer when ready."
                                             path))
                                     ((string= decision "rejected")
                                      (format nil "✗ Changes rejected by user for ~A~%~%~
                                                   The file remains unchanged. Consider revising your approach ~
                                                   or asking the user for clarification."
                                             path))
                                     (t
                                      (format nil "⚠ Unexpected response from diff UI: ~A~%~%~
                                                   The file may not have been modified."
                                             decision)))))
                             (error (e)
                               (format nil "Error showing diff for ~A: ~A~%~%~
                                            This might indicate an issue with the Emacs integration. ~
                                            Make sure sly-agent-q-diff.el is loaded."
                                      path e))))))))
  (register-tool *agent-q-registry* tool))
