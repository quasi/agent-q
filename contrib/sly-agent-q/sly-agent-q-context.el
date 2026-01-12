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

;;; Constants

(defconst agent-q-context-max-size 50000
  "Maximum size in bytes for context content.
Content larger than this will be truncated to prevent LLM context
window overflow.")

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

(provide 'sly-agent-q-context)
;;; sly-agent-q-context.el ends here
