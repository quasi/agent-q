;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q.TOOLS; Base: 10 -*-

(in-package :agent-q.tools)

;;; Buffer Tools
;;;
;;; Tools for file and buffer operations via Emacs.
;;; All operations delegate to Emacs through eval-in-emacs.

;;; 1. read-file

(let ((tool (define-tool
              "read_file"
              "Read the contents of a file from the filesystem. Returns the full file contents as a string."
              '((:name "path" :type :string :description "File path to read (absolute or relative)"))
              :required '("path")
              :safety-level :safe
              :categories '(:buffer :filesystem)
              :handler (lambda (args)
                         (let ((path (gethash "path" args)))
                           (handler-case
                               (let ((content (eval-in-emacs
                                              `(condition-case err
                                                   (with-temp-buffer
                                                     (insert-file-contents ,path)
                                                     (buffer-string))
                                                 (error (format "Error reading file: %s" err))))))
                                 (if (and (stringp content)
                                         (string= (subseq content 0 (min 18 (length content)))
                                                 "Error reading file"))
                                     content
                                     (format-for-llm content :max-length 5000)))
                             (error (e)
                               (format nil "Error reading file ~A: ~A" path e))))))))
  (register-tool *agent-q-registry* tool))

;;; 2. read-buffer

(let ((tool (define-tool
              "read_buffer"
              "Read the contents of an Emacs buffer. Can read entire buffer or a specific region."
              '((:name "buffer" :type :string :description "Buffer name (optional, defaults to current buffer)")
                (:name "start" :type :integer :description "Start position in buffer (optional)")
                (:name "end" :type :integer :description "End position in buffer (optional)"))
              :required '()
              :safety-level :safe
              :categories '(:buffer)
              :handler (lambda (args)
                         (let ((buffer (gethash "buffer" args))
                               (start (gethash "start" args))
                               (end (gethash "end" args)))
                           (handler-case
                               (let ((content (eval-in-emacs
                                              `(condition-case err
                                                   (with-current-buffer ,(or buffer '(current-buffer))
                                                     (buffer-substring-no-properties
                                                      ,(or start '(point-min))
                                                      ,(or end '(point-max))))
                                                 (error (format "Error reading buffer: %s" err))))))
                                 (format-for-llm content :max-length 5000))
                             (error (e)
                               (format nil "Error reading buffer: ~A" e))))))))
  (register-tool *agent-q-registry* tool))

;;; 3. write-file

(let ((tool (define-tool
              "write_file"
              "Write content to a file. WARNING: This will overwrite existing files. Use with caution."
              '((:name "path" :type :string :description "File path to write to")
                (:name "content" :type :string :description "Content to write to the file"))
              :required '("path" "content")
              :safety-level :dangerous
              :categories '(:buffer :filesystem :destructive)
              :handler (lambda (args)
                         (let ((path (gethash "path" args))
                               (content (gethash "content" args)))
                           (handler-case
                               (progn
                                 (eval-in-emacs
                                  `(condition-case err
                                       (with-temp-file ,path
                                         (insert ,content))
                                     (error (format "Error writing file: %s" err))))
                                 (format nil "Successfully wrote ~D bytes to ~A"
                                        (length content) path))
                             (error (e)
                               (format nil "Error writing file ~A: ~A" path e))))))))
  (register-tool *agent-q-registry* tool))

;;; 4. search-in-buffer

(let ((tool (define-tool
              "search_in_buffer"
              "Search for text in a buffer using regex or literal string. Returns all matches with line numbers and positions."
              '((:name "pattern" :type :string :description "Search pattern (string or regex)")
                (:name "buffer" :type :string :description "Buffer name (optional, defaults to current buffer)")
                (:name "regex" :type :boolean :description "If true, pattern is treated as a regex (default false)")
                (:name "all_matches" :type :boolean :description "If true, return all matches; otherwise just first (default true)"))
              :required '("pattern")
              :safety-level :safe
              :categories '(:buffer)
              :handler (lambda (args)
                         (let ((pattern (gethash "pattern" args))
                               (buffer (gethash "buffer" args))
                               (regex (gethash "regex" args))
                               (all-matches (if (null (gethash "all_matches" args))
                                               t
                                               (gethash "all_matches" args))))
                           (handler-case
                               (let ((matches (eval-in-emacs
                                              `(condition-case err
                                                   (with-current-buffer ,(or buffer '(current-buffer))
                                                     (save-excursion
                                                       (goto-char (point-min))
                                                       (let ((matches nil)
                                                             (search-fn (if ,regex
                                                                           're-search-forward
                                                                         'search-forward)))
                                                         (while (funcall search-fn ,pattern nil t)
                                                           (push (list :match (match-string 0)
                                                                      :position (match-beginning 0)
                                                                      :line (line-number-at-pos (match-beginning 0)))
                                                                 matches)
                                                           (unless ,all-matches
                                                             (cl-return)))
                                                         (nreverse matches))))
                                                 (error (format "Error searching: %s" err))))))
                                 (if matches
                                     (with-output-to-string (s)
                                       (format s "Found ~D match~:P for '~A':~%~%"
                                              (length matches) pattern)
                                       (dolist (match matches)
                                         (format s "Line ~D, Position ~D: ~S~%"
                                                (getf match :line)
                                                (getf match :position)
                                                (getf match :match))))
                                     (format nil "No matches found for '~A'" pattern)))
                             (error (e)
                               (format nil "Error searching buffer: ~A" e))))))))
  (register-tool *agent-q-registry* tool))
