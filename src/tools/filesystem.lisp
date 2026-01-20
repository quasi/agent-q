;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q.TOOLS; Base: 10 -*-
;; ABOUTME: Filesystem tools for directory listing, file info, and path operations.
;; ABOUTME: All paths validated against *project-root* for security.

(in-package :agent-q.tools)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun format-file-size (bytes)
  "Format BYTES as human-readable size."
  (cond
    ((null bytes) "?")
    ((< bytes 1024) (format nil "~D B" bytes))
    ((< bytes (* 1024 1024)) (format nil "~,1F KB" (/ bytes 1024.0)))
    ((< bytes (* 1024 1024 1024)) (format nil "~,1F MB" (/ bytes (* 1024.0 1024))))
    (t (format nil "~,1F GB" (/ bytes (* 1024.0 1024 1024))))))

(defun format-directory-listing (path entries sort-by)
  "Format directory listing for LLM consumption."
  (let ((sorted (sort (copy-list entries)
                      (case (intern (string-upcase (or sort-by "name")) :keyword)
                        (:size (lambda (a b) (> (or (getf a :size) 0)
                                                (or (getf b :size) 0))))
                        (:modified (lambda (a b)
                                    (string> (or (getf a :modified) "")
                                            (or (getf b :modified) ""))))
                        (otherwise (lambda (a b) (string< (getf a :name)
                                                         (getf b :name))))))))
    (with-output-to-string (s)
      (format s "Directory: ~A~%" path)
      (let ((files (count :file sorted :key (lambda (e) (getf e :type))))
            (dirs (count :directory sorted :key (lambda (e) (getf e :type)))))
        (format s "Total: ~D file~:P, ~D director~:@P~%~%" files dirs))
      (dolist (entry sorted)
        (let ((type (getf entry :type))
              (name (getf entry :name))
              (size (getf entry :size)))
          (format s "~A ~A~A~%"
                  (if (eq type :directory) "[DIR] " "[FILE]")
                  name
                  (if (and (eq type :file) size)
                      (format nil "~40T(~A)" (format-file-size size))
                      "")))))))

;;; ============================================================================
;;; list_directory Tool
;;; ============================================================================

(let ((tool (define-tool
              "list_directory"
              "List files and subdirectories in a directory. Returns names, types, and sizes.
               All paths are validated against the project root boundary."
              '((:name "path" :type :string :description "Directory path (relative to project root)")
                (:name "show_hidden" :type :boolean :description "Include hidden files (default false)")
                (:name "sort_by" :type :string :description "Sort by: name, size, modified (default: name)"))
              :required '("path")
              :safety-level :safe
              :categories '(:filesystem :navigation)
              :handler (lambda (args)
                         (block list-directory-handler
                           (let* ((path (gethash "path" args))
                                  (show-hidden (gethash "show_hidden" args))
                                  (sort-by (or (gethash "sort_by" args) "name"))
                                  (resolved (agent-q::resolve-project-path path)))
                             (unless resolved
                               (return-from list-directory-handler
                                 (format nil "Error: Path '~A' is outside project root (~A)"
                                        path (namestring (agent-q::ensure-project-root)))))
                             (handler-case
                                 (let ((entries (eval-in-emacs
                                                `(let* ((dir ,(namestring resolved))
                                                        (files (directory-files-and-attributes dir nil nil t)))
                                                   (cl-loop for (name . attrs) in files
                                                            unless (member name '("." "..") :test #'string=)
                                                            unless (and (not ,show-hidden)
                                                                       (string-prefix-p "." name))
                                                            collect (list :name name
                                                                         :type (if (eq (file-attribute-type attrs) t)
                                                                                  :directory :file)
                                                                         :size (file-attribute-size attrs)
                                                                         :modified (format-time-string
                                                                                   "%Y-%m-%d %H:%M"
                                                                                   (file-attribute-modification-time attrs))))))))
                                   (if (and (stringp entries) (search "Error" entries))
                                       entries
                                       (format-directory-listing (namestring resolved) entries sort-by)))
                               (error (e)
                                 (format nil "Error listing directory: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
