;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q.TOOLS; Base: 10 -*-
;; ABOUTME: Filesystem tools for directory listing, file info, and path operations.
;; ABOUTME: All paths validated against *project-root* for security.

(in-package :agent-q.tools)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun count-substring (needle haystack)
  "Count non-overlapping occurrences of NEEDLE in HAYSTACK.
   Matching is exact (whitespace-sensitive)."
  (let ((count 0)
        (pos 0)
        (needle-len (length needle)))
    (when (> needle-len 0)
      (loop
        (let ((found (search needle haystack :start2 pos)))
          (unless found (return))
          (incf count)
          (setf pos (+ found needle-len)))))
    count))

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

;;; ============================================================================
;;; get_file_info Tool
;;; ============================================================================

(let ((tool (define-tool
              "get_file_info"
              "Get detailed information about a file or directory including size, timestamps, and permissions."
              '((:name "path" :type :string :description "File or directory path (relative to project root)"))
              :required '("path")
              :safety-level :safe
              :categories '(:filesystem :navigation)
              :handler (lambda (args)
                         (block get-file-info-handler
                           (let* ((path (gethash "path" args))
                                  (resolved (agent-q::resolve-project-path path)))
                             (unless resolved
                               (return-from get-file-info-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))
                             (handler-case
                                 (let ((info (eval-in-emacs
                                             `(let* ((path ,(namestring resolved))
                                                     (attrs (file-attributes path)))
                                                (when attrs
                                                  (list :path path
                                                       :type (if (eq (file-attribute-type attrs) t)
                                                                :directory :file)
                                                       :size (file-attribute-size attrs)
                                                       :modified (format-time-string
                                                                 "%Y-%m-%d %H:%M:%S"
                                                                 (file-attribute-modification-time attrs))
                                                       :accessed (format-time-string
                                                                 "%Y-%m-%d %H:%M:%S"
                                                                 (file-attribute-access-time attrs))
                                                       :permissions (file-attribute-modes attrs)
                                                       :readable (file-readable-p path)
                                                       :writable (file-writable-p path)))))))
                                   (if info
                                       (with-output-to-string (s)
                                         (format s "File: ~A~%~%" (getf info :path))
                                         (format s "Type: ~A~%" (getf info :type))
                                         (format s "Size: ~A~%" (format-file-size (getf info :size)))
                                         (format s "Modified: ~A~%" (getf info :modified))
                                         (format s "Accessed: ~A~%" (getf info :accessed))
                                         (format s "Readable: ~A~%" (if (getf info :readable) "Yes" "No"))
                                         (format s "Writable: ~A~%" (if (getf info :writable) "Yes" "No")))
                                       (format nil "Error: File '~A' not found" path)))
                               (error (e)
                                 (format nil "Error getting file info: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))

;;; ============================================================================
;;; get_project_root Tool
;;; ============================================================================
;;; ABOUTME: Reports the current project root directory and how it was detected.
;;; Useful for the LLM to understand its working context and file boundaries.

(let ((tool (define-tool
              "get_project_root"
              "Get the current project root directory and how it was determined.
               The project root is the security boundary for all file operations."
              '()
              :required '()
              :safety-level :safe
              :categories '(:filesystem :configuration)
              :handler (lambda (args)
                         (declare (ignore args))
                         (let* ((root (agent-q::ensure-project-root))
                                (method (cond
                                          ;; Check if explicitly configured
                                          ((and (boundp 'agent-q:*project-root*)
                                                (not (null (symbol-value 'agent-q:*project-root*)))
                                                ;; Check if it was set before we called ensure-project-root
                                                ;; by comparing to detect-project-root result
                                                (not (equal root (agent-q::detect-project-root))))
                                           "Explicitly configured")
                                          ;; Check if from git root
                                          ((agent-q::find-git-root *default-pathname-defaults*)
                                           "Git repository root (.git directory)")
                                          ;; Check if from ASDF
                                          ((ignore-errors (asdf:system-source-directory :agent-q))
                                           "ASDF system directory")
                                          ;; Otherwise default
                                          (t "Default directory"))))
                           (format nil "Project root: ~A~%~%Detection method: ~A"
                                   (namestring root) method))))))
  (register-tool *agent-q-registry* tool))
