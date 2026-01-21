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

(defun matches-exclusion-p (name exclusions)
  "Check if NAME matches any glob pattern in EXCLUSIONS.
   Supported patterns:
     - Exact match: \"node_modules\", \".git\"
     - Suffix match: \"*.fasl\", \"*.o\"
     - Prefix match: \"temp*\", \"TODO*\"
     - Match all: \"*\" (matches everything, use with caution)"
  (when exclusions
    (some (lambda (pattern)
            (cond
              ;; Exact match
              ((not (find #\* pattern))
               (string= name pattern))
              ;; Suffix match (*.fasl)
              ((and (> (length pattern) 0)
                    (char= (char pattern 0) #\*)
                    (not (find #\* pattern :start 1)))
               (let ((suffix (subseq pattern 1)))
                 (and (>= (length name) (length suffix))
                      (string= suffix name :start2 (- (length name) (length suffix))))))
              ;; Prefix match (TODO*)
              ((and (> (length pattern) 0)
                    (char= (char pattern (1- (length pattern))) #\*)
                    (not (find #\* pattern :end (1- (length pattern)))))
               (let ((prefix (subseq pattern 0 (1- (length pattern)))))
                 (and (>= (length name) (length prefix))
                      (string= prefix name :end2 (length prefix)))))
              ;; Default: no match for complex patterns
              (t nil)))
          exclusions)))

(defun build-directory-tree (path exclusions)
  "Build a recursive directory tree structure.
   EXCLUSIONS is a list of glob patterns to skip."
  (handler-case
      (let ((entries (eval-in-emacs
                      `(let* ((dir ,path)
                              (files (directory-files-and-attributes dir nil nil t)))
                         (cl-loop for (name . attrs) in files
                                  unless (member name '("." "..") :test #'string=)
                                  collect (list :name name
                                               :type (if (eq (file-attribute-type attrs) t)
                                                        :directory :file)
                                               :size (file-attribute-size attrs)))))))
        ;; Check if eval-in-emacs returned an error string
        (when (stringp entries)
          (return-from build-directory-tree (list :error entries)))

        (let ((filtered (remove-if (lambda (entry)
                                    (matches-exclusion-p (getf entry :name) exclusions))
                                  entries)))
          (list :name (file-namestring (pathname path))
                :type :directory
                :children (mapcar (lambda (entry)
                                   (if (eq (getf entry :type) :directory)
                                       ;; Recurse into subdirectories
                                       (build-directory-tree
                                        (merge-pathnames
                                         (concatenate 'string (getf entry :name) "/")
                                         path)
                                        exclusions)
                                       ;; Files are leaf nodes
                                       entry))
                                 filtered))))
    (error (e)
      (list :error (format nil "~A" e)))))

(defun format-directory-tree (tree &optional (indent 0))
  "Format directory tree as indented text for LLM consumption."
  (if (getf tree :error)
      (format nil "Error: ~A" (getf tree :error))
      (with-output-to-string (s)
        (let ((spaces (make-string indent :initial-element #\Space))
              (name (getf tree :name))
              (type (getf tree :type))
              (children (getf tree :children)))

          ;; Print current node
          (format s "~A~A~A~%"
                  spaces
                  (if (eq type :directory) "[DIR] " "[FILE] ")
                  name)

          ;; Recurse for children
          (when children
            (dolist (child children)
              (if (eq (getf child :type) :directory)
                  (format s "~A" (format-directory-tree child (+ indent 2)))
                  (let ((child-name (getf child :name))
                        (child-size (getf child :size)))
                    (format s "~A[FILE] ~A~@[ (~A)~]~%"
                            (make-string (+ indent 2) :initial-element #\Space)
                            child-name
                            (when child-size (format-file-size child-size)))))))))))

(defun glob-matches-p (pattern filename)
  "Check if FILENAME matches glob PATTERN.
   Supports: * (any chars), ** (recursive), ? (single char)."
  (cond
    ;; Empty pattern protection
    ((= (length pattern) 0)
     (= (length filename) 0))

    ;; Exact match (no wildcards)
    ((not (or (find #\* pattern) (find #\? pattern)))
     (string= pattern filename))

    ;; Recursive wildcard **/*.ext
    ;; Matches files in ANY subdirectory with the suffix pattern
    ;; Example: "**/*.lisp" matches "src/foo.lisp", "a/b/c.lisp", "test.lisp"
    ((search "**/" pattern)
     (let* ((pos (search "**/" pattern))
            (suffix-pattern (subseq pattern (+ pos 3)))) ; Skip past "**/"
       (and suffix-pattern
            (> (length suffix-pattern) 0)
            ;; For **, just check if the suffix pattern matches the basename
            ;; This way **/*.lisp matches any file ending in .lisp regardless of depth
            (glob-matches-p suffix-pattern
                           (let ((slash-pos (position #\/ filename :from-end t)))
                             (if slash-pos
                                 (subseq filename (1+ slash-pos))
                                 filename))))))

    ;; Simple suffix match *.ext
    ((and (> (length pattern) 0)
          (char= (char pattern 0) #\*)
          (not (find #\* pattern :start 1))
          (not (find #\? pattern)))
     (let ((suffix (subseq pattern 1)))
       (and (>= (length filename) (length suffix))
            (string= suffix filename :start2 (- (length filename) (length suffix))))))

    ;; Simple prefix match prefix*
    ((and (> (length pattern) 0)
          (char= (char pattern (1- (length pattern))) #\*)
          (not (find #\* pattern :end (1- (length pattern))))
          (not (find #\? pattern)))
     (let ((prefix (subseq pattern 0 (1- (length pattern)))))
       (and (>= (length filename) (length prefix))
            (string= prefix filename :end2 (length prefix)))))

    ;; Question mark (single char)
    ((find #\? pattern)
     ;; ? matches any single character
     (and (= (length pattern) (length filename))
          (every (lambda (pc fc)
                  (or (char= pc #\?)      ; ? matches any single char
                      (char= pc fc)))     ; exact match for non-wildcard chars
                pattern filename)))

    ;; Default: no match
    (t nil)))

(defun search-files-recursively (path pattern exclusions)
  "Recursively search for files matching PATTERN under PATH.
   Returns list of relative paths."
  (let ((results '()))
    (labels ((walk (dir prefix)
               (handler-case
                   (let ((entries (eval-in-emacs
                                  `(let* ((dir ,dir)
                                          (files (directory-files-and-attributes dir nil nil t)))
                                     (cl-loop for (name . attrs) in files
                                              unless (member name '("." "..") :test #'string=)
                                              collect (list :name name
                                                           :type (if (eq (file-attribute-type attrs) t)
                                                                    :directory :file)))))))
                     (dolist (entry entries)
                       (let* ((name (getf entry :name))
                              (type (getf entry :type))
                              (rel-path (if prefix
                                           (concatenate 'string prefix "/" name)
                                           name)))
                         ;; Skip if matches exclusion
                         (unless (matches-exclusion-p name exclusions)
                           (if (eq type :directory)
                               ;; Recurse into subdirectory
                               (walk (merge-pathnames
                                     (concatenate 'string name "/")
                                     dir)
                                    rel-path)
                               ;; Check if file matches pattern
                               (when (glob-matches-p pattern rel-path)
                                 (push rel-path results)))))))
                 (error (e)
                   ;; Silently skip directories we can't read
                   nil))))
      (walk path nil))
    (nreverse results)))

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

;;; ============================================================================
;;; directory_tree Tool
;;; ============================================================================
;;; ABOUTME: Shows recursive directory structure with optional exclusions.
;;; Useful for understanding project layout and navigating large codebases.

(let ((tool (define-tool
              "directory_tree"
              "Get a recursive directory tree showing the hierarchical structure.
               Useful for understanding project layout. Can exclude patterns like .git, *.fasl."
              '((:name "path" :type :string :description "Directory path (relative to project root, default: '.')")
                (:name "exclude_patterns" :type :array :description "List of glob patterns to exclude (e.g., ['.git', '*.fasl'])"))
              :required '()
              :safety-level :safe
              :categories '(:filesystem :navigation)
              :handler (lambda (args)
                         (block directory-tree-handler
                           (let* ((path (or (gethash "path" args) "."))
                                  (exclusions (gethash "exclude_patterns" args))
                                  (resolved (agent-q::resolve-project-path path)))
                             (unless resolved
                               (return-from directory-tree-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (let* ((tree (build-directory-tree (namestring resolved) exclusions))
                                        (formatted (format-directory-tree tree)))
                                   (with-output-to-string (s)
                                     (format s "Directory tree: ~A~%~%" (namestring resolved))
                                     (when exclusions
                                       (format s "Excluding: ~{~A~^, ~}~%~%" exclusions))
                                     (format s "~A" formatted)))
                               (error (e)
                                 (format nil "Error building directory tree: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))

;;; ============================================================================
;;; search_files Tool
;;; ============================================================================
;;; ABOUTME: Search for files matching a glob pattern. Exposes file search
;;; functionality to the LLM with support for * and ** wildcards and exclusions.

(let ((tool (define-tool
              "search_files"
              "Search for files matching a glob pattern. Supports * (wildcard) and ** (recursive).
               Examples: '*.lisp' finds all Lisp files, '**/*.md' finds markdown files recursively."
              '((:name "pattern" :type :string :description "Glob pattern (e.g., '*.lisp', '**/*.md')")
                (:name "path" :type :string :description "Starting directory (relative to project root, default: '.')")
                (:name "exclude_patterns" :type :array :description "Patterns to exclude (e.g., ['.git', '*.fasl'])"))
              :required '("pattern")
              :safety-level :safe
              :categories '(:filesystem :search)
              :handler (lambda (args)
                         (block search-files-handler
                           (let* ((pattern (gethash "pattern" args))
                                  (path (or (gethash "path" args) "."))
                                  (exclusions (gethash "exclude_patterns" args))
                                  (resolved (agent-q::resolve-project-path path)))
                             (unless resolved
                               (return-from search-files-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (let ((results (search-files-recursively
                                                (namestring resolved) pattern exclusions)))
                                   (if results
                                       (with-output-to-string (s)
                                         (format s "Found ~D file~:P matching '~A':~%~%"
                                                (length results) pattern)
                                         (dolist (file results)
                                           (format s "  ~A~%" file)))
                                       (format nil "No files found matching pattern '~A'" pattern)))
                               (error (e)
                                 (format nil "Error searching files: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))

;;; ============================================================================
;;; File Content Helpers
;;; ============================================================================
;;; ABOUTME: Helpers for reading, writing, and diffing file contents.
;;; Used by edit_file tool for str_replace semantics.

(defun read-file-content (path)
  "Read file content via Emacs."
  (eval-in-emacs
   `(with-temp-buffer
      (insert-file-contents ,path)
      (buffer-string))))

(defun write-file-content (path content)
  "Write content to file via Emacs, syncing any open buffers."
  (eval-in-emacs
   `(let ((buf (find-buffer-visiting ,path)))
      (if buf
          ;; File is open - modify the buffer
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert ,content)
              (save-buffer)))
        ;; File not open - write directly
        (with-temp-file ,path
          (insert ,content)))
      t)))

(defun generate-mini-diff (old-str new-str)
  "Generate a minimal diff display for the change."
  (with-output-to-string (s)
    (format s "~%Diff:~%")
    (dolist (line (uiop:split-string old-str :separator '(#\Newline)))
      (format s "  - ~A~%" line))
    (dolist (line (uiop:split-string new-str :separator '(#\Newline)))
      (format s "  + ~A~%" line))))

(defun truncate-for-display (str max-len)
  "Truncate string for display if too long."
  (if (> (length str) max-len)
      (concatenate 'string (subseq str 0 (- max-len 3)) "...")
      str))

(defun literal-string-replace (old-str new-str content)
  "Replace OLD-STR with NEW-STR in CONTENT using literal (non-regex) matching.
   Only replaces the first occurrence."
  (let ((pos (search old-str content)))
    (when pos
      (concatenate 'string
                   (subseq content 0 pos)
                   new-str
                   (subseq content (+ pos (length old-str)))))))

;;; ============================================================================
;;; edit_file Tool
;;; ============================================================================
;;; ABOUTME: Core editing tool implementing str_replace semantics.
;;; Requires exact string matching with unique occurrences for safety.

(let ((tool (define-tool
              "edit_file"
              "Make targeted edits to a file using exact string replacement.
               The old_str must match EXACTLY once in the file.
               For multiple changes, call multiple times.
               WARNING: This modifies files. Changes are logged."
              '((:name "path" :type :string :description "File path (relative to project root)")
                (:name "old_str" :type :string :description "Exact string to find (must be unique)")
                (:name "new_str" :type :string :description "Replacement string")
                (:name "description" :type :string :description "What this change does (for logging)"))
              :required '("path" "old_str" "new_str")
              :safety-level :moderate
              :categories '(:filesystem :editing)
              :handler (lambda (args)
                         (block edit-file-handler
                           (let* ((path (gethash "path" args))
                                  (old-str (gethash "old_str" args))
                                  (new-str (gethash "new_str" args))
                                  (description (gethash "description" args))
                                  (resolved (agent-q::resolve-project-path path)))
                             ;; Validate path
                             (unless resolved
                               (return-from edit-file-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (let* ((content (read-file-content (namestring resolved)))
                                        (match-count (count-substring old-str content)))
                                   (cond
                                     ;; No matches
                                     ((zerop match-count)
                                      (format nil "Error: String not found in ~A~%~%Searched for:~%~A"
                                             path (truncate-for-display old-str 200)))

                                     ;; Multiple matches - need more context
                                     ((> match-count 1)
                                      (format nil "Error: Found ~D matches in ~A.~%~%~
                                                  Please provide more surrounding context to make a unique match."
                                             match-count path))

                                     ;; Exactly one match - apply edit
                                     (t
                                      (let* ((new-content (literal-string-replace old-str new-str content))
                                             (diff (generate-mini-diff old-str new-str)))
                                        ;; Write via Emacs
                                        (write-file-content (namestring resolved) new-content)
                                        ;; Return confirmation
                                        (format nil "Edit applied to ~A~%~%~@[Change: ~A~%~]~A"
                                               path description diff)))))
                               (error (e)
                                 (format nil "Error editing file: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))

;;; ============================================================================
;;; insert_at_line Helper Functions
;;; ============================================================================

(defun insert-content-at-line (lines content line-number)
  "Insert CONTENT at LINE-NUMBER in LINES (list of strings).
   Line numbering: 0 = beginning, -1 = end, 1-indexed otherwise.
   Returns new list of lines, or NIL if line-number invalid."
  (let ((total-lines (length lines)))
    (cond
      ;; Line 0: beginning of file
      ((= line-number 0)
       (cons content lines))

      ;; Line -1: end of file
      ((= line-number -1)
       (append lines (list content)))

      ;; Positive line number: 1-indexed insertion
      ((and (> line-number 0) (<= line-number total-lines))
       (let ((before (subseq lines 0 line-number))
             (after (subseq lines line-number)))
         (append before (list content) after)))

      ;; Invalid line number
      (t nil))))

;;; ============================================================================
;;; create_file Tool
;;; ============================================================================
;;; ABOUTME: Create new files with content. Prevents overwriting existing files
;;; unless explicitly requested. Creates parent directories if needed.

(let ((tool (define-tool
              "create_file"
              "Create a new file with the specified content. By default, fails if file exists.
               Use allow_overwrite to replace existing files.
               WARNING: This creates files. Changes are logged."
              '((:name "path" :type :string :description "File path (relative to project root)")
                (:name "content" :type :string :description "File content to write")
                (:name "allow_overwrite" :type :boolean :description "Allow overwriting existing file (default: false)")
                (:name "create_parents" :type :boolean :description "Create parent directories if needed (default: true)")
                (:name "description" :type :string :description "What this file is for (for logging)"))
              :required '("path" "content")
              :safety-level :moderate
              :categories '(:filesystem :editing)
              :handler (lambda (args)
                         (block create-file-handler
                           (let* ((path (gethash "path" args))
                                  (content (gethash "content" args))
                                  (allow-overwrite (gethash "allow_overwrite" args))
                                  (create-parents (if (nth-value 1 (gethash "create_parents" args))
                                                     (gethash "create_parents" args)
                                                     t)) ; Default to true
                                  (description (gethash "description" args))
                                  (resolved (agent-q::resolve-project-path path)))

                             ;; Validate path
                             (unless resolved
                               (return-from create-file-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (progn
                                   ;; Check if file exists
                                   (let ((exists (eval-in-emacs
                                                 `(file-exists-p ,(namestring resolved)))))
                                     (when (and exists (not allow-overwrite))
                                       (return-from create-file-handler
                                         (format nil "Error: File '~A' already exists.~%~%~
                                                     Use allow_overwrite: true to replace it."
                                                path))))

                                   ;; Create parent directories if needed
                                   (when create-parents
                                     (let ((parent-dir (directory-namestring resolved)))
                                       (eval-in-emacs
                                        `(make-directory ,parent-dir t))))

                                   ;; Write the file
                                   (write-file-content (namestring resolved) content)

                                   ;; Return confirmation
                                   (with-output-to-string (s)
                                     (format s "✓ Created file: ~A~%" path)
                                     (when description
                                       (format s "~%Purpose: ~A~%" description))
                                     (format s "~%Content size: ~A~%"
                                            (format-file-size (length content)))
                                     (let ((lines (1+ (count #\Newline content))))
                                       (format s "Lines: ~D~%" lines))))

                               (error (e)
                                 (format nil "Error creating file: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))

;;; ============================================================================
;;; move_file Tool
;;; ============================================================================
;;; ABOUTME: Move or rename files within the project. Prevents overwriting
;;; existing files unless explicitly requested. Updates open Emacs buffers.

(let ((tool (define-tool
              "move_file"
              "Move or rename a file. By default, fails if destination exists.
               Use allow_overwrite to replace existing files.
               WARNING: This moves files. Changes are logged."
              '((:name "source" :type :string :description "Source file path (relative to project root)")
                (:name "destination" :type :string :description "Destination file path (relative to project root)")
                (:name "allow_overwrite" :type :boolean :description "Allow overwriting existing file (default: false)")
                (:name "create_parents" :type :boolean :description "Create parent directories if needed (default: true)")
                (:name "description" :type :string :description "Why this file is being moved (for logging)"))
              :required '("source" "destination")
              :safety-level :moderate
              :categories '(:filesystem :editing)
              :handler (lambda (args)
                         (block move-file-handler
                           (let* ((source (gethash "source" args))
                                  (destination (gethash "destination" args))
                                  (allow-overwrite (gethash "allow_overwrite" args))
                                  (create-parents (if (nth-value 1 (gethash "create_parents" args))
                                                     (gethash "create_parents" args)
                                                     t)) ; Default to true
                                  (description (gethash "description" args))
                                  (resolved-source (agent-q::resolve-project-path source))
                                  (resolved-dest (agent-q::resolve-project-path destination)))

                             ;; Validate both paths
                             (unless resolved-source
                               (return-from move-file-handler
                                 (format nil "Error: Source path '~A' is outside project root" source)))

                             (unless resolved-dest
                               (return-from move-file-handler
                                 (format nil "Error: Destination path '~A' is outside project root" destination)))

                             (handler-case
                                 (progn
                                   ;; Check if source exists
                                   (let ((source-exists (eval-in-emacs
                                                        `(file-exists-p ,(namestring resolved-source)))))
                                     (unless source-exists
                                       (return-from move-file-handler
                                         (format nil "Error: Source file '~A' does not exist" source))))

                                   ;; Check if destination exists
                                   (let ((dest-exists (eval-in-emacs
                                                      `(file-exists-p ,(namestring resolved-dest)))))
                                     (when (and dest-exists (not allow-overwrite))
                                       (return-from move-file-handler
                                         (format nil "Error: Destination file '~A' already exists.~%~%~
                                                     Use allow_overwrite: true to replace it."
                                                destination))))

                                   ;; Create parent directories if needed
                                   (when create-parents
                                     (let ((parent-dir (directory-namestring resolved-dest)))
                                       (eval-in-emacs
                                        `(make-directory ,parent-dir t))))

                                   ;; Perform the move/rename (Emacs handles buffer updates)
                                   (eval-in-emacs
                                    `(let ((old-buf (find-buffer-visiting ,(namestring resolved-source))))
                                       (rename-file ,(namestring resolved-source)
                                                   ,(namestring resolved-dest)
                                                   ,allow-overwrite)
                                       ;; Update buffer if file was open
                                       (when old-buf
                                         (with-current-buffer old-buf
                                           (set-visited-file-name ,(namestring resolved-dest))
                                           (set-buffer-modified-p nil)))
                                       t))

                                   ;; Return confirmation
                                   (with-output-to-string (s)
                                     (format s "✓ Moved: ~A → ~A~%" source destination)
                                     (when description
                                       (format s "~%Reason: ~A~%" description))))

                               (error (e)
                                 (format nil "Error moving file: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))

;;; ============================================================================
;;; delete_file Tool
;;; ============================================================================
;;; ABOUTME: Delete files within the project. Requires confirmation by default
;;; to prevent accidental deletions. Closes associated Emacs buffers.

(let ((tool (define-tool
              "delete_file"
              "Delete a file from the project. This operation cannot be undone.
               WARNING: This permanently deletes files. Use with caution."
              '((:name "path" :type :string :description "File path to delete (relative to project root)")
                (:name "description" :type :string :description "Why this file is being deleted (for logging)"))
              :required '("path")
              :safety-level :moderate
              :categories '(:filesystem :editing)
              :handler (lambda (args)
                         (block delete-file-handler
                           (let* ((path (gethash "path" args))
                                  (description (gethash "description" args))
                                  (resolved (agent-q::resolve-project-path path)))

                             ;; Validate path
                             (unless resolved
                               (return-from delete-file-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (progn
                                   ;; Check if file exists
                                   (let ((exists (eval-in-emacs
                                                 `(file-exists-p ,(namestring resolved)))))
                                     (unless exists
                                       (return-from delete-file-handler
                                         (format nil "Error: File '~A' does not exist" path))))

                                   ;; Check if it's a directory
                                   (let ((is-dir (eval-in-emacs
                                                 `(file-directory-p ,(namestring resolved)))))
                                     (when is-dir
                                       (return-from delete-file-handler
                                         (format nil "Error: '~A' is a directory. Use a directory deletion tool instead." path))))

                                   ;; Get file info before deletion (for logging)
                                   (let ((file-size (eval-in-emacs
                                                    `(let ((attrs (file-attributes ,(namestring resolved))))
                                                       (when attrs
                                                         (file-attribute-size attrs))))))

                                     ;; Delete the file (Emacs handles buffer cleanup)
                                     (eval-in-emacs
                                      `(let ((buf (find-buffer-visiting ,(namestring resolved))))
                                         ;; Delete the file
                                         (delete-file ,(namestring resolved))
                                         ;; Kill associated buffer if exists
                                         (when buf
                                           (kill-buffer buf))
                                         t))

                                     ;; Return confirmation
                                     (with-output-to-string (s)
                                       (format s "✓ Deleted: ~A~%" path)
                                       (when description
                                         (format s "~%Reason: ~A~%" description))
                                       (when file-size
                                         (format s "~%Size freed: ~A~%" (format-file-size file-size))))))

                               (error (e)
                                 (format nil "Error deleting file: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))

;;; ============================================================================
;;; insert_at_line Tool
;;; ============================================================================

(let ((tool (define-tool
              "insert_at_line"
              "Insert content at a specific line number in a file. Line numbering is 1-indexed.
               Use line 0 for beginning of file, -1 for end of file.
               WARNING: This modifies files. Changes are logged."
              '((:name "path" :type :string :description "File path (relative to project root)")
                (:name "content" :type :string :description "Content to insert (can be multiple lines)")
                (:name "line" :type :integer :description "Line number (0=beginning, -1=end, 1-indexed otherwise)"))
              :required '("path" "content" "line")
              :safety-level :moderate
              :categories '(:filesystem :editing)
              :handler (lambda (args)
                         (block insert-at-line-handler
                           (let* ((path (gethash "path" args))
                                  (content (gethash "content" args))
                                  (line-num (gethash "line" args))
                                  (resolved (agent-q::resolve-project-path path)))

                             ;; Path safety check
                             (unless resolved
                               (return-from insert-at-line-handler
                                 (format nil "Error: Path '~A' is outside project root" path)))

                             (handler-case
                                 (progn
                                   ;; Read file content
                                   (let* ((file-content (eval-in-emacs
                                                         `(with-temp-buffer
                                                            (insert-file-contents ,(namestring resolved))
                                                            (buffer-string))))
                                          (lines (uiop:split-string file-content :separator '(#\Newline)))
                                          (updated-lines (insert-content-at-line lines content line-num)))

                                     (unless updated-lines
                                       (return-from insert-at-line-handler
                                         (format nil "Error: Invalid line number ~A (file has ~A lines)"
                                                 line-num (length lines))))

                                     ;; Write updated content back
                                     (let ((new-content (format nil "~{~A~^~%~}" updated-lines)))
                                       (eval-in-emacs
                                        `(with-temp-file ,(namestring resolved)
                                           (insert ,new-content)))

                                       ;; Return success message
                                       (with-output-to-string (s)
                                         (format s "✓ Inserted content at line ~A in ~A~%~%" line-num path)
                                         (format s "Content inserted: ~A~%" content)))))

                               (error (e)
                                 (format nil "Error inserting content: ~A" e)))))))))
  (register-tool *agent-q-registry* tool))
