;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

;; ABOUTME: Session management for Agent-Q.
;; ABOUTME: Provides persistent session storage, serialization, and multi-session support.
;; ABOUTME: Sessions wrap conversations with metadata for persistence across Emacs restarts.

(in-package :agent-q)

;;; ============================================================================
;;; Session Class
;;; ============================================================================

(defclass session ()
  ((id :initarg :id
       :accessor session-id
       :initform (generate-session-id)
       :documentation "Unique session identifier (session-YYYYMMDD-HHMMSS-XXXX)")
   (name :initarg :name
         :accessor session-name
         :initform nil
         :documentation "User-friendly session name")
   (created-at :initarg :created-at
               :accessor session-created-at
               :initform (get-universal-time)
               :documentation "Universal time when session was created")
   (updated-at :initarg :updated-at
               :accessor session-updated-at
               :initform (get-universal-time)
               :documentation "Universal time of last modification")
   (conversation :initarg :conversation
                 :accessor session-conversation
                 :initform (make-instance 'conversation)
                 :documentation "The conversation object containing messages and context")
   (model :initarg :model
          :accessor session-model
          :initform nil
          :documentation "Model used for this session (e.g., claude-sonnet-4-20250514)")
   (metadata :initarg :metadata
             :accessor session-metadata
             :initform nil
             :documentation "Plist for extensible data: :provider, :total-input-tokens, :total-output-tokens, etc."))
  (:documentation "A session wraps a conversation with persistence metadata.
Sessions are saved to disk and can be restored across Emacs/Lisp restarts."))

(defun generate-session-id ()
  "Generate a unique session ID in format: session-YYYYMMDD-HHMMSS-XXXX"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "session-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D-~4,'0X"
            year month day hour min sec (random #xFFFF))))

(defun make-session (&key name model)
  "Create a new session with optional NAME and MODEL."
  (make-instance 'session :name name :model model))

(defun session-messages (session)
  "Get messages from the session's conversation."
  (conversation-messages (session-conversation session)))

(defun session-add-message (session role content)
  "Add a message to the session's conversation and update timestamp."
  (setf (session-updated-at session) (get-universal-time))
  (add-message (session-conversation session) role content))

(defun session-message-count (session)
  "Return the number of messages in the session."
  (length (session-messages session)))

(defun session-add-tokens (session input-tokens output-tokens)
  "Accumulate token usage in session metadata."
  (let ((meta (session-metadata session)))
    (setf (getf meta :total-input-tokens)
          (+ (or (getf meta :total-input-tokens) 0) (or input-tokens 0)))
    (setf (getf meta :total-output-tokens)
          (+ (or (getf meta :total-output-tokens) 0) (or output-tokens 0)))
    (setf (session-metadata session) meta)))

;;; ============================================================================
;;; Session Manager
;;; ============================================================================

(defclass session-manager ()
  ((sessions-directory :initarg :directory
                       :accessor sessions-directory
                       :initform (default-sessions-directory)
                       :documentation "Directory where session files are stored")
   (current-session :initarg :current
                    :accessor current-session
                    :initform nil
                    :documentation "The currently active session")
   (session-cache :initform (make-hash-table :test 'equal)
                  :accessor session-cache
                  :documentation "Cache of loaded sessions: session-id -> session"))
  (:documentation "Manages multiple sessions with persistence support."))

(defvar *session-manager* nil
  "Global session manager instance.")

(defun default-sessions-directory ()
  "Return default sessions directory: ~/.emacs.d/agent-q-sessions/"
  (merge-pathnames ".emacs.d/agent-q-sessions/"
                   (user-homedir-pathname)))

(defun ensure-session-manager ()
  "Ensure *session-manager* exists, creating if necessary."
  (unless *session-manager*
    (setf *session-manager* (make-instance 'session-manager)))
  *session-manager*)

(defun ensure-sessions-directory (&optional (manager (ensure-session-manager)))
  "Ensure the sessions directory exists."
  (let ((dir (sessions-directory manager)))
    (ensure-directories-exist (merge-pathnames "dummy.txt" dir))
    dir))

;;; ============================================================================
;;; Serialization: Message
;;; ============================================================================

(defun message-to-plist (message)
  "Convert MESSAGE to a plist for storage."
  (list :role (message-role message)
        :content (message-content message)
        :timestamp (message-timestamp message)))

(defun plist-to-message (plist)
  "Reconstruct a message from PLIST."
  (make-instance 'message
                 :role (let ((role (getf plist :role)))
                         ;; Handle both symbol (v1) and keyword (v2) formats
                         (if (keywordp role)
                             role
                             (intern (string-upcase (string role)) :keyword)))
                 :content (getf plist :content)
                 :timestamp (or (getf plist :timestamp) (get-universal-time))))

;;; ============================================================================
;;; Serialization: Session
;;; ============================================================================

(defun session-to-plist (session)
  "Convert SESSION to a plist for storage.
Messages are stored oldest-first (natural reading order)."
  (list :version 2
        :id (session-id session)
        :name (session-name session)
        :created-at (session-created-at session)
        :updated-at (session-updated-at session)
        :model (session-model session)
        :metadata (session-metadata session)
        :messages (mapcar #'message-to-plist
                          (session-messages session))))

(defun plist-to-session (plist)
  "Reconstruct a session from PLIST.
Handles both v1 (Elisp) and v2 (CL) formats."
  (let ((version (or (getf plist :version) 1)))
    (case version
      (1 (convert-v1-to-session plist))
      (2 (convert-v2-to-session plist))
      (t (error "Unknown session format version: ~A" version)))))

(defun convert-v2-to-session (plist)
  "Convert v2 format (native CL) plist to session."
  (let* ((conv (make-instance 'conversation))
         (session (make-instance 'session
                                 :id (getf plist :id)
                                 :name (getf plist :name)
                                 :created-at (getf plist :created-at)
                                 :updated-at (getf plist :updated-at)
                                 :model (getf plist :model)
                                 :metadata (getf plist :metadata)
                                 :conversation conv)))
    ;; Restore messages (oldest-first on disk, same order in memory)
    (dolist (msg-plist (getf plist :messages))
      (let ((msg (plist-to-message msg-plist)))
        (setf (conversation-messages conv)
              (append (conversation-messages conv) (list msg)))))
    session))

(defun convert-v1-to-session (plist)
  "Convert v1 format (Elisp) plist to session.
Handles Emacs timestamps, symbol roles, etc."
  (let* ((conv (make-instance 'conversation))
         ;; v1 may use Emacs (HIGH LOW USEC PSEC) timestamps - convert to universal-time
         (created (convert-elisp-timestamp (getf plist :created-at)))
         (updated (convert-elisp-timestamp (getf plist :updated-at)))
         (session (make-instance 'session
                                 :id (getf plist :id)
                                 :name (getf plist :name)
                                 :created-at (or created (get-universal-time))
                                 :updated-at (or updated (get-universal-time))
                                 :model (getf plist :model)
                                 :metadata (getf plist :metadata)
                                 :conversation conv)))
    ;; v1 stores messages in reverse order (newest-first), so reverse them
    (dolist (msg-plist (reverse (getf plist :messages)))
      (let ((msg (plist-to-message-v1 msg-plist)))
        (setf (conversation-messages conv)
              (append (conversation-messages conv) (list msg)))))
    session))

(defun plist-to-message-v1 (plist)
  "Reconstruct a message from v1 (Elisp) format PLIST."
  (make-instance 'message
                 :role (let ((role (getf plist :role)))
                         ;; v1 uses symbols like 'user, 'assistant
                         (intern (string-upcase (string role)) :keyword))
                 :content (getf plist :content)
                 :timestamp (or (convert-elisp-timestamp (getf plist :timestamp))
                                (get-universal-time))))

(defun convert-elisp-timestamp (timestamp)
  "Convert an Elisp timestamp to universal-time.
Elisp timestamps are (HIGH LOW USEC PSEC) or just an integer.
Returns NIL if timestamp is NIL or unparseable."
  (cond
    ((null timestamp) nil)
    ((integerp timestamp) timestamp)  ; Already universal-time
    ((and (listp timestamp) (>= (length timestamp) 2))
     ;; Emacs timestamp: (HIGH LOW ...) where time = HIGH * 65536 + LOW
     ;; This is seconds since 1970-01-01, need to convert to universal-time
     ;; Universal time is seconds since 1900-01-01
     ;; Difference: 70 years = 2208988800 seconds
     (let* ((high (first timestamp))
            (low (second timestamp))
            (unix-time (+ (* high 65536) low))
            (unix-to-universal 2208988800))
       (+ unix-time unix-to-universal)))
    (t nil)))

;;; ============================================================================
;;; Persistence: Save
;;; ============================================================================

(defun save-session (session &optional (manager (ensure-session-manager)))
  "Save SESSION to disk as a readable S-expression file."
  (let* ((dir (ensure-sessions-directory manager))
         (filename (format nil "~A.lisp" (session-id session)))
         (filepath (merge-pathnames filename dir)))
    ;; Update timestamp
    (setf (session-updated-at session) (get-universal-time))
    ;; Write file
    (with-open-file (stream filepath
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      ;; Write header comments for fast metadata extraction
      (format stream ";;; -*- Mode: LISP; Syntax: COMMON-LISP -*-~%")
      (format stream ";;; Agent-Q Session v2~%")
      (format stream ";;; Created: ~A~%" (format-timestamp (session-created-at session)))
      (when (session-name session)
        (format stream ";;; Name: ~A~%" (session-name session)))
      (format stream "~%")
      ;; Write session data
      (let ((*print-pretty* t)
            (*print-right-margin* 100)
            (*print-case* :downcase))
        (prin1 (session-to-plist session) stream))
      (terpri stream))
    ;; Update cache
    (setf (gethash (session-id session) (session-cache manager)) session)
    filepath))

(defun format-timestamp (universal-time)
  "Format UNIVERSAL-TIME as YYYY-MM-DD HH:MM:SS."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))

;;; ============================================================================
;;; Persistence: Load
;;; ============================================================================

(defun load-session (session-id &optional (manager (ensure-session-manager)))
  "Load session by SESSION-ID from disk. Returns NIL if not found."
  ;; Check cache first
  (let ((cached (gethash session-id (session-cache manager))))
    (when cached
      (return-from load-session cached)))
  ;; Load from file
  (let* ((dir (sessions-directory manager))
         (filename (format nil "~A.lisp" session-id))
         (filepath (merge-pathnames filename dir)))
    (when (probe-file filepath)
      (handler-case
          (with-open-file (stream filepath :direction :input)
            ;; Skip header comments and blank lines
            ;; Use peek-char to check for comment start without reading full line
            (loop for char = (peek-char t stream nil nil)
                  while (and char (char= char #\;))
                  do (read-line stream nil nil))
            (let ((plist (read stream nil nil)))
              (when plist
                (let ((session (plist-to-session plist)))
                  (setf (gethash session-id (session-cache manager)) session)
                  session))))
        (error (e)
          (warn "Failed to load session ~A: ~A" session-id e)
          nil)))))

;;; ============================================================================
;;; Persistence: Delete
;;; ============================================================================

(defun delete-session (session-id &optional (manager (ensure-session-manager)))
  "Delete session by SESSION-ID from disk and cache. Returns T if deleted."
  (let* ((dir (sessions-directory manager))
         (filename (format nil "~A.lisp" session-id))
         (filepath (merge-pathnames filename dir)))
    ;; Remove from cache
    (remhash session-id (session-cache manager))
    ;; Delete file
    (when (probe-file filepath)
      (delete-file filepath)
      t)))

;;; ============================================================================
;;; Persistence: List Sessions
;;; ============================================================================

(defun list-sessions (&optional (manager (ensure-session-manager)))
  "List all sessions as plists with :id, :name, :created-at, :message-count.
Returns sessions sorted by updated-at (most recent first).
Uses fast metadata extraction from file headers when possible."
  (let* ((dir (sessions-directory manager))
         (pattern (merge-pathnames "session-*.lisp" dir))
         (files (directory pattern))
         (sessions nil))
    (dolist (filepath files)
      (let ((meta (read-session-metadata-fast filepath)))
        (when meta
          (push meta sessions))))
    ;; Sort by created-at descending (most recent first)
    (sort sessions #'> :key (lambda (s) (or (getf s :created-at) 0)))))

(defun read-session-metadata-fast (filepath)
  "Read just the metadata from a session file's header comments.
This is faster than loading the full session for listing purposes."
  (handler-case
      (with-open-file (stream filepath :direction :input)
        (let ((id (pathname-name filepath))
              (name nil)
              (created nil))
          ;; Parse header comments - check length before accessing char
          (loop for line = (read-line stream nil nil)
                while (and line
                           (> (length line) 0)
                           (char= (char line 0) #\;))
                do (cond
                     ((search "Name: " line)
                      (setf name (subseq line (+ (search "Name: " line) 6))))
                     ((search "Created: " line)
                      (setf created (parse-timestamp-string
                                     (subseq line (+ (search "Created: " line) 9)))))))
          (list :id id
                :name name
                :created-at created)))
    (error () nil)))

(defun parse-timestamp-string (str)
  "Parse a timestamp string YYYY-MM-DD HH:MM:SS to universal-time.
Returns NIL on parse failure."
  (handler-case
      (let ((year (parse-integer str :start 0 :end 4))
            (month (parse-integer str :start 5 :end 7))
            (day (parse-integer str :start 8 :end 10))
            (hour (parse-integer str :start 11 :end 13))
            (min (parse-integer str :start 14 :end 16))
            (sec (parse-integer str :start 17 :end 19)))
        (encode-universal-time sec min hour day month year))
    (error () nil)))

;;; ============================================================================
;;; Persistence: Search Sessions
;;; ============================================================================

(defun search-sessions (query &optional (manager (ensure-session-manager)))
  "Search sessions by name and message content.
QUERY is a string to search for (case-insensitive).
Returns list of session metadata plists for matching sessions."
  (let* ((query-lower (string-downcase query))
         (all-sessions (list-sessions manager))
         (matches nil))
    (dolist (meta all-sessions)
      (let ((session-id (getf meta :id)))
        ;; Check name first (fast)
        (if (and (getf meta :name)
                 (search query-lower (string-downcase (getf meta :name))))
            (push meta matches)
            ;; Check message content (requires loading session)
            (let ((session (load-session session-id manager)))
              (when (and session (session-contains-query-p session query-lower))
                (push meta matches))))))
    (nreverse matches)))

(defun session-contains-query-p (session query-lower)
  "Check if SESSION contains QUERY-LOWER in any message content."
  (some (lambda (msg)
          (search query-lower (string-downcase (message-content msg))))
        (session-messages session)))

;;; ============================================================================
;;; Session Manager Operations
;;; ============================================================================

(defun switch-session (session-id &optional (manager (ensure-session-manager)))
  "Switch to session by SESSION-ID, saving current session first.
Returns the new current session, or NIL if not found."
  ;; Save current session
  (when (current-session manager)
    (save-session (current-session manager) manager))
  ;; Load and switch to new session
  (let ((session (load-session session-id manager)))
    (when session
      (setf (current-session manager) session))
    session))

(defun create-session (&key name model (manager (ensure-session-manager)))
  "Create a new session, save current first, and make it current.
Returns the new session."
  ;; Save current session
  (when (current-session manager)
    (save-session (current-session manager) manager))
  ;; Create new session
  (let ((session (make-session :name name :model model)))
    (setf (current-session manager) session)
    (setf (gethash (session-id session) (session-cache manager)) session)
    session))

(defun ensure-current-session (&optional (manager (ensure-session-manager)))
  "Ensure there is a current session, creating one if necessary."
  (unless (current-session manager)
    (create-session :manager manager))
  (current-session manager))
