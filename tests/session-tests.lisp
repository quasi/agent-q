;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q-TESTS; Base: 10 -*-

;; ABOUTME: Tests for session management functionality.
;; ABOUTME: Covers serialization, persistence, v1 format migration, and session manager operations.

(in-package :agent-q-tests)

;;;; Session Tests
;;;;
;;;; Tests for:
;;;; - Session creation and manipulation
;;;; - Message and session serialization
;;;; - File persistence (save, load, delete, list)
;;;; - V1 (Elisp) format migration
;;;; - Session manager operations

(def-suite session-tests
  :description "Tests for session management functionality")

(in-suite session-tests)

;;; ============================================================================
;;; Test Utilities
;;; ============================================================================

(defvar *test-sessions-dir* nil
  "Temporary directory for session tests.")

(defun make-temp-sessions-dir ()
  "Create a temporary directory for session testing."
  (let ((dir (merge-pathnames
              (format nil "agent-q-test-~A/" (random 100000))
              (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "dummy.txt" dir))
    dir))

(defun cleanup-temp-sessions-dir (dir)
  "Remove temporary test directory and all files."
  (when (and dir (probe-file dir))
    (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))

(defmacro with-temp-session-manager (&body body)
  "Execute BODY with a fresh temporary session manager."
  `(let* ((*test-sessions-dir* (make-temp-sessions-dir))
          (agent-q:*session-manager*
           (make-instance 'agent-q::session-manager
                          :directory *test-sessions-dir*)))
     (unwind-protect
          (progn ,@body)
       (cleanup-temp-sessions-dir *test-sessions-dir*)
       (setf agent-q:*session-manager* nil))))

;;; ============================================================================
;;; Session Creation Tests
;;; ============================================================================

(test session/create-basic
  "Test creating a basic session."
  (let ((session (agent-q:make-session)))
    (is (typep session 'agent-q::session))
    (is (stringp (agent-q:session-id session)))
    (is (search "session-" (agent-q:session-id session)))
    (is (null (agent-q:session-name session)))
    (is (integerp (agent-q:session-created-at session)))
    (is (typep (agent-q:session-conversation session) 'agent-q::conversation))))

(test session/create-with-name
  "Test creating a session with a name."
  (let ((session (agent-q:make-session :name "Test Session")))
    (is (string= "Test Session" (agent-q:session-name session)))))

(test session/create-with-model
  "Test creating a session with a model."
  (let ((session (agent-q:make-session :model "claude-sonnet-4-20250514")))
    (is (string= "claude-sonnet-4-20250514" (agent-q:session-model session)))))

(test session/add-message
  "Test adding messages to a session."
  (let ((session (agent-q:make-session)))
    (agent-q:session-add-message session :user "Hello")
    (agent-q:session-add-message session :assistant "Hi there!")

    (is (= 2 (agent-q:session-message-count session)))
    (let ((messages (agent-q:session-messages session)))
      (is (eq :user (agent-q::message-role (first messages))))
      (is (eq :assistant (agent-q::message-role (second messages)))))))

(test session/add-tokens
  "Test accumulating token usage in session."
  (let ((session (agent-q:make-session)))
    (agent-q:session-add-tokens session 100 50)
    (agent-q:session-add-tokens session 200 100)

    (let ((meta (agent-q:session-metadata session)))
      (is (= 300 (getf meta :total-input-tokens)))
      (is (= 150 (getf meta :total-output-tokens))))))

(test session/id-format
  "Test that session IDs follow the expected format."
  (let ((session (agent-q:make-session)))
    (is (cl-ppcre:scan "^session-\\d{8}-\\d{6}-[0-9A-Fa-f]{4}$"
                       (agent-q:session-id session)))))

;;; ============================================================================
;;; Message Serialization Tests
;;; ============================================================================

(test serialization/message-to-plist
  "Test message serialization to plist."
  (let* ((msg (make-instance 'agent-q::message
                             :role :user
                             :content "Hello world"
                             :timestamp 3945234600))
         (plist (agent-q:message-to-plist msg)))
    (is (eq :user (getf plist :role)))
    (is (string= "Hello world" (getf plist :content)))
    (is (= 3945234600 (getf plist :timestamp)))))

(test serialization/plist-to-message
  "Test message deserialization from plist."
  (let* ((plist '(:role :assistant :content "Response" :timestamp 3945234600))
         (msg (agent-q:plist-to-message plist)))
    (is (eq :assistant (agent-q::message-role msg)))
    (is (string= "Response" (agent-q::message-content msg)))
    (is (= 3945234600 (agent-q::message-timestamp msg)))))

(test serialization/message-roundtrip
  "Test message serialization roundtrip."
  (let* ((original (make-instance 'agent-q::message
                                  :role :user
                                  :content "Test message"))
         (plist (agent-q:message-to-plist original))
         (restored (agent-q:plist-to-message plist)))
    (is (eq (agent-q::message-role original) (agent-q::message-role restored)))
    (is (string= (agent-q::message-content original)
                 (agent-q::message-content restored)))))

;;; ============================================================================
;;; Session Serialization Tests
;;; ============================================================================

(test serialization/session-to-plist
  "Test session serialization to plist."
  (let* ((session (agent-q:make-session :name "Test" :model "claude-3")))
    (agent-q:session-add-message session :user "Hello")
    (agent-q:session-add-tokens session 100 50)

    (let ((plist (agent-q:session-to-plist session)))
      (is (= 2 (getf plist :version)))
      (is (string= "Test" (getf plist :name)))
      (is (string= "claude-3" (getf plist :model)))
      (is (= 1 (length (getf plist :messages))))
      (is (= 100 (getf (getf plist :metadata) :total-input-tokens))))))

(test serialization/plist-to-session-v2
  "Test session deserialization from v2 plist."
  (let* ((plist '(:version 2
                  :id "session-20250112-120000-ABCD"
                  :name "Test Session"
                  :created-at 3945234600
                  :updated-at 3945234800
                  :model "claude-3"
                  :metadata (:provider :anthropic :total-input-tokens 500)
                  :messages ((:role :user :content "Hello" :timestamp 3945234600))))
         (session (agent-q:plist-to-session plist)))
    (is (string= "session-20250112-120000-ABCD" (agent-q:session-id session)))
    (is (string= "Test Session" (agent-q:session-name session)))
    (is (string= "claude-3" (agent-q:session-model session)))
    (is (= 1 (agent-q:session-message-count session)))
    (is (= 500 (getf (agent-q:session-metadata session) :total-input-tokens)))))

(test serialization/session-roundtrip
  "Test session serialization roundtrip."
  (let* ((original (agent-q:make-session :name "Roundtrip Test")))
    (agent-q:session-add-message original :user "Message 1")
    (agent-q:session-add-message original :assistant "Response 1")
    (agent-q:session-add-tokens original 150 75)

    (let* ((plist (agent-q:session-to-plist original))
           (restored (agent-q:plist-to-session plist)))
      (is (string= (agent-q:session-id original) (agent-q:session-id restored)))
      (is (string= (agent-q:session-name original) (agent-q:session-name restored)))
      (is (= (agent-q:session-message-count original)
             (agent-q:session-message-count restored)))
      (is (= (getf (agent-q:session-metadata original) :total-input-tokens)
             (getf (agent-q:session-metadata restored) :total-input-tokens))))))

;;; ============================================================================
;;; V1 Format Migration Tests
;;; ============================================================================

(test migration/v1-symbol-roles
  "Test v1 format with symbol roles (user vs :user)."
  ;; v1 format stores messages in reverse order (newest-first)
  ;; So (:assistant :user) in v1 means assistant=newest, user=oldest
  ;; After reversal: user first, assistant second (chronological)
  (let* ((plist '(:version 1
                  :id "session-20250111-143000"
                  :name "Old Session"
                  :messages ((:role assistant :content "Hi")
                             (:role user :content "Hello"))))
         (session (agent-q:plist-to-session plist)))
    (let ((messages (agent-q:session-messages session)))
      ;; Roles should be converted to keywords
      ;; After reversal, user (oldest) should be first, assistant (newest) second
      (is (eq :user (agent-q::message-role (first messages))))
      (is (eq :assistant (agent-q::message-role (second messages)))))))

(test migration/v1-emacs-timestamps
  "Test v1 format with Emacs-style timestamps."
  ;; Emacs timestamp (25765 12345) = 25765 * 65536 + 12345 = 1688154345 unix
  ;; + 2208988800 = 3897143145 universal-time
  (let* ((plist '(:version 1
                  :id "session-test"
                  :created-at (25765 12345)
                  :messages ()))
         (session (agent-q:plist-to-session plist)))
    ;; Should have converted the timestamp
    (is (integerp (agent-q:session-created-at session)))
    (is (> (agent-q:session-created-at session) 0))))

(test migration/v1-reversed-messages
  "Test v1 format with messages in reversed order."
  ;; V1 stores newest-first, we need oldest-first
  (let* ((plist '(:version 1
                  :id "session-test"
                  :messages ((:role user :content "Third" :timestamp 3)
                             (:role user :content "Second" :timestamp 2)
                             (:role user :content "First" :timestamp 1))))
         (session (agent-q:plist-to-session plist)))
    (let ((messages (agent-q:session-messages session)))
      ;; Should be in chronological order now
      (is (string= "First" (agent-q::message-content (first messages))))
      (is (string= "Second" (agent-q::message-content (second messages))))
      (is (string= "Third" (agent-q::message-content (third messages)))))))

;;; ============================================================================
;;; Persistence Tests
;;; ============================================================================

(test persistence/save-session
  "Test saving a session to disk."
  (with-temp-session-manager
    (let ((session (agent-q:make-session :name "Save Test")))
      (agent-q:session-add-message session :user "Hello")

      (let ((filepath (agent-q:save-session session)))
        (is (probe-file filepath))
        ;; Check file contains expected content
        (let ((content (uiop:read-file-string filepath)))
          (is (search ":version 2" content))
          (is (search "Save Test" content)))))))

(test persistence/load-session
  "Test loading a session from disk."
  (with-temp-session-manager
    (let* ((session (agent-q:make-session :name "Load Test"))
           (session-id (agent-q:session-id session)))
      (agent-q:session-add-message session :user "Test message")
      (agent-q:save-session session)

      ;; Clear cache to force reload from disk
      (clrhash (agent-q::session-cache agent-q:*session-manager*))

      (let ((loaded (agent-q:load-session session-id)))
        (is (not (null loaded)))
        (is (string= "Load Test" (agent-q:session-name loaded)))
        (is (= 1 (agent-q:session-message-count loaded)))))))

(test persistence/save-load-roundtrip
  "Test save and load roundtrip preserves data."
  (with-temp-session-manager
    (let* ((session (agent-q:make-session :name "Roundtrip" :model "gpt-4")))
      (agent-q:session-add-message session :user "Message 1")
      (agent-q:session-add-message session :assistant "Response 1")
      (agent-q:session-add-tokens session 200 100)

      (agent-q:save-session session)
      (clrhash (agent-q::session-cache agent-q:*session-manager*))

      (let ((loaded (agent-q:load-session (agent-q:session-id session))))
        (is (string= "Roundtrip" (agent-q:session-name loaded)))
        (is (string= "gpt-4" (agent-q:session-model loaded)))
        (is (= 2 (agent-q:session-message-count loaded)))
        (is (= 200 (getf (agent-q:session-metadata loaded) :total-input-tokens)))))))

(test persistence/delete-session
  "Test deleting a session from disk."
  (with-temp-session-manager
    (let* ((session (agent-q:make-session :name "Delete Test"))
           (session-id (agent-q:session-id session)))
      (agent-q:save-session session)

      ;; Verify file exists
      (let ((filepath (merge-pathnames
                       (format nil "~A.lisp" session-id)
                       *test-sessions-dir*)))
        (is (probe-file filepath))

        ;; Delete
        (is-true (agent-q:delete-session session-id))

        ;; Verify file is gone
        (is (null (probe-file filepath)))

        ;; Verify removed from cache
        (is (null (gethash session-id
                           (agent-q::session-cache agent-q:*session-manager*))))))))

(test persistence/list-sessions
  "Test listing sessions from disk."
  (with-temp-session-manager
    ;; Create multiple sessions
    (let ((s1 (agent-q:make-session :name "Session A"))
          (s2 (agent-q:make-session :name "Session B"))
          (s3 (agent-q:make-session :name "Session C")))
      (agent-q:save-session s1)
      (agent-q:save-session s2)
      (agent-q:save-session s3)

      (let ((sessions (agent-q:list-sessions)))
        (is (= 3 (length sessions)))
        ;; Each should have :id, :name, :created-at
        (dolist (meta sessions)
          (is (getf meta :id))
          (is (getf meta :name)))))))

(test persistence/search-sessions-by-name
  "Test searching sessions by name."
  (with-temp-session-manager
    (let ((s1 (agent-q:make-session :name "Alpha Project"))
          (s2 (agent-q:make-session :name "Beta Testing"))
          (s3 (agent-q:make-session :name "Alpha Release")))
      (agent-q:save-session s1)
      (agent-q:save-session s2)
      (agent-q:save-session s3)

      (let ((results (agent-q:search-sessions "Alpha")))
        (is (= 2 (length results)))))))

(test persistence/search-sessions-by-content
  "Test searching sessions by message content."
  (with-temp-session-manager
    (let ((s1 (agent-q:make-session :name "Session 1"))
          (s2 (agent-q:make-session :name "Session 2")))
      (agent-q:session-add-message s1 :user "How do I fix the authentication bug?")
      (agent-q:session-add-message s2 :user "What is the weather today?")
      (agent-q:save-session s1)
      (agent-q:save-session s2)

      (let ((results (agent-q:search-sessions "authentication")))
        (is (= 1 (length results)))
        (is (string= "Session 1" (getf (first results) :name)))))))

;;; ============================================================================
;;; Session Manager Tests
;;; ============================================================================

(test manager/ensure-session-manager
  "Test ensuring session manager exists."
  (let ((agent-q:*session-manager* nil))
    (unwind-protect
         (progn
           (agent-q:ensure-session-manager)
           (is (typep agent-q:*session-manager* 'agent-q::session-manager)))
      (setf agent-q:*session-manager* nil))))

(test manager/create-session
  "Test creating a session through the manager."
  (with-temp-session-manager
    (let ((session (agent-q:create-session :name "Manager Test")))
      (is (not (null session)))
      (is (string= "Manager Test" (agent-q:session-name session)))
      ;; Should be the current session
      (is (eq session (agent-q:current-session agent-q:*session-manager*))))))

(test manager/switch-session
  "Test switching between sessions."
  (with-temp-session-manager
    (let* ((s1 (agent-q:create-session :name "Session 1"))
           (s1-id (agent-q:session-id s1)))
      (agent-q:session-add-message s1 :user "Message in S1")

      ;; Create second session (switches automatically)
      (let* ((s2 (agent-q:create-session :name "Session 2"))
             (s2-id (agent-q:session-id s2)))
        (agent-q:session-add-message s2 :user "Message in S2")

        ;; Current should be S2
        (is (eq s2 (agent-q:current-session agent-q:*session-manager*)))

        ;; Switch back to S1
        (agent-q:switch-session s1-id)

        ;; Current should be S1
        (is (string= s1-id
                     (agent-q:session-id
                      (agent-q:current-session agent-q:*session-manager*))))))))

(test manager/auto-save-on-switch
  "Test that switching sessions auto-saves the current one."
  (with-temp-session-manager
    (let* ((s1 (agent-q:create-session :name "Auto-Save Test")))
      (agent-q:session-add-message s1 :user "This should be saved")

      ;; Switch to new session (should save S1)
      (agent-q:create-session :name "New Session")

      ;; Clear cache and reload S1
      (clrhash (agent-q::session-cache agent-q:*session-manager*))
      (let ((loaded (agent-q:load-session (agent-q:session-id s1))))
        (is (= 1 (agent-q:session-message-count loaded)))))))

(test manager/ensure-current-session
  "Test ensuring a current session exists."
  (with-temp-session-manager
    (is (null (agent-q:current-session agent-q:*session-manager*)))
    (let ((session (agent-q:ensure-current-session)))
      (is (not (null session)))
      (is (eq session (agent-q:current-session agent-q:*session-manager*))))))

;;; ============================================================================
;;; RPC Endpoint Tests
;;; ============================================================================

(test rpc/agent-q-create-session
  "Test RPC endpoint for creating sessions."
  (with-temp-session-manager
    (let ((session-id (agent-q:agent-q-create-session :name "RPC Test")))
      (is (stringp session-id))
      (is (search "session-" session-id)))))

(test rpc/agent-q-list-sessions
  "Test RPC endpoint for listing sessions."
  (with-temp-session-manager
    (agent-q:create-session :name "List Test 1")
    (agent-q:save-session (agent-q:current-session agent-q:*session-manager*))
    (agent-q:create-session :name "List Test 2")
    (agent-q:save-session (agent-q:current-session agent-q:*session-manager*))

    (let ((sessions (agent-q:agent-q-list-sessions)))
      (is (= 2 (length sessions))))))

(test rpc/agent-q-get-session-info
  "Test RPC endpoint for getting session info."
  (with-temp-session-manager
    (let ((session (agent-q:create-session :name "Info Test")))
      (agent-q:session-add-message session :user "Hello")
      (agent-q:session-add-tokens session 100 50)

      (let ((info (agent-q:agent-q-get-session-info)))
        (is (string= "Info Test" (getf info :name)))
        (is (= 1 (getf info :message-count)))
        (is (= 100 (getf info :total-input-tokens)))
        (is (= 50 (getf info :total-output-tokens)))))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-session-tests ()
  "Run all session tests."
  (run! 'session-tests))
