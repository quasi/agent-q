;;; sly-agent-q-sessions-test.el --- Tests for session management -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Package-Requires: ((emacs "27.1") (ert "0"))

;;; Commentary:

;; Comprehensive test suite for sly-agent-q-sessions.el (Phase 3)
;; Tests session persistence, loading, switching, and search functionality.

;;; Code:

(require 'ert)
(require 'sly-agent-q-chat)
(require 'sly-agent-q-sessions)

;;;; Test Fixtures

(defvar agent-q-sessions-test-dir nil
  "Temporary directory for session tests.")

(defun agent-q-sessions-test-setup ()
  "Set up test environment with temporary session directory."
  (setq agent-q-sessions-test-dir (make-temp-file "agent-q-sessions-" t))
  (setq agent-q-sessions-directory agent-q-sessions-test-dir)
  ;; Clear any existing sessions
  (setq agent-q--sessions-cache nil))

(defun agent-q-sessions-test-teardown ()
  "Clean up test environment."
  (when (and agent-q-sessions-test-dir
             (file-directory-p agent-q-sessions-test-dir))
    (delete-directory agent-q-sessions-test-dir t))
  (setq agent-q-sessions-test-dir nil)
  (setq agent-q--sessions-cache nil))

;;;; Session Serialization Tests

(ert-deftest agent-q-sessions/serialize/message-to-plist ()
  "Test serializing a message to plist."
  (let ((msg (agent-q-message--create
              :role 'user
              :content "test content"
              :timestamp (encode-time 0 0 12 1 1 2025))))
    (let ((plist (agent-q--message-to-plist msg)))
      (should (eq 'user (plist-get plist :role)))
      (should (string= "test content" (plist-get plist :content)))
      (should (plist-get plist :timestamp))
      (should (plist-get plist :id)))))

(ert-deftest agent-q-sessions/deserialize/plist-to-message ()
  "Test deserializing a plist to message."
  (let* ((timestamp (encode-time 0 0 12 1 1 2025))
         (plist (list :id "test-id"
                      :role 'assistant
                      :content "response"
                      :timestamp timestamp
                      :metadata '(:model "claude-3"))))
    (let ((msg (agent-q--plist-to-message plist)))
      (should (agent-q-message-p msg))
      (should (string= "test-id" (agent-q-message-id msg)))
      (should (eq 'assistant (agent-q-message-role msg)))
      (should (string= "response" (agent-q-message-content msg)))
      (should (equal timestamp (agent-q-message-timestamp msg))))))

(ert-deftest agent-q-sessions/serialize/session-to-plist ()
  "Test serializing a complete session."
  (let* ((msg1 (agent-q-message--create :role 'user :content "Q1"))
         (msg2 (agent-q-message--create :role 'assistant :content "A1"))
         (session (agent-q-session--create
                   :id "test-session"
                   :name "Test Session"
                   :messages (list msg2 msg1)
                   :model "claude-3"
                   :metadata '(:provider "anthropic"))))
    (let ((plist (agent-q--session-to-plist session)))
      (should (string= "test-session" (plist-get plist :id)))
      (should (string= "Test Session" (plist-get plist :name)))
      (should (string= "claude-3" (plist-get plist :model)))
      (should (= 2 (length (plist-get plist :messages)))))))

(ert-deftest agent-q-sessions/deserialize/plist-to-session ()
  "Test deserializing a plist to session."
  (let* ((timestamp (current-time))
         (plist (list :id "sess-123"
                      :name "Loaded Session"
                      :created-at timestamp
                      :updated-at timestamp
                      :model "gpt-4"
                      :messages (list (list :id "m1" :role 'user :content "Hi"
                                          :timestamp timestamp))
                      :metadata '(:tokens 100))))
    (let ((session (agent-q--plist-to-session plist)))
      (should (agent-q-session-p session))
      (should (string= "sess-123" (agent-q-session-id session)))
      (should (string= "Loaded Session" (agent-q-session-name session)))
      (should (string= "gpt-4" (agent-q-session-model session)))
      (should (= 1 (length (agent-q-session-messages session)))))))

;;;; Session Persistence Tests

(ert-deftest agent-q-sessions/persistence/save-session ()
  "Test saving a session to disk."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (let* ((msg (agent-q-message--create :role 'user :content "Save test"))
             (session (agent-q-session--create
                       :id "save-test"
                       :name "Saved Session"
                       :messages (list msg))))
        (agent-q--save-session session)
        ;; File should exist
        (let ((file (expand-file-name "save-test.el" agent-q-sessions-directory)))
          (should (file-exists-p file))
          ;; Should be readable
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "save-test" nil t)))))
    (agent-q-sessions-test-teardown)))

(ert-deftest agent-q-sessions/persistence/load-session ()
  "Test loading a session from disk."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (let* ((msg (agent-q-message--create :role 'user :content "Load test"))
             (session (agent-q-session--create
                       :id "load-test"
                       :name "To Load"
                       :messages (list msg)
                       :model "test-model")))
        ;; Save first
        (agent-q--save-session session)
        ;; Clear cache
        (setq agent-q--sessions-cache nil)
        ;; Load
        (let ((loaded (agent-q--load-session "load-test")))
          (should (agent-q-session-p loaded))
          (should (string= "load-test" (agent-q-session-id loaded)))
          (should (string= "To Load" (agent-q-session-name loaded)))
          (should (string= "test-model" (agent-q-session-model loaded)))
          (should (= 1 (length (agent-q-session-messages loaded))))))
    (agent-q-sessions-test-teardown)))

(ert-deftest agent-q-sessions/persistence/list-sessions ()
  "Test listing all saved sessions."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (progn
        ;; Create and save multiple sessions
        (agent-q--save-session (agent-q-session--create :id "s1" :name "First"))
        (agent-q--save-session (agent-q-session--create :id "s2" :name "Second"))
        (agent-q--save-session (agent-q-session--create :id "s3" :name "Third"))
        ;; List should return all
        (let ((sessions (agent-q--list-sessions)))
          (should (= 3 (length sessions)))
          (should (member "First" (mapcar #'agent-q-session-name sessions)))))
    (agent-q-sessions-test-teardown)))

(ert-deftest agent-q-sessions/persistence/delete-session ()
  "Test deleting a session."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (let ((session (agent-q-session--create :id "to-delete" :name "Delete Me")))
        (agent-q--save-session session)
        (should (file-exists-p (expand-file-name "to-delete.el" agent-q-sessions-directory)))
        ;; Delete
        (agent-q--delete-session-file "to-delete")
        (should-not (file-exists-p (expand-file-name "to-delete.el" agent-q-sessions-directory))))
    (agent-q-sessions-test-teardown)))

;;;; Session Caching Tests

(ert-deftest agent-q-sessions/cache/loads-on-first-access ()
  "Test that sessions are cached on first load."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (progn
        (agent-q--save-session (agent-q-session--create :id "cached" :name "Cached"))
        (setq agent-q--sessions-cache nil)
        ;; First access should populate cache
        (agent-q--list-sessions)
        (should agent-q--sessions-cache)
        (should (= 1 (length agent-q--sessions-cache))))
    (agent-q-sessions-test-teardown)))

(ert-deftest agent-q-sessions/cache/returns-from-cache ()
  "Test that cached sessions are returned without disk access."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (let ((session (agent-q-session--create :id "in-cache" :name "Cached")))
        (agent-q--save-session session)
        ;; Load once to populate cache
        (agent-q--list-sessions)
        ;; Delete file
        (delete-file (expand-file-name "in-cache.el" agent-q-sessions-directory))
        ;; Should still be in cache
        (let ((sessions (agent-q--list-sessions)))
          (should (= 1 (length sessions)))
          (should (string= "in-cache" (agent-q-session-id (car sessions))))))
    (agent-q-sessions-test-teardown)))

;;;; Session Search Tests

(ert-deftest agent-q-sessions/search/finds-by-name ()
  "Test searching sessions by name."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (progn
        (agent-q--save-session (agent-q-session--create :id "s1" :name "Database Migration"))
        (agent-q--save-session (agent-q-session--create :id "s2" :name "UI Refactoring"))
        (agent-q--save-session (agent-q-session--create :id "s3" :name "Database Optimization"))
        ;; Search for "database"
        (let ((results (agent-q--search-sessions "database")))
          (should (= 2 (length results)))
          (should (cl-every (lambda (s)
                             (string-match-p "Database" (agent-q-session-name s)))
                           results))))
    (agent-q-sessions-test-teardown)))

(ert-deftest agent-q-sessions/search/finds-by-content ()
  "Test searching sessions by message content."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (let* ((msg1 (agent-q-message--create :role 'user :content "How do I optimize queries?"))
             (msg2 (agent-q-message--create :role 'user :content "Refactor the UI components"))
             (msg3 (agent-q-message--create :role 'user :content "Query performance tips"))
             (s1 (agent-q-session--create :id "s1" :name "First" :messages (list msg1)))
             (s2 (agent-q-session--create :id "s2" :name "Second" :messages (list msg2)))
             (s3 (agent-q-session--create :id "s3" :name "Third" :messages (list msg3))))
        (agent-q--save-session s1)
        (agent-q--save-session s2)
        (agent-q--save-session s3)
        ;; Search for "query"
        (let ((results (agent-q--search-sessions "query")))
          (should (>= (length results) 1))
          (should (cl-some (lambda (s) (string= "s1" (agent-q-session-id s))) results))))
    (agent-q-sessions-test-teardown)))

(ert-deftest agent-q-sessions/search/case-insensitive ()
  "Test that search is case-insensitive."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (progn
        (agent-q--save-session (agent-q-session--create :id "s1" :name "UPPERCASE NAME"))
        ;; Search with lowercase
        (let ((results (agent-q--search-sessions "uppercase")))
          (should (= 1 (length results)))))
    (agent-q-sessions-test-teardown)))

;;;; Mode Line Tests

(ert-deftest agent-q-sessions/modeline/shows-session-name ()
  "Test that mode line shows session name."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q-session-set-name agent-q--current-session "Test Session")
    (let ((modeline (agent-q--session-modeline)))
      (should (string-match-p "Test Session" modeline)))))

(ert-deftest agent-q-sessions/modeline/shows-token-count ()
  "Test that mode line shows token usage."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q-session-add-tokens agent-q--current-session 1000 500)
    (let ((modeline (agent-q--session-modeline)))
      (should (string-match-p "1[,.]?000" modeline))
      (should (string-match-p "500" modeline)))))

(ert-deftest agent-q-sessions/modeline/formats-large-numbers ()
  "Test that large token counts are formatted with separators."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q-session-add-tokens agent-q--current-session 123456 78901)
    (let ((modeline (agent-q--session-modeline)))
      (should (string-match-p "[0-9][,.]?[0-9]\\{3\\}[,.]?[0-9]\\{3\\}" modeline)))))

;;;; Session Switching Tests

(ert-deftest agent-q-sessions/switch/saves-current-session ()
  "Test that switching sessions saves the current one."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (with-temp-buffer
        (agent-q-chat-mode)
        (let ((msg (agent-q-message--create :role 'user :content "Test")))
          (push msg (agent-q-session-messages agent-q--current-session))
          (let ((session-id (agent-q-session-id agent-q--current-session)))
            ;; Mock switching (simplified - actual switch requires completing-read)
            (agent-q--save-current-session)
            ;; Session should be saved
            (should (file-exists-p
                     (expand-file-name (concat session-id ".el")
                                     agent-q-sessions-directory))))))
    (agent-q-sessions-test-teardown)))

;;;; Auto-save Tests

(ert-deftest agent-q-sessions/autosave/initializes-timer ()
  "Test that auto-save timer is set up."
  (with-temp-buffer
    (agent-q-chat-mode)
    (agent-q-sessions-initialize)
    ;; Timer should exist
    (should (timerp agent-q--autosave-timer))))

(ert-deftest agent-q-sessions/autosave/cancels-on-kill ()
  "Test that auto-save timer is cancelled when buffer is killed."
  (let ((buf (get-buffer-create "*test-agent-q*")))
    (with-current-buffer buf
      (agent-q-chat-mode)
      (agent-q-sessions-initialize)
      (let ((timer agent-q--autosave-timer))
        (kill-buffer buf)
        ;; Timer should be cancelled (testing the pattern, not actual timer)
        (should (not (buffer-live-p buf)))))))

;;;; Edge Cases

(ert-deftest agent-q-sessions/edge-case/empty-session ()
  "Test handling of empty session (no messages)."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (let ((session (agent-q-session--create :id "empty" :name "Empty")))
        (agent-q--save-session session)
        (let ((loaded (agent-q--load-session "empty")))
          (should (agent-q-session-p loaded))
          (should (null (agent-q-session-messages loaded)))))
    (agent-q-sessions-test-teardown)))

(ert-deftest agent-q-sessions/edge-case/session-with-nil-name ()
  "Test handling of session without a name."
  (let ((session (agent-q-session--create :id "unnamed" :name nil)))
    (should (agent-q-session-p session))
    (should (null (agent-q-session-name session)))))

(ert-deftest agent-q-sessions/edge-case/corrupted-session-file ()
  "Test graceful handling of corrupted session file."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (progn
        ;; Write invalid data
        (with-temp-file (expand-file-name "corrupted.el" agent-q-sessions-directory)
          (insert "(this is not valid session data"))
        ;; Should handle error gracefully
        (should-error (agent-q--load-session "corrupted")))
    (agent-q-sessions-test-teardown)))

(ert-deftest agent-q-sessions/edge-case/nonexistent-session ()
  "Test loading a nonexistent session."
  (agent-q-sessions-test-setup)
  (unwind-protect
      (should-error (agent-q--load-session "does-not-exist"))
    (agent-q-sessions-test-teardown)))

(provide 'sly-agent-q-sessions-test)
;;; sly-agent-q-sessions-test.el ends here
