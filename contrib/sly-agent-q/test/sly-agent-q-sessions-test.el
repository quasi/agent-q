;;; sly-agent-q-sessions-test.el --- Tests for session management UI -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Package-Requires: ((emacs "27.1") (ert "0"))

;;; Commentary:

;; Test suite for sly-agent-q-sessions.el
;; Tests the UI layer; persistence is tested on the CL side.

;;; Code:

(require 'ert)
(require 'sly-agent-q-chat)
(require 'sly-agent-q-sessions)

;;;; Mock SLY Connection

(defvar agent-q-test--sly-connected nil
  "Mock SLY connection state for testing.")

(defvar agent-q-test--mock-sessions nil
  "Mock session list for testing.")

(defvar agent-q-test--mock-session-info nil
  "Mock current session info for testing.")

(defun agent-q-test--setup-mock-sly ()
  "Set up mock SLY functions for testing."
  (setq agent-q-test--sly-connected t)
  (setq agent-q-test--mock-sessions nil)
  (setq agent-q-test--mock-session-info nil))

(defun agent-q-test--teardown-mock-sly ()
  "Clean up mock SLY functions."
  (setq agent-q-test--sly-connected nil)
  (setq agent-q-test--mock-sessions nil)
  (setq agent-q-test--mock-session-info nil))

;;;; SLY Connection Tests

(ert-deftest agent-q-sessions/connection/checks-sly-connection ()
  "Test that operations check for SLY connection."
  (agent-q-test--setup-mock-sly)
  (unwind-protect
      (cl-letf (((symbol-function 'sly-connected-p)
                 (lambda () agent-q-test--sly-connected)))
        ;; Should work when connected
        (setq agent-q-test--sly-connected t)
        (should (eq t (condition-case nil
                          (progn (agent-q--check-sly-connection) t)
                        (error nil))))
        ;; Should error when not connected
        (setq agent-q-test--sly-connected nil)
        (should-error (agent-q--check-sly-connection)))
    (agent-q-test--teardown-mock-sly)))

;;;; Timestamp Formatting Tests

(ert-deftest agent-q-sessions/timestamp/formats-universal-time ()
  "Test formatting CL universal time to readable string."
  ;; 3945234600 is approximately 2025-01-15 in universal time
  ;; (70 years of seconds from 1900 to 1970 = 2208988800)
  ;; So 3945234600 - 2208988800 = 1736245800 unix timestamp
  (let ((result (agent-q--format-timestamp 3945234600)))
    (should (stringp result))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" result))))

(ert-deftest agent-q-sessions/timestamp/handles-nil ()
  "Test that nil timestamp returns empty string."
  (should (string= "" (agent-q--format-timestamp nil))))

(ert-deftest agent-q-sessions/timestamp/handles-non-number ()
  "Test that non-number returns empty string."
  (should (string= "" (agent-q--format-timestamp "not a number"))))

;;;; Mode Line Tests

(ert-deftest agent-q-sessions/modeline/shows-session-name ()
  "Test that mode line shows session name."
  (agent-q-test--setup-mock-sly)
  (unwind-protect
      (with-temp-buffer
        (setq agent-q--current-session (agent-q-session--create))
        (agent-q-session-set-name agent-q--current-session "Test Session")
        (cl-letf (((symbol-function 'sly-connected-p) (lambda () nil)))
          (let ((modeline (agent-q--mode-line-session-info)))
            (should (stringp modeline))
            (should (string-match-p "Test Session" modeline)))))
    (agent-q-test--teardown-mock-sly)))

(ert-deftest agent-q-sessions/modeline/shows-abbreviated-id ()
  "Test that mode line shows abbreviated session ID when no name."
  (with-temp-buffer
    (setq agent-q--current-session (agent-q-session--create))
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () nil)))
      (let ((modeline (agent-q--mode-line-session-info)))
        (should (stringp modeline))
        ;; Should show date portion of ID
        (should (> (length modeline) 5))))))

(ert-deftest agent-q-sessions/modeline/shows-message-count ()
  "Test that mode line shows message count."
  (with-temp-buffer
    (setq agent-q--current-session (agent-q-session--create))
    ;; Add some messages
    (let ((msg (agent-q-message--create :role 'user :content "Test")))
      (push msg (agent-q-session-messages agent-q--current-session))
      (push msg (agent-q-session-messages agent-q--current-session)))
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () nil)))
      (let ((modeline (agent-q--mode-line-session-info)))
        (should (string-match-p ":2" modeline))))))

(ert-deftest agent-q-sessions/modeline/shows-token-usage ()
  "Test that mode line shows token usage."
  (with-temp-buffer
    (setq agent-q--current-session (agent-q-session--create))
    (agent-q-session-add-tokens agent-q--current-session 1000 500)
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () nil)))
      (let ((modeline (agent-q--mode-line-session-info)))
        (should (string-match-p "↑1000" modeline))
        (should (string-match-p "↓500" modeline))))))

(ert-deftest agent-q-sessions/modeline/uses-rpc-when-connected ()
  "Test that mode line fetches info from CL when connected."
  (with-temp-buffer
    (setq agent-q--current-session (agent-q-session--create))
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () t))
              ((symbol-function 'sly-eval)
               (lambda (_form)
                 '(:id "rpc-session" :name "From RPC"
                   :message-count 5 :total-input-tokens 2000
                   :total-output-tokens 1000))))
      (let ((modeline (agent-q--mode-line-session-info)))
        (should (string-match-p "From RPC" modeline))
        (should (string-match-p ":5" modeline))))))

;;;; Auto-Save Tests

(ert-deftest agent-q-sessions/autosave/setup-creates-timer ()
  "Test that auto-save setup creates a timer."
  (let ((agent-q-auto-save-sessions t)
        (agent-q-auto-save-interval 60)
        (agent-q--auto-save-timer nil))
    (unwind-protect
        (progn
          (agent-q--setup-auto-save)
          (should (timerp agent-q--auto-save-timer)))
      (agent-q--cancel-auto-save))))

(ert-deftest agent-q-sessions/autosave/cancel-removes-timer ()
  "Test that cancel removes the timer."
  (let ((agent-q-auto-save-sessions t)
        (agent-q-auto-save-interval 60)
        (agent-q--auto-save-timer nil))
    (agent-q--setup-auto-save)
    (should (timerp agent-q--auto-save-timer))
    (agent-q--cancel-auto-save)
    (should (null agent-q--auto-save-timer))))

(ert-deftest agent-q-sessions/autosave/disabled-when-interval-zero ()
  "Test that auto-save is disabled when interval is zero."
  (let ((agent-q-auto-save-sessions t)
        (agent-q-auto-save-interval 0)
        (agent-q--auto-save-timer nil))
    (agent-q--setup-auto-save)
    (should (null agent-q--auto-save-timer))))

(ert-deftest agent-q-sessions/autosave/disabled-when-flag-nil ()
  "Test that auto-save is disabled when flag is nil."
  (let ((agent-q-auto-save-sessions nil)
        (agent-q-auto-save-interval 60)
        (agent-q--auto-save-timer nil))
    (agent-q--setup-auto-save)
    (should (null agent-q--auto-save-timer))))

;;;; Session Initialization Tests

(ert-deftest agent-q-sessions/init/adds-mode-line-info ()
  "Test that initialization adds mode line session info."
  (with-temp-buffer
    (setq-local mode-line-misc-info nil)
    (setq agent-q--current-session (agent-q-session--create))
    (agent-q-sessions-initialize)
    (should (member '(:eval (agent-q--mode-line-session-info))
                    mode-line-misc-info))))

;;;; Edge Cases

(ert-deftest agent-q-sessions/edge-case/no-current-session ()
  "Test mode line handles no current session."
  (let ((agent-q--current-session nil))
    (should (null (agent-q--mode-line-session-info)))))

(ert-deftest agent-q-sessions/edge-case/session-without-name ()
  "Test mode line handles session without name."
  (with-temp-buffer
    (setq agent-q--current-session (agent-q-session--create))
    (setf (agent-q-session-name agent-q--current-session) nil)
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () nil)))
      (let ((modeline (agent-q--mode-line-session-info)))
        ;; Should not error, should show something
        (should (stringp modeline))))))

(ert-deftest agent-q-sessions/edge-case/session-without-tokens ()
  "Test mode line handles session without token info."
  (with-temp-buffer
    (setq agent-q--current-session (agent-q-session--create))
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () nil)))
      (let ((modeline (agent-q--mode-line-session-info)))
        ;; Should not show token arrows when no tokens
        (should-not (string-match-p "↑" modeline))))))

;;;; RPC Function Tests

(ert-deftest agent-q-sessions/rpc/save-calls-eval-async ()
  "Test that save uses async RPC."
  (let ((called nil))
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () t))
              ((symbol-function 'sly-eval-async)
               (lambda (form callback)
                 (setq called t)
                 (should (equal '(agent-q:agent-q-save-session) form)))))
      (agent-q--save-current-session)
      (should called))))

(ert-deftest agent-q-sessions/rpc/list-calls-eval-sync ()
  "Test that list uses sync RPC."
  (let ((called nil))
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () t))
              ((symbol-function 'sly-eval)
               (lambda (form)
                 (setq called t)
                 (should (equal '(agent-q:agent-q-list-sessions) form))
                 nil)))
      (agent-q--list-sessions-sync)
      (should called))))

(ert-deftest agent-q-sessions/rpc/create-passes-name ()
  "Test that create passes name to RPC."
  (let ((received-name nil))
    (cl-letf (((symbol-function 'sly-connected-p) (lambda () t))
              ((symbol-function 'sly-eval)
               (lambda (form)
                 (setq received-name (plist-get (cdr form) :name))
                 "new-session-id")))
      (agent-q--create-session-rpc "My Session")
      (should (string= "My Session" received-name)))))

(provide 'sly-agent-q-sessions-test)
;;; sly-agent-q-sessions-test.el ends here
