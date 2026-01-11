;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q-TESTS; Base: 10 -*-

(in-package :agent-q-tests)

;;;; Core Functionality Tests
;;;;
;;;; Tests for:
;;;; - Configuration (config.lisp)
;;;; - Conversation management (conversation.lisp)
;;;; - Agent functionality (agent.lisp)
;;;; - Context handling (context.lisp)

(def-suite core-functionality
  :description "Tests for core Agent-Q functionality (config, conversation, agent)")

(in-suite core-functionality)

;;;; Configuration Tests

(test config/sync-from-cl-llm-provider
  "Test syncing provider instance from cl-llm-provider."
  ;; Setup: Create a provider with model
  (let ((agent-q:*provider-instance* nil))
    (unwind-protect
         (progn
           (setf cl-llm-provider:*default-provider*
                 (cl-llm-provider:make-provider :openai :model "gpt-4"))
           (setf cl-llm-provider:*default-model* "gpt-4")

           ;; Sync should copy provider and populate model
           (is-true (agent-q:sync-from-cl-llm-provider))
           (is-true agent-q:*provider-instance*)

           ;; Model should be set on the provider instance
           (is (string= "gpt-4"
                       (cl-llm-provider:provider-default-model
                        agent-q:*provider-instance*))))
      ;; Cleanup
      (setf agent-q:*provider-instance* nil)
      (setf cl-llm-provider:*default-provider* nil))))

(test config/get-config-info-success
  "Test get-config-info returns correct info when configured."
  (let ((agent-q:*provider-instance* nil))
    (unwind-protect
         (progn
           ;; Setup provider with model
           (setf agent-q:*provider-instance*
                 (cl-llm-provider:make-provider :anthropic :model "claude-3"))

           (let ((info (agent-q:get-config-info)))
             (is (eq :anthropic (getf info :provider)))
             (is (string= "Anthropic" (getf info :provider-name)))
             (is (string= "claude-3" (getf info :model)))))
      ;; Cleanup
      (setf agent-q:*provider-instance* nil))))

(test config/get-config-info-error
  "Test get-config-info returns error when not configured."
  (let ((agent-q:*provider-instance* nil))
    (let ((info (agent-q:get-config-info)))
      (is (getf info :error))
      (is (search "not configured" (getf info :error))))))

(test config/configure-creates-provider
  "Test configure function creates provider with model."
  (unwind-protect
       (progn
         (agent-q:configure :provider :openai
                           :model "gpt-4-turbo"
                           :api-key "test-key")

         (is-true agent-q:*provider-instance*)
         (is (eq :openai (cl-llm-provider:provider-type agent-q:*provider-instance*)))

         ;; Model should be set
         (is (string= "gpt-4-turbo"
                     (cl-llm-provider:provider-default-model
                      agent-q:*provider-instance*))))
    ;; Cleanup
    (setf agent-q:*provider-instance* nil)))

;;;; Conversation Tests

(test conversation/create-new-conversation
  "Test creating a new conversation."
  (let ((conv (make-instance 'agent-q::conversation)))
    (is (typep conv 'agent-q::conversation))
    (is (null (agent-q::conversation-messages conv)))
    (is (typep (agent-q::conversation-context conv) 'list))))

(test conversation/add-user-message
  "Test adding a user message to conversation."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-message conv :user "Hello")

    (is (= 1 (length (agent-q::conversation-messages conv))))
    (let ((msg (first (agent-q::conversation-messages conv))))
      (is (eq :user (agent-q::message-role msg)))
      (is (string= "Hello" (agent-q::message-content msg))))))

(test conversation/add-assistant-message
  "Test adding an assistant message to conversation."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-message conv :assistant "Hi there")

    (is (= 1 (length (agent-q::conversation-messages conv))))
    (let ((msg (first (agent-q::conversation-messages conv))))
      (is (eq :assistant (agent-q::message-role msg)))
      (is (string= "Hi there" (agent-q::message-content msg))))))

(test conversation/add-debug-message
  "Test adding a debug message to conversation."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-message conv :debug "Tool executed")

    (is (= 1 (length (agent-q::conversation-messages conv))))
    (let ((msg (first (agent-q::conversation-messages conv))))
      (is (eq :debug (agent-q::message-role msg))))))

(test conversation/messages-in-reverse-order
  "Test that messages are stored in reverse chronological order."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-message conv :user "First")
    (agent-q::add-message conv :assistant "Second")
    (agent-q::add-message conv :user "Third")

    (is (= 3 (length (agent-q::conversation-messages conv))))
    ;; Most recent should be first
    (is (string= "Third"
                (agent-q::message-content
                 (first (agent-q::conversation-messages conv)))))))

(test conversation/build-messages-for-llm
  "Test building message list for LLM API."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-message conv :user "Question 1")
    (agent-q::add-message conv :assistant "Answer 1")
    (agent-q::add-message conv :user "Question 2")

    (let ((messages (agent-q::build-messages-for-llm conv)))
      ;; Should have 3 messages
      (is (= 3 (length messages)))
      ;; Should be in chronological order (oldest first)
      (is (string= "Question 1" (getf (first messages) :content)))
      (is (string= "Answer 1" (getf (second messages) :content)))
      (is (string= "Question 2" (getf (third messages) :content)))
      ;; Debug messages should be filtered out
      (is (every (lambda (msg) (member (getf msg :role) '(:user :assistant)))
                messages)))))

(test conversation/filters-debug-messages
  "Test that debug messages are filtered from LLM messages."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-message conv :user "Question")
    (agent-q::add-message conv :debug "Tool info")
    (agent-q::add-message conv :assistant "Answer")

    (let ((messages (agent-q::build-messages-for-llm conv)))
      ;; Should only have 2 messages (user and assistant)
      (is (= 2 (length messages)))
      (is (every (lambda (msg) (member (getf msg :role) '(:user :assistant)))
                messages)))))

;;;; Context Tests

(test context/create-context-item
  "Test creating a context item."
  (let ((item (agent-q:make-context-item :code
                                        "(defun foo () 42)"
                                        '(:file "test.lisp"))))
    (is (typep item 'agent-q::context-item))
    (is (eq :code (agent-q:context-item-type item)))
    (is (string= "(defun foo () 42)" (agent-q:context-item-content item)))
    (is (equal '(:file "test.lisp") (agent-q:context-item-metadata item)))))

(test context/add-to-context
  "Test adding items to conversation context."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-to-context conv :code "(defun test () t)")

    (is (= 1 (length (agent-q::conversation-context conv))))
    (let ((item (first (agent-q::conversation-context conv))))
      (is (eq :code (agent-q:context-item-type item))))))

(test context/clear-context
  "Test clearing conversation context."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-to-context conv :code "code1")
    (agent-q::add-to-context conv :code "code2")
    (is (= 2 (length (agent-q::conversation-context conv))))

    (agent-q::clear-context conv)
    (is (= 0 (length (agent-q::conversation-context conv))))))

(test context/format-for-llm
  "Test formatting context items for LLM consumption."
  (let ((conv (make-instance 'agent-q::conversation)))
    (agent-q::add-to-context conv :code "(defun foo () 42)")

    (let ((formatted (agent-q::context-to-string (agent-q::conversation-context conv))))
      (is (stringp formatted))
      (is (search "(defun foo () 42)" formatted)))))

;;;; Agent Tests

(test agent/create-agent
  "Test creating an agent instance."
  (let ((provider (cl-llm-provider:make-provider :anthropic :model "claude-3")))
    (let ((agent (make-instance 'agent-q::agent :provider provider)))
      (is (typep agent 'agent-q::agent))
      (is (eq provider (agent-q::agent-provider agent)))
      (is (typep (agent-q::agent-conversation agent) 'agent-q::conversation)))))

(test agent/ensure-agent-creates-when-needed
  "Test that ensure-agent creates agent if not exists."
  (let ((agent-q::*current-agent* nil)
        (agent-q:*provider-instance*
         (cl-llm-provider:make-provider :openai :model "gpt-4")))
    (unwind-protect
         (progn
           (agent-q::ensure-agent)
           (is-true agent-q::*current-agent*)
           (is (typep agent-q::*current-agent* 'agent-q::agent)))
      ;; Cleanup
      (setf agent-q::*current-agent* nil)
      (setf agent-q:*provider-instance* nil))))

(test agent/ensure-agent-errors-without-provider
  "Test that ensure-agent errors if provider not configured."
  (let ((agent-q::*current-agent* nil)
        (agent-q:*provider-instance* nil))
    (signals error
      (agent-q::ensure-agent))))

(test agent/get-last-response
  "Test getting last assistant response from agent."
  (let* ((provider (cl-llm-provider:make-provider :anthropic :model "claude-3"))
         (agent (make-instance 'agent-q::agent :provider provider)))
    (let ((agent-q::*current-agent* agent))
      ;; Add some messages
      (agent-q::add-message (agent-q::agent-conversation agent) :user "Q1")
      (agent-q::add-message (agent-q::agent-conversation agent) :assistant "A1")
      (agent-q::add-message (agent-q::agent-conversation agent) :user "Q2")
      (agent-q::add-message (agent-q::agent-conversation agent) :assistant "A2")

      ;; Last response should be A2
      (is (string= "A2" (agent-q::get-last-response))))))

(test agent/report-session-info
  "Test that session info reporting doesn't error."
  ;; This just tests the function doesn't error (actual emacs communication is stubbed)
  (finishes
    (agent-q::report-session-info-to-emacs "gpt-4" :openai 100 50)))

;;;; RPC Interface Tests

(test sly-interface/agent-q-send-exists
  "Test that agent-q-send RPC endpoint exists."
  (is (fboundp 'agent-q:agent-q-send)))

(test sly-interface/get-config-info-exists
  "Test that get-config-info RPC endpoint exists."
  (is (fboundp 'agent-q:get-config-info)))

;;;; Test Runners

(defun run-core-tests ()
  "Run all core functionality tests."
  (format t "~%~%=== Running Core Functionality Tests ===~%~%")
  (run! 'core-functionality))

(defun run-agent-q-tests ()
  "Run all Agent-Q tests (core + phase 2)."
  (format t "~%~%╔════════════════════════════════════════╗~%")
  (format t "║   Agent-Q Comprehensive Test Suite   ║~%")
  (format t "╚════════════════════════════════════════╝~%~%")

  ;; Core tests
  (format t "~%=== Core Functionality Tests ===~%~%")
  (run! 'core-functionality)

  ;; Phase 2 tests
  (format t "~%~%=== Phase 2 Tests ===~%~%")
  (run-phase-2-tests)

  (format t "~%~%╔════════════════════════════════════════╗~%")
  (format t "║   All Tests Complete                   ║~%")
  (format t "╚════════════════════════════════════════╝~%~%"))
