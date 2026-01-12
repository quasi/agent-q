;;; sly-agent-q-context-test.el --- Tests for context management -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Package-Requires: ((emacs "27.1") (ert "0"))

;; ABOUTME: Test suite for sly-agent-q-context.el
;; Tests context item data structure creation and validation.

;;; Commentary:

;; Test suite for sly-agent-q-context.el - context management for Agent-Q chat.
;; Task 1: Context item data structure tests.

;;; Code:

(require 'ert)
(require 'sly-agent-q-context)

;;;; Task 1: Context Item Data Structure

(ert-deftest agent-q-context/struct/creates-item ()
  "Test creating a context item."
  (let ((item (make-agent-q-context-item
               :type :file
               :display-name "foo.lisp"
               :data '(:path "/path/to/foo.lisp")
               :content "(defun foo () 42)")))
    (should (agent-q-context-item-p item))
    (should (eq :file (agent-q-context-item-type item)))
    (should (string= "foo.lisp" (agent-q-context-item-display-name item)))
    (should (equal '(:path "/path/to/foo.lisp") (agent-q-context-item-data item)))
    (should (string= "(defun foo () 42)" (agent-q-context-item-content item)))))

(ert-deftest agent-q-context/struct/type-variants ()
  "Test all context item type variants."
  (dolist (type '(:file :symbol :buffer :region :url))
    (let ((item (make-agent-q-context-item :type type :display-name "test")))
      (should (eq type (agent-q-context-item-type item))))))

(ert-deftest agent-q-context/struct/default-values ()
  "Test context item with default nil values."
  (let ((item (make-agent-q-context-item)))
    (should (agent-q-context-item-p item))
    (should (null (agent-q-context-item-type item)))
    (should (null (agent-q-context-item-display-name item)))
    (should (null (agent-q-context-item-data item)))
    (should (null (agent-q-context-item-content item)))))

(ert-deftest agent-q-context/struct/accessors-work ()
  "Test that struct accessors work correctly."
  (let ((item (make-agent-q-context-item
               :type :buffer
               :display-name "*scratch*"
               :data '(:buffer-name "*scratch*")
               :content "some content")))
    ;; Test each accessor
    (should (eq :buffer (agent-q-context-item-type item)))
    (should (string= "*scratch*" (agent-q-context-item-display-name item)))
    (should (equal '(:buffer-name "*scratch*") (agent-q-context-item-data item)))
    (should (string= "some content" (agent-q-context-item-content item)))))

(provide 'sly-agent-q-context-test)
;;; sly-agent-q-context-test.el ends here
