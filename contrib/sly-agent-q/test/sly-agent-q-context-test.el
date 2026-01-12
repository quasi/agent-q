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

;;;; Task 2: File Candidates

(ert-deftest agent-q-context/candidates/file-returns-list ()
  "Test that file candidates returns a list."
  (let ((candidates (agent-q--file-candidates "")))
    (should (listp candidates))))

(ert-deftest agent-q-context/candidates/file-filters-by-prefix ()
  "Test that file candidates are filtered by prefix."
  ;; Create temp project with known files
  (let* ((temp-dir (make-temp-file "agent-q-test" t))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Create test files
          (with-temp-file (expand-file-name "foo.lisp" temp-dir) (insert ""))
          (with-temp-file (expand-file-name "bar.lisp" temp-dir) (insert ""))
          (with-temp-file (expand-file-name "foobar.py" temp-dir) (insert ""))
          ;; Mock project-current to return our temp dir
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir)
                       (cons 'transient temp-dir)))
                    ((symbol-function 'project-files)
                     (lambda (_)
                       (directory-files temp-dir t "\\`[^.]"))))
            (let ((foo-candidates (agent-q--file-candidates "foo")))
              (should (= 2 (length foo-candidates)))
              (should (cl-every (lambda (c) (string-prefix-p "foo" c t))
                               foo-candidates)))))
      ;; Cleanup
      (delete-directory temp-dir t))))

(ert-deftest agent-q-context/candidates/file-has-properties ()
  "Test that file candidates have required text properties."
  (let* ((temp-dir (make-temp-file "agent-q-test" t))
         (default-directory temp-dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "test.lisp" temp-dir) (insert ""))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt _dir)
                       (cons 'transient temp-dir)))
                    ((symbol-function 'project-files)
                     (lambda (_)
                       (directory-files temp-dir t "\\.lisp\\'"))))
            (let* ((candidates (agent-q--file-candidates "test"))
                   (candidate (car candidates)))
              (when candidate
                (should (eq :file (get-text-property 0 'agent-q-context-type candidate)))
                (should (plist-get (get-text-property 0 'agent-q-context-data candidate) :path))))))
      (delete-directory temp-dir t))))

(ert-deftest agent-q-context/candidates/file-returns-nil-without-project ()
  "Test that file candidates returns nil when no project."
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _maybe-prompt _dir) nil)))
    (should (null (agent-q--file-candidates "test")))))

(provide 'sly-agent-q-context-test)
;;; sly-agent-q-context-test.el ends here
