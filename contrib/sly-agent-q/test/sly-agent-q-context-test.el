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

;;;; Task 3: Symbol Candidates

(ert-deftest agent-q-context/candidates/symbol-returns-list ()
  "Test that symbol candidates returns a list."
  (let ((candidates (agent-q--symbol-candidates "")))
    (should (listp candidates))))

(ert-deftest agent-q-context/candidates/symbol-from-lisp-buffers ()
  "Test that symbols are gathered from Lisp buffers."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun test-symbol-foo () 42)\n")
    (insert "(defvar test-symbol-bar 123)\n")
    ;; Force imenu to scan
    (setq imenu--index-alist nil)
    (let ((candidates (agent-q--symbol-candidates "test-symbol")))
      ;; Should find at least our function
      (should (cl-some (lambda (c) (string-prefix-p "test-symbol" c t))
                       candidates)))))

(ert-deftest agent-q-context/candidates/symbol-has-properties ()
  "Test that symbol candidates have required text properties."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-test-fn () nil)\n")
    (setq imenu--index-alist nil)
    (let* ((candidates (agent-q--symbol-candidates "my-test"))
           (candidate (car candidates)))
      (when candidate
        (should (eq :symbol (get-text-property 0 'agent-q-context-type candidate)))
        (should (plist-get (get-text-property 0 'agent-q-context-data candidate) :name))))))

(ert-deftest agent-q-context/candidates/symbol-limits-results ()
  "Test that symbol candidates are limited to prevent overwhelming completions."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Insert many definitions
    (dotimes (i 100)
      (insert (format "(defun test-limit-fn-%d () nil)\n" i)))
    (setq imenu--index-alist nil)
    (let ((candidates (agent-q--symbol-candidates "test-limit")))
      ;; Should be capped at 50
      (should (<= (length candidates) 50)))))

(ert-deftest agent-q-context/candidates/symbol-has-position ()
  "Test that symbol candidates include position data."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-pos-test-fn () nil)\n")
    (setq imenu--index-alist nil)
    (let* ((candidates (agent-q--symbol-candidates "my-pos-test"))
           (candidate (car candidates)))
      (when candidate
        (let ((data (get-text-property 0 'agent-q-context-data candidate)))
          (should (plist-get data :position)))))))

(ert-deftest agent-q-context/flatten-imenu/handles-nested ()
  "Test that flatten-imenu handles nested imenu structures."
  (let* ((nested '(("Functions"
                    ("test-nested-a" . 10)
                    ("test-nested-b" . 20))
                   ("Variables"
                    ("test-nested-c" . 30))))
         (result (agent-q--flatten-imenu nested "test-nested")))
    (should (= 3 (length result)))
    (should (cl-every (lambda (c) (string-prefix-p "test-nested" c t))
                      result))))

(ert-deftest agent-q-context/flatten-imenu/filters-by-prefix ()
  "Test that flatten-imenu filters by prefix."
  (let* ((index '(("alpha-fn" . 10)
                  ("beta-fn" . 20)
                  ("alpha-var" . 30)))
         (alpha-result (agent-q--flatten-imenu index "alpha"))
         (beta-result (agent-q--flatten-imenu index "beta")))
    (should (= 2 (length alpha-result)))
    (should (= 1 (length beta-result)))))

;;;; Task 4: Buffer Candidates

(ert-deftest agent-q-context/candidates/buffer-returns-list ()
  "Test that buffer candidates returns a list."
  (let ((candidates (agent-q--buffer-candidates "")))
    (should (listp candidates))))

(ert-deftest agent-q-context/candidates/buffer-excludes-internal ()
  "Test that internal buffers (starting with space) are excluded."
  (let ((candidates (agent-q--buffer-candidates "")))
    (should-not (cl-some (lambda (c) (string-prefix-p " " c))
                         candidates))))

(ert-deftest agent-q-context/candidates/buffer-filters-by-prefix ()
  "Test that buffer candidates filter by prefix."
  (with-temp-buffer
    (rename-buffer "agent-q-test-buffer-xyz" t)
    (unwind-protect
        (let ((candidates (agent-q--buffer-candidates "agent-q-test")))
          (should (cl-some (lambda (c)
                             (string= "agent-q-test-buffer-xyz" c))
                           candidates)))
      (kill-buffer "agent-q-test-buffer-xyz"))))

(ert-deftest agent-q-context/candidates/buffer-has-properties ()
  "Test that buffer candidates have required text properties."
  (with-temp-buffer
    (rename-buffer "agent-q-prop-test" t)
    (unwind-protect
        (let* ((candidates (agent-q--buffer-candidates "agent-q-prop"))
               (candidate (car candidates)))
          (should candidate)
          (should (eq :buffer (get-text-property 0 'agent-q-context-type candidate)))
          (should (string= "agent-q-prop-test"
                          (plist-get (get-text-property 0 'agent-q-context-data candidate)
                                     :buffer-name))))
      (kill-buffer "agent-q-prop-test"))))

;;;; Task 5: @-mention Bounds Detection

(ert-deftest agent-q-context/mention/detects-at-symbol ()
  "Test detecting @ at beginning of mention."
  (with-temp-buffer
    (insert "Hello @fo")
    (should (agent-q--context-mention-bounds))
    (let ((bounds (agent-q--context-mention-bounds)))
      (should (= (car bounds) 7))  ; Position of @
      (should (= (cdr bounds) 10))))) ; Position after 'o'

(ert-deftest agent-q-context/mention/returns-nil-without-at ()
  "Test that nil is returned when no @ present."
  (with-temp-buffer
    (insert "Hello world")
    (should-not (agent-q--context-mention-bounds))))

(ert-deftest agent-q-context/mention/handles-empty-prefix ()
  "Test detecting @ with no prefix yet."
  (with-temp-buffer
    (insert "Hello @")
    (let ((bounds (agent-q--context-mention-bounds)))
      (should bounds)
      (should (= (car bounds) 7))
      (should (= (cdr bounds) 8)))))

(ert-deftest agent-q-context/mention/stays-on-current-line ()
  "Test that detection doesn't cross line boundaries."
  (with-temp-buffer
    (insert "@previous\nworld")
    (goto-char (point-max))
    (should-not (agent-q--context-mention-bounds))))

(ert-deftest agent-q-context/mention/handles-mid-line ()
  "Test detecting @ in middle of line."
  (with-temp-buffer
    (insert "See @file.lisp for details")
    (goto-char 15)  ; After ".lisp"
    (let ((bounds (agent-q--context-mention-bounds)))
      (should bounds)
      (should (= (car bounds) 5))))) ; Position of @

(ert-deftest agent-q-context/mention/stops-at-whitespace ()
  "Test that bounds only include the mention, not text after whitespace."
  (with-temp-buffer
    (insert "@file hello")
    (goto-char 6)  ; Right at end of "@file" (position after 'e')
    (let ((bounds (agent-q--context-mention-bounds)))
      (should bounds)
      (should (= (car bounds) 1))
      (should (= (cdr bounds) 6))))  ; Should end at "file", not extend to "hello"
  ;; Also test when point is beyond the space
  (with-temp-buffer
    (insert "@file hello")
    (goto-char 7)  ; After space, at 'h' in "hello"
    (let ((bounds (agent-q--context-mention-bounds)))
      ;; Should return nil because point is beyond the match
      (should-not bounds))))

;;;; Task 6: Completion-at-Point

(ert-deftest agent-q-context/capf/returns-nil-without-mention ()
  "Test that CAPF returns nil when no @ present."
  (with-temp-buffer
    (insert "Hello world")
    (should-not (agent-q-context-complete-at-point))))

(ert-deftest agent-q-context/capf/returns-completion-data ()
  "Test that CAPF returns proper completion data for @-mention."
  (with-temp-buffer
    (insert "Hello @te")
    (let ((result (agent-q-context-complete-at-point)))
      (should result)
      ;; Result is (start end table . props) where props is a plist
      (should (>= (length result) 3))
      (should (numberp (nth 0 result)))  ; start
      (should (numberp (nth 1 result)))  ; end
      (should (functionp (nth 2 result)))))) ; completion table

(ert-deftest agent-q-context/capf/has-annotation-function ()
  "Test that CAPF includes annotation function in properties."
  (with-temp-buffer
    (insert "Hello @te")
    (let ((result (agent-q-context-complete-at-point)))
      (should result)
      (let ((props (nthcdr 3 result)))
        (should (plist-get props :annotation-function))))))

(ert-deftest agent-q-context/capf/combined-candidates ()
  "Test that combined candidates include all types."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun capf-test-fn () nil)")
    (setq imenu--index-alist nil)
    (let ((candidates (agent-q--context-candidates "capf-test")))
      ;; Should have symbol from our buffer
      (should (cl-some (lambda (c) (string-match-p "capf-test" c))
                       candidates)))))

(ert-deftest agent-q-context/capf/combined-candidates-strips-at ()
  "Test that combined candidates strips leading @ from prefix."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun strip-at-test-fn () nil)")
    (setq imenu--index-alist nil)
    ;; Prefix with @ should still work
    (let ((candidates (agent-q--context-candidates "@strip-at-test")))
      (should (cl-some (lambda (c) (string-match-p "strip-at-test" c))
                       candidates)))))

(ert-deftest agent-q-context/capf/annotation-function-file ()
  "Test annotation function returns [file] for file type."
  (let ((candidate (propertize "test.lisp" 'agent-q-context-type :file)))
    (should (string= " [file]" (agent-q--context-annotation candidate)))))

(ert-deftest agent-q-context/capf/annotation-function-symbol ()
  "Test annotation function returns [symbol] for symbol type."
  (let ((candidate (propertize "my-func" 'agent-q-context-type :symbol)))
    (should (string= " [symbol]" (agent-q--context-annotation candidate)))))

(ert-deftest agent-q-context/capf/annotation-function-buffer ()
  "Test annotation function returns [buffer] for buffer type."
  (let ((candidate (propertize "*scratch*" 'agent-q-context-type :buffer)))
    (should (string= " [buffer]" (agent-q--context-annotation candidate)))))

(ert-deftest agent-q-context/capf/annotation-function-region ()
  "Test annotation function returns [region] for region type."
  (let ((candidate (propertize "selection" 'agent-q-context-type :region)))
    (should (string= " [region]" (agent-q--context-annotation candidate)))))

(ert-deftest agent-q-context/capf/annotation-function-url ()
  "Test annotation function returns [url] for url type."
  (let ((candidate (propertize "https://example.com" 'agent-q-context-type :url)))
    (should (string= " [url]" (agent-q--context-annotation candidate)))))

(ert-deftest agent-q-context/capf/annotation-function-unknown ()
  "Test annotation function returns empty string for unknown type."
  (let ((candidate (propertize "test" 'agent-q-context-type :unknown)))
    (should (string= "" (agent-q--context-annotation candidate)))))

(ert-deftest agent-q-context/capf/annotation-function-no-property ()
  "Test annotation function handles missing type property."
  (let ((candidate "plain-string"))
    (should (string= "" (agent-q--context-annotation candidate)))))

(provide 'sly-agent-q-context-test)
;;; sly-agent-q-context-test.el ends here
