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

;;;; Task 7: Content Fetching

(ert-deftest agent-q-context/fetch/file-content ()
  "Test fetching file content."
  (let* ((temp-file (make-temp-file "agent-q-fetch-test"))
         (content "Test file content"))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert content))
          (let ((fetched (agent-q--fetch-context-content
                         :file (list :path temp-file))))
            (should (string= content fetched))))
      (delete-file temp-file))))

(ert-deftest agent-q-context/fetch/file-limits-size ()
  "Test that file content is limited to 50KB."
  (let ((temp-file (make-temp-file "agent-q-size-test")))
    (unwind-protect
        (progn
          ;; Write 100KB of data
          (with-temp-file temp-file
            (dotimes (_ 10000)
              (insert "1234567890")))
          (let ((fetched (agent-q--fetch-context-content
                         :file (list :path temp-file))))
            (should (<= (length fetched) agent-q-context-max-size))))
      (delete-file temp-file))))

(ert-deftest agent-q-context/fetch/buffer-content ()
  "Test fetching buffer content."
  (with-temp-buffer
    (rename-buffer "agent-q-fetch-buf-test" t)
    (insert "Buffer test content")
    (unwind-protect
        (let ((fetched (agent-q--fetch-context-content
                       :buffer (list :buffer-name "agent-q-fetch-buf-test"))))
          (should (string= "Buffer test content" fetched)))
      (kill-buffer "agent-q-fetch-buf-test"))))

(ert-deftest agent-q-context/fetch/buffer-limits-size ()
  "Test that buffer content is limited to 50KB."
  (with-temp-buffer
    (rename-buffer "agent-q-size-buf-test" t)
    (dotimes (_ 10000)
      (insert "1234567890"))
    (unwind-protect
        (let ((fetched (agent-q--fetch-context-content
                       :buffer (list :buffer-name "agent-q-size-buf-test"))))
          (should (<= (length fetched) agent-q-context-max-size)))
      (kill-buffer "agent-q-size-buf-test"))))

(ert-deftest agent-q-context/fetch/nonexistent-returns-nil ()
  "Test that fetching nonexistent content returns nil."
  (should-not (agent-q--fetch-context-content
               :file (list :path "/nonexistent/path/file.txt")))
  (should-not (agent-q--fetch-context-content
               :buffer (list :buffer-name "nonexistent-buffer-xyz"))))

(ert-deftest agent-q-context/fetch/symbol-content ()
  "Test fetching symbol definition content."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun agent-q-fetch-sym-test ()\n  \"Test function.\"\n  42)\n")
    (goto-char (point-min))
    ;; Create a marker at the function position
    (let* ((pos (point-marker))
           (fetched (agent-q--fetch-context-content
                    :symbol (list :name "agent-q-fetch-sym-test"
                                  :position pos))))
      (should fetched)
      (should (string-match-p "defun agent-q-fetch-sym-test" fetched)))))

(ert-deftest agent-q-context/fetch/region-content ()
  "Test fetching region content."
  (with-temp-buffer
    (rename-buffer "agent-q-region-fetch-test" t)
    (insert "line 1\nselected text\nline 3")
    (unwind-protect
        (let ((fetched (agent-q--fetch-context-content
                       :region (list :buffer "agent-q-region-fetch-test"
                                     :start 8
                                     :end 21))))
          (should (string= "selected text" fetched)))
      (kill-buffer "agent-q-region-fetch-test"))))

(ert-deftest agent-q-context/fetch/unknown-type-returns-nil ()
  "Test that unknown content type returns nil."
  (should-not (agent-q--fetch-context-content
               :unknown (list :foo "bar"))))

(ert-deftest agent-q-context/max-size-constant ()
  "Test that max size constant is defined as 50000."
  (should (boundp 'agent-q-context-max-size))
  (should (= 50000 agent-q-context-max-size)))

;;;; Task 8: Context Pill Rendering

(ert-deftest agent-q-context/pill/inserts-formatted-text ()
  "Test that context pill inserts bracketed text."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :file
                 :display-name "foo.lisp")))
      (agent-q--insert-context-pill item)
      (goto-char (point-min))
      (should (search-forward "[@foo.lisp]" nil t)))))

(ert-deftest agent-q-context/pill/has-face ()
  "Test that context pill has proper face."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :file
                 :display-name "test.lisp")))
      (agent-q--insert-context-pill item)
      (goto-char (point-min))
      (search-forward "[@")
      (should (eq 'agent-q-context-pill-face
                  (get-text-property (point) 'face))))))

(ert-deftest agent-q-context/pill/stores-item ()
  "Test that pill stores context item in text property."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :symbol
                 :display-name "my-func")))
      (agent-q--insert-context-pill item)
      (goto-char (point-min))
      (search-forward "[@")
      (let ((stored (get-text-property (point) 'agent-q-context-item)))
        (should (agent-q-context-item-p stored))
        (should (eq :symbol (agent-q-context-item-type stored)))))))

(ert-deftest agent-q-context/pill/has-keymap ()
  "Test that pill has its own keymap."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :buffer
                 :display-name "*scratch*")))
      (agent-q--insert-context-pill item)
      (goto-char (point-min))
      (search-forward "[@")
      (should (keymapp (get-text-property (point) 'keymap))))))

(ert-deftest agent-q-context/pill/tooltip ()
  "Test that pill has help-echo tooltip."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :file
                 :display-name "config.el"
                 :data '(:path "/path/to/config.el"))))
      (agent-q--insert-context-pill item)
      (goto-char (point-min))
      (search-forward "[@")
      (let ((tooltip (get-text-property (point) 'help-echo)))
        (should tooltip)
        (should (stringp tooltip))))))

(ert-deftest agent-q-context/pill/tooltip-file-content ()
  "Test that file pill tooltip contains the path."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :file
                 :display-name "config.el"
                 :data '(:path "/path/to/config.el"))))
      (agent-q--insert-context-pill item)
      (goto-char (point-min))
      (search-forward "[@")
      (let ((tooltip (get-text-property (point) 'help-echo)))
        (should (string-match-p "File:" tooltip))
        (should (string-match-p "/path/to/config.el" tooltip))))))

(ert-deftest agent-q-context/pill/tooltip-symbol-content ()
  "Test that symbol pill tooltip contains the name."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :symbol
                 :display-name "my-function"
                 :data '(:name "my-function"))))
      (agent-q--insert-context-pill item)
      (goto-char (point-min))
      (search-forward "[@")
      (let ((tooltip (get-text-property (point) 'help-echo)))
        (should (string-match-p "Symbol:" tooltip))
        (should (string-match-p "my-function" tooltip))))))

(ert-deftest agent-q-context/pill/tooltip-buffer-content ()
  "Test that buffer pill tooltip contains buffer name."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :buffer
                 :display-name "*scratch*"
                 :data '(:buffer-name "*scratch*"))))
      (agent-q--insert-context-pill item)
      (goto-char (point-min))
      (search-forward "[@")
      (let ((tooltip (get-text-property (point) 'help-echo)))
        (should (string-match-p "Buffer:" tooltip))))))

(ert-deftest agent-q-context/pill/keymap-has-mouse-binding ()
  "Test that pill keymap has mouse-1 binding."
  (should (keymapp agent-q-context-pill-map))
  (should (lookup-key agent-q-context-pill-map [mouse-1])))

(ert-deftest agent-q-context/pill/keymap-has-ret-binding ()
  "Test that pill keymap has RET binding."
  (should (lookup-key agent-q-context-pill-map (kbd "RET"))))

(ert-deftest agent-q-context/pill/keymap-has-del-binding ()
  "Test that pill keymap has DEL binding."
  (should (lookup-key agent-q-context-pill-map (kbd "DEL"))))

(ert-deftest agent-q-context/pill/keymap-has-describe-binding ()
  "Test that pill keymap has ? binding for describe."
  (should (lookup-key agent-q-context-pill-map (kbd "?"))))

(ert-deftest agent-q-context/pill/face-defined ()
  "Test that the context pill face is defined."
  (should (facep 'agent-q-context-pill-face)))

(ert-deftest agent-q-context/pill/describe-shows-tooltip ()
  "Test that describe command displays tooltip."
  (with-temp-buffer
    (let ((item (make-agent-q-context-item
                 :type :file
                 :display-name "test.el"
                 :data '(:path "/test/path/test.el"))))
      (agent-q--insert-context-pill item)
      (goto-char (+ (point-min) 2))  ; Inside the pill
      ;; The describe function should not error and should return something
      (should (agent-q--context-pill-tooltip item)))))

;;;; Task 9: Context Panel

(ert-deftest agent-q-context/panel/creates-buffer ()
  "Test that context panel creates a buffer."
  (unwind-protect
      (progn
        (agent-q-show-context-panel)
        (should (get-buffer "*Agent-Q Context*")))
    (when (get-buffer "*Agent-Q Context*")
      (kill-buffer "*Agent-Q Context*"))))

(ert-deftest agent-q-context/panel/shows-empty-message ()
  "Test that panel shows message when no context."
  (let ((agent-q-context-items nil))
    (unwind-protect
        (progn
          (agent-q-show-context-panel)
          (with-current-buffer "*Agent-Q Context*"
            (goto-char (point-min))
            (should (search-forward "No context" nil t))))
      (when (get-buffer "*Agent-Q Context*")
        (kill-buffer "*Agent-Q Context*")))))

(ert-deftest agent-q-context/panel/toggle-works ()
  "Test toggling context panel."
  (unwind-protect
      (progn
        ;; First toggle shows
        (agent-q-toggle-context-panel)
        (should (get-buffer-window "*Agent-Q Context*"))
        ;; Second toggle hides
        (agent-q-toggle-context-panel)
        (should-not (get-buffer-window "*Agent-Q Context*")))
    (when (get-buffer "*Agent-Q Context*")
      (kill-buffer "*Agent-Q Context*"))))

(ert-deftest agent-q-context/panel/has-correct-mode ()
  "Test that panel buffer has correct major mode."
  (unwind-protect
      (progn
        (agent-q-show-context-panel)
        (with-current-buffer "*Agent-Q Context*"
          (should (eq major-mode 'agent-q-context-panel-mode))))
    (when (get-buffer "*Agent-Q Context*")
      (kill-buffer "*Agent-Q Context*"))))

(ert-deftest agent-q-context/panel/buffer-name-variable ()
  "Test that buffer name variable is defined."
  (should (boundp 'agent-q-context-panel-buffer))
  (should (stringp agent-q-context-panel-buffer))
  (should (string= "*Agent-Q Context*" agent-q-context-panel-buffer)))

(ert-deftest agent-q-context/panel/context-items-variable ()
  "Test that context items buffer-local variable exists."
  (with-temp-buffer
    (should (local-variable-if-set-p 'agent-q-context-items))))

(ert-deftest agent-q-context/panel/button-face-defined ()
  "Test that context button face is defined."
  (should (facep 'agent-q-context-button-face)))

(ert-deftest agent-q-context/panel/refresh-function-exists ()
  "Test that refresh function is defined."
  (should (fboundp 'agent-q--refresh-context-panel)))

(ert-deftest agent-q-context/panel/insert-panel-item-function-exists ()
  "Test that insert panel item function is defined."
  (should (fboundp 'agent-q--insert-context-panel-item)))

(ert-deftest agent-q-context/panel/visit-context-item-function-exists ()
  "Test that visit context item function is defined."
  (should (fboundp 'agent-q--visit-context-item)))

(ert-deftest agent-q-context/panel/remove-context-item-function-exists ()
  "Test that remove context item function is defined."
  (should (fboundp 'agent-q--remove-context-item)))

(ert-deftest agent-q-context/panel/displays-items ()
  "Test that panel displays context items when present."
  (with-temp-buffer
    (rename-buffer "*Agent-Q Chat*" t)
    (setq-local agent-q-context-items
                (list (make-agent-q-context-item
                       :type :file
                       :display-name "test.lisp"
                       :data '(:path "/path/to/test.lisp")
                       :content "(defun test () 42)")))
    (unwind-protect
        (progn
          (agent-q-show-context-panel)
          (with-current-buffer "*Agent-Q Context*"
            (goto-char (point-min))
            ;; Search from beginning for both: FILE label and name
            (should (search-forward "[FILE]" nil t))
            (goto-char (point-min))
            (should (search-forward "test.lisp" nil t))))
      (when (get-buffer "*Agent-Q Context*")
        (kill-buffer "*Agent-Q Context*"))
      (kill-buffer "*Agent-Q Chat*"))))

(ert-deftest agent-q-context/panel/has-clear-all-button ()
  "Test that panel has Clear All button."
  (unwind-protect
      (progn
        (agent-q-show-context-panel)
        (with-current-buffer "*Agent-Q Context*"
          (goto-char (point-min))
          (should (search-forward "[Clear All]" nil t))))
    (when (get-buffer "*Agent-Q Context*")
      (kill-buffer "*Agent-Q Context*"))))

(ert-deftest agent-q-context/panel/mode-derived-from-special ()
  "Test that panel mode is derived from special-mode."
  (unwind-protect
      (progn
        (agent-q-show-context-panel)
        (with-current-buffer "*Agent-Q Context*"
          (should (derived-mode-p 'special-mode))))
    (when (get-buffer "*Agent-Q Context*")
      (kill-buffer "*Agent-Q Context*"))))

(ert-deftest agent-q-context/panel/remove-item-from-list ()
  "Test that remove function removes item from context list."
  (with-temp-buffer
    (rename-buffer "*Agent-Q Chat*" t)
    (let ((item1 (make-agent-q-context-item :type :file :display-name "a.lisp"))
          (item2 (make-agent-q-context-item :type :buffer :display-name "*scratch*")))
      (setq-local agent-q-context-items (list item1 item2))
      (unwind-protect
          (progn
            (should (= 2 (length agent-q-context-items)))
            (agent-q--remove-context-item item1)
            (should (= 1 (length agent-q-context-items)))
            (should (eq item2 (car agent-q-context-items))))
        (kill-buffer "*Agent-Q Chat*")))))

;;;; Task 10: Interactive Commands

(ert-deftest agent-q-context/commands/clear-empties-list ()
  "Test that clear-context empties the context list."
  (with-temp-buffer
    (setq-local agent-q-context-items
                (list (make-agent-q-context-item :type :file :display-name "test")))
    (should agent-q-context-items)
    (agent-q-clear-context)
    (should-not agent-q-context-items)))

(ert-deftest agent-q-context/commands/clear-is-interactive ()
  "Test that clear-context is an interactive command."
  (should (commandp 'agent-q-clear-context)))

(ert-deftest agent-q-context/commands/add-file-creates-item ()
  "Test that add-context-file creates proper item."
  (let* ((temp-file (make-temp-file "agent-q-add-test"))
         (item nil))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert "content"))
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) temp-file)))
            (setq item (agent-q--add-context-file)))
          (should (agent-q-context-item-p item))
          (should (eq :file (agent-q-context-item-type item)))
          (should (agent-q-context-item-content item)))
      (delete-file temp-file))))

(ert-deftest agent-q-context/commands/add-file-sets-display-name ()
  "Test that add-context-file uses file basename as display name."
  (let* ((temp-file (make-temp-file "agent-q-name-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert "content"))
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) temp-file)))
            (let ((item (agent-q--add-context-file)))
              (should (string= (file-name-nondirectory temp-file)
                              (agent-q-context-item-display-name item))))))
      (delete-file temp-file))))

(ert-deftest agent-q-context/commands/add-file-stores-path ()
  "Test that add-context-file stores expanded file path in data."
  (let* ((temp-file (make-temp-file "agent-q-path-test")))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert "content"))
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) temp-file)))
            (let* ((item (agent-q--add-context-file))
                   (data (agent-q-context-item-data item)))
              (should (string= (expand-file-name temp-file)
                              (plist-get data :path))))))
      (delete-file temp-file))))

(ert-deftest agent-q-context/commands/add-buffer-creates-item ()
  "Test that add-context-buffer creates proper item."
  (with-temp-buffer
    (rename-buffer "agent-q-add-buf-test" t)
    (insert "buffer content")
    (unwind-protect
        (cl-letf (((symbol-function 'read-buffer)
                   (lambda (&rest _) "agent-q-add-buf-test")))
          (let ((item (agent-q--add-context-buffer)))
            (should (agent-q-context-item-p item))
            (should (eq :buffer (agent-q-context-item-type item)))
            (should (string= "buffer content"
                            (agent-q-context-item-content item)))))
      (kill-buffer "agent-q-add-buf-test"))))

(ert-deftest agent-q-context/commands/add-buffer-sets-display-name ()
  "Test that add-context-buffer uses buffer name as display name."
  (with-temp-buffer
    (rename-buffer "agent-q-display-buf-test" t)
    (insert "content")
    (unwind-protect
        (cl-letf (((symbol-function 'read-buffer)
                   (lambda (&rest _) "agent-q-display-buf-test")))
          (let ((item (agent-q--add-context-buffer)))
            (should (string= "agent-q-display-buf-test"
                            (agent-q-context-item-display-name item)))))
      (kill-buffer "agent-q-display-buf-test"))))

(ert-deftest agent-q-context/commands/add-region-creates-item ()
  "Test that add-context-region creates proper item."
  (with-temp-buffer
    (rename-buffer "agent-q-region-test" t)
    (transient-mark-mode 1)  ; Enable for batch mode
    (insert "line 1\nselected text\nline 3")
    (goto-char 8)
    (push-mark 21 t t)  ; Activate mark
    (unwind-protect
        (let ((item (agent-q--add-context-region)))
          (should (agent-q-context-item-p item))
          (should (eq :region (agent-q-context-item-type item)))
          (should (string= "selected text"
                          (agent-q-context-item-content item))))
      (kill-buffer "agent-q-region-test"))))

(ert-deftest agent-q-context/commands/add-region-requires-active-region ()
  "Test that add-context-region errors when no region is active."
  (with-temp-buffer
    (insert "no selection")
    (deactivate-mark)
    (should-error (agent-q--add-context-region) :type 'user-error)))

(ert-deftest agent-q-context/commands/add-region-display-name-format ()
  "Test that add-context-region display name includes buffer and line numbers."
  (with-temp-buffer
    (rename-buffer "region-name-test" t)
    (transient-mark-mode 1)  ; Enable for batch mode
    (insert "line 1\nselected\nline 3")
    (goto-char 8)
    (push-mark 16 t t)  ; Activate mark
    (unwind-protect
        (let ((item (agent-q--add-context-region)))
          (let ((display-name (agent-q-context-item-display-name item)))
            (should (string-match-p "region-name-test" display-name))
            (should (string-match-p ":" display-name))))
      (kill-buffer "region-name-test"))))

(ert-deftest agent-q-context/commands/add-symbol-creates-item ()
  "Test that add-context-symbol creates proper item."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun agent-q-sym-test-fn () 42)\n")
    (setq imenu--index-alist nil)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _)
                 (propertize "agent-q-sym-test-fn"
                             'agent-q-context-type :symbol
                             'agent-q-context-data (list :name "agent-q-sym-test-fn"
                                                         :position (point-min-marker))))))
      (let ((item (agent-q--add-context-symbol)))
        (should (agent-q-context-item-p item))
        (should (eq :symbol (agent-q-context-item-type item)))))))

(ert-deftest agent-q-context/commands/add-context-is-interactive ()
  "Test that add-context is an interactive command."
  (should (commandp 'agent-q-add-context)))

(ert-deftest agent-q-context/commands/add-context-dispatches-file ()
  "Test that add-context dispatches to file handler."
  (let* ((temp-file (make-temp-file "agent-q-dispatch-test"))
         (dispatched nil))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert "content"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "file"))
                    ((symbol-function 'agent-q--add-context-file)
                     (lambda ()
                       (setq dispatched t)
                       (make-agent-q-context-item :type :file :display-name "test"))))
            (with-temp-buffer
              (agent-q-add-context))
            (should dispatched)))
      (delete-file temp-file))))

(ert-deftest agent-q-context/commands/add-context-adds-to-list ()
  "Test that add-context adds item to context list."
  (with-temp-buffer
    (setq-local agent-q-context-items nil)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "buffer"))
              ((symbol-function 'agent-q--add-context-buffer)
               (lambda ()
                 (make-agent-q-context-item :type :buffer
                                            :display-name "*test*"
                                            :content "content"))))
      (agent-q-add-context)
      (should (= 1 (length agent-q-context-items)))
      (should (eq :buffer (agent-q-context-item-type (car agent-q-context-items)))))))

;;;; Task 11: Chat Integration

(ert-deftest agent-q-context/integration/format-for-llm ()
  "Test formatting context items for LLM prompt."
  (let ((agent-q-context-items
         (list (make-agent-q-context-item
                :type :file
                :display-name "test.lisp"
                :content "(defun test () 42)"))))
    (let ((formatted (agent-q--format-context-for-llm)))
      (should (stringp formatted))
      (should (string-match-p "<context>" formatted))
      (should (string-match-p "test.lisp" formatted))
      (should (string-match-p "(defun test () 42)" formatted)))))

(ert-deftest agent-q-context/integration/empty-context-returns-nil ()
  "Test that empty context returns nil, not empty string."
  (let ((agent-q-context-items nil))
    (should-not (agent-q--format-context-for-llm))))

(ert-deftest agent-q-context/integration/multiple-items ()
  "Test formatting multiple context items."
  (let ((agent-q-context-items
         (list (make-agent-q-context-item
                :type :file
                :display-name "a.lisp"
                :content "content-a")
               (make-agent-q-context-item
                :type :buffer
                :display-name "*scratch*"
                :content "content-b"))))
    (let ((formatted (agent-q--format-context-for-llm)))
      (should (string-match-p "a.lisp" formatted))
      (should (string-match-p "\\*scratch\\*" formatted))
      (should (string-match-p "content-a" formatted))
      (should (string-match-p "content-b" formatted)))))

(ert-deftest agent-q-context/integration/format-includes-closing-tag ()
  "Test that formatted context includes closing </context> tag."
  (let ((agent-q-context-items
         (list (make-agent-q-context-item
                :type :file
                :display-name "test.lisp"
                :content "test"))))
    (let ((formatted (agent-q--format-context-for-llm)))
      (should (string-match-p "</context>" formatted)))))

(ert-deftest agent-q-context/integration/format-includes-type ()
  "Test that formatted context includes item type."
  (let ((agent-q-context-items
         (list (make-agent-q-context-item
                :type :file
                :display-name "test.lisp"
                :content "content"))))
    (let ((formatted (agent-q--format-context-for-llm)))
      (should (string-match-p ":file\\|file" formatted)))))

(ert-deftest agent-q-context/integration/format-handles-nil-content ()
  "Test that formatted context handles items with nil content."
  (let ((agent-q-context-items
         (list (make-agent-q-context-item
                :type :file
                :display-name "test.lisp"
                :content nil))))
    (let ((formatted (agent-q--format-context-for-llm)))
      (should (stringp formatted))
      (should (string-match-p "unavailable\\|nil\\|empty" formatted)))))

(provide 'sly-agent-q-context-test)
;;; sly-agent-q-context-test.el ends here
