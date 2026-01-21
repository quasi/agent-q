# Chat Phase 4: Context Management Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement @-mention completion and context panel, allowing users to attach files, symbols, and buffers to conversations via inline pills and sidebar.

**Architecture:** New `sly-agent-q-context.el` module handles context items, completion, and panel UI. Context state lives as buffer-local variable in chat buffer. Integration with chat via hooks and modified send function. Uses `completion-at-point` for framework-agnostic completion.

**Tech Stack:** Emacs Lisp 27.1+, ERT for testing, `project.el` for file candidates, `imenu` for symbol candidates

---

## Task Overview

| Task | Description | Files |
|------|-------------|-------|
| 1 | Create context item data structure | context.el, context-test.el |
| 2 | Implement file candidates | context.el, context-test.el |
| 3 | Implement symbol candidates | context.el, context-test.el |
| 4 | Implement buffer candidates | context.el, context-test.el |
| 5 | Implement @-mention bounds detection | context.el, context-test.el |
| 6 | Implement completion-at-point function | context.el, context-test.el |
| 7 | Implement content fetching | context.el, context-test.el |
| 8 | Implement context pill rendering | context.el, context-test.el |
| 9 | Implement context panel buffer | context.el, context-test.el |
| 10 | Implement interactive add/clear commands | context.el, context-test.el |
| 11 | Integrate context with chat send | chat.el |
| 12 | Add faces and keybindings | context.el, chat.el |
| 13 | Final integration and test runner | run.el |

---

## Task 1: Create Context Item Data Structure

**Files:**
- Create: `contrib/sly-agent-q/sly-agent-q-context.el`
- Create: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 1.1: Write the failing test for context item struct

```elisp
;;; sly-agent-q-context-test.el --- Tests for context management -*- lexical-binding: t; -*-

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

(provide 'sly-agent-q-context-test)
;;; sly-agent-q-context-test.el ends here
```

### Step 1.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "Cannot open load file" (module doesn't exist)

### Step 1.3: Write minimal implementation

```elisp
;;; sly-agent-q-context.el --- Context management for Agent-Q chat -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: lisp, ai, context
;; URL: https://github.com/quasilabs/agent-q

;;; Commentary:

;; Context management for Agent-Q chat interface.
;; Provides @-mention completion, context pills, and a context panel.
;;
;; Context items can be:
;; - Files from the project
;; - Symbols (functions, variables) from Lisp buffers
;; - Buffer contents
;; - Selected regions
;; - URLs

;;; Code:

(require 'cl-lib)

;;; Data Structures

(cl-defstruct (agent-q-context-item (:constructor make-agent-q-context-item))
  "A context item attached to a message."
  (type nil :documentation "Type: :file | :symbol | :buffer | :region | :url")
  (display-name nil :documentation "Short name shown in pill")
  (data nil :documentation "Type-specific data (plist)")
  (content nil :documentation "Actual content to send to LLM"))

(provide 'sly-agent-q-context)
;;; sly-agent-q-context.el ends here
```

### Step 1.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (2 tests)

### Step 1.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add context item data structure

Phase 4 foundation - cl-defstruct for context items with type,
display-name, data, and content slots. Supports file, symbol,
buffer, region, and url types.
EOF
)"
```

---

## Task 2: Implement File Candidates

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 2.1: Write the failing test

Add to test file after Task 1 tests:

```elisp
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
                     (lambda () (cons 'transient temp-dir)))
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
                     (lambda () (cons 'transient temp-dir)))
                    ((symbol-function 'project-files)
                     (lambda (_)
                       (directory-files temp-dir t "\\.lisp\\'"))))
            (let* ((candidates (agent-q--file-candidates "test"))
                   (candidate (car candidates)))
              (when candidate
                (should (eq :file (get-text-property 0 'agent-q-context-type candidate)))
                (should (plist-get (get-text-property 0 'agent-q-context-data candidate) :path))))))
      (delete-directory temp-dir t))))
```

### Step 2.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q--file-candidates"

### Step 2.3: Write minimal implementation

Add after data structures in context.el:

```elisp
;;; Candidate Gathering

(defun agent-q--file-candidates (prefix)
  "Return file candidates matching PREFIX.
Searches project files if `project-current' returns a project."
  (when-let ((project (project-current)))
    (let ((files (project-files project)))
      (mapcar (lambda (file)
                (propertize (file-name-nondirectory file)
                            'agent-q-context-type :file
                            'agent-q-context-data (list :path file)))
              (seq-filter (lambda (f)
                            (string-prefix-p prefix (file-name-nondirectory f) t))
                          files)))))
```

Also add require at top:
```elisp
(require 'project)
(require 'seq)
```

### Step 2.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (5 tests)

### Step 2.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add file candidate gathering

Uses project.el to find project files, filters by prefix,
attaches :file type and :path data as text properties for
completion framework to use.
EOF
)"
```

---

## Task 3: Implement Symbol Candidates

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 3.1: Write the failing test

Add to test file:

```elisp
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
```

### Step 3.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q--symbol-candidates"

### Step 3.3: Write minimal implementation

Add to context.el:

```elisp
(require 'imenu)

(defun agent-q--flatten-imenu (index prefix)
  "Flatten imenu INDEX, filtering by PREFIX.
Returns list of propertized candidate strings."
  (let ((result nil))
    (dolist (item index)
      (cond
       ((imenu--subalist-p item)
        (setq result (nconc result (agent-q--flatten-imenu (cdr item) prefix))))
       ((and (stringp (car item))
             (string-prefix-p prefix (car item) t))
        (push (propertize (car item)
                          'agent-q-context-type :symbol
                          'agent-q-context-data (list :name (car item)
                                                      :position (cdr item)))
              result))))
    result))

(defun agent-q--symbol-candidates (prefix)
  "Return symbol candidates matching PREFIX.
Gathers from imenu of recent Lisp buffers."
  (let ((symbols nil))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'lisp-mode 'emacs-lisp-mode)
          (ignore-errors
            (let ((index (imenu--make-index-alist t)))
              (setq symbols (nconc symbols
                                   (agent-q--flatten-imenu index prefix))))))))
    (seq-take symbols 50)))
```

### Step 3.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (9 tests)

### Step 3.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add symbol candidate gathering

Scans imenu of all open Lisp/Elisp buffers for function and
variable definitions. Flattens nested imenu structures and
limits to 50 results to prevent completion overload.
EOF
)"
```

---

## Task 4: Implement Buffer Candidates

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 4.1: Write the failing test

Add to test file:

```elisp
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
```

### Step 4.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q--buffer-candidates"

### Step 4.3: Write minimal implementation

Add to context.el:

```elisp
(defun agent-q--buffer-candidates (prefix)
  "Return buffer candidates matching PREFIX.
Excludes internal buffers (those starting with space)."
  (mapcar (lambda (buf)
            (propertize (buffer-name buf)
                        'agent-q-context-type :buffer
                        'agent-q-context-data (list :buffer-name (buffer-name buf))))
          (seq-filter (lambda (buf)
                        (let ((name (buffer-name buf)))
                          (and (not (string-prefix-p " " name))
                               (string-prefix-p prefix name t))))
                      (buffer-list))))
```

### Step 4.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (13 tests)

### Step 4.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add buffer candidate gathering

Lists all non-internal buffers filtered by prefix.
Attaches :buffer type and :buffer-name data for later content fetching.
EOF
)"
```

---

## Task 5: Implement @-mention Bounds Detection

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 5.1: Write the failing test

Add to test file:

```elisp
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
```

### Step 5.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q--context-mention-bounds"

### Step 5.3: Write minimal implementation

Add to context.el:

```elisp
;;; @-Mention Detection

(defun agent-q--context-mention-bounds ()
  "Return bounds of @-mention at point, or nil.
Returns (START . END) where START is position of @ and END is point."
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward "@\\([^ \t\n]*\\)" (line-beginning-position) t)
        (cons (match-beginning 0) end)))))
```

### Step 5.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (18 tests)

### Step 5.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add @-mention bounds detection

Detects @prefix patterns on current line for completion.
Returns cons of (start . end) positions or nil.
EOF
)"
```

---

## Task 6: Implement Completion-at-Point Function

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 6.1: Write the failing test

Add to test file:

```elisp
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
      (should (= 4 (length result)))  ; (start end table . props)
      (should (numberp (nth 0 result)))  ; start
      (should (numberp (nth 1 result)))  ; end
      (should (functionp (nth 2 result)))))) ; completion table

(ert-deftest agent-q-context/capf/combined-candidates ()
  "Test that combined candidates include all types."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun capf-test-fn () nil)")
    (setq imenu--index-alist nil)
    (let ((candidates (agent-q--context-candidates "@capf-test")))
      ;; Should have symbol from our buffer
      (should (cl-some (lambda (c) (string-match-p "capf-test" c))
                       candidates)))))

(ert-deftest agent-q-context/capf/annotation-function ()
  "Test annotation function returns type labels."
  (should (string= " [file]"
                   (agent-q--context-annotation
                    (propertize "test" 'agent-q-context-type :file))))
  (should (string= " [symbol]"
                   (agent-q--context-annotation
                    (propertize "test" 'agent-q-context-type :symbol))))
  (should (string= " [buffer]"
                   (agent-q--context-annotation
                    (propertize "test" 'agent-q-context-type :buffer)))))
```

### Step 6.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q-context-complete-at-point"

### Step 6.3: Write minimal implementation

Add to context.el:

```elisp
;;; Completion at Point

(defun agent-q--context-candidates (prefix)
  "Return completion candidates matching PREFIX.
Combines files, symbols, and buffers."
  (let ((prefix-no-at (if (string-prefix-p "@" prefix)
                          (substring prefix 1)
                        prefix)))
    (append
     (agent-q--file-candidates prefix-no-at)
     (agent-q--symbol-candidates prefix-no-at)
     (agent-q--buffer-candidates prefix-no-at))))

(defun agent-q--context-annotation (candidate)
  "Return annotation for CANDIDATE showing its type."
  (let ((type (get-text-property 0 'agent-q-context-type candidate)))
    (pcase type
      (:file " [file]")
      (:symbol " [symbol]")
      (:buffer " [buffer]")
      (:region " [region]")
      (:url " [url]")
      (_ ""))))

(defun agent-q-context-complete-at-point ()
  "Completion-at-point function for @-mentions.
Returns completion data when point is after an @-mention."
  (when-let ((bounds (agent-q--context-mention-bounds)))
    (list (car bounds)
          (cdr bounds)
          (completion-table-dynamic #'agent-q--context-candidates)
          :exclusive 'no
          :annotation-function #'agent-q--context-annotation)))
```

### Step 6.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (22 tests)

### Step 6.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add completion-at-point function

Integrates with Emacs completion framework via CAPF.
Combines file, symbol, and buffer candidates with type annotations.
Works with any completion UI (vertico, ivy, helm, default).
EOF
)"
```

---

## Task 7: Implement Content Fetching

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 7.1: Write the failing test

Add to test file:

```elisp
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
            (should (<= (length fetched) 50000))))
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
          (should (<= (length fetched) 50000)))
      (kill-buffer "agent-q-size-buf-test"))))

(ert-deftest agent-q-context/fetch/nonexistent-returns-nil ()
  "Test that fetching nonexistent content returns nil."
  (should-not (agent-q--fetch-context-content
               :file (list :path "/nonexistent/path/file.txt")))
  (should-not (agent-q--fetch-context-content
               :buffer (list :buffer-name "nonexistent-buffer-xyz"))))
```

### Step 7.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q--fetch-context-content"

### Step 7.3: Write minimal implementation

Add to context.el:

```elisp
;;; Content Fetching

(defconst agent-q-context-max-size 50000
  "Maximum size in bytes for context content.")

(defun agent-q--fetch-context-content (type data)
  "Fetch actual content for context item.
TYPE is the context type (:file, :buffer, :symbol, :region).
DATA is the type-specific data plist."
  (pcase type
    (:file
     (let ((path (plist-get data :path)))
       (when (and path (file-readable-p path))
         (with-temp-buffer
           (insert-file-contents path nil 0 agent-q-context-max-size)
           (buffer-string)))))
    (:symbol
     (when-let* ((pos (plist-get data :position))
                 (buf (and (markerp pos) (marker-buffer pos))))
       (with-current-buffer buf
         (save-excursion
           (goto-char pos)
           (let ((bounds (bounds-of-thing-at-point 'defun)))
             (when bounds
               (buffer-substring-no-properties (car bounds) (cdr bounds))))))))
    (:buffer
     (when-let ((buf (get-buffer (plist-get data :buffer-name))))
       (with-current-buffer buf
         (buffer-substring-no-properties
          (point-min)
          (min (point-max) (+ (point-min) agent-q-context-max-size))))))
    (:region
     (let ((buf (get-buffer (plist-get data :buffer)))
           (start (plist-get data :start))
           (end (plist-get data :end)))
       (when (and buf start end)
         (with-current-buffer buf
           (buffer-substring-no-properties start (min end (+ start agent-q-context-max-size)))))))
    (_ nil)))
```

### Step 7.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (27 tests)

### Step 7.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add content fetching for context items

Fetches actual content for file, buffer, symbol, and region types.
Limits content to 50KB to prevent context window overflow.
Returns nil for nonexistent or unreadable sources.
EOF
)"
```

---

## Task 8: Implement Context Pill Rendering

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 8.1: Write the failing test

Add to test file:

```elisp
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
```

### Step 8.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q--insert-context-pill"

### Step 8.3: Write minimal implementation

Add to context.el:

```elisp
;;; Faces

(defface agent-q-context-pill-face
  '((t :foreground "#61afef"
       :background "#3e4451"
       :box (:line-width -1 :color "#61afef")
       :weight bold))
  "Face for context pills in input."
  :group 'agent-q-chat)

;;; Context Pill Rendering

(defvar agent-q-context-pill-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'agent-q-context-pill-click)
    (define-key map (kbd "RET") #'agent-q-context-pill-visit)
    (define-key map (kbd "DEL") #'agent-q-context-pill-remove)
    (define-key map (kbd "?") #'agent-q-context-pill-describe)
    map)
  "Keymap for context pills.")

(defun agent-q--context-pill-tooltip (item)
  "Generate tooltip string for context ITEM."
  (let ((type (agent-q-context-item-type item))
        (data (agent-q-context-item-data item)))
    (pcase type
      (:file (format "File: %s" (plist-get data :path)))
      (:symbol (format "Symbol: %s" (plist-get data :name)))
      (:buffer (format "Buffer: %s" (plist-get data :buffer-name)))
      (:region (format "Region in %s" (plist-get data :buffer)))
      (:url (format "URL: %s" (plist-get data :url)))
      (_ "Context item"))))

(defun agent-q--insert-context-pill (item)
  "Insert visual pill for context ITEM."
  (let ((start (point)))
    (insert (propertize (format "[@%s]" (agent-q-context-item-display-name item))
                        'face 'agent-q-context-pill-face
                        'agent-q-context-item item
                        'read-only t
                        'rear-nonsticky t
                        'mouse-face 'highlight
                        'keymap agent-q-context-pill-map
                        'help-echo (agent-q--context-pill-tooltip item)))
    (insert " ")))

;; Placeholder pill commands (to be implemented)
(defun agent-q-context-pill-click (_event)
  "Handle click on context pill."
  (interactive "e")
  (message "Context pill clicked"))

(defun agent-q-context-pill-visit ()
  "Visit source of context pill at point."
  (interactive)
  (message "Visit not yet implemented"))

(defun agent-q-context-pill-remove ()
  "Remove context pill at point."
  (interactive)
  (message "Remove not yet implemented"))

(defun agent-q-context-pill-describe ()
  "Describe context pill at point."
  (interactive)
  (when-let ((item (get-text-property (point) 'agent-q-context-item)))
    (message "%s" (agent-q--context-pill-tooltip item))))
```

### Step 8.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (32 tests)

### Step 8.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add context pill rendering

Inserts [@name] pills with custom face, keymap, and tooltip.
Pills store original context item for later access.
Includes placeholder commands for visit/remove/describe.
EOF
)"
```

---

## Task 9: Implement Context Panel Buffer

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 9.1: Write the failing test

Add to test file:

```elisp
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
```

### Step 9.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q-show-context-panel"

### Step 9.3: Write minimal implementation

Add to context.el:

```elisp
;;; Buffer-Local State

(defvar-local agent-q-context-items nil
  "List of context items attached to current message.")

;;; Context Panel

(defvar agent-q-context-panel-buffer "*Agent-Q Context*"
  "Buffer name for context panel.")

(defface agent-q-context-button-face
  '((t :foreground "#98c379" :weight bold))
  "Face for clickable buttons."
  :group 'agent-q-chat)

(define-derived-mode agent-q-context-panel-mode special-mode "Context"
  "Major mode for Agent-Q context panel."
  (setq-local revert-buffer-function #'agent-q--refresh-context-panel))

(defun agent-q--refresh-context-panel (&rest _)
  "Refresh context panel contents."
  (let ((inhibit-read-only t)
        (items (when-let ((chat-buf (get-buffer "*Agent-Q Chat*")))
                 (buffer-local-value 'agent-q-context-items chat-buf))))
    (erase-buffer)
    (insert (propertize "Context Items\n" 'face 'bold))
    (insert (propertize (make-string 30 ?─) 'face 'shadow))
    (insert "\n\n")

    (if items
        (dolist (item items)
          (agent-q--insert-context-panel-item item))
      (insert (propertize "No context attached.\n\n" 'face 'shadow)
              "Use @filename to add files,\n"
              "or C-c @ to add interactively."))

    (insert "\n\n")
    (insert-button "[Clear All]"
                   'action (lambda (_)
                             (agent-q-clear-context)
                             (agent-q--refresh-context-panel))
                   'face 'agent-q-context-button-face)))

(defun agent-q--insert-context-panel-item (item)
  "Insert ITEM in context panel."
  (let ((type (agent-q-context-item-type item))
        (name (agent-q-context-item-display-name item)))
    (insert (propertize (format "[%s] "
                                (pcase type
                                  (:file "FILE")
                                  (:symbol "SYM")
                                  (:buffer "BUF")
                                  (:region "REG")
                                  (:url "URL")
                                  (_ "?")))
                        'face 'font-lock-type-face))
    (insert-button name
                   'action (lambda (_) (agent-q--visit-context-item item))
                   'face 'link
                   'help-echo "Click to visit")
    (insert "  ")
    (insert-button "✕"
                   'action (lambda (_)
                             (agent-q--remove-context-item item)
                             (agent-q--refresh-context-panel))
                   'face 'error
                   'help-echo "Remove from context")
    (insert "\n")))

(defun agent-q--visit-context-item (item)
  "Visit the source of context ITEM."
  (let ((type (agent-q-context-item-type item))
        (data (agent-q-context-item-data item)))
    (pcase type
      (:file (find-file (plist-get data :path)))
      (:buffer (switch-to-buffer (plist-get data :buffer-name)))
      (:symbol (when-let ((pos (plist-get data :position)))
                 (when (markerp pos)
                   (switch-to-buffer (marker-buffer pos))
                   (goto-char pos))))
      (_ (message "Cannot visit this context type")))))

(defun agent-q--remove-context-item (item)
  "Remove ITEM from context list."
  (when-let ((chat-buf (get-buffer "*Agent-Q Chat*")))
    (with-current-buffer chat-buf
      (setq agent-q-context-items
            (delete item agent-q-context-items)))))

(defun agent-q-show-context-panel ()
  "Show context panel in side window."
  (interactive)
  (let ((buf (get-buffer-create agent-q-context-panel-buffer)))
    (with-current-buffer buf
      (agent-q-context-panel-mode)
      (agent-q--refresh-context-panel))
    (display-buffer buf
                    '(display-buffer-in-side-window
                      (side . right)
                      (window-width . 35)
                      (preserve-size . (t . nil))))))

(defun agent-q-toggle-context-panel ()
  "Toggle context panel sidebar."
  (interactive)
  (let ((win (get-buffer-window agent-q-context-panel-buffer)))
    (if win
        (delete-window win)
      (agent-q-show-context-panel))))
```

### Step 9.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (36 tests)

### Step 9.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add context panel sidebar

Side window showing all attached context items.
Items are clickable (visits source) with remove buttons.
Toggle with C-c C-p, includes Clear All action.
EOF
)"
```

---

## Task 10: Implement Interactive Add/Clear Commands

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 10.1: Write the failing test

Add to test file:

```elisp
;;;; Task 10: Interactive Commands

(ert-deftest agent-q-context/commands/clear-empties-list ()
  "Test that clear-context empties the context list."
  (with-temp-buffer
    (setq agent-q-context-items
          (list (make-agent-q-context-item :type :file :display-name "test")))
    (should agent-q-context-items)
    (agent-q-clear-context)
    (should-not agent-q-context-items)))

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

(ert-deftest agent-q-context/commands/add-region-creates-item ()
  "Test that add-context-region creates proper item."
  (with-temp-buffer
    (rename-buffer "agent-q-region-test" t)
    (insert "line 1\nselected text\nline 3")
    (set-mark 8)
    (goto-char 21)
    (unwind-protect
        (let ((item (agent-q--add-context-region)))
          (should (agent-q-context-item-p item))
          (should (eq :region (agent-q-context-item-type item)))
          (should (string= "selected text"
                          (agent-q-context-item-content item))))
      (kill-buffer "agent-q-region-test"))))
```

### Step 10.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q-clear-context"

### Step 10.3: Write minimal implementation

Add to context.el:

```elisp
;;; Interactive Commands

(defun agent-q-clear-context ()
  "Clear all context items."
  (interactive)
  (setq agent-q-context-items nil)
  (when (get-buffer agent-q-context-panel-buffer)
    (with-current-buffer agent-q-context-panel-buffer
      (agent-q--refresh-context-panel)))
  (message "Context cleared"))

(defun agent-q--add-context-file ()
  "Add file to context interactively.
Returns the created context item."
  (let* ((file (read-file-name "Add file: "))
         (content (agent-q--fetch-context-content :file (list :path file))))
    (make-agent-q-context-item
     :type :file
     :display-name (file-name-nondirectory file)
     :data (list :path (expand-file-name file))
     :content content)))

(defun agent-q--add-context-buffer ()
  "Add buffer to context interactively.
Returns the created context item."
  (let* ((buf-name (read-buffer "Add buffer: " nil t))
         (content (agent-q--fetch-context-content :buffer (list :buffer-name buf-name))))
    (make-agent-q-context-item
     :type :buffer
     :display-name buf-name
     :data (list :buffer-name buf-name)
     :content content)))

(defun agent-q--add-context-symbol ()
  "Add symbol to context interactively.
Returns the created context item."
  (let* ((sym (completing-read "Add symbol: "
                               (agent-q--symbol-candidates "")))
         (data (get-text-property 0 'agent-q-context-data sym))
         (content (agent-q--fetch-context-content :symbol data)))
    (make-agent-q-context-item
     :type :symbol
     :display-name sym
     :data data
     :content content)))

(defun agent-q--add-context-region ()
  "Add current region to context.
Returns the created context item."
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((start (region-beginning))
         (end (region-end))
         (buf-name (buffer-name))
         (content (buffer-substring-no-properties start end))
         (display (format "%s:%d-%d"
                         buf-name
                         (line-number-at-pos start)
                         (line-number-at-pos end))))
    (make-agent-q-context-item
     :type :region
     :display-name display
     :data (list :buffer buf-name :start start :end end)
     :content content)))

(defun agent-q-add-context ()
  "Interactively add context item."
  (interactive)
  (let* ((type (completing-read "Context type: "
                                '("file" "symbol" "buffer" "region")))
         (item (pcase type
                 ("file" (agent-q--add-context-file))
                 ("symbol" (agent-q--add-context-symbol))
                 ("buffer" (agent-q--add-context-buffer))
                 ("region" (agent-q--add-context-region)))))
    (when item
      (push item agent-q-context-items)
      (when (get-buffer agent-q-context-panel-buffer)
        (with-current-buffer agent-q-context-panel-buffer
          (agent-q--refresh-context-panel)))
      (message "Added %s to context" (agent-q-context-item-display-name item)))))
```

### Step 10.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (40 tests)

### Step 10.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add interactive add/clear commands

C-c @ prompts for type and adds file/symbol/buffer/region.
C-c C-x clears all context items. Each add function creates
properly typed context item with fetched content.
EOF
)"
```

---

## Task 11: Integrate Context with Chat Send

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/sly-agent-q-chat.el`
- Modify: `contrib/sly-agent-q/test/sly-agent-q-context-test.el`

### Step 11.1: Write the failing test

Add to test file:

```elisp
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
```

### Step 11.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q--format-context-for-llm"

### Step 11.3: Write minimal implementation

Add to context.el:

```elisp
;;; LLM Integration

(defun agent-q--format-context-for-llm ()
  "Format current context items for LLM prompt.
Returns nil if no context items are present."
  (when agent-q-context-items
    (concat
     "\n\n<context>\n"
     (mapconcat
      (lambda (item)
        (format "### %s (%s)\n```\n%s\n```\n"
                (agent-q-context-item-display-name item)
                (agent-q-context-item-type item)
                (or (agent-q-context-item-content item)
                    "(content unavailable)")))
      (reverse agent-q-context-items)
      "\n")
     "</context>\n")))
```

### Step 11.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (43 tests)

### Step 11.5: Modify chat.el to use context

Now modify `sly-agent-q-chat.el` to integrate context:

In `sly-agent-q-chat.el`, add require after existing requires:
```elisp
(require 'sly-agent-q-context)
```

Replace the existing `agent-q--send-to-agent` function:
```elisp
(defun agent-q--send-to-agent (content)
  "Send CONTENT to the Lisp agent via SLY.
Appends formatted context if any context items are present."
  (let ((full-content (concat content (agent-q--format-context-for-llm))))
    (agent-q--begin-assistant-response)
    (sly-eval-async
        `(agent-q:agent-q-send ,full-content :include-context t)
      (lambda (result)
        (agent-q--finalize-response result)))))
```

Replace the placeholder `agent-q-add-context` and `agent-q-clear-context`:
```elisp
;; Context commands are provided by sly-agent-q-context.el
;; Remove the placeholder definitions
```

Add keybinding for context panel in the keymap section:
```elisp
(define-key map (kbd "C-c C-p") #'agent-q-toggle-context-panel)
```

### Step 11.6: Run full test suite

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l test-helper.el -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (all tests)

### Step 11.7: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/sly-agent-q-chat.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): integrate context with chat message sending

Context items are formatted as <context> block appended to user
messages. Chat module now requires context module and uses real
add/clear implementations instead of placeholders.
EOF
)"
```

---

## Task 12: Add Completion Hook and Faces

**Files:**
- Modify: `contrib/sly-agent-q/sly-agent-q-context.el`
- Modify: `contrib/sly-agent-q/sly-agent-q-chat.el`

### Step 12.1: Write the failing test

Add to test file:

```elisp
;;;; Task 12: Completion Hook Integration

(ert-deftest agent-q-context/hook/capf-in-functions-list ()
  "Test that CAPF is added to completion-at-point-functions."
  (with-temp-buffer
    (agent-q-chat-mode)
    (should (memq #'agent-q-context-complete-at-point
                  completion-at-point-functions))))

(ert-deftest agent-q-context/hook/exit-function-creates-item ()
  "Test that completing @-mention creates context item."
  (with-temp-buffer
    (agent-q-chat-mode)
    (setq agent-q-context-items nil)
    ;; Simulate completion exit
    (let* ((temp-file (make-temp-file "agent-q-exit-test"))
           (candidate (propertize "test.lisp"
                                  'agent-q-context-type :file
                                  'agent-q-context-data (list :path temp-file))))
      (unwind-protect
          (progn
            (insert "@test")
            (agent-q--context-exit-function candidate 'finished)
            (should (= 1 (length agent-q-context-items))))
        (delete-file temp-file)))))
```

### Step 12.2: Run test to verify it fails

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: FAIL with "void function agent-q--context-exit-function"

### Step 12.3: Write implementation

Add to context.el:

```elisp
;;; Completion Exit Function

(defun agent-q--context-exit-function (candidate status)
  "Handle completion exit for CANDIDATE with STATUS.
When STATUS is 'finished, creates context item and inserts pill."
  (when (eq status 'finished)
    (let* ((type (get-text-property 0 'agent-q-context-type candidate))
           (data (get-text-property 0 'agent-q-context-data candidate))
           (item (make-agent-q-context-item
                  :type type
                  :display-name candidate
                  :data data
                  :content (agent-q--fetch-context-content type data))))
      ;; Add to context list
      (push item agent-q-context-items)
      ;; Replace @mention text with pill
      (let ((bounds (agent-q--context-mention-bounds)))
        (when bounds
          (delete-region (car bounds) (cdr bounds))
          (agent-q--insert-context-pill item)))
      ;; Refresh panel if visible
      (when (get-buffer-window agent-q-context-panel-buffer)
        (with-current-buffer agent-q-context-panel-buffer
          (agent-q--refresh-context-panel))))))
```

Update `agent-q-context-complete-at-point` to include exit function:
```elisp
(defun agent-q-context-complete-at-point ()
  "Completion-at-point function for @-mentions.
Returns completion data when point is after an @-mention."
  (when-let ((bounds (agent-q--context-mention-bounds)))
    (list (car bounds)
          (cdr bounds)
          (completion-table-dynamic #'agent-q--context-candidates)
          :exclusive 'no
          :annotation-function #'agent-q--context-annotation
          :exit-function #'agent-q--context-exit-function)))
```

Add setup function and hook:
```elisp
;;; Mode Setup

(defun agent-q-context-setup ()
  "Set up context completion for current buffer.
Call this from `agent-q-chat-mode-hook'."
  (add-to-list 'completion-at-point-functions
               #'agent-q-context-complete-at-point))
```

In `sly-agent-q-chat.el`, add to `agent-q-chat-mode` definition after existing hooks:
```elisp
;; Set up context completion
(agent-q-context-setup)
```

### Step 12.4: Run test to verify it passes

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -L . -L test -l ert -l sly-agent-q-context-test.el -f ert-run-tests-batch-and-exit
```
Expected: PASS (45 tests)

### Step 12.5: Commit

```bash
git add contrib/sly-agent-q/sly-agent-q-context.el contrib/sly-agent-q/sly-agent-q-chat.el contrib/sly-agent-q/test/sly-agent-q-context-test.el
git commit -m "$(cat <<'EOF'
feat(context): add completion hook and exit function

Completion exit creates context item, replaces @mention with
pill, and refreshes panel. Chat mode now calls agent-q-context-setup
to enable @-completion in input region.
EOF
)"
```

---

## Task 13: Final Integration and Test Runner

**Files:**
- Modify: `contrib/sly-agent-q/test/run.el`
- Modify: `contrib/sly-agent-q/test/test-helper.el`

### Step 13.1: Update test-helper.el

```elisp
;;; test-helper.el --- Test helper for sly-agent-q -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for testing sly-agent-q

;;; Code:

(require 'ert)

(defvar sly-agent-q-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing tests.")

(defvar sly-agent-q-root-dir
  (file-name-directory (directory-file-name sly-agent-q-test-dir))
  "Root directory of sly-agent-q.")

;; Add parent directory to load path
(add-to-list 'load-path sly-agent-q-root-dir)

;; Mock SLY functions for testing without a connection
(unless (featurep 'sly)
  (message "SLY not available - loading mocks for testing")

  ;; Minimal SLY mocks
  (defun sly-connected-p () nil)
  (defun sly-eval-async (form callback)
    "Mock sly-eval-async for testing."
    (funcall callback nil))

  ;; Provide fake sly feature
  (provide 'sly))

;; Load all modules being tested
(require 'sly-agent-q-context)
(require 'sly-agent-q-diff)
(require 'sly-agent-q-chat)
(require 'sly-agent-q-sessions)

(provide 'test-helper)
;;; test-helper.el ends here
```

### Step 13.2: Update run.el

```elisp
;;; run.el --- Batch test runner for sly-agent-q -*- lexical-binding: t; -*-

;; Author: Abhijit Rao <quasi@quasilabs.in>

;;; Commentary:

;; Batch test runner for all sly-agent-q test suites.
;; Run with: emacs -Q --batch -l test/run.el

;;; Code:

(let ((dir (file-name-directory load-file-name)))
  ;; Add parent directory (package root) to load path
  (add-to-list 'load-path (expand-file-name ".." dir))
  ;; Add test directory to load path
  (add-to-list 'load-path dir))

(require 'ert)

;; Load test helper
(require 'test-helper)

;; Load all test files
(message "Loading test suites...")
(require 'sly-agent-q-chat-test)
(require 'sly-agent-q-sessions-test)
(require 'sly-agent-q-diff-test)
(require 'sly-agent-q-context-test)

(message "\n========================================")
(message "Running sly-agent-q test suite")
(message "========================================\n")

;; Run all tests
(ert-run-tests-batch-and-exit)

;;; run.el ends here
```

### Step 13.3: Run full test suite

Run:
```bash
cd contrib/sly-agent-q && emacs -Q --batch -l test/run.el
```
Expected: All tests PASS

### Step 13.4: Commit

```bash
git add contrib/sly-agent-q/test/run.el contrib/sly-agent-q/test/test-helper.el
git commit -m "$(cat <<'EOF'
feat(context): add context tests to test runner

Includes sly-agent-q-context-test in full test suite.
Updates test-helper to load context module.
EOF
)"
```

---

## Verification Checklist

After completing all tasks, verify:

- [ ] Typing `@` followed by chars triggers completion (test in running Emacs)
- [ ] File completion shows project files
- [ ] Symbol completion shows function/variable names
- [ ] Buffer completion shows open buffers
- [ ] Selecting completion inserts pill `[@name]`
- [ ] Pill is clickable (visits source)
- [ ] Pill can be deleted (DEL key)
- [ ] `C-c @` adds context interactively
- [ ] `C-c C-p` toggles context panel
- [ ] Context panel shows all items with remove buttons
- [ ] Context included in LLM prompt (check `agent-q--format-context-for-llm`)
- [ ] All tests pass: `emacs -Q --batch -l test/run.el`

---

## Final Commit

After verification:

```bash
git add -A
git commit -m "$(cat <<'EOF'
feat(chat): complete Phase 4 context management

Implements @-mention completion, context pills, and context panel
sidebar. Users can attach files, symbols, and buffers to messages
via @file.lisp syntax or C-c @ interactive command.

Features:
- completion-at-point for @-mentions (works with vertico/ivy/helm)
- Visual pills [@name] with click-to-visit and remove actions
- Context panel sidebar (C-c C-p) showing all attached items
- Context formatted as <context> block in LLM prompts
- Content size limited to 50KB per item

Closes Phase 4 of chat interface enhancement.
EOF
)"
```

---

## Summary

This plan implements Chat Phase 4 in 13 TDD tasks:

1. **Task 1-4**: Data structures and candidate gathering (files, symbols, buffers)
2. **Task 5-6**: @-mention detection and completion-at-point
3. **Task 7-8**: Content fetching and pill rendering
4. **Task 9-10**: Context panel and interactive commands
5. **Task 11-13**: Chat integration and test runner updates

Each task follows RED-GREEN-REFACTOR with small commits. Total: ~45 tests covering all functionality.
