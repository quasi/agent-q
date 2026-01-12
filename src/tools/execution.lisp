;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q.TOOLS; Base: 10 -*-

(in-package :agent-q.tools)

;;; Execution Tools
;;;
;;; Tools that execute code and modify the Lisp image state.
;;; These tools are :moderate - they auto-execute but are logged.

;;; Global state for error tracking and history

(defvar *last-error* nil
  "Most recent error condition and backtrace.
   Plist with :condition :form :backtrace :timestamp")

(defvar *repl-history* (make-array 100 :fill-pointer 0 :adjustable t)
  "Ring buffer of recent REPL interactions.
   Each entry is a plist with :form :result :timestamp")

(defvar *compilation-notes* nil
  "Recent compilation warnings and errors.")

;;; Helper functions

(defun record-repl-interaction (form-string result-string)
  "Record a REPL interaction in history."
  (when (>= (fill-pointer *repl-history*)
           (array-dimension *repl-history* 0))
    ;; Remove oldest entry when full
    (loop for i from 0 below (1- (fill-pointer *repl-history*))
          do (setf (aref *repl-history* i)
                  (aref *repl-history* (1+ i))))
    (decf (fill-pointer *repl-history*)))
  (vector-push-extend
   (list :form form-string
         :result result-string
         :timestamp (get-universal-time))
   *repl-history*))

;;; 1. eval-form

(let ((tool (define-tool
              "eval_form"
              "Evaluate a Lisp form and return the result. Use this to test code, check values, or run computations. Results are logged in REPL history."
              '((:name "form" :type :string :description "The Lisp form to evaluate (as a string)")
                (:name "package" :type :string :description "Package context for evaluation (optional)"))
              :required '("form")
              :safety-level :moderate
              :categories '(:execution)
              :handler (lambda (args)
                         (let* ((form-string (gethash "form" args))
                                (pkg (find-package (or (gethash "package" args) *package*))))
                           (if (not pkg)
                               (format nil "Package ~A not found" (gethash "package" args))
                               (handler-case
                                   (let* ((*package* pkg)
                                          (form (read-from-string form-string))
                                          (values (multiple-value-list (eval form)))
                                          (result-string (format nil "~{~S~^~%~}" values)))
                                     ;; Record in history
                                     (record-repl-interaction form-string result-string)
                                     ;; Format result
                                     (with-output-to-string (s)
                                       (format s "Evaluated in package ~A:~%~%" (package-name pkg))
                                       (format s "Form: ~A~%~%" form-string)
                                       (format s "Result~P:~%~{  ~S~%~}"
                                              (length values) values)))
                                 (error (e)
                                   (setf *last-error*
                                         (list :condition e
                                               :form form-string
                                               :backtrace (capture-backtrace-portable)
                                               :timestamp (get-universal-time)))
                                   (format nil "Error evaluating form:~%~A~%~%Use get_last_error to see backtrace."
                                          e)))))))))
  (register-tool *agent-q-registry* tool))

;;; 2. compile-form

(let ((tool (define-tool
              "compile_form"
              "Compile a Lisp form and load it into the image. Typically used for definitions (defun, defclass, etc.). Returns any warnings or errors from compilation."
              '((:name "form" :type :string :description "The Lisp form to compile (typically a defun, defclass, defmethod, etc.)")
                (:name "package" :type :string :description "Package context for compilation (optional)"))
              :required '("form")
              :safety-level :moderate
              :categories '(:execution)
              :handler (lambda (args)
                         (let* ((form-string (gethash "form" args))
                                (pkg (find-package (or (gethash "package" args) *package*)))
                                (warnings '())
                                (style-warnings '())
                                (notes '()))
                           (if (not pkg)
                               (format nil "Package ~A not found" (gethash "package" args))
                               (handler-case
                                   (let* ((*package* pkg))
                                     ;; Read the form
                                     (let ((form (read-from-string form-string)))
                                       ;; Compile and load
                                       (handler-bind
                                           ((warning
                                             (lambda (w)
                                              (if (typep w 'style-warning)
                                                  (push (princ-to-string w) style-warnings)
                                                  (push (princ-to-string w) warnings))
                                               (muffle-warning w))))
                                         ;; Evaluate the form (which compiles definitions)
                                         (eval form))
                                       ;; Store compilation notes
                                       (setf *compilation-notes*
                                             (append warnings style-warnings notes))
                                       ;; Format result
                                       (with-output-to-string (s)
                                         (format s "Successfully compiled and loaded form in package ~A.~%~%"
                                                (package-name pkg))
                                         (format s "Form: ~A~%~%" form-string)
                                         (when warnings
                                           (format s "Warnings (~D):~%~{  ~A~%~}~%"
                                                  (length warnings) (nreverse warnings)))
                                         (when style-warnings
                                           (format s "Style Warnings (~D):~%~{  ~A~%~}~%"
                                                  (length style-warnings)
                                                  (nreverse style-warnings)))
                                         (unless (or warnings style-warnings)
                                           (format s "No warnings.~%")))))
                                 (error (e)
                                   (setf *last-error*
                                         (list :condition e
                                               :form form-string
                                               :type :compilation
                                               :timestamp (get-universal-time)))
                                   (format nil "Compilation failed:~%~A~%~%Use get_last_error for details."
                                          e)))))))))
  (register-tool *agent-q-registry* tool))

;;; 3. get-last-error

(let ((tool (define-tool
              "get_last_error"
              "Get details about the most recent error including the condition, form that caused it, and backtrace. Useful for debugging failed eval_form or compile_form calls."
              '()
              :required '()
              :safety-level :safe
              :categories '(:execution :debugging)
              :handler (lambda (args)
                         (declare (ignore args))
                         (if *last-error*
                             (with-output-to-string (s)
                               (format s "Last Error (at ~A):~%~%"
                                      (or (getf *last-error* :timestamp) "unknown time"))
                               (when (getf *last-error* :type)
                                 (format s "Type: ~A~%~%" (getf *last-error* :type)))
                               (format s "Condition:~%  ~A~%~%"
                                      (getf *last-error* :condition))
                               (format s "Form:~%  ~A~%~%"
                                      (getf *last-error* :form))
                               (let ((bt (getf *last-error* :backtrace)))
                                 (when bt
                                   (format s "Backtrace:~%~A" bt))))
                             "No recent errors recorded.")))))
  (register-tool *agent-q-registry* tool))

;;; 4. get-repl-history

(let ((tool (define-tool
              "get_repl_history"
              "Get recent REPL interactions (forms evaluated via eval_form and their results). Useful for reviewing what has been tested."
              '((:name "limit" :type :integer :description "Number of recent interactions to return (default 10, max 50)"))
              :required '()
              :safety-level :safe
              :categories '(:execution)
              :handler (lambda (args)
                         (let* ((limit (min (or (gethash "limit" args) 10) 50))
                                (count (fill-pointer *repl-history*))
                                (start (max 0 (- count limit))))
                           (if (zerop count)
                               "No REPL history available."
                               (with-output-to-string (s)
                                 (format s "Recent REPL History (~D of ~D entries):~%~%"
                                        (min limit count) count)
                                 (loop for i from start below count
                                       for entry = (aref *repl-history* i)
                                       for idx from 1
                                       do (format s "[~D] Form: ~A~%"
                                                 idx (getf entry :form))
                                       do (format s "    Result: ~A~%~%"
                                                 (getf entry :result))))))))))
  (register-tool *agent-q-registry* tool))
