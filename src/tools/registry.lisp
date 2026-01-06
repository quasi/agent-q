;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q.TOOLS; Base: 10 -*-

(in-package :agent-q.tools)

;;; Tool Registry
;;;
;;; This file sets up Agent-Q's tool registry using cl-llm-provider's infrastructure.
;;; All tools are registered here and made available to the LLM.

(defvar *agent-q-registry* nil
  "The Agent-Q tool registry. Holds all tools available to the agent.")

(defun initialize-registry ()
  "Create and initialize the Agent-Q tool registry."
  (unless *agent-q-registry*
    (setf *agent-q-registry*
          (make-tool-registry
           :name "agent-q"
           :default-safety-level :safe)))
  *agent-q-registry*)

;; Initialize the registry at load time
(initialize-registry)

(defun register-agent-q-tools ()
  "Register all Agent-Q tools in the registry.
   This is called after all tool files are loaded."
  (initialize-registry)
  ;; Tools register themselves via define-tool calls in their respective files
  ;; This function exists as a hook point for any post-registration setup
  (values))

(defun get-agent-q-tools (&key max-safety-level categories)
  "Get tools formatted for LLM consumption.

   MAX-SAFETY-LEVEL: Limit tools to this safety level (default :moderate)
   CATEGORIES: Filter to specific tool categories (list of keywords)

   Returns: List of tool schemas suitable for passing to cl-llm-provider:complete"
  (initialize-registry)
  (tools-for-llm
   :registry *agent-q-registry*
   :max-safety-level (or max-safety-level :moderate)
   :categories categories))

(defun list-registered-tools ()
  "List all registered tools for debugging."
  (initialize-registry)
  (let ((tools '()))
    (maphash (lambda (name tool)
               (declare (ignore tool))
               (push (list :name name)
                     tools))
             (registry-tools *agent-q-registry*))
    (nreverse tools)))

;;; Utility functions for tool handlers

(defun capture-backtrace-portable ()
  "Capture current backtrace in a portable way.
   Uses SLYNK when available (inside SLY), otherwise implementation-specific."
  (with-output-to-string (s)
    (handler-case
        (cond
          ;; Try SLYNK (SLY) if available
          ((find-package :slynk-backend)
           (let* ((slynk-backend (find-package :slynk-backend))
                  (compute-backtrace (find-symbol "COMPUTE-BACKTRACE" slynk-backend))
                  (frame-to-string (find-symbol "FRAME-TO-STRING" slynk-backend)))
             (when (and compute-backtrace frame-to-string)
               (let ((bt (funcall compute-backtrace 0 20)))
                 (dolist (frame bt)
                   (format s "~A~%" (funcall frame-to-string frame)))))))
          ;; Fallback to implementation-specific
          #+sbcl
          (t
           (sb-debug:print-backtrace :stream s :count 20))
          #+ccl
          (t
           (ccl:print-call-history :count 20 :stream s))
          ;; Generic fallback
          #-(or sbcl ccl)
          (t
           (format s "Backtrace not available on this implementation~%")))
      (error (e)
        (format s "Error capturing backtrace: ~A~%" e)))))

(defun safe-symbol-lookup (symbol-name &optional (package *package*))
  "Safely look up a symbol, returning NIL if not found.
   Returns (values symbol package-name status)
   STATUS is :internal, :external, :inherited, or :not-found"
  (let ((pkg (if (packagep package)
                 package
                 (find-package package))))
    (if pkg
        (multiple-value-bind (sym status)
            (find-symbol (string-upcase symbol-name) pkg)
          (values sym (package-name pkg) (or status :not-found)))
        (values nil nil :package-not-found))))

(defun format-for-llm (object &key (max-length 2000))
  "Format OBJECT as a string suitable for LLM consumption.
   Truncates if longer than MAX-LENGTH."
  (let ((str (typecase object
               (string object)
               (null "nil")
               (t (with-output-to-string (s)
                    (pprint object s))))))
    (if (> (length str) max-length)
        (concatenate 'string
                    (subseq str 0 (- max-length 20))
                    "\n\n[... truncated ...]")
        str)))

(defun eval-in-emacs (form)
  "Evaluate FORM in Emacs via SLYNK.
   Uses runtime checking to find the appropriate function."
  (handler-case
      (cond
        ;; Try SLYNK (SLY)
        ((find-package :slynk)
         (let* ((slynk (find-package :slynk))
                (eval-fn (find-symbol "EVAL-IN-EMACS" slynk)))
           (when eval-fn
             (funcall eval-fn form))))
        ;; Try SWANK (SLIME) as fallback
        ((find-package :swank)
         (let* ((swank (find-package :swank))
                (eval-fn (find-symbol "EVAL-IN-EMACS" swank)))
           (when eval-fn
             (funcall eval-fn form))))
        (t
         (error "Not connected to Emacs (neither SLYNK nor SWANK available)")))
    (error (e)
      (format nil "Error evaluating in Emacs: ~A" e))))
