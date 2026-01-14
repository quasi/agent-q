;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-
;;; ABOUTME: Cost estimation and budget checking for Agent-Q LLM requests

(in-package :agent-q)

;;; Cost Estimation
;;;
;;; Provides pre-flight cost estimation, budget checking, and session cost
;;; tracking for LLM requests. Integrates with cl-llm-provider's cost API
;;; and uses *request-log* from observability.lisp for session totals.

;;;; Conditions

(define-condition budget-exceeded-error (error)
  ((estimated-cost :initarg :estimated-cost
                   :reader error-estimated-cost
                   :documentation "The estimated cost in USD.")
   (budget :initarg :budget
           :reader error-budget
           :documentation "The budget limit in USD."))
  (:documentation "Signaled when estimated request cost exceeds budget.")
  (:report (lambda (c s)
             (format s "Estimated cost ~A exceeds budget ~A"
                     (format-cost-usd (error-estimated-cost c))
                     (format-cost-usd (error-budget c))))))

;;;; Cost Estimation Functions

(defun estimate-request-cost (messages &key system-prompt (max-tokens 4096))
  "Estimate cost for a request before sending.

   MESSAGES - List of message plists (:role, :content)
   SYSTEM-PROMPT - Optional system prompt string
   MAX-TOKENS - Expected maximum output tokens (default 4096)

   Returns (values input-cost output-cost total-cost) in USD.
   Returns NIL values if provider not configured or pricing unavailable.

   Example:
     (multiple-value-bind (in out total)
         (estimate-request-cost '((:role \"user\" :content \"Hello\"))
                                :system-prompt \"You are helpful.\"
                                :max-tokens 1000)
       (format t \"Estimated total cost: ~A~%\" (format-cost-usd total)))"
  (when *provider-instance*
    (let ((model (cl-llm-provider:provider-default-model *provider-instance*)))
      (multiple-value-bind (input-cost output-cost total-cost)
          (cl-llm-provider:estimate-cost
           messages
           :provider *provider-instance*
           :model model
           :system system-prompt
           :max-tokens max-tokens)
        (values input-cost output-cost total-cost)))))

(defun format-cost-usd (cost)
  "Format a cost value for display.

   COST - Cost in USD (float), or NIL

   Returns string like \"$0.0025\" or \"N/A\" if cost is NIL.

   Example:
     (format-cost-usd 0.0025) => \"$0.0025\"
     (format-cost-usd nil)    => \"N/A\""
  (if cost
      (cl-llm-provider:format-cost cost nil)
      "N/A"))

(defun check-budget (messages &key system-prompt (max-tokens 4096) (budget 0.10))
  "Check if estimated request cost is within budget.

   MESSAGES - List of message plists
   SYSTEM-PROMPT - Optional system prompt string
   MAX-TOKENS - Expected maximum output tokens (default 4096)
   BUDGET - Maximum allowed cost in USD (default $0.10)

   Returns T if within budget.
   Signals BUDGET-EXCEEDED-ERROR if estimated cost exceeds budget.
   Returns T with warning if cost estimation unavailable.

   Example:
     ;; Check before sending expensive request
     (when (check-budget messages :budget 0.50)
       (send-to-agent agent user-message))

     ;; Handle budget exceeded
     (handler-case
         (check-budget messages :budget 0.01)
       (budget-exceeded-error (e)
         (format t \"Request too expensive: ~A~%\" e)))"
  (multiple-value-bind (input-cost output-cost total-cost)
      (estimate-request-cost messages
                             :system-prompt system-prompt
                             :max-tokens max-tokens)
    (declare (ignore input-cost output-cost))
    (cond
      ;; Cost estimation succeeded
      (total-cost
       (if (<= total-cost budget)
           (progn
             (when *verbose-mode*
               (format t "~&[AGENT-Q] Estimated cost: ~A (within budget ~A)~%"
                       (format-cost-usd total-cost)
                       (format-cost-usd budget)))
             t)
           (error 'budget-exceeded-error
                  :estimated-cost total-cost
                  :budget budget)))
      ;; Cost estimation unavailable
      (t
       (when *verbose-mode*
         (format t "~&[AGENT-Q] Cost estimation unavailable for current model~%"))
       t))))

(defun get-session-cost ()
  "Calculate total cost for current session based on logged token usage.

   Uses *request-log* from observability.lisp to aggregate actual token usage,
   then applies pricing from cl-llm-provider:model-metadata.

   Returns plist with:
     :input-cost       - Cost of input tokens in USD (or NIL if pricing unavailable)
     :output-cost      - Cost of output tokens in USD (or NIL if pricing unavailable)
     :total-cost       - Total cost in USD (or NIL if pricing unavailable)
     :input-tokens     - Total input tokens used
     :output-tokens    - Total output tokens used
     :request-count    - Number of completed requests

   Example:
     (let ((cost-info (get-session-cost)))
       (format t \"Session cost: ~A (~D requests, ~D/~D tokens)~%\"
               (format-cost-usd (getf cost-info :total-cost))
               (getf cost-info :request-count)
               (getf cost-info :input-tokens)
               (getf cost-info :output-tokens)))"
  (let ((input-tokens 0)
        (output-tokens 0)
        (request-count 0))
    ;; Sum actual token usage from request log
    ;; The observability module logs :prompt-tokens and :completion-tokens
    (dolist (entry *request-log*)
      (when (eq (getf entry :event) :response)
        (incf request-count)
        ;; Use actual token breakdown if available
        (let ((prompt (getf entry :prompt-tokens))
              (completion (getf entry :completion-tokens))
              (total (getf entry :tokens)))
          (cond
            ;; Have detailed breakdown
            ((and prompt completion)
             (incf input-tokens prompt)
             (incf output-tokens completion))
            ;; Only have total - estimate split (typically ~80% input, 20% output for chat)
            (total
             (incf input-tokens (floor (* total 0.8)))
             (incf output-tokens (floor (* total 0.2))))))))

    ;; Get pricing from model metadata
    (if *provider-instance*
        (let* ((model (cl-llm-provider:provider-default-model *provider-instance*))
               (meta (cl-llm-provider:model-metadata *provider-instance* model)))
          (if meta
              (let ((input-price (getf meta :input-cost-per-1m-tokens))
                    (output-price (getf meta :output-cost-per-1m-tokens)))
                (if (and input-price output-price)
                    (let ((input-cost (* input-tokens (/ input-price 1000000.0d0)))
                          (output-cost (* output-tokens (/ output-price 1000000.0d0))))
                      (list :input-cost input-cost
                            :output-cost output-cost
                            :total-cost (+ input-cost output-cost)
                            :input-tokens input-tokens
                            :output-tokens output-tokens
                            :request-count request-count))
                    ;; Pricing not in metadata
                    (list :input-cost nil
                          :output-cost nil
                          :total-cost nil
                          :input-tokens input-tokens
                          :output-tokens output-tokens
                          :request-count request-count)))
              ;; No metadata
              (list :input-cost nil
                    :output-cost nil
                    :total-cost nil
                    :input-tokens input-tokens
                    :output-tokens output-tokens
                    :request-count request-count)))
        ;; No provider configured
        (list :input-cost nil
              :output-cost nil
              :total-cost nil
              :input-tokens input-tokens
              :output-tokens output-tokens
              :request-count request-count))))

(defun format-session-cost (&optional (stream *standard-output*))
  "Print formatted session cost summary.

   STREAM - Output stream (default *standard-output*)

   Example output:
     Agent-Q Session Cost
     ====================
     Requests: 5
     Input tokens: 4,523
     Output tokens: 1,234
     Input cost: $0.0226
     Output cost: $0.0185
     Total cost: $0.0411"
  (let ((cost-info (get-session-cost)))
    (format stream "~&Agent-Q Session Cost~%")
    (format stream "====================~%")
    (format stream "Requests: ~:D~%" (getf cost-info :request-count))
    (format stream "Input tokens: ~:D~%" (getf cost-info :input-tokens))
    (format stream "Output tokens: ~:D~%" (getf cost-info :output-tokens))
    (format stream "Input cost: ~A~%" (format-cost-usd (getf cost-info :input-cost)))
    (format stream "Output cost: ~A~%" (format-cost-usd (getf cost-info :output-cost)))
    (format stream "Total cost: ~A~%" (format-cost-usd (getf cost-info :total-cost))))
  (values))
