;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-
;;; ABOUTME: Observability infrastructure for Agent-Q - logging hooks, metrics, and debugging

(in-package :agent-q)

;;; Observability Infrastructure
;;;
;;; Provides hooks for logging, metrics, and debugging LLM interactions.
;;; Integrates with cl-llm-provider's observability system to track all
;;; requests/responses flowing through Agent-Q.

;;;; Global State

(defvar *agent-q-hooks* nil
  "Global hooks structure for Agent-Q LLM interactions.
   Created by `setup-observability' and installed as cl-llm-provider:*global-hooks*.")

(defvar *request-log* nil
  "Chronological log of LLM requests and responses.
   Each entry is a plist with :timestamp, :event, :provider, :model, and event-specific data.

   Request entries: (:timestamp T :event :request :provider P :model M :message-count N)
   Response entries: (:timestamp T :event :response :provider P :model M :timing S :tokens N)")

(defvar *observability-level* :info
  "Current observability level. One of :debug, :info, :warn.")

(defvar *observability-stream* nil
  "File stream for logging, if log-to-file was specified.
   NIL means use *standard-output*.")

;;;; Setup Functions

(defun setup-observability (&key (level :info) (log-to-file nil))
  "Configure observability hooks for Agent-Q.

   LEVEL - Logging level, one of :debug, :info, :warn
           :debug - Log full message contents and detailed responses
           :info  - Log request/response summaries with timing and tokens
           :warn  - Log only errors and warnings

   LOG-TO-FILE - Optional pathname for logging output.
                 When NIL, logs to *standard-output*.
                 When provided, opens a file for appending.

   This function:
   1. Creates base logging hooks via cl-llm-provider:make-logging-hooks
   2. Adds custom :before-request hook to record requests in *request-log*
   3. Adds custom :after-response hook to record responses with timing/tokens
   4. Sets cl-llm-provider:*global-hooks* so all requests are logged

   Returns: The configured hooks structure.

   Example:
     ;; Enable info-level logging to stdout
     (setup-observability :level :info)

     ;; Enable debug logging to file
     (setup-observability :level :debug :log-to-file #p\"~/agent-q.log\")"
  ;; Store level for later queries
  (setf *observability-level* level)

  ;; Setup output stream
  (let ((stream (if log-to-file
                    (progn
                      ;; Close previous file stream if any
                      (when (and *observability-stream*
                                 (open-stream-p *observability-stream*))
                        (close *observability-stream*))
                      ;; Open new file stream
                      (setf *observability-stream*
                            (open log-to-file
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)))
                    *standard-output*)))

    ;; Create base hooks with logging
    (let ((hooks (cl-llm-provider:make-logging-hooks
                  :stream stream
                  :level level)))

      ;; Add custom :before-request hook for Agent-Q request logging
      (cl-llm-provider:add-hook hooks :before-request
        (lambda (provider model messages)
          (push (list :timestamp (get-universal-time)
                      :event :request
                      :provider (when provider
                                  (cl-llm-provider:provider-type provider))
                      :model model
                      :message-count (length messages))
                *request-log*)))

      ;; Add custom :after-response hook for metrics tracking
      (cl-llm-provider:add-hook hooks :after-response
        (lambda (provider model response timing)
          (let* ((usage (cl-llm-provider:response-usage response))
                 (total-tokens (when usage (getf usage :total-tokens)))
                 (prompt-tokens (when usage (getf usage :prompt-tokens)))
                 (completion-tokens (when usage (getf usage :completion-tokens))))
            (push (list :timestamp (get-universal-time)
                        :event :response
                        :provider (when provider
                                    (cl-llm-provider:provider-type provider))
                        :model model
                        :timing timing
                        :tokens total-tokens
                        :prompt-tokens prompt-tokens
                        :completion-tokens completion-tokens)
                  *request-log*))))

      ;; Store and install globally
      (setf *agent-q-hooks* hooks)
      (setf cl-llm-provider:*global-hooks* hooks)

      hooks)))

(defun teardown-observability ()
  "Disable observability and cleanup resources.

   Clears *agent-q-hooks* and cl-llm-provider:*global-hooks*.
   Closes log file stream if open."
  (setf cl-llm-provider:*global-hooks* nil)
  (setf *agent-q-hooks* nil)
  (when (and *observability-stream*
             (open-stream-p *observability-stream*))
    (close *observability-stream*)
    (setf *observability-stream* nil))
  t)

;;;; Request Log Management

(defun clear-request-log ()
  "Clear the request log.
   Returns the number of entries cleared."
  (let ((count (length *request-log*)))
    (setf *request-log* nil)
    count))

(defun get-request-log (&key (limit nil) (event-type nil))
  "Get entries from the request log.

   LIMIT - Optional maximum number of entries to return (most recent first)
   EVENT-TYPE - Optional filter, one of :request or :response

   Returns list of log entry plists, most recent first."
  (let ((log *request-log*))
    ;; Filter by event type if specified
    (when event-type
      (setf log (remove-if-not
                 (lambda (entry) (eq (getf entry :event) event-type))
                 log)))
    ;; Apply limit if specified
    (if (and limit (> (length log) limit))
        (subseq log 0 limit)
        log)))

;;;; Statistics

(defun get-request-stats ()
  "Get summary statistics from request log.

   Returns a plist with:
     :total-requests - Number of completed request/response cycles
     :total-tokens   - Sum of all tokens used
     :prompt-tokens  - Sum of input tokens
     :completion-tokens - Sum of output tokens
     :avg-timing     - Average response time in seconds
     :total-timing   - Total time spent in LLM calls

   Example:
     (get-request-stats)
     ;; => (:TOTAL-REQUESTS 5 :TOTAL-TOKENS 1234 :AVG-TIMING 1.5 ...)"
  (let ((responses (remove-if-not
                    (lambda (entry) (eq (getf entry :event) :response))
                    *request-log*)))
    (if (null responses)
        ;; No data yet
        (list :total-requests 0
              :total-tokens 0
              :prompt-tokens 0
              :completion-tokens 0
              :avg-timing 0.0
              :total-timing 0.0)
        ;; Calculate stats
        (let ((total-tokens 0)
              (prompt-tokens 0)
              (completion-tokens 0)
              (total-timing 0.0))
          (dolist (entry responses)
            (incf total-tokens (or (getf entry :tokens) 0))
            (incf prompt-tokens (or (getf entry :prompt-tokens) 0))
            (incf completion-tokens (or (getf entry :completion-tokens) 0))
            (incf total-timing (or (getf entry :timing) 0.0)))
          (list :total-requests (length responses)
                :total-tokens total-tokens
                :prompt-tokens prompt-tokens
                :completion-tokens completion-tokens
                :avg-timing (/ total-timing (length responses))
                :total-timing total-timing)))))

(defun get-model-stats ()
  "Get per-model statistics from request log.

   Returns an alist of (model . stats-plist) where each stats-plist contains:
     :request-count, :total-tokens, :avg-timing

   Example:
     (get-model-stats)
     ;; => ((\"claude-sonnet-4-20250514\" :REQUEST-COUNT 3 :TOTAL-TOKENS 890 :AVG-TIMING 1.2)
     ;;     (\"gpt-4\" :REQUEST-COUNT 2 :TOTAL-TOKENS 450 :AVG-TIMING 0.8))"
  (let ((model-stats (make-hash-table :test 'equal)))
    ;; Aggregate stats by model
    (dolist (entry *request-log*)
      (when (eq (getf entry :event) :response)
        (let* ((model (getf entry :model))
               (stats (gethash model model-stats
                               (list :request-count 0
                                     :total-tokens 0
                                     :total-timing 0.0))))
          (setf (gethash model model-stats)
                (list :request-count (1+ (getf stats :request-count))
                      :total-tokens (+ (getf stats :total-tokens)
                                       (or (getf entry :tokens) 0))
                      :total-timing (+ (getf stats :total-timing)
                                       (or (getf entry :timing) 0.0)))))))
    ;; Convert to alist with computed averages
    (let ((result nil))
      (maphash (lambda (model stats)
                 (push (cons model
                             (list :request-count (getf stats :request-count)
                                   :total-tokens (getf stats :total-tokens)
                                   :avg-timing (if (> (getf stats :request-count) 0)
                                                   (/ (getf stats :total-timing)
                                                      (getf stats :request-count))
                                                   0.0)))
                       result))
               model-stats)
      result)))

(defun format-stats (&optional (stream *standard-output*))
  "Print formatted statistics to STREAM.

   Example output:
     Agent-Q Request Statistics
     ===========================
     Total Requests: 5
     Total Tokens: 1234 (prompt: 800, completion: 434)
     Average Timing: 1.52s
     Total Time: 7.60s"
  (let ((stats (get-request-stats)))
    (format stream "~&Agent-Q Request Statistics~%")
    (format stream "===========================~%")
    (format stream "Total Requests: ~D~%" (getf stats :total-requests))
    (format stream "Total Tokens: ~D (prompt: ~D, completion: ~D)~%"
            (getf stats :total-tokens)
            (getf stats :prompt-tokens)
            (getf stats :completion-tokens))
    (format stream "Average Timing: ~,2Fs~%" (getf stats :avg-timing))
    (format stream "Total Time: ~,2Fs~%" (getf stats :total-timing))
    ;; Per-model breakdown
    (let ((model-stats (get-model-stats)))
      (when model-stats
        (format stream "~%By Model:~%")
        (dolist (entry model-stats)
          (format stream "  ~A: ~D requests, ~D tokens, ~,2Fs avg~%"
                  (car entry)
                  (getf (cdr entry) :request-count)
                  (getf (cdr entry) :total-tokens)
                  (getf (cdr entry) :avg-timing))))))
  (values))

;;;; Configuration Query

(defun get-observability-info ()
  "Get current observability configuration as a plist.

   Returns:
     :level - Current observability level
     :hooks-active - Whether hooks are installed
     :log-to-file - Whether logging to file
     :log-entries - Number of entries in request log"
  (list :level *observability-level*
        :hooks-active (not (null cl-llm-provider:*global-hooks*))
        :log-to-file (not (null *observability-stream*))
        :log-entries (length *request-log*)))
