;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Configuration variables

(defparameter *default-provider* :anthropic
  "Default LLM provider. Options: :anthropic, :openai, :ollama, :openrouter")

(defparameter *default-model* "claude-sonnet-4-20250514"
  "Default model name for the provider.")

(defvar *provider-instance* nil
  "Active cl-llm-provider instance.")

;;; Configuration functions

(defun load-config ()
  "Load configuration from user config file at ~/.config/agent-q/config.lisp"
  (let ((config-file (merge-pathnames
                      ".config/agent-q/config.lisp"
                      (user-homedir-pathname))))
    (when (probe-file config-file)
      (handler-case
          (load config-file)
        (error (e)
          (warn "Failed to load config file ~A: ~A" config-file e)
          nil)))))

(defun configure (&key provider model api-key base-url)
  "Configure agent settings and create provider instance.

  PROVIDER - Provider type (:anthropic, :openai, :ollama, :openrouter)
  MODEL - Model name string
  API-KEY - API key (optional, reads from env var if not provided)
  BASE-URL - Base URL for provider (optional, for Ollama/custom endpoints)

  Returns T on success, NIL on failure."
  (handler-case
      (progn
        ;; Update defaults
        (when provider (setf *default-provider* provider))
        (when model (setf *default-model* model))

        ;; Create provider instance using cl-llm-provider
        (setf *provider-instance*
              (cl-llm-provider:make-provider
               *default-provider*
               :model *default-model*
               :api-key api-key
               :base-url base-url))
        t)
    (error (e)
      (warn "Failed to configure provider: ~A" e)
      nil)))

;;; Initialize provider on load (reads from env vars)
(unless *provider-instance*
  (configure))
