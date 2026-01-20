;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Configuration variables

(defvar *project-root* nil
  "Root directory for file operations. Auto-detected if NIL.
   All file system tools validate paths against this boundary.")

(defparameter *default-provider* :anthropic
  "Default LLM provider. Options: :anthropic, :openai, :ollama, :openrouter")

(defparameter *default-model* "claude-sonnet-4-20250514"
  "Default model name for the provider.")

(defvar *provider-instance* nil
  "Active cl-llm-provider instance.")

(defparameter *verbose-mode* t
  "When T, log tool execution details to the conversation buffer.")

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

  Performs capability checking after creation:
  - Warns if provider doesn't support tool calling
  - Warns if provider doesn't support streaming
  - Logs available capabilities in verbose mode

  Returns T on success, signals error on failure."
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

  ;; Check required capabilities
  (unless (cl-llm-provider:provider-supports-p *provider-instance* :tools)
    (warn "Provider ~A may not support tool calling. ~
           Agent-Q tool execution may not work correctly."
          (cl-llm-provider:provider-name *provider-instance*)))

  ;; Check streaming support
  (unless (cl-llm-provider:provider-supports-p *provider-instance* :streaming)
    (warn "Provider ~A may not support streaming. ~
           Responses will appear all at once instead of incrementally."
          (cl-llm-provider:provider-name *provider-instance*)))

  ;; Log configuration in verbose mode
  (when *verbose-mode*
    (format t "~&[AGENT-Q] Configured: ~A (~A)~%"
            (cl-llm-provider:provider-name *provider-instance*)
            *default-model*)
    (format t "~&[AGENT-Q] Capabilities: ~S~%"
            (cl-llm-provider:provider-capabilities *provider-instance*)))

  t)

(defun sync-from-cl-llm-provider ()
  "Sync *provider-instance* from cl-llm-provider:*default-provider*.
   Call this after cl-llm-provider:load-configuration-from-file.

   Ensures the provider instance has its default-model slot populated
   from cl-llm-provider:*default-model* if not already set.

   Returns T if provider is now configured, NIL otherwise."
  (when (and (not *provider-instance*)
             cl-llm-provider:*default-provider*)
    (setf *provider-instance* cl-llm-provider:*default-provider*)
    ;; Ensure provider instance has model set from cl-llm-provider's global
    (when (and cl-llm-provider:*default-model*
               (null (cl-llm-provider:provider-default-model *provider-instance*)))
      (setf (cl-llm-provider:provider-default-model *provider-instance*)
            cl-llm-provider:*default-model*)))
  (not (null *provider-instance*)))

;;; NOTE: No auto-configuration on load.
;;; Configuration happens via:
;;; 1. cl-llm-provider:load-configuration-from-file (loads user config)
;;; 2. agent-q:sync-from-cl-llm-provider (syncs to agent-q)
