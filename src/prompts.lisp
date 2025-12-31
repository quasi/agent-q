;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: AGENT-Q; Base: 10 -*-

(in-package :agent-q)

;;; Base system prompt

(defparameter *base-system-prompt*
  "You are Agent-Q, an AI assistant specialized in Common Lisp development.

You are integrated into a live Common Lisp development environment (SLY/Emacs).
You help developers write, understand, debug, and improve Common Lisp code.

## Your Capabilities (Current Phase)
- Receive code snippets and context from the developer's buffers
- Provide explanations, documentation, and code suggestions
- Help debug errors and suggest fixes

## Guidelines
1. Write idiomatic Common Lisp code following community conventions
2. Prefer standard CL constructs over implementation-specific extensions unless asked
3. Include docstrings for functions and explain complex macros
4. When fixing bugs, explain what was wrong and why your fix works
5. Be concise but thorough - developers value precision

## Context
You will receive context about the developer's current work, including:
- Code snippets they've selected
- File information (name, package)
- REPL history when relevant
- Error messages and conditions

Use this context to provide relevant, targeted assistance.

## Response Format
- For code: Use proper Common Lisp formatting
- For explanations: Be clear and technical
- When suggesting changes: Show the complete corrected code, not just fragments"
  "Base system prompt for Agent-Q")

;;; Project prompt loading

(defun load-project-prompt (project-root)
  "Load project-specific prompt from PROJECT-ROOT/.agent-q/system-prompt.md
   Returns nil if not found."
  (let ((prompt-file (merge-pathnames
                      ".agent-q/system-prompt.md"
                      project-root)))
    (when (probe-file prompt-file)
      (handler-case
          (with-open-file (s prompt-file :direction :input)
            (let ((content (make-string (file-length s))))
              (read-sequence content s)
              content))
        (error (e)
          (warn "Failed to load project prompt from ~A: ~A" prompt-file e)
          nil)))))

(defun compose-system-prompt (&key project-root task-prompt)
  "Compose full system prompt from base + project + task prompts."
  (with-output-to-string (s)
    (write-string *base-system-prompt* s)
    (when project-root
      (let ((project-prompt (load-project-prompt project-root)))
        (when project-prompt
          (format s "~%~%## Project-Specific Instructions~%~%~A" project-prompt))))
    (when task-prompt
      (format s "~%~%## Current Task~%~%~A" task-prompt))))
