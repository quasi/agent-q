# Agent-Q Quick Start Guide

Get up and running with Agent-Q in 5 minutes!

## Prerequisites Checklist

- [ ] Emacs with SLY installed
- [ ] Common Lisp (SBCL recommended)
- [ ] Quicklisp installed
- [ ] API key for LLM provider (Anthropic/OpenAI/etc.)

## Installation (5 steps)

### Step 1: Set Your API Key

```bash
# For Anthropic Claude (recommended)
export ANTHROPIC_API_KEY="sk-ant-your-key-here"

# OR for OpenAI
export OPENAI_API_KEY="sk-your-key-here"
```

Add to `~/.bashrc` or `~/.zshrc` to make permanent.

### Step 2: Install cl-llm-provider

Start your Lisp REPL and run:

```lisp
(ql:quickload "cl-llm-provider")
```

### Step 3: Clone Agent-Q

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/yourusername/agent-q.git
```

### Step 4: Load Agent-Q in Lisp

In your Lisp REPL:

```lisp
(asdf:load-asd "~/quicklisp/local-projects/agent-q/agent-q.asd")
(asdf:load-system :agent-q)
```

You should see:
```
✓ Agent-Q loaded successfully
```

### Step 5: Configure Emacs

Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(with-eval-after-load 'sly
  (add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")
  (require 'sly-agent-q)
  (sly-agent-q-setup))
```

Restart Emacs or evaluate the code with `M-x eval-buffer`.

## First Use

### 1. Start SLY

```
M-x sly
```

### 2. Open a Lisp File

Open any `.lisp` file. Agent-Q mode should activate automatically (look for "AgentQ" in the mode line).

### 3. Try a Simple Question

Press `C-c q s` and ask:
```
What is the difference between let and let*?
```

You should see a response in the `*Agent-Q*` buffer!

### 4. Try Context-Aware Help

Write some code:

```lisp
(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))
```

- Mark the function with `C-x h` or select the region
- Press `C-c q c r` to add to context
- Press `C-c q S` (capital S) and ask: "Optimize this for performance"
- The agent will understand your code and provide optimized version!

### 5. Insert Response

After getting a response:
- Press `C-c q i` to insert at point
- Or `C-c q w` to copy to kill ring

## Common Workflows

### Document a Function

1. Place cursor inside a function
2. Press `C-c q q d`
3. Agent adds comprehensive docstring

### Explain Code

1. Select code region
2. Press `C-c q q e`
3. Agent explains what it does

### Debug an Error

1. When you get an error in REPL
2. Press `C-c q q f`
3. Agent analyzes and suggests fix

### Multi-turn Conversation

1. Press `C-c q s`: "Write a function to parse CSV"
2. Review response
3. Press `C-c q s`: "Add error handling"
4. Press `C-c q s`: "Add docstrings"
5. Agent remembers context!

## Keybinding Cheat Sheet

All commands under `C-c q`:

```
Context:
  c r  - Add region to context
  c b  - Add buffer to context
  c d  - Add current defun to context
  c c  - Clear all context
  c s  - Show context summary

Conversation:
  s    - Send message (without context)
  S    - Send message WITH context
  r    - Send region with custom instruction
  n    - New conversation
  v    - View conversation buffer

Response:
  i    - Insert last response at point
  w    - Copy last response to kill ring

Quick Actions:
  q d  - Document current function
  q e  - Explain selected code
  q f  - Fix recent error
```

## Troubleshooting

### "Authentication failed"

Check your API key:
```bash
echo $ANTHROPIC_API_KEY  # Should show your key
```

If empty, set it and restart your Lisp REPL.

### "No response"

Check internet connection and API status. Try:
```lisp
;; Test LLM connection
(agent-q:agent-q-send "Hello")
```

### "Can't find sly-agent-q"

Verify path in Emacs config:
```elisp
;; Check if file exists
(file-exists-p "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/sly-agent-q.el")
```

Should return `t`.

### Commands don't work

Make sure:
1. SLY is connected: Check for "SLY" in mode line
2. Agent-Q mode is active: Look for "AgentQ" in mode line
3. Try `M-x sly-agent-q-mode` to toggle

## Advanced Configuration

### Use a Different Model

Create `~/.config/agent-q/config.lisp`:

```lisp
(in-package :agent-q)

;; Use GPT-4
(configure :provider :openai
           :model "gpt-4-turbo")

;; Or use local Ollama
(configure :provider :ollama
           :model "llama2"
           :base-url "http://localhost:11434")
```

### Project-Specific Instructions

Create `.agent-q/system-prompt.md` in your project:

```markdown
This project uses:
- Alexandria for utilities
- FiveAM for testing
- CLOS for data structures

Naming convention: verb-noun (e.g., parse-data, emit-code)
```

## Next Steps

- Read the full [README.md](README.md) for complete documentation
- Check [IMPLEMENTATION-SUMMARY.md](IMPLEMENTATION-SUMMARY.md) for architecture details
- Explore `specs/` directory for future phases
- Try different LLM providers (OpenAI, Ollama, etc.)

## Getting Help

- Check the Troubleshooting section in [README.md](README.md)
- Review conversation buffer (`C-c q v`) for error messages
- Test from Lisp REPL to isolate issues

---

**You're all set!** Start using Agent-Q to supercharge your Common Lisp development. 🚀
