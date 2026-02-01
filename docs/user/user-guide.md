# Agent-Q User Guide

**Your AI-powered autonomous development partner for Common Lisp**

Agent-Q integrates with SLY/Emacs to provide REPL-aware assistance. Unlike generic AI coding assistants, Agent-Q can introspect your running Lisp image, execute code, observe results, and iterate autonomously.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Core Concepts](#core-concepts)
3. [Context Management](#context-management)
4. [Chat Interface](#chat-interface)
5. [Tool System](#tool-system)
6. [Diff Review](#diff-review)
7. [Sessions](#sessions)
8. [Practical Workflows](#practical-workflows)
9. [Keyboard Reference](#keyboard-reference)
10. [Troubleshooting](#troubleshooting)
11. [Configuration](#configuration)

---

## Getting Started

### Prerequisites

- Emacs with SLY installed
- Common Lisp (SBCL recommended)
- Quicklisp for package management
- API key for Anthropic Claude or OpenAI

### Installation

See [Quickstart Guide](quickstart.md) for detailed installation instructions.

**Quick version:**

```bash
# Set API key
export ANTHROPIC_API_KEY="your-key-here"

# Clone and load
cd ~/quicklisp/local-projects
git clone https://github.com/yourusername/agent-q.git

# In Lisp REPL
(ql:quickload :agent-q)

# In Emacs config
(with-eval-after-load 'sly
  (add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")
  (require 'sly-agent-q)
  (sly-agent-q-setup))
```

---

## Core Concepts

### What Makes Agent-Q Different?

Agent-Q is **REPL-aware**. It has direct access to your running Lisp environment through 18 specialized tools:

```
Traditional AI Assistant:
  User: "Fix this function"
  AI: [Guesses based on static analysis]

Agent-Q:
  User: "Fix this function"
  Agent-Q: [Inspects symbol] → [Tests in REPL] → [Examines error] → [Proposes fix] → [Verifies fix]
```

### The Autonomous Loop

Agent-Q iterates autonomously:

1. **Understands**: Reads your code and question
2. **Explores**: Uses introspection tools to understand context
3. **Experiments**: Runs code in REPL to test hypotheses
4. **Proposes**: Suggests changes with reviewable diffs
5. **Verifies**: Tests that the fix works
6. **Reports**: Explains what it did and why

### Architecture

```
┌─────────────┐
│    Emacs    │  ← User interacts here
└──────┬──────┘
       │ SLYNK Protocol (RPC)
       ▼
┌─────────────┐
│ Common Lisp │  ← Agent-Q runs here
│    REPL     │
└──────┬──────┘
       │ HTTP/JSON
       ▼
┌─────────────┐
│     LLM     │  ← Claude, GPT-4, etc.
│   Provider  │
└─────────────┘
```

---

## Context Management

**Context** is how you tell Agent-Q what code to pay attention to. Think of it as "here's what's relevant to my question."

### Context Window

Agent-Q maintains a **sliding window of 50 context items**. When full, the oldest item is automatically removed (FIFO).

### Adding Context

**Add current function:**
```
C-c q c d
```

Places cursor on any top-level form (function, class, macro) and press the key. The entire form is added to context.

**Add selected region:**
```
1. Select code with mouse or mark (C-SPC)
2. Press C-c q c r
```

**Add entire buffer:**
```
C-c q c b
```

**Add specific content via @-mention** (while typing):
```
@src/utils.lisp   ← Complete file
@make-hash-table  ← Symbol lookup
@*scratch*        ← Buffer contents
```

Type `@` followed by 2+ characters to trigger completion.

### Viewing Context

**Show summary:**
```
C-c q c s

; Displays in minibuffer:
; "Context: 3 items (2 code, 1 error)"
```

**View in detail:**

Context items appear in the conversation when you send with context (`C-c q S`).

### Clearing Context

**Clear all:**
```
C-c q c c
```

**Context is conversation-scoped**: Starting a new conversation (`C-c q n`) doesn't clear context. Clear explicitly if needed.

### When to Use Context

**Use context when:**
- Asking about specific code ("optimize this function")
- The question relates to your codebase ("how does authentication work?")
- Iterating on a solution ("now add error handling")

**Skip context when:**
- Asking general questions ("what is CLOS?")
- The answer doesn't depend on your code
- Starting an unrelated topic

---

## Chat Interface

### Starting a Chat

**Simple message (no context):**
```
C-c q s
```

Type your question and press Enter. Agent-Q responds in the `*Agent-Q*` buffer.

**Message with context:**
```
C-c q S    (capital S)
```

Includes all accumulated context items in the prompt.

**Send region with custom instruction:**
```
1. Select code region
2. Press C-c q r
3. Type instruction: "Refactor to use CLOS"
4. Press Enter
```

This automatically adds the region to context and sends your instruction.

### The Conversation Buffer

Agent-Q displays conversations in `*Agent-Q*`:

```
=== Agent-Q Conversation ===
Press 'q' to hide, 'n' for new conversation

[USER]
How do I make a hash table?

[AGENT-Q]
You can create a hash table with MAKE-HASH-TABLE...

[TOOL: eval_form]
Args: "(make-hash-table :test 'equal)"
→ #<HASH-TABLE :TEST EQUAL :COUNT 0 {1003A8F123}>

[AGENT-Q]
Here's a complete example with explanation...
```

**Keys in conversation buffer:**
- `q` - Hide buffer
- `n` - New conversation
- `C-c C-c` - Send new message from buffer

### Working with Responses

**Insert at cursor:**
```
C-c q i
```

**Copy to clipboard:**
```
C-c q w    (then paste with C-y)
```

### Streaming

Responses stream token-by-token in real time. Tool calls are synchronous (wait for completion before next step).

---

## Tool System

Agent-Q has **18 tools** organized into 4 categories. You don't call tools directly - the agent decides when to use them.

### Tool Categories

#### 1. Introspection (9 tools - Safe)

Read-only inspection of your Lisp environment.

| Tool | Purpose | Example |
|------|---------|---------|
| `describe_symbol` | Get documentation for any symbol | "What does MAKE-HASH-TABLE do?" |
| `apropos_search` | Find symbols by pattern | "Find hash table functions" |
| `function_arglist` | Show function parameters | "Arguments for MAPCAR?" |
| `who_calls` | Find callers of a function | "What calls INITIALIZE-INSTANCE?" |
| `who_references` | Find references to variable | "What uses *PRINT-LENGTH*?" |
| `list_package_symbols` | List package exports | "What does Alexandria export?" |
| `class_slots` | Show CLOS slots | "Slots of STANDARD-CLASS?" |
| `class_hierarchy` | Show class precedence | "Inheritance for CONDITION?" |
| `macroexpand_form` | Expand macros | "Expand (WHEN x y)" |

#### 2. Execution (4 tools - Moderate)

Execute code in your REPL.

| Tool | Purpose | Safety |
|------|---------|--------|
| `eval_form` | Evaluate Lisp expressions | Auto-execute |
| `compile_form` | Compile and load code | Auto-execute |
| `get_last_error` | Retrieve error details | Safe (read-only) |
| `get_repl_history` | Show recent evaluations | Safe (read-only) |

#### 3. Buffer/File (4 tools - Mixed)

Access files and buffers.

| Tool | Purpose | Safety |
|------|---------|--------|
| `read_file` | Read file contents | Safe |
| `read_buffer` | Read Emacs buffer | Safe |
| `search_in_buffer` | Find pattern in buffer | Safe |
| `write_file` | Write to file | **Dangerous** (requires approval) |

#### 4. Diff (1 tool - Moderate)

Propose changes for review.

| Tool | Purpose |
|------|---------|
| `propose_file_edit` | Generate diff, show for review, apply if accepted |

### Safety Levels

- `:safe` - Auto-execute, no approval needed
- `:cautious` - Log but auto-execute
- `:moderate` - May require approval (context-dependent)
- `:dangerous` - Always requires approval

**Default setting**: `:moderate` (execution tools run automatically, but `write_file` requires approval)

### Watching Tools in Action

Tool calls appear in the conversation buffer as they execute:

```
[TOOL: describe_symbol]
Args: "MAKE-HASH-TABLE"
→ Function: MAKE-HASH-TABLE
  Arguments: (&key test size rehash-size rehash-threshold)
  Creates a new hash table...

[TOOL: eval_form]
Args: "(make-hash-table :test 'equal)"
→ #<HASH-TABLE :TEST EQUAL :COUNT 0>
```

This transparency lets you understand the agent's reasoning process.

---

## Diff Review

When Agent-Q proposes file changes, it shows a **unified diff** for your review.

### The Diff Buffer

```
Agent-Q proposes changes to: src/utils.lisp
Description: Add error handling and improve performance

Progress: 0/3 applied, 0 rejected, 3 pending  |  [a]ccept [r]eject [n]ext [p]rev [q]uit

--- a/src/utils.lisp
+++ b/src/utils.lisp
@@ -10,7 +10,10 @@
 (defun process-items (items)
-  (mapcar #'1+ items))
+  "Process items, incrementing each numeric value."
+  (mapcar (lambda (x)
+            (if (numberp x)
+                (1+ x)
+                x))
+          items))
```

### Review Options

#### Per-Hunk Review (Recommended)

Navigate and review each change individually:

**Navigation:**
```
n     Next hunk
p     Previous hunk
```

**Review:**
```
a     Accept hunk (applies immediately to file)
r     Reject hunk (skip it)
SPC   Toggle hunk state
RET   Preview source location
```

**Finish:**
```
q     Done reviewing (closes buffer)
?     Show help overlay
```

#### All-or-Nothing (Legacy)

```
C-c C-c   Accept all changes
C-c C-k   Reject all changes
```

### Visual Feedback

Hunks are color-coded as you review:

- **Green background + [APPLIED]** - Applied to file
- **Red background + [REJECTED]** - Rejected
- **No highlight** - Pending review

Progress line updates in real-time:
```
Progress: 2/3 applied, 1 rejected, 0 pending
```

### Example Workflow

```
Agent proposes 3 changes:

[Hunk 1] Add docstring
  → Press 'a' ✅ Applied

[Hunk 2] Rename function
  → Press 'r' ❌ Rejected (I like current name)

[Hunk 3] Add type checking
  → Press 'a' ✅ Applied

Press 'q' to finish
  → 2/3 hunks applied
  → File is modified but not yet saved
  → Save with C-x C-s when ready
```

### Important Notes

- **Applied hunks are permanent** (until manual undo)
- **File is not auto-saved** - you control when to save
- **Can't unapply** - use `C-x u` (undo) in source buffer if needed

---

## Sessions

Sessions provide **persistent conversations** across Emacs restarts.

### What is a Session?

A session is a conversation plus metadata, stored in SQLite:

- Conversation history (all messages)
- Context items
- Model information
- Token usage statistics
- Timestamps

### Session Management

**Create new session:**
```
M-x sly-agent-q-create-session

; Prompts for name: "Feature X implementation"
```

**Switch session:**
```
M-x sly-agent-q-switch-session

; Shows list of existing sessions with timestamps
```

**List sessions:**
```
M-x sly-agent-q-list-sessions

; Displays:
; - session-20260122-103045-A3F2: "Feature X" (2 hours ago, 45 messages)
; - session-20260121-140000-B1C4: "Debug auth" (1 day ago, 12 messages)
```

**Rename session:**
```
M-x sly-agent-q-rename-session

; Updates the friendly name
```

**Delete session:**
```
M-x sly-agent-q-delete-session

; Removes from disk and memory (cannot be undone)
```

### Session Lifecycle

1. **Create**: New session starts with empty conversation
2. **Use**: Messages accumulate in conversation
3. **Auto-save**: Session saves automatically (60-second interval)
4. **Switch**: Previous session auto-saves before switching
5. **Restore**: Reopening Emacs restores last active session

### Session ID Format

```
session-YYYYMMDD-HHMMSS-XXXX

Example: session-20260122-103045-A3F2
         └──────┬──────┘ └─┬─┘ └─┬─┘
            ISO date    Time   Random hex
```

### Storage Location

```
~/.agent-q/sessions/
├── session-20260122-103045-A3F2.db    ← SQLite database
├── session-20260121-140000-B1C4.db
└── ...
```

---

## Practical Workflows

### Workflow 1: Learning a New API

**Goal**: Understand how `alexandria:when-let` works

```
1. Press C-c q s
2. Type: "How does Alexandria's when-let macro work?"
3. Agent-Q:
   - Uses describe_symbol("alexandria:when-let")
   - Uses macroexpand_form to show expansion
   - Shows example with eval_form
   - Explains binding and conditional behavior
```

### Workflow 2: Debugging a Crash

**Goal**: Fix function that crashes on edge cases

```lisp
;; Your buggy code
(defun process-data (data)
  (let ((items (split-string data ",")))
    (mapcar #'parse-integer items)))  ; Crashes on non-numeric data!
```

```
1. Place cursor on function
2. Press C-c q c d (add to context)
3. Press C-c q S (send with context)
4. Type: "This crashes on non-numeric input. Fix it."
5. Agent-Q:
   - Uses describe_symbol("process-data")
   - Uses eval_form("(process-data \"1,2,foo\")") → reproduces crash
   - Uses get_last_error → examines backtrace
   - Proposes fix with error handling
   - Uses propose_file_edit → shows diff
6. Review diff, press 'a' to accept hunks
7. Agent-Q:
   - Uses eval_form to verify fix works
   - Confirms: (1 2) - success!
```

### Workflow 3: Code Refactoring

**Goal**: Convert procedural code to CLOS

```lisp
;; Your procedural code
(defun make-person (name age)
  (list :name name :age age))

(defun person-greet (person)
  (format nil "Hello, I'm ~A" (getf person :name)))
```

```
1. Select both functions (mark region)
2. Press C-c q r (send region with instruction)
3. Type: "Refactor to use CLOS with a person class and greet method"
4. Agent-Q proposes:

   (defclass person ()
     ((name :initarg :name :accessor person-name)
      (age :initarg :age :accessor person-age)))

   (defmethod greet ((p person))
     (format nil "Hello, I'm ~A" (person-name p)))

5. Review diff per-hunk:
   - Hunk 1 (class definition): Press 'a' ✅
   - Hunk 2 (method): Press 'a' ✅
6. Press 'q' to finish
7. Agent-Q:
   - Uses compile_form to load definitions
   - Uses eval_form to test: (greet (make-instance 'person :name "Alice"))
   - Confirms it works!
```

### Workflow 4: Adding Documentation

**Goal**: Document an undocumented function

```lisp
(defun calculate-metrics (data threshold)
  (let ((filtered (remove-if (lambda (x) (< x threshold)) data)))
    (list :count (length filtered)
          :mean (/ (reduce #'+ filtered) (length filtered))
          :max (reduce #'max filtered))))
```

```
1. Place cursor on function
2. Press C-c q q d (quick action: document defun)
3. Agent-Q:
   - Uses describe_symbol("calculate-metrics")
   - Analyzes the implementation
   - Proposes comprehensive docstring with examples
4. Review diff, press C-c C-c to accept
```

### Workflow 5: Package Exploration

**Goal**: Find useful functions in a library

```
1. Press C-c q s
2. Type: "What utility functions does Alexandria provide for lists?"
3. Agent-Q:
   - Uses list_package_symbols("ALEXANDRIA")
   - Filters for list-related exports
   - Uses describe_symbol on interesting ones (flatten, ensure-list, lastcar)
   - Shows usage examples with eval_form
   - Suggests which to use for common patterns
```

### Workflow 6: Performance Optimization

**Goal**: Make a slow function faster

```lisp
(defun find-duplicates (list)
  (remove-if-not (lambda (x) (> (count x list) 1))
                 (remove-duplicates list)))  ; O(n²) - slow!
```

```
1. Add function to context (C-c q c d)
2. Press C-c q S
3. Type: "This is slow on large lists. Optimize it."
4. Agent-Q:
   - Uses describe_symbol("find-duplicates")
   - Uses eval_form with timing:
     (time (find-duplicates (loop repeat 10000 collect (random 100))))
     → "Evaluation took: 2.3 seconds"
   - Proposes hash-table version (O(n))
   - Shows diff
5. Accept diff
6. Agent-Q verifies:
   - Uses eval_form with same timing test
   - Reports: "Evaluation took: 0.012 seconds" (192x faster!)
```

---

## Keyboard Reference

All Agent-Q commands use `C-c q` prefix.

### Context

| Key | Action |
|-----|--------|
| `C-c q c r` | Add region to context |
| `C-c q c b` | Add buffer to context |
| `C-c q c d` | Add current defun to context |
| `C-c q c c` | Clear all context |
| `C-c q c s` | Show context summary |

### Conversation

| Key | Action |
|-----|--------|
| `C-c q s` | Send message (no context) |
| `C-c q S` | Send message WITH context |
| `C-c q r` | Send region with instruction |
| `C-c q n` | New conversation |
| `C-c q v` | View conversation buffer |

### Response

| Key | Action |
|-----|--------|
| `C-c q i` | Insert last response at point |
| `C-c q w` | Copy last response to kill ring |

### Quick Actions

| Key | Action |
|-----|--------|
| `C-c q q d` | Document current function |
| `C-c q q e` | Explain selected code |
| `C-c q q f` | Fix recent error |

### Diff Buffer

| Key | Action |
|-----|--------|
| `a` | Accept current hunk |
| `r` | Reject current hunk |
| `SPC` | Toggle hunk state |
| `n` | Next hunk |
| `p` | Previous hunk |
| `RET` | Preview source location |
| `q` | Finish review |
| `?` | Show help |
| `C-c C-c` | Accept all (legacy) |
| `C-c C-k` | Reject all (legacy) |

---

## Troubleshooting

### Authentication Issues

**Symptom**: "Authentication failed" error

**Check API key:**
```bash
echo $ANTHROPIC_API_KEY  # Should show your key
```

**Fix:**
```bash
export ANTHROPIC_API_KEY="sk-ant-your-key-here"
# Add to ~/.bashrc or ~/.zshrc to make permanent

# IMPORTANT: Restart Lisp REPL after setting
```

### Tools Not Working

**Symptom**: Agent says "Tool X not available"

**Verify tools loaded:**
```lisp
(length (agent-q.tools:get-agent-q-tools))
;; Should return 18

(agent-q.tools:list-registered-tools)
;; Should list all tool names
```

**Fix:**
```lisp
(ql:quickload :agent-q :force t)
```

### No Response

**Symptom**: Agent doesn't respond after sending message

**Check connection:**
```lisp
;; Test LLM connection directly
(agent-q:agent-q-send "Hello, are you working?")
```

**Common causes:**
- No internet connection
- API service down
- Invalid API key
- Firewall blocking requests
- Rate limiting (wait a few minutes)

### SLYNK Tools Fail

**Symptom**: `who_calls`, `who_references` fail with "SLYNK not available"

**Cause**: These tools require active SLY connection

**Fix:**
- Ensure connected via `M-x sly`
- Tools will degrade gracefully with error message if SLYNK unavailable

### Diff Buffer Doesn't Appear

**Check:**
```elisp
;; Verify sly-agent-q-diff is loaded
(require 'sly-agent-q-diff)

;; Check *Messages* buffer for errors
C-h e
```

**Verify diff command exists:**
```bash
which diff  # Should return /usr/bin/diff or similar
```

### Emacs Can't Find sly-agent-q

**Symptom**: `(require 'sly-agent-q)` fails

**Fix:**
```elisp
;; Check path
(add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")

;; Verify files exist
M-x find-file ~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/sly-agent-q.el

;; Load manually
(load "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/sly-agent-q.el")
```

---

## Configuration

### LLM Provider

Create `~/.config/agent-q/config.lisp`:

```lisp
(in-package :agent-q)

;; Use Claude Opus (more powerful)
(configure :provider :anthropic
           :model "claude-opus-4-20250514")

;; OR use GPT-4
(configure :provider :openai
           :model "gpt-4-turbo")

;; OR use local Ollama (free, private)
(configure :provider :ollama
           :model "llama2"
           :base-url "http://localhost:11434")
```

### Tool Safety Level

```lisp
;; Allow all tools without approval (risky!)
(configure :max-tool-safety-level :dangerous)

;; Restrict to read-only tools only (safest)
(configure :max-tool-safety-level :safe)

;; Default: moderate (execution auto-runs, write_file needs approval)
(configure :max-tool-safety-level :moderate)
```

### Project-Specific Instructions

Create `.agent-q/system-prompt.md` in your project root:

```markdown
This project uses:
- Alexandria for utilities
- FiveAM for testing
- CLOS for data structures

Code conventions:
- Naming: verb-noun (e.g., parse-data, emit-code)
- Always add docstrings
- Use type declarations for public APIs

When debugging:
1. Use describe_symbol to understand functions
2. Use eval_form to reproduce bugs
3. Use get_last_error to examine backtraces
4. Test fixes with eval_form before proposing
```

Agent-Q automatically includes these instructions when working in this project.

### Emacs Customization

```elisp
;; Change conversation buffer name
(setq sly-agent-q-conversation-buffer-name "*My Agent*")

;; Change key prefix from C-c q to C-c a
(setq sly-agent-q-keymap-prefix (kbd "C-c a"))

;; Disable automatic response insertion
(setq sly-agent-q-insert-response-at-point nil)

;; Adjust context window size
(setq sly-agent-q-max-context-items 100)  ; Default: 50
```

### Face Customization

```elisp
;; Customize conversation buffer colors
(set-face-attribute 'sly-agent-q-user-face nil
                    :foreground "blue" :weight 'bold)

(set-face-attribute 'sly-agent-q-assistant-face nil
                    :foreground "green")
```

---

## Tips and Best Practices

### Getting Best Results

1. **Be specific**: "Add error handling for nil values" > "improve this"
2. **Use context wisely**: Add relevant code, not everything
3. **Iterate**: Start simple, then refine
   - "Write a hash table wrapper"
   - "Add thread safety"
   - "Add tests"
4. **Review diffs carefully**: Check each hunk for complex changes
5. **Let agent test**: Phase 2 tools enable self-verification

### Keyboard Efficiency

**Common sequences:**
```
C-c q c d    Add defun
C-c q S      Send with context
C-c q i      Insert response

(or)

Select code
C-c q r      Send with instruction
n n a a q    Review: next, next, accept, accept, quit
```

### When to Clear Context

- Starting unrelated topic
- Context has grown to 40+ items
- Getting irrelevant responses (too much noise)

### When to Start New Conversation

- Switching to completely different task
- Previous conversation has 50+ messages (performance)
- Want to preserve current conversation for later

---

## What's Next?

### Further Reading

- **Reference Documentation**: `docs/reference/` - Detailed API specs
- **Keyboard Shortcuts PDF**: `docs/reference/keybindings.pdf`
- **Canon Specifications**: `canon/` - Formal specifications for contributors

### Upcoming Features (Phase 4)

- Semantic code indexing
- Profiling integration
- Automated refactoring suggestions
- Pattern detection and learning
- Enhanced knowledge base

---

**Happy Lisping with Agent-Q!** 🚀

For bug reports and feature requests, see GitHub issues.
