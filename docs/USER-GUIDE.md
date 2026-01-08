# Agent-Q User Guide

**Your AI-powered autonomous development partner for Common Lisp**

Agent-Q is an intelligent assistant that integrates with SLY/Emacs to provide context-aware help with Common Lisp development. It can understand your code, answer questions, generate documentation, debug errors, and autonomously iterate on solutions by interacting with your running Lisp image.

---

## Table of Contents

1. [Getting Started](#getting-started)
2. [Quick Start (5 Minutes)](#quick-start-5-minutes)
3. [Understanding Agent-Q](#understanding-agent-q)
4. [User Interface](#user-interface)
5. [Context Management](#context-management)
6. [Conversation Workflow](#conversation-workflow)
7. [Tool System (Phase 2)](#tool-system-phase-2)
8. [Diff Review](#diff-review)
9. [Practical Examples](#practical-examples)
10. [Keyboard Shortcuts Reference](#keyboard-shortcuts-reference)
11. [Troubleshooting](#troubleshooting)
12. [Advanced Configuration](#advanced-configuration)

---

## Getting Started

### What You Need

- **Emacs** with **SLY** installed
- **Common Lisp** implementation (SBCL recommended)
- **Quicklisp** for package management
- **API key** for your chosen LLM provider (Anthropic Claude, OpenAI, or Ollama for local)

### Installation

#### Step 1: Set Your API Key

Add to your shell configuration (`~/.bashrc`, `~/.zshrc`, etc.):

```bash
# For Anthropic Claude (recommended)
export ANTHROPIC_API_KEY="sk-ant-your-key-here"

# OR for OpenAI
export OPENAI_API_KEY="sk-your-key-here"

# OR for Ollama (local, no key needed)
# No API key required
```

Restart your terminal or run `source ~/.bashrc` to apply.

#### Step 2: Install Dependencies

Start your Lisp REPL:

```lisp
(ql:quickload "cl-llm-provider")
```

#### Step 3: Clone and Load Agent-Q

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/yourusername/agent-q.git
```

Then in your Lisp REPL:

```lisp
(ql:quickload :agent-q)
```

#### Step 4: Configure Emacs

Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(with-eval-after-load 'sly
  (add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")
  (require 'sly-agent-q)
  (sly-agent-q-setup))
```

Restart Emacs or evaluate the configuration with `M-x eval-buffer`.

---

## Quick Start (5 Minutes)

### Your First Agent-Q Session

1. **Start SLY**: `M-x sly` (Agent-Q mode activates automatically)

2. **Write some code** in a `.lisp` file:
   ```lisp
   (defun greet (name)
     (format nil "Hello, ~A!" name))
   ```

3. **Add to context**: Place cursor on the function and press `C-c q c d` (add defun to context)

4. **Ask the agent**: Press `C-c q S` (send with context), then type:
   ```
   Add a docstring and make this function handle nil names gracefully
   ```

5. **Review response**: The agent will suggest improvements. Press `C-c q i` to insert at point.

6. **Watch the magic**: With Phase 2 tools enabled, the agent can test its suggestions, see errors, and iterate!

### Verify Tools Are Working (Phase 2)

In your Lisp REPL:

```lisp
;; Check that all 18 tools are loaded
(length (agent-q.tools:get-agent-q-tools))
;; => Should return 18
```

---

## Understanding Agent-Q

### What Makes Agent-Q Different?

Unlike generic AI coding assistants, Agent-Q is **REPL-aware**. It can:

- **Introspect** your running Lisp image (inspect symbols, trace calls, examine classes)
- **Execute code** in the REPL and observe real results
- **Iterate autonomously** based on what it learns (test → error → fix → test)
- **Propose changes** with reviewable diffs
- **Learn context** from your codebase as you work

### The Agent Loop

```
You: "Optimize this function"
  ↓
Agent-Q reads your code (context)
  ↓
Agent-Q introspects the symbol (describe_symbol tool)
  ↓
Agent-Q tests current implementation (eval_form tool)
  ↓
Agent-Q proposes optimized version
  ↓
Agent-Q shows diff for review
  ↓
You: Accept or reject
  ↓
Agent-Q verifies fix works (eval_form again)
  ↓
Done!
```

### Current Capabilities (Phase 2)

**✅ 18 Powerful Tools** organized in four categories:

1. **Introspection** (9 tools) - Explore your Lisp environment
2. **Execution** (4 tools) - Run and compile code
3. **Buffer/File** (4 tools) - Read and write files
4. **Diff** (1 tool) - Propose changes for review

See [Tool System](#tool-system-phase-2) section for details.

---

## User Interface

Agent-Q provides three ways to interact:

### 1. Menu Bar (Easiest for Discovery)

When `sly-agent-q-mode` is active, you'll see an **"Agent-Q"** menu in the menu bar with all commands organized by category:

- **Top-level**: Send Message, Send with Context, Send Region
- **Context** submenu: Add/show/clear context
- **Conversation** submenu: Show buffer, new conversation
- **Response** submenu: Insert/copy last response
- **Quick Actions** submenu: Document, Explain, Fix Error

### 2. Keyboard Shortcuts (Fastest)

All commands start with `C-c q`:

```
C-c q s     Send message
C-c q S     Send with context (capital S)
C-c q c d   Add current function to context
C-c q i     Insert last response
```

See [Keyboard Shortcuts Reference](#keyboard-shortcuts-reference) for complete list.

### 3. M-x Commands (Explicit)

All functions are available as `M-x` commands:

```
M-x sly-agent-q-send
M-x sly-agent-q-add-defun-to-context
M-x sly-agent-q-document-defun
```

### The Conversation Buffer

Agent-Q displays conversations in the `*Agent-Q*` buffer:

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
Here's a complete example...
```

**Keys in conversation buffer:**
- `q` - Hide buffer
- `n` - New conversation
- `C-c C-c` - Send new message

---

## Context Management

Context is how you tell Agent-Q what code to pay attention to.

### What is Context?

Context is a collection of code snippets, buffer contents, and other information that Agent-Q uses when answering your questions. Think of it as "here's what I want you to know about."

### Adding Context

**Add current function:**
```
C-c q c d       (or M-x sly-agent-q-add-defun-to-context)
```

**Add selected region:**
```
1. Select code with mouse or mark (C-SPC)
2. Press C-c q c r
```

**Add entire buffer:**
```
C-c q c b       (or M-x sly-agent-q-add-buffer-to-context)
```

### Viewing Context

**Show summary:**
```
C-c q c s       Shows "3 items: Code, Code, Error"
```

### Clearing Context

**Clear all context:**
```
C-c q c c       (or M-x sly-agent-q-clear-context)
```

### When to Use Context

**Use context when:**
- You want help with specific code ("optimize this function")
- The question relates to your codebase ("how does this work?")
- You're iterating on a solution ("now add error handling")

**Don't use context when:**
- Asking general questions ("what is CLOS?")
- The answer doesn't depend on your code
- Starting a new unrelated topic

### Example Session with Context

```lisp
;; 1. Write code
(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

;; 2. Add to context: C-c q c d

;; 3. Ask with context: C-c q S
;; "Make this tail-recursive and add memoization"

;; 4. Agent reads your code, proposes optimized version

;; 5. Review diff, accept if good

;; 6. Done!
```

---

## Conversation Workflow

### Basic Conversation

**Start a conversation:**
```
C-c q s     (send message without context)
```
Type your question, press Enter.

**Send with context:**
```
C-c q S     (capital S - includes accumulated context)
```

**View conversation:**
```
C-c q v     (switch to *Agent-Q* buffer)
```

**Start fresh:**
```
C-c q n     (new conversation - clears history)
```

### Response Handling

**Insert response at cursor:**
```
C-c q i     (insert last response)
```

**Copy to clipboard:**
```
C-c q w     (copy last response to kill ring)
```

Then paste with `C-y` wherever you need it.

### Region-Based Workflow

**Send code with instruction:**
```
1. Select code region
2. Press C-c q r
3. Type instruction: "Refactor to use CLOS"
4. Press Enter
```

This automatically adds the region to context and sends your instruction.

### Quick Actions

**Document current function:**
```
C-c q q d   (cursor on function)
```
Agent adds docstring and parameter descriptions.

**Explain selected code:**
```
1. Select code
2. Press C-c q q e
```
Agent explains what it does and why.

**Fix last error:**
```
C-c q q f   (after seeing an error in REPL)
```
Agent examines the error and suggests a fix.

---

## Tool System (Phase 2)

Agent-Q has **18 tools** that let it autonomously explore and modify your Lisp environment.

### Tool Categories

#### 1. Introspection Tools (9 tools)

These are **safe** - they only read information:

| Tool | What It Does | Example Use |
|------|--------------|-------------|
| `describe_symbol` | Get documentation for any symbol | "What does MAKE-HASH-TABLE do?" |
| `apropos_search` | Find symbols matching a pattern | "Find all symbols about hash tables" |
| `function_arglist` | Show function parameters | "What arguments does MAPCAR take?" |
| `who_calls` | Find what calls a function | "What calls INITIALIZE-INSTANCE?" |
| `who_references` | Find what uses a variable | "What code uses *PRINT-LENGTH*?" |
| `list_package_symbols` | List package exports | "What does Alexandria export?" |
| `class_slots` | Show CLOS class slots | "What slots does STANDARD-CLASS have?" |
| `class_hierarchy` | Show class precedence | "Show inheritance for STANDARD-OBJECT" |
| `macroexpand_form` | Expand macros | "Expand (WHEN condition body)" |

#### 2. Execution Tools (4 tools)

These **run code** in your REPL (moderate safety):

| Tool | What It Does | Example Use |
|------|--------------|-------------|
| `eval_form` | Evaluate Lisp expressions | Agent tests "(+ 1 2 3)" to verify math |
| `compile_form` | Compile and load code | Agent defines new function and loads it |
| `get_last_error` | Retrieve error details | Agent examines backtrace after crash |
| `get_repl_history` | Show recent evaluations | Agent sees what you've been testing |

#### 3. Buffer/File Tools (4 tools)

These **access files**:

| Tool | Safety | What It Does |
|------|--------|--------------|
| `read_file` | Safe | Read file contents |
| `read_buffer` | Safe | Read current Emacs buffer |
| `search_in_buffer` | Safe | Find pattern in buffer |
| `write_file` | Dangerous | Write to file (requires approval) |

#### 4. Diff Tool (1 tool)

The most powerful tool:

| Tool | What It Does |
|------|--------------|
| `propose_file_edit` | Generates a diff showing proposed changes, displays in Emacs for review, applies only if you accept |

### How Tools Work

**You don't call tools directly.** The agent decides when to use them.

**Example autonomous workflow:**

```
You: "The function FOO crashes on empty lists. Fix it."

Agent thinks: "I need to understand FOO first"
→ Uses describe_symbol("FOO")
→ Sees it calls CAR without checking

Agent thinks: "Let me reproduce the bug"
→ Uses eval_form("(foo '())")
→ Sees error: "CAR: () is not a list"

Agent thinks: "I know the fix"
→ Uses propose_file_edit to add (when list ...) guard
→ Shows you the diff

You: Press C-c C-c (accept)

Agent thinks: "Let me verify the fix"
→ Uses eval_form("(foo '())")
→ Sees: NIL (no error!)

Agent: "Fixed! The function now handles empty lists."
```

### Tool Safety Levels

**:safe** - Read-only, no side effects (introspection, reading files)
**:moderate** - Can execute code but contained (eval, compile)
**:dangerous** - Can modify system (write files, delete data)

Default setting: `:moderate` (safe + execution tools, but write_file requires approval)

### Watching Tools in Action

Tools appear in the conversation buffer as they execute:

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

---

## Diff Review

When Agent-Q proposes changes to files, it shows a **unified diff** for your review.

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

#### Option 1: All-or-Nothing (Legacy)

**Accept all changes:**
```
C-c C-c
```
Applies entire diff, saves file (or leaves modified for manual save).

**Reject all changes:**
```
C-c C-k
```
Discards all proposed changes.

#### Option 2: Per-Hunk Review (New in v0.3)

Review each change individually:

**Navigate between hunks:**
```
n     Next hunk
p     Previous hunk
```

**Review current hunk:**
```
a     Accept hunk (applies immediately)
r     Reject hunk (skip it)
SPC   Toggle hunk accepted/rejected
RET   Preview source location
```

**Finish review:**
```
q     Done reviewing (applies accepted hunks)
?     Show help overlay
```

### Visual Feedback

As you review, hunks are color-coded:

- **Green background + [APPLIED]** - Hunk was applied
- **Red background + [REJECTED]** - Hunk was rejected
- **No highlight** - Hunk is pending review

Progress line shows live counts:
```
Progress: 2/3 applied, 1 rejected, 0 pending
```

### Example Workflow

```
Agent proposes 3 changes:

[Hunk 1] Add docstring
  → Press 'a' to accept
  → Turns green, [APPLIED] appears
  → Progress: 1/3 applied

[Hunk 2] Rename process-items to handle-items
  → Press 'r' to reject (I like the current name)
  → Turns red, [REJECTED] appears
  → Progress: 1/3 applied, 1 rejected

[Hunk 3] Add type checking
  → Press 'a' to accept
  → Turns green
  → Progress: 2/3 applied, 1 rejected

Press 'q' to finish
  → Applied hunks are saved
  → Rejected hunks are discarded
  → Buffer shows 2/3 hunks applied
```

### Tips for Diff Review

**Preview before accepting:**
```
Press RET on a hunk → jumps to source location
Review the surrounding code
Press C-x o to return to diff buffer
Press 'a' to accept or 'r' to reject
```

**Mix approaches:**
```
Use 'a'/'r' for most hunks
Use C-c C-c at the end to bulk-accept remaining hunks
```

**Undo accepted hunks:**
```
Can't unapply directly in diff buffer
But file isn't saved yet - use C-x u in source buffer
Or close without saving
```

---

## Practical Examples

### Example 1: Learning a New Library

**Goal:** Understand how Alexandria's `when-let` works

```
You: C-c q s
     "How does Alexandria's when-let macro work?"

Agent-Q:
  → Uses describe_symbol("when-let")
  → Uses macroexpand_form("(when-let ((x (find-value))) (process x))")
  → Explains the binding and conditional behavior
  → Shows example usage
```

### Example 2: Debugging a Function

**Goal:** Fix a function that crashes on edge cases

```lisp
;; Your buggy code
(defun process-data (data)
  (let ((items (split-string data ",")))
    (mapcar #'parse-integer items)))

;; Crashes on non-numeric data!
```

```
You: Add function to context (C-c q c d)
     Then C-c q S
     "This crashes on non-numeric input. Fix it."

Agent-Q:
  → Uses describe_symbol("process-data")
  → Uses eval_form("(process-data \"1,2,foo\")") to reproduce bug
  → Uses get_last_error to see the error
  → Proposes fix with error handling:

(defun process-data (data)
  "Parse comma-separated integers, skipping invalid values."
  (let ((items (split-string data ",")))
    (remove nil
            (mapcar (lambda (item)
                      (ignore-errors (parse-integer item)))
                    items))))

  → Uses propose_file_edit to show diff

You: Review diff, press C-c C-c to accept

Agent-Q:
  → Uses eval_form("(process-data \"1,2,foo\")") to verify fix
  → Confirms: (1 2) - works!
```

### Example 3: Code Refactoring

**Goal:** Convert procedural code to use CLOS

```lisp
;; Your procedural code
(defun make-person (name age)
  (list :name name :age age))

(defun person-greet (person)
  (format nil "Hello, I'm ~A" (getf person :name)))
```

```
You: Select both functions
     C-c q r
     "Refactor to use CLOS with a person class and greet method"

Agent-Q:
  → Reads your code from context
  → Proposes CLOS version:

(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)))

(defmethod greet ((p person))
  (format nil "Hello, I'm ~A" (person-name p)))

  → Uses propose_file_edit to show diff

You: Press 'a' to accept class definition
     Press 'a' to accept method definition
     Press 'q' to finish

Agent-Q:
  → Uses compile_form to load new definitions
  → Uses eval_form to test:
      (greet (make-instance 'person :name "Alice" :age 30))
  → Confirms it works!
```

### Example 4: Adding Documentation

**Goal:** Document an undocumented function

```lisp
(defun calculate-metrics (data threshold)
  (let ((filtered (remove-if (lambda (x) (< x threshold)) data)))
    (list :count (length filtered)
          :mean (/ (reduce #'+ filtered) (length filtered))
          :max (reduce #'max filtered))))
```

```
You: Cursor on function
     C-c q q d (quick action: document defun)

Agent-Q:
  → Uses describe_symbol("calculate-metrics")
  → Analyzes the code
  → Proposes documentation:

(defun calculate-metrics (data threshold)
  "Calculate metrics for DATA values above THRESHOLD.

DATA is a list of numeric values.
THRESHOLD is the minimum value to include in calculations.

Returns a plist with:
  :COUNT - Number of values above threshold
  :MEAN - Average of included values
  :MAX - Maximum value in filtered set

Example:
  (calculate-metrics '(1 5 3 8 2) 3)
  => (:COUNT 2 :MEAN 6.5 :MAX 8)"
  (let ((filtered (remove-if (lambda (x) (< x threshold)) data)))
    (list :count (length filtered)
          :mean (/ (reduce #'+ filtered) (length filtered))
          :max (reduce #'max filtered))))

  → Shows diff

You: C-c C-c to accept
```

### Example 5: Exploring a Package

**Goal:** Find useful functions in Alexandria

```
You: C-c q s
     "What utility functions does Alexandria provide for lists?"

Agent-Q:
  → Uses list_package_symbols("ALEXANDRIA")
  → Finds list-related exports
  → Uses describe_symbol for interesting ones:
      - flatten, ensure-list, lastcar, mappend
  → Shows examples of each with eval_form
  → Suggests which ones to use for your use case
```

### Example 6: Performance Optimization

**Goal:** Make a slow function faster

```lisp
(defun find-duplicates (list)
  (remove-if-not (lambda (x)
                   (> (count x list) 1))
                 (remove-duplicates list)))
```

```
You: Add to context (C-c q c d)
     C-c q S
     "This is slow on large lists. Optimize it."

Agent-Q:
  → Uses describe_symbol("find-duplicates")
  → Uses eval_form("(time (find-duplicates (loop repeat 10000 collect (random 100))))")
  → Sees timing: "Evaluation took: 2.3 seconds"
  → Proposes hash-table version:

(defun find-duplicates (list)
  "Find duplicate elements in LIST. Optimized for large lists."
  (let ((seen (make-hash-table :test 'equal))
        (duplicates (make-hash-table :test 'equal)))
    (dolist (item list)
      (if (gethash item seen)
          (setf (gethash item duplicates) t)
          (setf (gethash item seen) t)))
    (loop for key being the hash-keys of duplicates collect key)))

  → Shows diff

You: Accept diff

Agent-Q:
  → Uses eval_form("(time (find-duplicates (loop repeat 10000 collect (random 100))))")
  → Reports: "Evaluation took: 0.012 seconds" (192x faster!)
```

---

## Keyboard Shortcuts Reference

All Agent-Q commands use the `C-c q` prefix.

### Context Commands

| Key | Command | Description |
|-----|---------|-------------|
| `C-c q c r` | `sly-agent-q-add-region-to-context` | Add selected region to context |
| `C-c q c b` | `sly-agent-q-add-buffer-to-context` | Add entire buffer to context |
| `C-c q c d` | `sly-agent-q-add-defun-to-context` | Add current top-level form to context |
| `C-c q c c` | `sly-agent-q-clear-context` | Clear all accumulated context |
| `C-c q c s` | `sly-agent-q-show-context` | Show context summary in minibuffer |

### Conversation Commands

| Key | Command | Description |
|-----|---------|-------------|
| `C-c q s` | `sly-agent-q-send` | Send message (without context) |
| `C-c q S` | `sly-agent-q-send-with-context` | Send message WITH context (capital S) |
| `C-c q r` | `sly-agent-q-send-region-with-instruction` | Send selected region with instruction |
| `C-c q n` | `sly-agent-q-new-conversation` | Start new conversation (clears history) |
| `C-c q v` | `sly-agent-q-show-conversation` | Show/switch to conversation buffer |

### Response Commands

| Key | Command | Description |
|-----|---------|-------------|
| `C-c q i` | `sly-agent-q-insert-last-response` | Insert last response at point |
| `C-c q w` | `sly-agent-q-copy-last-response` | Copy last response to kill ring |

### Quick Action Commands

| Key | Command | Description |
|-----|---------|-------------|
| `C-c q q d` | `sly-agent-q-document-defun` | Add documentation to current function |
| `C-c q q e` | `sly-agent-q-explain-region` | Explain selected code |
| `C-c q q f` | `sly-agent-q-fix-error` | Help fix recent error |

### Diff Buffer Commands

When reviewing proposed changes in `*Agent-Q Diff*` buffer:

| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-c` | `sly-agent-q-diff-accept` | Accept all changes |
| `C-c C-k` | `sly-agent-q-diff-reject` | Reject all changes |
| `a` | `sly-agent-q-diff-accept-hunk` | Accept current hunk |
| `r` | `sly-agent-q-diff-reject-hunk` | Reject current hunk |
| `SPC` | `sly-agent-q-diff-toggle-hunk` | Toggle hunk state |
| `n` | `diff-hunk-next` | Next hunk |
| `p` | `diff-hunk-prev` | Previous hunk |
| `RET` | `sly-agent-q-diff-preview-hunk` | Preview source location |
| `q` | `sly-agent-q-diff-finish` | Finish review |
| `?` | `sly-agent-q-diff-show-help` | Show help overlay |

### Conversation Buffer Commands

When in `*Agent-Q*` conversation buffer:

| Key | Command | Description |
|-----|---------|-------------|
| `q` | `quit-window` | Hide conversation buffer |
| `n` | `sly-agent-q-new-conversation` | Start new conversation |
| `C-c C-c` | `sly-agent-q-send` | Send new message |

---

## Troubleshooting

### Common Issues

#### "Agent-Q not loaded" message

**Symptom:** Error when running Agent-Q commands

**Fix:**
```lisp
;; In your Lisp REPL:
(ql:quickload :agent-q)
```

Make sure it loads without errors. If you see errors, check that `cl-llm-provider` is installed.

#### "Authentication failed" error

**Symptom:** Agent can't connect to LLM provider

**Diagnosis:**
```bash
# Check environment variable is set
echo $ANTHROPIC_API_KEY  # Should show your key

# If empty:
export ANTHROPIC_API_KEY="sk-ant-your-key-here"

# Make permanent by adding to ~/.bashrc or ~/.zshrc
```

**Important:** Restart your Lisp REPL after setting environment variables!

#### "Rate limited" error

**Symptom:** API returns 429 error

**Fix:**
- Wait a few minutes (rate limits reset)
- Reduce message frequency
- Consider using Ollama for local development (no rate limits)

#### No response from agent

**Diagnosis:**
```lisp
;; Test LLM connection directly
(agent-q:agent-q-send "Hello, are you working?")
```

**Common causes:**
- No internet connection
- API service is down
- Invalid API key
- Firewall blocking requests

#### Tools not working

**Symptom:** Agent says "Tool X not available"

**Diagnosis:**
```lisp
;; Check tool count
(length (agent-q.tools:get-agent-q-tools))
;; Should return 18

;; List all tool names
(agent-q.tools:list-registered-tools)
```

**Fix:** If count is wrong, reload Agent-Q:
```lisp
(ql:quickload :agent-q :force t)
```

#### "SLYNK not available" errors

**Symptom:** Tools like `who_calls` fail

**Cause:** Some tools require active SLY connection

**Fix:**
- Make sure you're connected via `M-x sly`
- Tools will degrade gracefully, but some features require SLYNK

#### Diff buffer doesn't appear

**Diagnosis:**
```
Check *Messages* buffer (C-h e) for errors
Verify diff command exists: which diff
```

**Fix:**
```elisp
;; Ensure sly-agent-q-diff is loaded
(require 'sly-agent-q-diff)
```

#### Emacs can't find sly-agent-q

**Symptom:** `(require 'sly-agent-q)` fails

**Fix:**
```elisp
;; Check path is correct
(add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")

;; Verify files exist
;; M-x find-file ~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/sly-agent-q.el

;; Try loading manually
(load "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/sly-agent-q.el")
```

### Getting More Help

**Check conversation buffer:**
```
C-c q v
```
Look for error messages or tool failures.

**Run test suite:**
```lisp
(ql:quickload :agent-q/tests)
(agent-q-tests:run-phase-2-tests)
```

**Enable debug output:**
```lisp
;; See all tool calls
(setf agent-q:*debug-tools* t)
```

**Check Emacs Messages:**
```
M-x view-messages-buffer
```
Look for Elisp errors.

---

## Advanced Configuration

### Customizing the Provider

Create `~/.config/agent-q/config.lisp`:

```lisp
(in-package :agent-q)

;; Use Claude Opus (more powerful, slower)
(configure :provider :anthropic
           :model "claude-opus-4-20250514")

;; Use GPT-4
(configure :provider :openai
           :model "gpt-4-turbo")

;; Use local Ollama (free, private)
(configure :provider :ollama
           :model "llama2"
           :base-url "http://localhost:11434")
```

### Adjusting Tool Safety

```lisp
;; Allow all tools without approval (risky!)
(configure :max-tool-safety-level :dangerous)

;; Restrict to read-only tools only (safest)
(configure :max-tool-safety-level :safe)

;; Default: moderate (execution tools auto-run, write_file requires approval)
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

Agent-Q will automatically include these instructions when working in this project.

### Emacs Customization

```elisp
;; Change conversation buffer name
(setq sly-agent-q-conversation-buffer-name "*My Agent*")

;; Disable automatic response insertion
(setq sly-agent-q-insert-response-at-point nil)

;; Change key prefix from C-c q to something else
(setq sly-agent-q-keymap-prefix (kbd "C-c a"))

;; Disable tool execution display
(setq sly-agent-q-tools-show-execution nil)
```

### Face Customization

Change colors used in conversation buffer:

```elisp
;; Customize faces
(set-face-attribute 'sly-agent-q-user-face nil
                    :foreground "blue" :weight 'bold)

(set-face-attribute 'sly-agent-q-assistant-face nil
                    :foreground "green")

(set-face-attribute 'sly-agent-q-debug-face nil
                    :foreground "gray50" :slant 'italic)
```

### Tool Filtering

```lisp
;; Get only introspection tools
(agent-q.tools:get-agent-q-tools :categories '(:introspection))

;; Get only safe tools
(remove-if-not (lambda (tool)
                 (eq (slot-value tool 'cl-llm-provider::safety-level) :safe))
               (agent-q.tools:get-agent-q-tools))
```

---

## Tips and Best Practices

### Getting the Best Results

1. **Be specific:** "Add error handling for nil values" is better than "improve this"

2. **Use context wisely:** Add relevant code, but don't overload with unrelated functions

3. **Iterate:** Start simple, then ask for refinements
   - "Write a hash table wrapper"
   - "Add thread safety"
   - "Add tests"

4. **Review diffs carefully:** Especially for complex changes, review hunk-by-hunk

5. **Let the agent test:** With Phase 2 tools, the agent can verify its own solutions

### Workflow Patterns

**Learning pattern:**
```
1. C-c q s "Explain CLOS method dispatch"
2. Agent uses describe_symbol, macroexpand_form
3. You learn from examples
```

**Debugging pattern:**
```
1. Add buggy function to context (C-c q c d)
2. C-c q S "This crashes on empty lists"
3. Agent reproduces, analyzes, proposes fix
4. Review diff, accept
5. Agent verifies fix
```

**Refactoring pattern:**
```
1. Select code region
2. C-c q r "Refactor to use CLOS"
3. Review diff hunk-by-hunk
4. Accept good changes, reject others
5. Iterate with follow-up questions
```

**Documentation pattern:**
```
1. Cursor on undocumented function
2. C-c q q d (quick document)
3. Review and accept
```

### Keyboard Efficiency

**Common sequences:**
```
C-c q c d    Add defun to context
C-c q S      Send with context
C-c q i      Insert response

(or)

Select code
C-c q r      Send region with instruction
C-c C-c      Accept diff
```

**Quick review:**
```
n n n        Jump through hunks
a            Accept good ones
r            Reject bad ones
q            Finish
```

---

## What's Next?

### Upcoming Features

**Phase 3 (Autonomous Developer):**
- Condition system integration (automatic restarts)
- Test framework integration (run tests, write tests)
- Knowledge base (learn from past sessions)
- Execution tracing

**Phase 4 (Intelligent Partner):**
- Semantic code indexing
- Profiling integration
- Automated refactoring suggestions
- Pattern detection

### Learning Resources

- **Specifications:** See `specs/` directory for technical details
- **Testing Guide:** `TESTING.md` for running tests
- **Diff Guide:** `docs/DIFF-REVIEW-GUIDE.md` for advanced diff review
- **Quick Start:** `QUICKSTART.md` for Phase 2 tool system

### Getting Involved

Agent-Q is open source! Contributions welcome:

1. Report issues on GitHub
2. Suggest new features
3. Write custom tools
4. Improve documentation
5. Share your workflows

---

## Summary

Agent-Q transforms Emacs into an intelligent Common Lisp development environment:

✅ **Context-aware** - Understands your code
✅ **REPL-integrated** - Tests and iterates autonomously
✅ **Tool-powered** - 18 tools for introspection, execution, and modification
✅ **Safe by default** - Review diffs before accepting changes
✅ **Extensible** - Configure providers, safety levels, and instructions

**Start simple:**
1. Load Agent-Q: `(ql:quickload :agent-q)`
2. Ask a question: `C-c q s`
3. Add code context when needed: `C-c q c d`
4. Review and accept suggestions: `C-c C-c`

**Get powerful:**
- Let the agent use tools to explore and test
- Review changes hunk-by-hunk
- Iterate on solutions
- Build custom workflows

Happy Lisping with Agent-Q! 🚀
