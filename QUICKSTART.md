# Agent-Q Quick Start Guide (Phase 2)

Get up and running with Agent-Q's REPL-aware tool system in 10 minutes!

## What's New in Phase 2

Agent-Q now has **18 powerful tools** that let it:
- ✅ Introspect your live Lisp image (9 tools)
- ✅ Execute and compile code (4 tools)
- ✅ Read and write files via Emacs (4 tools)
- ✅ Propose file edits with diffs (1 tool)

The agent can now **autonomously debug**, **explore your codebase**, and **iterate on solutions** by observing real results from the running REPL!

## Prerequisites Checklist

- [ ] Emacs with SLY installed
- [ ] Common Lisp (SBCL recommended)
- [ ] Quicklisp installed
- [ ] API key for LLM provider (Anthropic/OpenAI/etc.)
- [ ] `diff` command available (for propose_file_edit)

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
(ql:quickload :agent-q)
```

You should see all 18 tools being registered!

### Step 5: Configure Emacs

Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(with-eval-after-load 'sly
  (add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")
  (require 'sly-agent-q)
  (sly-agent-q-setup))
```

Restart Emacs or evaluate the code with `M-x eval-buffer`.

## Phase 2: Testing the Tool System

### Verify Tools Are Loaded

In your Lisp REPL:

```lisp
;; Check that all 18 tools are registered
(length (agent-q.tools:get-agent-q-tools))
;; => Should return 18

;; List all tool names
(mapcar (lambda (tool)
          (slot-value tool 'cl-llm-provider::name))
        (agent-q.tools:get-agent-q-tools))
```

### Test Category 1: Introspection Tools (9 tools)

These tools let the agent explore your Lisp environment:

#### 1. describe_symbol

```lisp
;; In Emacs, send to agent:
;; "What does the symbol DEFUN do?"
```

Agent will use `describe_symbol` to get detailed info about DEFUN.

#### 2. apropos_search

```lisp
;; Ask agent:
;; "Find all symbols related to 'hash table'"
```

Agent will use `apropos_search` to find matching symbols.

#### 3. function_arglist

```lisp
;; Ask agent:
;; "What are the arguments for MAKE-HASH-TABLE?"
```

Agent will use `function_arglist` to show the lambda list.

#### 4. who_calls (requires SLY connection)

```lisp
;; Ask agent:
;; "What functions call MAKE-INSTANCE?"
```

Agent will use SLYNK to find callers.

#### 5. who_references

```lisp
;; Ask agent:
;; "What code references *PRINT-LENGTH*?"
```

#### 6. list_package_symbols

```lisp
;; Ask agent:
;; "Show me the exported symbols from the CL package"
```

#### 7. class_slots

```lisp
;; Ask agent:
;; "What slots does STANDARD-CLASS have?"
```

#### 8. class_hierarchy

```lisp
;; Ask agent:
;; "Show me the class hierarchy for STANDARD-OBJECT"
```

#### 9. macroexpand_form

```lisp
;; Ask agent:
;; "Expand the macro (WHEN condition body)"
```

### Test Category 2: Execution Tools (4 tools)

These tools let the agent run code in your REPL:

#### 10. eval_form

```lisp
;; Ask agent:
;; "What is (+ 1 2 3)?"
```

Agent will evaluate it and see the result: 6

#### 11. compile_form

```lisp
;; Ask agent:
;; "Define a function that doubles a number"
```

Agent will use `compile_form` to compile and load the function.

#### 12. get_last_error

```lisp
;; Trigger an error in REPL:
(error "test error")

;; Then ask agent:
;; "What was the last error?"
```

Agent will retrieve error details including backtrace.

#### 13. get_repl_history

```lisp
;; Ask agent:
;; "What have I evaluated recently?"
```

Agent will show recent REPL evaluations.

### Test Category 3: Buffer/File Tools (4 tools)

These tools let the agent interact with Emacs buffers:

#### 14. read_file

```lisp
;; Create a test file:
(with-open-file (f "/tmp/test.lisp" :direction :output :if-exists :supersede)
  (format f "(defun hello () (print \"Hello, world!\"))"))

;; Ask agent:
;; "Read /tmp/test.lisp and tell me what it does"
```

#### 15. read_buffer

```lisp
;; With a Lisp buffer open in Emacs, ask agent:
;; "What code is in my current buffer?"
```

#### 16. search_in_buffer

```lisp
;; Ask agent:
;; "Find all occurrences of 'defun' in the current buffer"
```

#### 17. write_file (dangerous - requires approval)

```lisp
;; Ask agent:
;; "Create a new file /tmp/greet.lisp with a greeting function"
```

This requires user approval before execution.

### Test Category 4: Diff Tool (1 tool)

The most powerful tool - lets agent propose changes with user review:

#### 18. propose_file_edit

```lisp
;; Create a file to edit:
(with-open-file (f "/tmp/math.lisp" :direction :output :if-exists :supersede)
  (format f "(defun add (a b)~%  (+ a b))"))

;; Ask agent:
;; "Read /tmp/math.lisp and add a subtract function"
```

Agent will:
1. Read the file
2. Propose changes as a unified diff
3. Show diff in Emacs buffer
4. Wait for you to press `C-c C-c` (accept) or `C-c C-k` (reject)
5. Apply changes if accepted

## End-to-End Testing Scenarios

### Scenario 1: Autonomous Bug Fix

```lisp
;; Create buggy code
(defun process-items (items)
  (mapcar #'1+ items))  ; Bug: doesn't handle non-numbers

;; Ask agent:
;; "The function PROCESS-ITEMS crashes on non-numbers. Fix it."
```

Agent will:
1. Use `describe_symbol` to inspect the function
2. Use `eval_form` to test it and see the error
3. Use `get_last_error` to examine the backtrace
4. Propose a fix using `propose_file_edit`
5. Test the fix with `eval_form`

### Scenario 2: Code Exploration

```lisp
;; Ask agent:
;; "Find all functions that call MAKE-HASH-TABLE,
;; then show me the lambda list for the first one"
```

Agent will:
1. Use `who_calls` to find callers
2. Use `function_arglist` to show arguments

### Scenario 3: Package Discovery

```lisp
;; Ask agent:
;; "What utility functions are exported by the Alexandria package?"
```

Agent will:
1. Use `list_package_symbols` to list exports
2. Use `apropos_search` to find related symbols
3. Use `describe_symbol` to explain key functions

### Scenario 4: Iterative Development

```lisp
;; Ask agent:
;; "Write a function to calculate factorial"

;; Agent writes it. Then ask:
;; "Make it tail-recursive"

;; Agent rewrites. Then ask:
;; "Add memoization"

;; Agent adds caching. Then ask:
;; "Test it with (factorial 10)"
```

Agent will use `eval_form` after each iteration to verify the code works.

## Manual Testing Checklist

Use this to verify all Phase 2 features:

### Registry Tests
- [ ] All 18 tools load without errors
- [ ] `(agent-q.tools:get-agent-q-tools)` returns list
- [ ] Safe tools accessible at `:safe` level
- [ ] Moderate tools require `:moderate` level

### Introspection Tools
- [ ] `describe_symbol` retrieves symbol info
- [ ] `apropos_search` finds matching symbols
- [ ] `function_arglist` shows lambda lists
- [ ] `who_calls` works (with SLY connected)
- [ ] `who_references` works (with SLY connected)
- [ ] `list_package_symbols` lists package exports
- [ ] `class_slots` shows CLOS slot info
- [ ] `class_hierarchy` shows class precedence
- [ ] `macroexpand_form` expands macros

### Execution Tools
- [ ] `eval_form` evaluates simple expressions
- [ ] `eval_form` handles errors gracefully
- [ ] `compile_form` compiles and loads code
- [ ] `get_last_error` retrieves error info
- [ ] `get_repl_history` shows recent evaluations

### Buffer Tools
- [ ] `read_file` reads file contents
- [ ] `read_buffer` reads current buffer
- [ ] `search_in_buffer` finds patterns
- [ ] `write_file` requires approval

### Diff Tool
- [ ] `propose_file_edit` shows unified diff
- [ ] Diff buffer appears with `C-c C-c` / `C-c C-k` instructions
- [ ] Accepting diff (C-c C-c) applies changes
- [ ] Rejecting diff (C-c C-k) discards changes
- [ ] Agent receives feedback about accept/reject

### Agent Loop
- [ ] Agent can call multiple tools in sequence
- [ ] Tool results feed back to agent for iteration
- [ ] Errors don't crash the agent loop
- [ ] Max iterations (10) prevents infinite loops

## Common Workflows

### Workflow 1: Debug a Function

1. Write buggy code in Emacs
2. Ask agent: "Debug the function FOO"
3. Agent introspects, tests, proposes fix
4. Review diff, accept if correct
5. Agent verifies fix works

### Workflow 2: Learn About a Symbol

1. Ask agent: "Explain how LOOP works"
2. Agent uses `describe_symbol`
3. Agent uses `macroexpand_form` for examples
4. Agent uses `apropos_search` for related symbols

### Workflow 3: Refactor Code

1. Select code region in Emacs
2. Ask agent: "Refactor this to use CLOS"
3. Agent reads code, proposes changes
4. Review diff, iterate if needed
5. Accept final version

### Workflow 4: Explore Package

1. Ask agent: "What does the FiveAM package provide?"
2. Agent uses `list_package_symbols`
3. Agent uses `describe_symbol` on key exports
4. Agent summarizes testing framework features

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

**Diff Review (in *Agent-Q Diff* buffer):**
```
Per-Hunk (New in v0.3):
  a        - Accept current hunk (apply immediately)
  r        - Reject current hunk (skip)
  SPC      - Toggle hunk state
  n        - Next hunk
  p        - Previous hunk
  RET      - Preview source location
  q        - Finish review
  ?        - Show help

All-or-Nothing:
  C-c C-c  - Accept all changes
  C-c C-k  - Reject all changes
```

### Reviewing Diffs Per-Hunk (New in v0.3)

When Agent-Q proposes changes, you can review each change individually:

**Quick Start:**
1. Diff buffer opens with all proposed changes
2. Press `n` to go to next hunk, `p` for previous
3. Press `a` to accept current hunk (applies immediately)
4. Press `r` to reject current hunk (skip it)
5. Press `q` when done reviewing

**Visual Feedback:**
- Green background with [APPLIED] = applied
- Red background with [REJECTED] = rejected
- Progress line shows: X/Y applied, Z rejected, W pending

**Example Workflow:**
```
Agent proposes 3 changes to defun FOO:
  [Hunk 1] Add docstring          ← Press 'a' to accept
  [Hunk 2] Rename to foo-impl     ← Press 'r' to reject
  [Hunk 3] Add type declaration   ← Press 'a' to accept

Result: 2/3 hunks applied, file modified, save when ready
```

See `docs/DIFF-REVIEW-GUIDE.md` for detailed examples.

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

### "Tool not found"

Verify tools are loaded:
```lisp
;; Should return 18
(length (agent-q.tools:get-agent-q-tools))

;; List all tool names
(agent-q.tools:list-registered-tools)
```

### "SLYNK not available" errors

Some tools require SLY connection:
- Make sure you're connected via `M-x sly`
- Tools like `who_calls` need SLYNK backend
- Without SLY, these tools degrade gracefully with error messages

### Diff buffer doesn't appear

1. Check Emacs has `sly-agent-q-diff.el` loaded
2. Verify `diff` command exists: `which diff`
3. Check `*Messages*` buffer for errors

### "Malformed property list" errors

This is a known issue with direct tool access. Use helper functions:
```lisp
;; Don't do this:
(getf tool :name)

;; Do this instead:
(slot-value tool 'cl-llm-provider::name)
```

## Running the Test Suite

Verify everything works:

```lisp
;; Load tests
(ql:quickload :agent-q/tests)

;; Run all Phase 2 tests
(agent-q-tests:run-phase-2-tests)
```

You should see:
```
=== Running Phase 2 Tool System Tests ===
...
=== All Phase 2 Tests Complete ===
Pass: 124 (95%)
Fail: 6 (5%)
```

## Advanced Configuration

### Adjust Tool Safety Levels

In `~/.config/agent-q/config.lisp`:

```lisp
(in-package :agent-q)

;; Allow all tools without approval
(configure :max-tool-safety-level :dangerous)

;; Or restrict to safe tools only
(configure :max-tool-safety-level :safe)

;; Or moderate (default - auto-execute eval/compile)
(configure :max-tool-safety-level :moderate)
```

### Custom Tool Categories

```lisp
;; Get only introspection tools
(agent-q.tools:get-agent-q-tools :categories '(:introspection))

;; Get only execution tools
(agent-q.tools:get-agent-q-tools :categories '(:execution))
```

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

When fixing bugs:
1. Use describe_symbol to understand the function
2. Use eval_form to reproduce the bug
3. Use get_last_error to examine the backtrace
4. Propose fixes using propose_file_edit
5. Test fixes with eval_form
```

## Performance Notes

- Tool execution is fast (<50ms per tool call)
- Registry initialization: <100ms
- Diff generation depends on file size
- LLM calls are the main latency (1-3 seconds)
- Agent can iterate up to 10 times per conversation

## Next Steps

- Read [TESTING.md](TESTING.md) for comprehensive test documentation
- Check [IMPLEMENTATION-SUMMARY.md](IMPLEMENTATION-SUMMARY.md) for architecture details
- Explore `specs/` directory for future phases (Phase 3, 4)
- Try building custom tools using the tool protocol
- Experiment with different LLM providers

## Getting Help

- Check the Troubleshooting section above
- Review conversation buffer (`C-c q v`) for error messages
- Run the test suite to verify installation
- Check `*Messages*` buffer in Emacs for Elisp errors
- Test from Lisp REPL to isolate issues

## Quick Reference: All 18 Tools

### Introspection (9 tools - :safe)
1. `describe_symbol` - Get symbol documentation
2. `apropos_search` - Find symbols by pattern
3. `function_arglist` - Show function parameters
4. `who_calls` - Find function callers (SLYNK)
5. `who_references` - Find variable references (SLYNK)
6. `list_package_symbols` - List package exports
7. `class_slots` - Show CLOS class slots
8. `class_hierarchy` - Show class precedence
9. `macroexpand_form` - Expand macros

### Execution (4 tools - :moderate/:safe)
10. `eval_form` - Evaluate Lisp code (:moderate)
11. `compile_form` - Compile and load (:moderate)
12. `get_last_error` - Get error details (:safe)
13. `get_repl_history` - Show recent evaluations (:safe)

### Buffer/File (4 tools - :safe/:dangerous)
14. `read_file` - Read file contents (:safe)
15. `read_buffer` - Read buffer contents (:safe)
16. `search_in_buffer` - Search in buffer (:safe)
17. `write_file` - Write to file (:dangerous)

### Diff (1 tool - :moderate)
18. `propose_file_edit` - Show diff for user review (:moderate)

---

**You're all set!** Start using Agent-Q's powerful tool system to create an autonomous Lisp development partner. 🚀

The agent can now introspect, execute, and iterate - making it a true REPL-aware coding assistant!
