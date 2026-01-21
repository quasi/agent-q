# How-to: Understand Agent-Q's 18 Tools

Learn what Agent-Q can do by exploring its autonomous toolset.

## Problem

You want to know what Agent-Q can actually do—what information it can access, what code it can run, and how it makes changes.

## Solution

Agent-Q has **18 specialized tools** organized into 4 categories. You don't call these tools directly—the agent decides when to use them based on your requests. This guide explains what each tool does and when the agent uses it.

---

## Tool Categories Overview

| Category | Tools | Purpose | Safety |
|----------|-------|---------|--------|
| **Introspection** | 9 | Read-only inspection of Lisp environment | ✅ Safe |
| **Execution** | 4 | Run code in REPL, examine errors | ⚠️ Cautious |
| **Buffer/File** | 4 | Access files and buffers | Mixed |
| **Diff** | 1 | Propose code changes | ⚠️ Requires approval |

**Total: 18 tools**

---

## How Tools Work

### The Agent Decides

You don't call tools manually. When you ask a question or give an instruction, Agent-Q:

1. **Analyzes** your request
2. **Selects** appropriate tools
3. **Executes** the tools
4. **Interprets** results
5. **Responds** with an answer

### Tool Execution is Visible

Tool calls appear in the chat:

```
[TOOL: describe_symbol]
Args: "make-hash-table"
→ Function: MAKE-HASH-TABLE
  Arguments: (&key test size rehash-size rehash-threshold)
  Creates a new hash table...

Agent-Q:
Based on this, here's how to use it...
```

This transparency helps you understand the agent's reasoning.

### Safety Levels

Tools have safety ratings:

- **Safe**: Read-only, auto-executes
- **Cautious**: Execution logged, auto-executes
- **Dangerous**: Requires user approval

---

## Introspection Tools (9 Safe Tools)

These tools inspect your running Lisp environment without making changes.

### 1. describe_symbol

**What it does:** Gets detailed information about any Lisp symbol.

**When Agent-Q uses it:**
- You ask "What does X do?"
- You ask "How do I use X?"
- Agent needs to understand a function/macro/variable

**Example:**
```
You: What does MAPCAR do?

[TOOL: describe_symbol]
Args: "MAPCAR"
→ Function: MAPCAR
  Arguments: (function &rest lists)
  Documentation: Apply FUNCTION to successive elements of lists...

Agent-Q: MAPCAR applies a function to each element of one or more lists...
```

**Returns:** Function type, arguments, documentation, examples

---

### 2. apropos_search

**What it does:** Searches for symbols matching a pattern (fuzzy search across all loaded packages).

**When Agent-Q uses it:**
- You ask "Find hash table functions"
- Agent needs to discover related functionality
- You ask "What utilities does package X have?"

**Example:**
```
You: Find all hash table functions

[TOOL: apropos_search]
Args: "hash-table"
→ MAKE-HASH-TABLE (function)
  HASH-TABLE-P (function)
  GETHASH (function)
  REMHASH (function)
  ...

Agent-Q: Here are the main hash table functions...
```

**Returns:** List of matching symbols with types

---

### 3. function_arglist

**What it does:** Shows the parameters a function accepts.

**When Agent-Q uses it:**
- You ask about function arguments
- Agent needs to understand function signature
- Checking compatibility before calling

**Example:**
```
You: What arguments does MAPCAR need?

[TOOL: function_arglist]
Args: "MAPCAR"
→ (FUNCTION &REST LISTS)

Agent-Q: MAPCAR takes a function and one or more lists...
```

**Returns:** Lambda list with required, optional, keyword, and rest parameters

---

### 4. who_calls

**What it does:** Finds all functions that call a specified function (XREF cross-referencing).

**When Agent-Q uses it:**
- You ask "What calls this function?"
- Agent traces function usage
- Understanding code dependencies

**Example:**
```
You: What calls PROCESS-DATA?

[TOOL: who_calls]
Args: "PROCESS-DATA"
→ MAIN-LOOP (my-package.lisp:45)
  VALIDATE-INPUT (my-package.lisp:78)
  RUN-TESTS (test-suite.lisp:102)

Agent-Q: PROCESS-DATA is called by three functions...
```

**Returns:** List of callers with file locations

**Note:** Requires SLYNK/SWANK for XREF support. Degrades gracefully if unavailable.

---

### 5. who_references

**What it does:** Finds all code that references a variable (reads or writes it).

**When Agent-Q uses it:**
- You ask "Where is this variable used?"
- Agent traces variable dependencies
- Understanding state management

**Example:**
```
You: Where is *CONFIG* used?

[TOOL: who_references]
Args: "*CONFIG*"
→ INITIALIZE (config.lisp:12)
  GET-SETTING (config.lisp:34)
  RELOAD-CONFIG (config.lisp:56)

Agent-Q: *CONFIG* is referenced in three places...
```

**Returns:** List of references with file locations

---

### 6. list_package_symbols

**What it does:** Lists all exported symbols from a package with their types.

**When Agent-Q uses it:**
- You ask "What does package X provide?"
- Exploring library APIs
- Discovering available functions

**Example:**
```
You: What functions does Alexandria export?

[TOOL: list_package_symbols]
Args: "ALEXANDRIA"
→ ENSURE-LIST (function)
  FLATTEN (function)
  LAST-ELT (function)
  WHEN-LET (macro)
  ...

Agent-Q: Alexandria exports many utility functions, including...
```

**Returns:** List of symbols with types (function, macro, variable, class, etc.)

---

### 7. class_slots

**What it does:** Shows all slots (instance variables) defined in a CLOS class.

**When Agent-Q uses it:**
- You ask about class structure
- Understanding data models
- Creating instances correctly

**Example:**
```
You: What slots does the PERSON class have?

[TOOL: class_slots]
Args: "PERSON"
→ NAME (accessor: PERSON-NAME, initarg: :name)
  AGE (accessor: PERSON-AGE, initarg: :age)
  EMAIL (accessor: PERSON-EMAIL, initarg: :email, optional)

Agent-Q: The PERSON class has three slots: name, age, and email...
```

**Returns:** Slot names, accessors, initargs, types, default values

---

### 8. class_hierarchy

**What it does:** Shows superclasses and subclasses of a CLOS class.

**When Agent-Q uses it:**
- You ask about inheritance
- Understanding class relationships
- Designing class hierarchies

**Example:**
```
You: What's the hierarchy for STANDARD-OBJECT?

[TOOL: class_hierarchy]
Args: "STANDARD-OBJECT"
→ Superclasses: (T)
  Subclasses: (MY-BASE-CLASS, MY-MODEL, PERSON, ...)

Agent-Q: STANDARD-OBJECT is the base class for all CLOS objects...
```

**Returns:** Lists of superclasses and subclasses

---

### 9. macroexpand_form

**What it does:** Expands macros in a Lisp form to show what code they generate.

**When Agent-Q uses it:**
- You ask "What does this macro expand to?"
- Understanding macro behavior
- Debugging macro issues

**Example:**
```
You: What does (WHEN x (print x)) expand to?

[TOOL: macroexpand_form]
Args: "(WHEN x (print x))"
→ (IF X (PROGN (PRINT X)))

Agent-Q: WHEN is syntactic sugar for IF with PROGN...
```

**Returns:** Fully expanded form

---

## Execution Tools (4 Cautious/Safe Tools)

These tools run code and examine the REPL state.

### 10. eval_form

**What it does:** Evaluates a Lisp expression in your running image and returns the result.

**Safety:** ⚠️ Cautious (logged, auto-executes)

**When Agent-Q uses it:**
- Testing hypotheses ("Does this work?")
- Demonstrating examples
- Verifying fixes
- Reproducing bugs

**Example:**
```
You: Show me an example of MAKE-HASH-TABLE

[TOOL: eval_form]
Args: "(make-hash-table :test 'equal)"
→ #<HASH-TABLE :TEST EQUAL :COUNT 0 {1003A8F123}>

[TOOL: eval_form]
Args: "(setf (gethash \"key\" ht) \"value\")"
→ "value"

Agent-Q: Here's how to use hash tables...
```

**Returns:** Printed representation of the result or error message

**Important:** This executes code in your live image. Agent-Q typically uses benign examples, but be aware.

---

### 11. compile_form

**What it does:** Compiles and loads a Lisp form into your image.

**Safety:** ⚠️ Cautious (logged, auto-executes)

**When Agent-Q uses it:**
- Loading new function definitions
- Testing compiled code
- Verifying compilation warnings

**Example:**
```
You: Define a function to greet users

[TOOL: compile_form]
Args: "(defun greet (name) (format nil \"Hello, ~A!\" name))"
→ GREET

[TOOL: eval_form]
Args: "(greet \"Alice\")"
→ "Hello, Alice!"

Agent-Q: I've defined and tested the greet function...
```

**Returns:** Function name or compilation warnings

---

### 12. get_last_error

**What it does:** Retrieves the most recent error from your REPL history.

**Safety:** ✅ Safe (read-only)

**When Agent-Q uses it:**
- You report an error
- Debugging crashes
- Understanding backtraces

**Example:**
```
You: I got an error calling PROCESS-DATA

[TOOL: get_last_error]
Args: none
→ The value NIL is not of type LIST.
  Backtrace:
    0: (PROCESS-DATA NIL)
    1: (MAIN-LOOP)
    ...

Agent-Q: The error shows PROCESS-DATA received NIL. Let me add a check...
```

**Returns:** Error message, backtrace, restarts

---

### 13. get_repl_history

**What it does:** Gets recent evaluations from your REPL history.

**Safety:** ✅ Safe (read-only)

**When Agent-Q uses it:**
- Understanding what you've been doing
- Reproducing issues
- Continuing interrupted work

**Example:**
```
You: I tried something earlier but forgot what

[TOOL: get_repl_history]
Args: {"limit": 10}
→ (make-hash-table)
  (setf *ht* (make-hash-table))
  (gethash "key" *ht*)
  ...

Agent-Q: I see you were experimenting with hash tables...
```

**Returns:** List of recent evaluations

---

## Buffer/File Tools (4 Mixed Safety Tools)

These tools access files and Emacs buffers.

### 14. read_file

**What it does:** Reads file contents from disk.

**Safety:** ✅ Safe (read-only)

**When Agent-Q uses it:**
- You mention a file: "@src/utils.lisp"
- Agent needs current file contents
- Analyzing code structure

**Example:**
```
You: [@src/utils.lisp] Explain PROCESS-ITEMS

[TOOL: read_file]
Args: "src/utils.lisp"
→ (in-package :my-package)
  (defun process-items (items)
    (mapcar #'1+ items))

Agent-Q: This function increments each item. Let me suggest improvements...
```

**Returns:** File contents as text

---

### 15. read_buffer

**What it does:** Reads an Emacs buffer's current contents via SLY RPC.

**Safety:** ✅ Safe (read-only)

**When Agent-Q uses it:**
- You want to discuss code in an unsaved buffer
- Analyzing current editor state
- Getting latest changes

**Example:**
```
You: [@*scratch*] Explain this code

[TOOL: read_buffer]
Args: "*scratch*"
→ (defun test ()
    (print "testing"))

Agent-Q: This defines a simple test function that prints...
```

**Returns:** Buffer contents

---

### 16. write_file

**What it does:** Writes content to a file on disk.

**Safety:** ⚠️ **DANGEROUS** (requires approval)

**When Agent-Q uses it:**
- Creating new files
- Overwriting existing files
- Generating configuration files

**Example:**
```
You: Create a config file with default settings

[TOOL: write_file]
Args: {
  "path": "config/settings.lisp",
  "content": "(defparameter *debug* nil)\n(defparameter *port* 8080)"
}

[Approval prompt appears]
→ Agent-Q asks: "Write to config/settings.lisp?"
→ You approve: Yes

Agent-Q: Created config/settings.lisp with defaults.
```

**Returns:** Success or approval rejection

**Important:** You must approve file writes. Agent-Q won't write files without your permission.

---

### 17. search_in_buffer

**What it does:** Searches for a pattern in buffer contents.

**Safety:** ✅ Safe (read-only)

**When Agent-Q uses it:**
- Finding specific code
- Locating definitions
- Checking if something exists

**Example:**
```
You: Is there an error handler in utils.lisp?

[TOOL: search_in_buffer]
Args: {"buffer": "utils.lisp", "pattern": "error"}
→ Line 45: (handler-case
  Line 67: ; Handle errors gracefully
  Line 89: (error "Invalid input")

Agent-Q: Yes, there are error handlers at lines 45 and 89...
```

**Returns:** Matching lines with line numbers

---

## Diff Tool (1 Dangerous Tool)

### 18. propose_file_edit

**What it does:** Proposes changes to a file with a unified diff, shows for your review, and applies if accepted.

**Safety:** ⚠️ **DANGEROUS** (requires approval)

**When Agent-Q uses it:**
- Modifying existing code
- Refactoring
- Adding features
- Fixing bugs

**Example:**
```
You: [@src/utils.lisp] Add error handling to PROCESS-ITEMS

[TOOL: propose_file_edit]
Args: {
  "path": "src/utils.lisp",
  "old_content": "(defun process-items (items)\n  (mapcar #'1+ items))",
  "new_content": "(defun process-items (items)\n  (when (listp items)\n    (mapcar #'1+ items)))",
  "description": "Add listp check"
}

[Diff buffer opens showing changes]
→ You review hunk-by-hunk
→ Press 'a' to accept or 'r' to reject
→ Press 'q' when done

Agent-Q: Applied changes to utils.lisp
```

**Returns:** "accepted" or "rejected" with details

**Important:** See `diff-approval.md` for full review workflow.

---

## Safety System

### How Safety Works

Each tool has a **safety level**:

| Level | Behavior | Examples |
|-------|----------|----------|
| **Safe** | Auto-executes, no approval needed | describe_symbol, read_file |
| **Cautious** | Logs and executes, no approval needed | eval_form, compile_form |
| **Dangerous** | **Requires user approval** | write_file, propose_file_edit |

### Configuring Safety

Default: `:moderate` (execution tools auto-run, file writes need approval)

**Allow all tools without approval (risky!):**
```lisp
(agent-q:configure :max-tool-safety-level :dangerous)
```

**Restrict to read-only tools only:**
```lisp
(agent-q:configure :max-tool-safety-level :safe)
```

---

## Common Tool Combinations

Agent-Q often uses tools together:

### Learning a New API

```
1. apropos_search("hash-table")     → Find hash table functions
2. describe_symbol("make-hash-table") → Learn arguments
3. eval_form("(make-hash-table ...)")  → Test it
4. macroexpand_form("(with-hash-table-iterator ...)")  → Understand macros
```

### Debugging an Error

```
1. get_last_error()                 → Examine backtrace
2. read_file("src/utils.lisp")      → Read function definition
3. eval_form("(process-data nil)")  → Reproduce bug
4. propose_file_edit(...)           → Fix with error handling
```

### Refactoring Code

```
1. read_file("src/utils.lisp")      → Read current code
2. who_calls("old-function")        → Find all callers
3. class_hierarchy("base-class")    → Understand inheritance
4. propose_file_edit(...)           → Refactor with diff review
```

### Exploring a Package

```
1. list_package_symbols("alexandria") → See all exports
2. describe_symbol("when-let")       → Learn about specific function
3. macroexpand_form("(when-let ...)")  → See expansion
4. eval_form("(when-let ...)")       → Try example
```

---

## Tips for Working with Tools

### Trust the Agent's Tool Selection

Agent-Q knows when to use tools. You don't need to request specific tools—just ask naturally:

❌ **Don't say:** "Use describe_symbol on MAPCAR"
✅ **Do say:** "What does MAPCAR do?"

### Watch Tool Calls

Tool calls show the agent's reasoning. If you see unexpected tool usage, it might indicate:
- Misunderstood request
- Agent exploring possibilities
- Verification before answering

### Interrupt Long Tool Sequences

If Agent-Q uses many tools and you want it to stop:
- Wait for response to complete
- Clarify your request: "I just need a simple example"

### Approve Carefully

**Dangerous tools require approval** for good reason. Always review:
- `write_file`: Check path and contents
- `propose_file_edit`: Review each hunk in the diff

---

## Troubleshooting

### "Tool not available"

**Cause:** Tool requires a dependency (e.g., SLYNK for XREF tools)

**Fix:**
- Ensure SLY is connected: `M-x sly`
- Check if tool is loaded: `(agent-q.tools:list-registered-tools)`

### "Tool execution failed"

**Cause:** Error during tool execution (e.g., invalid arguments, file not found)

**Fix:**
Agent-Q usually recovers automatically. If it doesn't:
- Check the error message in the chat
- Clarify your request
- Try rephrasing

### "No approval prompt appeared"

**Cause:** Tool safety level allows auto-execution

**Verify:**
```lisp
(agent-q:get-tool-safety-level "write_file")
;; Should return :dangerous
```

**Fix:**
```lisp
(agent-q:configure :max-tool-safety-level :moderate)
```

### "XREF tools return empty"

**Cause:** `who_calls` and `who_references` require XREF database

**Fix:**
- Compile files first: `C-c C-k` in buffer
- XREF needs compiled definitions to track

---

## What's Not a Tool

**The agent can also:**
- **Reason**: Analyze, explain, suggest
- **Generate text**: Write documentation, explain concepts
- **Remember**: Use conversation history without tools

**Tools are for actions**, not reasoning.

---

## Next Steps

- **Diff Approval**: See `diff-approval.md` for reviewing proposed changes
- **Context**: See `context-completion.md` for attaching files to messages
- **Chat Interface**: See `chat-interface.md` for the full workflow
