# How-to: Attach Context with @-mentions

Quickly add files, symbols, and buffers to your chat without typing paths.

## Problem

You want to ask Agent-Q about specific code, but copying and pasting is slow. You need a way to reference code inline.

## Solution

Use `@` in the chat input to mention files, symbols, or buffers. They appear as visual pills and are automatically included with your message.

## Quickstart (2 minutes)

### 1. Open Chat Input

Press `C-c q s` to start a message.

### 2. Type @ and File Name

Start typing:
```
@src/
```

**Completion appears showing:**
- src/agent.lisp
- src/context.lisp
- src/tools/

### 3. Select Your File

Press Enter or click. You'll see:
```
[@src/agent.lisp]
```

The pill appears in your input. Your file is now attached.

### 4. Add Your Question

```
[@src/agent.lisp] Explain the process-instruction function
```

### 5. Send Message

Press `C-c RET`. Agent-Q receives your message **with the file contents included**.

That's it. The file is automatically sent with your message.

---

## Common Tasks

### Attach a File

```
Type: @src/filename.lisp
Select: Press RET or click completion
Result: [@src/filename.lisp] pill appears
```

### Attach a Symbol

```
Type: @make-hash-table
Select: The symbol from completion
Result: [@make-hash-table] appears
```

The agent will look up the symbol's documentation when you send the message.

### Attach a Buffer

```
Type: @*scratch*
Select: Press RET
Result: [@*scratch*] appears
```

Your buffer's current contents are included.

### Remove Context

Click the pill or place cursor on it and press `DEL`. The pill disappears and the context is removed.

---

## How @-mentions Work

### Trigger

After you type `@` and 2+ characters, completion appears automatically.

```
@s    ← Triggers after "s"
@src/ ← File completion shows src/agent.lisp, src/tools/, etc.
```

### Sources

Completion searches three places:

1. **Files** - Project files by name
2. **Symbols** - Lisp symbols (from describe_symbol)
3. **Buffers** - Open Emacs buffers

### Lazy Loading

Content isn't loaded until you send your message. This means:
- ✅ Fast pill creation
- ✅ Only files you actually use are loaded
- ✅ Large projects don't slow down completion

### Size Limit

Each item is limited to **50KB**. Large files are automatically truncated.

**Why?** Prevents LLM context overflow. Most source files are well under this limit.

---

## Using Pills Effectively

### Pills in Your Message

Pills stay visible while you compose:

```
[@src/agent.lisp] Explain the process-instruction function

[Click to visit source]
[DEL to remove]
```

### Viewing Attached Context

Press `C-c q c s` to see a summary:

```
Context: 2 items
  - src/agent.lisp (2.5 KB)
  - make-hash-table (1 KB)
```

### Context Panel

Press `C-c q v` to show the context panel on the right side. It displays:

- **Type:** File, symbol, or buffer
- **Name:** What was attached
- **Size:** How many KB
- **Actions:** Click to visit source or remove

---

## When to Use @-mentions

### ✅ Use @-mentions When

- Asking about specific code ("Explain this function")
- Debugging a file ("This crashes, fix it")
- Reviewing code ("Improve this algorithm")
- Learning a library ("Show me Alexandria examples")

### ❌ Skip @-mentions When

- Asking general questions ("What is CLOS?")
- The answer doesn't need your code
- You're starting a new unrelated topic

---

## Tips & Tricks

### Quick Symbol Lookup

Type `@describe` to attach describe's documentation:

```
@describe_symbol
```

The agent will look up what this tool does.

### Find Files Fast

Partial matching works:

```
@agent     ← Matches: src/agent.lisp, src/agent-q.asd, etc.
@test      ← Matches: test/agent-q-test.el, tests/tests.lisp, etc.
```

### Fuzzy Matching

Completion supports fuzzy search:

```
@art       ← Matches: src/part.lisp, cart/main.lisp (fuzzy)
@src/main  ← Matches: src/main.lisp, src/mainloop.lisp (prefix)
```

### Completion Frames

If you use Helm, Vertico, or Ivy, @-mentions work with all of them. Use your normal completion keybinds.

---

## Troubleshooting

### "No completions shown"

**Check:**
1. Did you type `@` followed by 2+ characters?
2. Is SLY connected? `M-x sly`
3. Are files in your project?

**Fix:**
```bash
# Restart SLY
M-x sly
```

### "Pill disappeared after sending"

**This is normal.** Pills only exist while composing. After you send, they disappear. The context was already sent to the agent.

### "File is huge - why truncated?"

**Files over 50KB are automatically truncated to the first 50KB** to prevent context overflow. If you need the full file:

1. Check the file size: `C-c q c s`
2. Consider splitting the file into smaller units
3. Ask the agent specifically about a function instead of the whole file

### "Can't find symbol"

Symbols must exist in your running Lisp image. If the symbol isn't recognized:

1. Load the file first: `C-c C-c` in the buffer
2. Try the file instead: `@filename.lisp`
3. Ask the agent directly: "Show me how to use make-hash-table"

---

## What Happens Behind the Scenes

When you send a message with pills:

1. **Pills are converted** to context items
2. **Content is fetched** (50KB limit applied)
3. **XML blocks are created** with the content
4. **Your message** receives a `<context>` block:

```
Your message: Explain the process-instruction function

<context type="file" path="src/agent.lisp">
(in-package :agent-q)
(defun process-instruction ...)
</context>
```

5. **Agent-Q receives both** your message and the context

The agent can then reference "the process-instruction function" directly because it has the code.

---

## Advanced: Managing Large Context

### Context Window (50 items)

Agent-Q keeps a **sliding window of 50 context items**. When full, the oldest item is removed automatically.

**View current context:**
```
C-c q c s
```

**Clear all context:**
```
C-c q c c
```

### When to Clear

- Context exceeds 40 items (getting too much)
- Switching to unrelated work
- Previous context is making responses noisy

---

## Next Steps

- **Learn Context Management**: See `context-management.md` for other ways to add context
- **Explore Chat Interface**: See `chat-interface.md` for message sending
- **Review Sessions**: See `session-management.md` for persistent conversations
