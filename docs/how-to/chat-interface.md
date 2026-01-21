# How-to: Send Messages and Use the Chat Interface

Learn to communicate with Agent-Q through the interactive chat interface.

## Problem

You need to ask questions, give instructions, and see Agent-Q's responses in a clean, organized interface.

## Solution

Agent-Q provides a dual-region chat buffer: conversation history above (read-only), input below (editable). Messages stream in real-time with full markdown rendering.

---

## Quickstart (1 minute)

### 1. Open Chat

```
M-x sly-agent-q-chat
```

Or press `C-c q v` if already in an Agent-Q session.

### 2. Type Your Message

The cursor is in the input region at the bottom:

```
Input> How do I create a hash table in Common Lisp?
```

### 3. Send

Press `C-c RET` (or `C-c C-c`).

Your message appears in the conversation area, followed by Agent-Q's streaming response:

```
User:
How do I create a hash table in Common Lisp?

Agent-Q:
You can create a hash table with `make-hash-table`. Here's an example...
```

That's it. The interface handles the rest.

---

## Understanding the Layout

### The Two Regions

```
┌─────────────────────────────────────────┐
│ Header: Model | Session | Cost          │
├─────────────────────────────────────────┤
│                                         │
│ Conversation History                    │
│ (scrollable, read-only)                 │
│                                         │
│ User: Hello                             │
│ Agent-Q: Hi! How can I help?            │
│                                         │
├─────────────────────────────────────────┤
│ Input> [Type message here...]           │
└─────────────────────────────────────────┘
```

**Conversation Region (Top)**
- Read-only
- Shows all messages
- Scrolls automatically as conversation grows
- Markdown-rendered (bold, italic, code blocks)

**Input Region (Bottom)**
- Editable
- Where you type
- Input history available (up/down arrows)
- Auto-clears after sending

### The Header

Shows current session metadata:

```
claude-sonnet-4 | Session: feature-work | Cost: $0.23
```

- **Model**: Which LLM is active
- **Session**: Current session name
- **Cost**: Accumulated cost (if available)

---

## Sending Messages

### Basic Send

**Keyboard:**
```
C-c RET    Send message
C-c C-c    Alternative send
```

**What happens:**
1. Your message is added to conversation
2. Input clears automatically
3. Agent-Q's response streams in real-time
4. Conversation auto-scrolls to bottom

### Send with Context

Include context items (files, symbols) in your message:

```
C-c q S    Send with all accumulated context
```

Context appears as pills in your input:
```
Input> [@src/agent.lisp] Explain process-instruction
```

See `context-completion.md` for details on @-mentions.

### Send Region with Instruction

Select code and send with a prompt:

```
1. Select code (mark region with C-SPC)
2. Press C-c q r
3. Type instruction: "Refactor this to use CLOS"
4. Press RET
```

The selected code is automatically added as context.

---

## Managing Input

### Clear Input

**Keyboard:**
```
C-c C-k    Clear input region
```

Useful when you change your mind mid-message.

### Input History

Navigate previously sent messages:

```
M-p        Previous message (up)
M-n        Next message (down)
```

**Example:**
```
Input> How do I make a hash table?    [C-c RET - sent]
Input>                                [Empty after send]
[M-p]
Input> How do I make a hash table?    [Previous message restored]
```

Duplicate messages aren't added to history.

### Multi-line Input

Press `RET` (Enter) to add newlines:

```
Input> Explain how this works:

(defun process-data (items)
  (mapcar #'1+ items))
```

Then `C-c RET` to send.

---

## Reading Responses

### Streaming Display

Responses appear **token-by-token** in real-time:

```
Agent-Q:
You can create a hash table with `make-hash█
```

The cursor (`█`) shows where tokens are being added.

**Why streaming?**
- ✅ Faster perceived response time
- ✅ See progress on long answers
- ✅ Can interrupt if needed

### Markdown Rendering

Agent-Q's responses use markdown:

**Bold and Italic:**
```
This is **important** and this is *emphasized*.
```

**Inline Code:**
```
Use `make-hash-table` to create a hash table.
```

**Code Blocks:**
````
```lisp
(defun greet (name)
  (format nil "Hello, ~A!" name))
```
````

Syntax highlighting is automatic (via markdown-mode).

**Links:**
```
See [the documentation](https://example.com/docs).
```

### Tool Call Display

When Agent-Q uses tools, you see them inline:

```
Agent-Q:
Let me check what that function does.

[TOOL: describe_symbol]
Args: "make-hash-table"
→ Function: MAKE-HASH-TABLE
  Arguments: (&key test size rehash-size rehash-threshold)
  Creates a new hash table...

Based on this, here's how to use it...
```

This shows the agent's reasoning process.

---

## Session Management

### Current Session

The header shows which session is active:

```
claude-sonnet-4 | Session: debugging-auth | Cost: $0.15
```

### Switch Session

```
M-x sly-agent-q-switch-session
```

Select from existing sessions. The conversation history loads automatically.

### New Session

```
C-c q n    Create new session
```

Prompts for a session name. Starts fresh conversation.

### Rename Session

```
M-x sly-agent-q-rename-session
```

Updates the session name (shown in header).

See `session-management.md` for full session details.

---

## Common Tasks

### Ask a Question

```
Input> What does MAPCAR do?
[C-c RET]
```

Agent-Q responds with explanation and examples.

### Give an Instruction

```
Input> [@src/utils.lisp] Add error handling to process-items
[C-c RET]
```

Agent-Q proposes changes via diff.

### Request Code

```
Input> Write a function that counts words in a string
[C-c RET]
```

Agent-Q generates code. Use `C-c q i` to insert at cursor.

### Debug an Error

```
Input> I got this error: "The value NIL is not of type LIST"
[C-c RET]
```

Agent-Q uses `get_last_error` to examine the backtrace and suggest fixes.

### Learn a Library

```
Input> Show me how to use Alexandria's when-let macro
[C-c RET]
```

Agent-Q uses `describe_symbol` and `macroexpand_form` to explain.

---

## Customization

### Disable Markdown Rendering

```elisp
(setq sly-agent-q-chat-markdown-rendering nil)
```

Messages display as plain text (faster for very long conversations).

### Change Buffer Name

```elisp
(setq sly-agent-q-conversation-buffer-name "*My Agent Chat*")
```

### Change Input Send Key

```elisp
;; Use C-c C-s instead of C-c RET
(define-key sly-agent-q-chat-mode-map (kbd "C-c C-s")
  'sly-agent-q-send-message)
```

### Adjust Cost Display

Cost appears in header when available. If you don't want to see it:

```elisp
(setq sly-agent-q-show-cost nil)
```

---

## Keyboard Reference

### Sending

| Key | Action |
|-----|--------|
| `C-c RET` | Send message |
| `C-c C-c` | Send message (alternative) |
| `C-c q S` | Send with context |
| `C-c q r` | Send region with instruction |

### Input Management

| Key | Action |
|-----|--------|
| `C-c C-k` | Clear input |
| `M-p` | Previous input (history) |
| `M-n` | Next input (history) |
| `RET` | New line |

### Navigation

| Key | Action |
|-----|--------|
| `C-c q v` | View/switch to chat buffer |
| `C-c q n` | New session |
| `q` | Hide chat buffer (from conversation) |

### Response Actions

| Key | Action |
|-----|--------|
| `C-c q i` | Insert last response at cursor |
| `C-c q w` | Copy last response to kill ring |

---

## Troubleshooting

### "Nothing happens when I press C-c RET"

**Check:**
1. Is SLY connected? `M-x sly`
2. Is cursor in input region (bottom)?
3. Is input non-empty?

**Fix:**
```
M-x sly    Reconnect
```

### "Response takes forever"

Agent-Q uses Claude or GPT-4, which can take 5-30 seconds for complex responses.

**If it's stuck:**
1. Check `*Messages*` buffer: `C-h e`
2. Verify internet connection
3. Check API key: `echo $ANTHROPIC_API_KEY`
4. Try simple message: "Hello"

### "Markdown looks wrong"

**Ensure markdown-mode is installed:**
```elisp
M-x package-install RET markdown-mode RET
```

**Or disable markdown:**
```elisp
(setq sly-agent-q-chat-markdown-rendering nil)
```

### "Input region disappeared"

The layout can break if you manually edit regions. **Fix:**

```
M-x sly-agent-q-chat    Reinitialize buffer
```

### "Cost shows N/A"

Cost estimation isn't always available (depends on LLM provider and configuration). This is normal. See `cost-estimation.md` for details.

---

## Tips

### Efficient Workflow

```
1. Ask question                 [C-c RET]
2. Review response
3. Follow-up question           [M-p to recall, edit, C-c RET]
4. Iterate until satisfied
```

### Context Accumulation

Add context incrementally:

```
1. [@file1.lisp] How does this work?
2. [Follow-up] Now add error handling
3. [Follow-up] Add tests
```

Agent-Q remembers the conversation context.

### Quick Documentation

```
Input> @make-hash-table
[C-c RET]
```

The agent describes the symbol automatically when you only send a @-mention.

### Save Good Responses

```
C-c q w    Copy response to kill ring
C-y        Paste into your buffer
```

---

## What's Happening Behind the Scenes

### Message Flow

```
1. You type in input region
2. Press C-c RET
3. Message sent to Common Lisp (RPC)
4. CL calls LLM API
5. Response streams back (SSE)
6. Tokens rendered in conversation region
7. Markdown applied after completion
```

### Streaming Protocol

Agent-Q uses Server-Sent Events (SSE) for real-time streaming:

```
1. Start: Create placeholder
2. Chunk: Append tokens one-by-one
3. Finalize: Apply markdown, update cost
4. Error: Display error, allow retry
```

See `streaming.md` for technical details.

---

## Next Steps

- **Add Context**: See `context-completion.md` for @-mentions
- **Review Code Changes**: See `diff-approval.md` for hunk-by-hunk review
- **Persistent Conversations**: See `session-management.md`
- **Explore Tools**: See `tool-system.md` for what Agent-Q can do
