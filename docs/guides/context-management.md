# Context Management Guide

**Agent-Q Chat Context Management** - Attach files, symbols, and buffers to your conversations using @-mentions.

---

## What is Context Management?

Context management lets you attach relevant code, files, and documentation to your Agent-Q conversations. Instead of copying and pasting code snippets, you can use inline @-mentions to reference context items directly in your messages.

When you type `@` in the chat input, Agent-Q provides smart completion for:
- **Files** in your project
- **Lisp symbols** (functions, macros, variables)
- **Open buffers** in Emacs

Selected items appear as visual pills `[@name]` and their content is automatically included when you send your message to the LLM.

---

## Quick Start

### Your First @-Mention

1. **Open Agent-Q chat** (`M-x sly-agent-q-chat` or `C-c q v`)

2. **Type a message with @-mention**:
   ```
   > Can you help optimize @fibonacci?
   ```

3. **Press TAB** after typing `@fib`:
   - Completion menu appears showing matching symbols
   - Select `fibonacci` from the list
   - Press RET or TAB to insert

4. **Result**: The message now shows:
   ```
   > Can you help optimize [@fibonacci]?
   ```

5. **Send the message** (C-c C-c or RET):
   - The LLM receives your message with the full source code of `fibonacci`
   - The function's context is automatically included

---

## Completion Types

### File Completion

Attach files from your project by typing `@` followed by a path:

```
> Please review @src/agent.lisp
```

**How it works:**
- Uses `file-name-all-completions` to find files in your project
- Supports relative paths and fuzzy matching
- Shows file paths in completion menu

**Example completions:**
```
@src/       → @src/agent.lisp, @src/config.lisp, @src/context.lisp
@README     → @README.md
@test/      → @test/run.el, @test/test-helper.el
```

### Symbol Completion

Attach Lisp symbols (functions, macros, variables) by typing `@` followed by the symbol name:

```
> How does @agent-q-send work?
```

**How it works:**
- Uses `apropos` to find symbols in the running Lisp image
- Searches for functions, macros, and variables
- Shows symbol type in completion annotation

**Example completions:**
```
@defun fib     → @fibonacci (function)
@agent-q       → @agent-q-send (function)
@*default      → @*default-provider* (variable)
```

### Buffer Completion

Attach content from open Emacs buffers:

```
> Compare @*scratch* with @my-notes.org
```

**How it works:**
- Lists all open buffers in Emacs
- Includes special buffers like `*scratch*`, `*messages*`
- Shows buffer names in completion menu

**Example completions:**
```
@buffer *mes   → @*messages*, @*compilation*
@buffer test   → @test.lisp, @test-helper.el
```

---

## Visual Pills

When you complete an @-mention, it creates a visual pill `[@name]` in your message:

### Pill Appearance
- **Bold blue text** with rounded background
- **Type indicator** in annotation (file/symbol/buffer)
- **Hover tooltip** shows first few lines of content

### Pill Actions

**Visit source** (RET):
- Files: Opens the file
- Symbols: Jumps to definition (if available)
- Buffers: Switches to the buffer

**Remove from context** (DEL):
- Removes the pill and context item
- Frees up context space
- Can't be undone (retype @-mention to re-add)

**Example interaction:**
```
Message: Please optimize [@fibonacci]

Move cursor to pill:
- Press RET → jumps to fibonacci definition
- Press DEL → removes [@fibonacci] from message and context
```

---

## Context Panel

The context panel is a sidebar that shows all attached context items.

### Opening the Panel

**Keybinding**: `C-c @` (in chat buffer)

**Menu**: `M-x agent-q-toggle-context-panel`

### Panel Layout

```
┌─ Context ───────────────────┐
│ 📄 file: src/agent.lisp     │
│    12.3 KB                  │
│    [visit] [remove]         │
│                             │
│ 🔧 symbol: fibonacci        │
│    234 bytes                │
│    [visit] [remove]         │
│                             │
│ 📋 buffer: *scratch*        │
│    1.5 KB                   │
│    [visit] [remove]         │
│                             │
│ Total: 14.0 KB / 50 KB     │
└─────────────────────────────┘
```

### Panel Actions

**RET on item**: Visit the source
**d on item**: Remove from context
**q**: Close the panel
**g**: Refresh panel (updates sizes)

---

## Content Limits

Each context item is limited to **50 KB** to prevent overwhelming the LLM.

### What happens with large files?

**Files < 50 KB**:
- Full content included
- No truncation

**Files > 50 KB**:
- Content truncated to 50 KB
- Message appended: `[Content truncated: actual size 127KB]`
- LLM is notified of truncation

**Example**:
```lisp
;; Large file (127 KB)
(defun big-function ()
  ...)

;; [Content truncated: actual size 127KB]
;; File was too large for full inclusion
```

### Best Practices

✅ **Do**:
- Attach specific functions rather than entire files
- Use symbol completion for focused context
- Review context panel to check total size

❌ **Avoid**:
- Attaching multiple large files
- Including generated or binary files
- Exceeding the total 50 KB limit per conversation

---

## Keyboard Shortcuts

| Key | Action | Context |
|-----|--------|---------|
| `@` | Start @-mention completion | Chat input |
| `TAB` | Complete @-mention | During @-mention |
| `RET` | Visit pill source | Cursor on pill |
| `DEL` | Remove pill from context | Cursor on pill |
| `C-c @` | Toggle context panel | Chat buffer |
| `C-c C-x` | Clear all context | Chat buffer |
| `d` | Remove item | Context panel |
| `g` | Refresh panel | Context panel |
| `q` | Close panel | Context panel |

---

## Common Workflows

### Code Review

1. Open Agent-Q chat
2. Attach the file: `@src/new-feature.lisp`
3. Ask: "Please review this code for bugs and style issues"
4. LLM receives full file content and provides detailed review

### Function Optimization

1. Find the slow function in your profiler
2. Attach the symbol: `@slow-function`
3. Ask: "Can you optimize this for performance?"
4. LLM sees the implementation and suggests improvements

### Comparing Implementations

1. Attach multiple files:
   ```
   > Compare @src/old-parser.lisp with @src/new-parser.lisp
   ```
2. Ask: "Which implementation is better and why?"
3. LLM analyzes both files and provides comparison

### Bug Fixing

1. Copy error message from `*messages*` buffer
2. Attach the failing function: `@broken-function`
3. Ask: "This function throws error: [paste error]. How do I fix it?"
4. LLM sees the code and error, suggests fix

### Documentation Generation

1. Attach a function: `@undocumented-function`
2. Ask: "Generate a docstring for this function"
3. LLM analyzes the code and writes appropriate documentation

---

## Troubleshooting

### @-completion doesn't trigger

**Symptoms**: Typing `@` doesn't show completion menu

**Solutions**:
1. Check you're in the chat input region (after `> ` prompt)
2. Verify `agent-q-context-setup` was called (should be automatic)
3. Try `M-x completion-at-point` manually to test
4. Check `*Messages*` buffer for errors

### No file candidates shown

**Symptoms**: `@` completion shows no files

**Solutions**:
1. Verify you're in a project directory
2. Check `default-directory` is set correctly
3. Try absolute paths: `@/full/path/to/file`
4. Files in `.gitignore` are still shown (no filtering)

### Symbol not found

**Symptoms**: `@defun foo` shows no completions

**Solutions**:
1. Ensure Lisp image is connected via SLY
2. Verify the symbol is defined (try `M-. symbol-name`)
3. Package may not be loaded yet (use `(ql:quickload "system")`)
4. Symbol might be internal (try with package prefix: `@package::symbol`)

### Pills don't appear

**Symptoms**: Completion succeeds but no `[@name]` pill shown

**Solutions**:
1. Check if `agent-q--context-exit-function` is registered
2. Verify `agent-q-context-complete-at-point` includes `:exit-function`
3. Look for errors in `*Messages*` buffer
4. Try restarting Agent-Q mode

### Content not sent to LLM

**Symptoms**: Pills appear but LLM doesn't seem to see the context

**Solutions**:
1. Toggle context panel (`C-c @`) to verify items are added
2. Check `agent-q-context-items` buffer-local variable
3. Verify `agent-q--format-context-for-llm` is called in `agent-q--send-to-agent`
4. Look for `<context>` block in outgoing messages (debug mode)

### Context panel empty

**Symptoms**: Panel opens but shows no items

**Solutions**:
1. Verify you've completed at least one @-mention
2. Check `agent-q-context-items` is not nil
3. Try `g` to refresh the panel
4. Ensure you're viewing the correct chat buffer's panel

---

## Advanced Usage

### Custom Candidate Sources

You can extend the completion system with custom candidate sources by modifying `agent-q--context-candidates`:

```elisp
(defun agent-q--context-candidates (prefix)
  "Return completion candidates for PREFIX."
  (append
   (agent-q--file-candidates prefix)
   (agent-q--symbol-candidates prefix)
   (agent-q--buffer-candidates prefix)
   (agent-q--custom-candidates prefix)))  ; Your custom source

(defun agent-q--custom-candidates (prefix)
  "Custom candidates from your source."
  (mapcar (lambda (item)
            (propertize item
                        'agent-q-context-type :custom
                        'agent-q-context-data (list :name item)))
          (my-custom-completion-source prefix)))
```

### Programmatic Context Addition

Add context items without using @-mentions:

```elisp
;; Add a file
(push (make-agent-q-context-item
       :type :file
       :display-name "config.lisp"
       :data (list :path "/path/to/config.lisp")
       :content (with-temp-buffer
                  (insert-file-contents "/path/to/config.lisp")
                  (buffer-string)))
      agent-q-context-items)

;; Add a symbol
(push (make-agent-q-context-item
       :type :symbol
       :display-name "my-function"
       :data (list :name "my-function" :package "my-package")
       :content (documentation 'my-function 'function))
      agent-q-context-items)

;; Refresh panel
(agent-q-context-refresh-panel)
```

### Custom Content Fetching

Override content fetching for special item types:

```elisp
(cl-defmethod agent-q--fetch-context-content ((type (eql :custom)) data)
  "Fetch content for custom context type."
  (let ((name (plist-get data :name)))
    (my-custom-content-fetcher name)))
```

---

## Performance Tips

### Minimize Context Size

- Attach specific functions rather than entire files
- Use symbol completion when possible (smaller than files)
- Remove unused context items with `DEL` on pills

### Lazy Loading

Content is fetched lazily when you send the message, not when you complete the @-mention. This means:
- Completion is fast (only searches for names)
- Content reading only happens once
- You can attach many items without slowdown

### Context Reuse

Context persists across multiple messages in the same conversation:
- Add context once, send multiple messages
- LLM builds understanding incrementally
- Clear context (`C-c C-x`) when switching topics

---

## Migration from Old Context System

If you were using the old context management commands (`C-c q c r`, `C-c q c d`), here's how to transition:

### Old Way
```elisp
;; Mark region
C-c q c r

;; Mark defun
C-c q c d

;; Send with context
C-c q S
```

### New Way (Recommended)
```
;; In chat input, use @-mentions directly:
> Please review @my-function

;; Or for files:
> Optimize @src/slow-module.lisp
```

### Benefits of New System
- Visual feedback (pills show what's included)
- Inline in message (context is part of conversation flow)
- Persistent across messages (no need to re-add context)
- Manageable (context panel shows everything)

**Note**: Old commands still work and can coexist with @-mentions.

---

## Next Steps

- **Try it**: Open chat and type `@` followed by a filename
- **Experiment**: Attach multiple items and see how LLM uses them
- **Optimize**: Use context panel to monitor total size
- **Share**: Show teammates the @-mention workflow

For more information:
- See `README.md` for installation and setup
- Read `CLAUDE.md` for architecture details
- Check `specs/plans/chat-phase-4-context.md` for implementation plan
