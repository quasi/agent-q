# Testing the Agent-Q Chat Interface

Manual testing guide for the interactive chat interface (Phase 1 Foundation).

**Prerequisites:** Emacs 27.1+, SLY, Agent-Q loaded in a running Lisp image.

---

## Quick Verification

Run these checks to confirm basic functionality:

```elisp
;; In Emacs *scratch* buffer or M-:
(require 'sly-agent-q-chat)  ; Should load without error
(agent-q-chat)               ; Should open *Agent-Q Chat* buffer
```

Expected: A buffer opens with "Agent-Q Chat" header, a separator line, and an input prompt `> `.

---

## Test Scenarios

### 1. Buffer Layout

**Goal:** Verify the buffer has correct structure with output region, separator, and input region.

**Steps:**
1. Run `M-x agent-q-chat`
2. Observe the buffer layout

**Expected Result:**
```
Agent-Q Chat
──────────────────────────────────────────────────


──────────────────────────────────────────────────
                       Input
──────────────────────────────────────────────────
> █
```

**Checkpoints:**
- [ ] Header "Agent-Q Chat" is bold and larger
- [ ] Separator lines use box-drawing characters (─)
- [ ] "Input" label appears centered on its own line between two separator lines
- [ ] Cursor is positioned after `> ` prompt
- [ ] Prompt `> ` is purple/magenta colored
- [ ] Can type text after the prompt (self-insert works)

---

### 2. Output Region Read-Only

**Goal:** Verify the output region cannot be edited.

**Steps:**
1. Open chat buffer with `M-x agent-q-chat`
2. Move cursor to the header area (above the "Input" separator)
3. Try to type text

**Expected Result:**
- Emacs displays "Text is read-only" in the minibuffer
- No text is inserted

**Checkpoint:**
- [ ] Cannot type in output region

---

### 3. Input Region Editable

**Goal:** Verify the input region accepts text.

**Steps:**
1. Open chat buffer with `M-x agent-q-chat`
2. Cursor should be in input region (after `> `)
3. Type: `Hello, Agent-Q!`

**Expected Result:**
```
> Hello, Agent-Q!█
```

**Checkpoint:**
- [ ] Can type freely in input region

---

### 4. Multi-Line Input

**Goal:** Verify multi-line input works correctly.

**Steps:**
1. Open chat buffer
2. Type: `First line`
3. Press `RET` (while cursor is NOT at end of buffer, or input is empty)
4. Type: `Second line`

**Expected Result:**
```
> First line
> Second line█
```

**Checkpoints:**
- [ ] RET inserts newline when in middle of input
- [ ] Each line starts with `> ` prompt
- [ ] Can continue typing on new line

---

### 5. Send Message with RET

**Goal:** Verify RET sends message when at end of buffer with non-empty input.

**Prerequisites:** SLY connected to a Lisp with Agent-Q loaded.

**Steps:**
1. Open chat buffer
2. Type: `What is 2 + 2?`
3. Press `RET` (cursor must be at end of buffer)

**Expected Result:**
- User message appears in output region with [USER] header and timestamp
- Input region is cleared
- [AGENT-Q] header appears for response
- Response text streams in

**Checkpoints:**
- [ ] User message shows: `[USER] HH:MM:SS`
- [ ] Input clears after send
- [ ] Response shows: `[AGENT-Q] HH:MM:SS`

---

### 6. Send Message with C-c C-c

**Goal:** Verify C-c C-c always sends, regardless of cursor position.

**Steps:**
1. Open chat buffer
2. Type: `Test message`
3. Move cursor to middle of text
4. Press `C-c C-c`

**Expected Result:**
- Message is sent even though cursor wasn't at end
- Same behavior as RET-at-end

**Checkpoint:**
- [ ] C-c C-c sends from any cursor position

---

### 7. Empty Input Rejection

**Goal:** Verify empty input cannot be sent.

**Steps:**
1. Open chat buffer
2. Without typing anything, press `C-c C-c`

**Expected Result:**
- Minibuffer shows: "Input is empty"
- Nothing is sent

**Checkpoint:**
- [ ] Empty input shows error message

---

### 8. Input History Navigation (M-p / M-n)

**Goal:** Verify history navigation works like shell-mode.

**Steps:**
1. Open chat buffer
2. Send message: `First message` (C-c C-c)
3. Wait for response
4. Send message: `Second message` (C-c C-c)
5. Wait for response
6. Type: `Draft text` (don't send)
7. Press `M-p` (previous history)
8. Press `M-p` again
9. Press `M-n` (next history)
10. Press `M-n` again

**Expected Result:**
| Step | Input Shows |
|------|-------------|
| After step 6 | `> Draft text` |
| After step 7 (M-p) | `> Second message` |
| After step 8 (M-p) | `> First message` |
| After step 9 (M-n) | `> Second message` |
| After step 10 (M-n) | `> Draft text` |

**Checkpoints:**
- [ ] M-p navigates to previous inputs
- [ ] M-n navigates to next inputs
- [ ] Draft is preserved when navigating
- [ ] Draft is restored when returning from history

---

### 9. History with No Previous Input

**Goal:** Verify graceful handling when no history exists.

**Steps:**
1. Open a fresh chat buffer (or use `C-c C-l` to clear)
2. Press `M-p`

**Expected Result:**
- Minibuffer shows: "No history"

**Checkpoint:**
- [ ] "No history" message shown

---

### 10. Streaming Response Display

**Goal:** Verify responses stream in incrementally (not all at once).

**Prerequisites:** Agent-Q with streaming enabled on the Lisp side.

**Steps:**
1. Open chat buffer
2. Send a message that requires a long response: `Explain the Common Lisp condition system in detail.`
3. Watch the response area

**Expected Result:**
- Text appears incrementally as it's received
- Response builds up character by character or chunk by chunk
- Final response is complete

**Checkpoint:**
- [ ] Response streams incrementally (visible typing effect)

---

### 11. Debug/Tool Messages

**Goal:** Verify tool execution messages appear during agent iteration.

**Prerequisites:** Agent-Q with tools enabled.

**Steps:**
1. Open chat buffer
2. Send: `Describe the function MAPCAR`
3. Watch for tool execution indicators

**Expected Result:**
- Tool messages appear in gray/italic text
- Format: `[TOOL: tool_name] executing...` or similar
- Final response follows tool execution

**Checkpoint:**
- [ ] Tool execution messages appear
- [ ] Messages are visually distinct (gray/italic)

---

### 12. Clear Conversation (C-c C-l)

**Goal:** Verify conversation clearing works.

**Steps:**
1. Open chat buffer with some messages
2. Press `C-c C-l`
3. Confirm when prompted

**Expected Result:**
- Prompt: "Clear conversation? (yes or no)"
- If yes: Buffer resets to initial state
- If no: Nothing changes

**Checkpoints:**
- [ ] Confirmation prompt appears
- [ ] "yes" clears everything
- [ ] "no" preserves conversation

---

### 13. Quit Window (q)

**Goal:** Verify `q` quits in output region but types in input region.

**Steps:**
1. Open chat buffer
2. Move cursor to output region (above Input separator)
3. Press `q`
4. Reopen chat buffer
5. Move cursor to input region
6. Press `q`

**Expected Result:**
- Step 3: Window closes (like `quit-window`)
- Step 6: Letter "q" is inserted

**Checkpoints:**
- [ ] `q` in output region closes window
- [ ] `q` in input region inserts "q"

---

### 14. Scroll to Bottom (C-c C-o)

**Goal:** Verify scroll-to-bottom works.

**Steps:**
1. Open chat buffer with many messages (enough to scroll)
2. Scroll up to view older messages
3. Press `C-c C-o`

**Expected Result:**
- Buffer scrolls to bottom
- Cursor moves to end

**Checkpoint:**
- [ ] C-c C-o scrolls to bottom

---

### 15. Pending Response Blocking

**Goal:** Verify cannot send while waiting for response.

**Steps:**
1. Open chat buffer
2. Send a message
3. Immediately try to send another message (before response arrives)

**Expected Result:**
- Minibuffer shows: "Already waiting for response"
- Second message is not sent

**Checkpoint:**
- [ ] Cannot send while response pending

---

### 16. Cancel Request (C-c C-k)

**Goal:** Verify request cancellation.

**Steps:**
1. Open chat buffer
2. Send a message
3. Before response completes, press `C-c C-k`

**Expected Result:**
- Minibuffer shows: "Request cancelled"
- Can send new messages

**Checkpoints:**
- [ ] C-c C-k cancels pending request
- [ ] Can send new messages after cancel

---

### 17. Diff Tool Integration

**Goal:** Verify diff workflow integrates with chat buffer.

**Prerequisites:** Agent-Q Phase 2 diff tool functional.

**Steps:**
1. Open chat buffer
2. Request a file edit: `Add a docstring to the function foo in /tmp/test.lisp`
3. Observe the chat buffer before diff opens
4. When diff buffer appears, review and accept/reject with `a` or `r`
5. Return to chat buffer and observe

**Expected Result:**
- Chat shows: `[TOOL: propose_file_edit] reviewing changes to test.lisp...`
- Diff buffer (*Agent-Q Diff*) opens
- Per-hunk review works (a=accept, r=reject, n=next, p=prev, q=finish)
- After decision, chat shows: `[TOOL: propose_file_edit] → changes applied` (or rejected)
- Agent continues with response

**Checkpoints:**
- [ ] Chat shows tool start notification
- [ ] Diff buffer opens with per-hunk controls
- [ ] `a` key accepts current hunk and moves to next
- [ ] `r` key rejects current hunk and moves to next
- [ ] `q` key finishes review
- [ ] Chat shows tool result after diff closes
- [ ] Cursor is NOT in header when pressing a/r (moves to first hunk automatically)

---

### 18. Backward Compatibility

**Goal:** Verify old commands still work.

**Steps:**
1. Run `M-x sly-agent-q-show-conversation`

**Expected Result:**
- Opens the new chat buffer (not old *Agent-Q* buffer)

**Checkpoint:**
- [ ] Old command opens new chat interface

---

### 19. Insert Generated Code (C-c q i)

**Goal:** Verify code generated by Agent-Q can be inserted into a buffer.

**Prerequisites:** SLY connected, code buffer open.

**Steps:**
1. Open a `.lisp` file and position cursor where you want code
2. Open chat buffer with `M-x agent-q-chat`
3. Ask: `Write a function that converts a string to morse code`
4. Wait for response
5. Switch to your `.lisp` buffer
6. Press `C-c q i` to insert

**Expected Result:**
- The agent's response (including the morse code function) is inserted at point
- Code is inserted as plain text (no special formatting)

**Checkpoints:**
- [ ] Response appears in chat buffer
- [ ] `C-c q i` inserts the response at point in the Lisp buffer
- [ ] Can also use `C-c q w` to copy to kill ring instead

**Note:** The inserted text is the full response. If the agent includes explanatory text along with code, all of it is inserted. For code-only insertion, use the copy command (`C-c q w`) and manually select from the kill ring, or ask the agent to respond with code only.

---

## Keybinding Reference

| Key | Function | Context |
|-----|----------|---------|
| `RET` | Send or newline | Send if at end with input; else newline |
| `C-c C-c` | Send | Always sends |
| `C-c C-k` | Cancel | Cancels pending request |
| `M-p` | History previous | Navigate to older input |
| `M-n` | History next | Navigate to newer input |
| `C-c C-l` | Clear | Clear conversation (with confirm) |
| `C-c C-o` | Scroll | Scroll to bottom |
| `q` | Quit/insert | Quit in output region, insert in input |
| `C-c C-s` | Switch session | (Phase 3 - placeholder) |
| `C-c C-n` | New session | (Phase 3 - placeholder) |
| `C-c @` | Add context | (Phase 4 - placeholder) |
| `C-c C-x` | Clear context | (Phase 4 - placeholder) |

---

## Troubleshooting

### "Symbol's function definition is void: agent-q-chat"

**Cause:** Chat module not loaded.

**Fix:**
```elisp
(require 'sly-agent-q-chat)
```

Or ensure `sly-agent-q.el` is loaded (it requires the chat module).

---

### "Text is read-only" when trying to type

**Cause:** Cursor is in the output region.

**Fix:** Move cursor below the "Input" separator line.

---

### Messages not appearing in chat

**Cause:** Buffer name mismatch or streaming callbacks not configured.

**Fix:** Verify buffer is named `*Agent-Q Chat*`:
```elisp
(get-buffer "*Agent-Q Chat*")  ; Should return buffer, not nil
```

---

### History navigation not working

**Cause:** No messages have been sent yet.

**Fix:** Send at least one message before using M-p.

---

### Diff shows "Can't find the beginning of the hunk"

**Cause:** Cursor was in the diff header area (before the first `@@` line) when pressing `a` or `r`.

**Fix:** This is now auto-handled - pressing `a` or `r` in the header will automatically move to the first hunk. If no hunks exist, you'll see "No hunks found in this diff".

---

### Diff shows "\ No newline at end of file"

**Cause:** The source file or proposed modification doesn't end with a newline character. This is a standard diff notation, not an error.

**Note:** This is informational output from the `diff` command, not a bug.

---

## Test Summary Checklist

Copy this checklist for testing sessions:

```
[ ] Buffer layout correct
[ ] Output region read-only
[ ] Input region editable
[ ] Multi-line input works
[ ] RET sends at end
[ ] C-c C-c always sends
[ ] Empty input rejected
[ ] M-p/M-n history works
[ ] Draft preserved in history
[ ] Agent responses appear in output
[ ] Tool messages appear in chat
[ ] C-c C-l clears (with confirm)
[ ] q quits in output, types in input
[ ] C-c C-o scrolls to bottom
[ ] Cannot send while pending
[ ] C-c C-k cancels request
[ ] Diff shows tool start/result in chat
[ ] Diff per-hunk a/r works from any position
[ ] Old commands redirect to new chat
[ ] C-c q i inserts last response at point
[ ] C-c q w copies last response to kill ring
```

---

## See Also

- [QUICKSTART.md](QUICKSTART.md) - Getting started with Agent-Q
- [USER-GUIDE.md](USER-GUIDE.md) - Complete user documentation
- [DIFF-REVIEW-GUIDE.md](DIFF-REVIEW-GUIDE.md) - Diff approval workflow
- [TESTING.md](TESTING.md) - Lisp-side automated tests
