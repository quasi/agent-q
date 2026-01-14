# Agent-Q Chat Manual Testing Guide

Manual verification steps for streaming, observability, and session management features.

## Prerequisites

1. **SLY connected** to a running Lisp image with Agent-Q loaded
2. **API key configured** for your LLM provider (Anthropic, OpenAI, etc.)
3. **Observability enabled** (optional but recommended):
   ```lisp
   (agent-q:setup-observability :level :debug)
   ```

## Quick Smoke Test

```
M-x agent-q-chat RET
```

**Expected:**
- Chat buffer opens immediately (no waiting/blocking)
- Header shows session info: `[New Session] | Tokens: 0/0`
- Provider/model info appears after ~0.1s: "Using Anthropic claude-sonnet-4-20250514"
- Cursor is in input area after `> ` prompt

**If it hangs:** Press `C-c C-k` to cancel, check `*Messages*` for errors.

---

## Test 1: Basic Streaming Response

**Steps:**
1. Open chat: `M-x agent-q-chat`
2. Type: `count from 1 to 10`
3. Press `RET` to send

**Expected:**
- Text streams in token-by-token (not all at once)
- Numbers appear incrementally: 1, 2, 3...
- Header updates with token count after completion
- No "Press any key" or blocking prompts

**Verify in `*Messages*`:**
```
[AGENT-Q] DEBUG: Starting streaming request...
[AGENT-Q] DEBUG: Streaming complete, final-usage=...
```

---

## Test 2: Token Tracking

**Steps:**
1. After Test 1, check the header line
2. Send another message: `what is 2+2?`
3. Check header again

**Expected:**
- Header shows accumulated tokens: `Tokens: X/Y` where X=input, Y=output
- Token counts increase after each message
- If cost tracking enabled, shows: `Cost: $0.00XX`

**Common Issues:**
- If tokens show `0/0` after response, the update callback isn't working
- If tokens only update on second message, there's an async timing issue

---

## Test 3: Tool Calls (Introspection)

**Steps:**
1. Send: `describe the function MAPCAR`
2. Watch for tool call indicators

**Expected:**
- Debug output shows tool being called
- Response includes MAPCAR documentation from live Lisp image
- Token counts update (even for tool call responses)

**Verify in `*Messages*` or REPL:**
```
[AGENT-Q] DEBUG: Tool call: describe-symbol
[AGENT-Q] DEBUG: Using sync response usage: (:PROMPT-TOKENS X :COMPLETION-TOKENS Y)
```

---

## Test 4: Cancel Hung Request

**Steps:**
1. Send a complex query that takes time
2. Immediately press `C-c C-k` (agent-q-cancel-request)

**Expected:**
- Message appears: "Request cancelled"
- `[Request cancelled]` indicator in chat buffer
- Can send new messages immediately
- No "already waiting for answer" errors on next send

---

## Test 5: Session Persistence

**Steps:**
1. Note your current session name in header (or name it via `C-c q n`)
2. Send a few messages
3. Kill the chat buffer: `C-x k`
4. Reopen: `M-x agent-q-chat`
5. Load previous session: `C-c q l` and select it

**Expected:**
- Previous messages restored
- Token counts restored
- Can continue conversation

---

## Test 6: Error Recovery

**Steps:**
1. Temporarily break your API key (rename env var)
2. Try to send a message
3. Observe error handling
4. Fix API key and retry

**Expected:**
- Clear error message (not a stack trace)
- Can recover and continue after fixing
- No hung state

---

## Test 7: Multiple Rapid Messages

**Steps:**
1. Send a message
2. While streaming, try to send another message

**Expected:**
- Second send blocked with message: "Already waiting for response"
- First response completes normally
- Can send after first completes

---

## Debugging Tips

### Check Observability Logs
```lisp
;; In REPL
(agent-q:get-request-stats)
;; => (:TOTAL-REQUESTS 5 :TOTAL-TOKENS 1234 :AVG-TIMING-SECS 1.5)
```

### View Debug Output
Set debug level before testing:
```lisp
(agent-q:setup-observability :level :debug)
```

### Common Issues

| Symptom | Likely Cause | Fix |
|---------|--------------|-----|
| Chat opens but hangs | SLY not connected | `M-x sly` first |
| Tokens always 0/0 | Streaming callback issue | Check `*Messages*` for errors |
| "Already waiting" stuck | State not cleared | `C-c C-k` to cancel |
| No provider shown | Config fetch failed | Check API key, run `(agent-q:get-config-info)` |

### Reset Everything
```lisp
;; In REPL - full reset
(agent-q:teardown-observability)
(agent-q:setup-observability :level :debug)
```

Then kill and reopen chat buffer.

---

## Feature Checklist

After testing, verify:

- [ ] Chat opens without blocking
- [ ] Provider/model info displays
- [ ] Streaming works (incremental display)
- [ ] Token counts update in header
- [ ] Tool calls work and show token usage
- [ ] Cancel (`C-c C-k`) works
- [ ] Sessions persist and restore
- [ ] Errors show clearly (no stack traces)
- [ ] No "already waiting" stuck states

---

## Reporting Issues

If a test fails, capture:
1. Contents of `*Messages*` buffer
2. Any REPL debug output
3. Steps to reproduce
4. Expected vs actual behavior
