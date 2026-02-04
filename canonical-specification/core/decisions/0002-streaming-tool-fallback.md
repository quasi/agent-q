# ADR-0002: Hybrid Streaming with Synchronous Tool Fallback

**Status:** Accepted
**Date:** 2026-01-13 (decision), recovered via git archaeology
**Deciders:** Development team
**Commits:** d9b899d, 35bcc82

---

## Context

Agent-Q integrates with cl-llm-provider for LLM communication. The library supports streaming responses (token-by-token delivery) for better UX, allowing users to see responses as they're generated rather than waiting for completion.

### Problem Statement

**Challenge:** LLM responses come in two forms:
1. **Text responses**: Pure content meant for display to users
2. **Tool calls**: Structured requests for the agent to execute functions (introspection, code execution, etc.)

**Initial approach:** Attempt to stream both text and tool calls uniformly.

**Discovered issues:**
- Tool calls require complete JSON structures to parse and execute
- Partial tool call chunks (streaming) can't be reliably parsed mid-stream
- Error handling for tool execution needs synchronous confirmation
- Token counting becomes complex with partial tool call streams

**Impact:**
- Medium-High: Tool execution reliability is critical
- UX: Users expect real-time text feedback but don't need to see tool parsing
- Reliability: Incorrect tool parsing could execute wrong actions

---

## Decision

**Implement a hybrid approach: stream text responses, fall back to synchronous for tool calls.**

### Streaming Behavior

**Text Responses:**
- Use cl-llm-provider's streaming API
- Deliver tokens in real-time via callbacks
- Display immediately in Emacs chat buffer
- Finish reason: `:stop` or `:end-turn`

**Tool Call Responses:**
- Detect tool-use finish reason during streaming
- Fall back to synchronous `chat` call to get complete tool structure
- Parse and execute tool calls with full error handling
- Return tool results, continue conversation loop

### Token Counting Protection

**Problem:** Fallback could double-count tokens (streaming attempt + synchronous call).

**Solution:** Track finish reason during streaming:
- If `:tool-use` or `:tool-calls` detected, abort streaming early
- Only count tokens from the final synchronous call
- Prevents billing/metric inflation

---

## Alternatives Considered

### Alternative 1: Fully Synchronous (No Streaming)

**Approach:** Disable streaming entirely, use synchronous calls for all responses.

**Rejected because:**
- UX regression: Users lose real-time feedback
- Competitive disadvantage: Other tools stream successfully
- Waste of cl-llm-provider capability

**Advantages:**
- Simpler code (single code path)
- No token counting complexity

### Alternative 2: Stream Everything, Parse Incrementally

**Approach:** Stream tool calls, parse JSON incrementally, execute when complete.

**Rejected because:**
- Complex: Incremental JSON parsing is error-prone
- Latency: Still need to wait for complete structure before execution
- Error recovery: Harder to handle malformed tool calls
- Library limitation: cl-llm-provider doesn't provide partial tool structures

**Advantages:**
- Uniform streaming interface
- Potentially lower latency for first tool parameter

### Alternative 3: Buffered Streaming with Lookahead

**Approach:** Buffer streamed chunks, detect tool patterns, switch modes dynamically.

**Rejected because:**
- Complexity: Requires heuristic pattern detection
- Unreliable: False positives could break text streaming
- Latency: Buffering defeats streaming purpose

**Advantages:**
- Single API call (no fallback needed)

---

## Consequences

### Positive

✅ **Reliable tool execution**
- Complete tool call structures guaranteed
- No partial parse failures
- Full error handling available

✅ **Excellent text UX**
- Real-time token delivery for prose responses
- Users see thinking in progress
- Responsive feel

✅ **Simple mental model**
- Text = stream, Tools = sync
- Easy to reason about in code

✅ **Accurate token counting**
- Double-counting prevented
- Metrics remain accurate

### Negative

⚠️ **Slight latency on tool calls**
- Must abort stream, restart synchronous call
- Additional network round-trip
- User sees brief pause before tool execution

⚠️ **Two code paths**
- More complexity than pure streaming or pure sync
- Need to maintain both modes

⚠️ **Finish reason dependency**
- Relies on LLM provider correctly signaling tool use
- If finish reason wrong, fallback won't trigger

---

## Implementation

### Changed Files

- `src/agent.lisp` - Hybrid streaming loop in `send-to-agent`
- `src/streaming.lisp` - Streaming callback factories
- `src/llm-integration.lisp` - Synchronous fallback logic

### Code Example

```lisp
;; src/agent.lisp (simplified)
(defun send-to-agent (message)
  "Send MESSAGE to LLM with hybrid streaming/sync approach."
  (let ((finish-reason nil)
        (accumulated-text ""))

    ;; Attempt streaming
    (let ((callback (make-streaming-callback
                     :on-chunk (lambda (chunk)
                                 (setf accumulated-text
                                       (concatenate 'string accumulated-text chunk))
                                 (emit-to-emacs chunk))
                     :on-complete (lambda (reason)
                                    (setf finish-reason reason)))))
      (cl-llm-provider:stream-chat *provider* messages :callback callback))

    ;; Check finish reason
    (cond
      ((member finish-reason '(:tool-use :tool-calls))
       ;; Fallback to synchronous for complete tool structure
       (let ((response (cl-llm-provider:chat *provider* messages)))
         (execute-tools (response-tool-calls response))))

      ((member finish-reason '(:stop :end-turn))
       ;; Streaming completed successfully
       accumulated-text))))
```

### Token Counting Fix

```lisp
;; src/agent.lisp:35 (commit 35bcc82)
;; Before: Counted streaming tokens + sync tokens = double count

;; After: Only count final tokens
(cond
  ((tool-call-p finish-reason)
   (let ((response (cl-llm-provider:chat ...)))
     ;; Count only these tokens, discard streaming attempt
     (record-tokens (response-usage response))))

  ((text-response-p finish-reason)
   ;; Count streamed tokens (already tracked by callback)
   (record-tokens streaming-usage)))
```

### Testing

**Manual Test:**
1. Send message requiring tool use → Agent executes tool correctly
2. Send conversational message → Response streams in real-time
3. Alternate between text and tools → Both work reliably
4. Check token counts → No double-counting

**Test Result:** ✅ Hybrid approach works, no tool execution failures

---

## Experience Report

### What Worked Well

- Hybrid approach is pragmatic compromise
- Users don't notice the fallback (happens quickly)
- Tool execution is now rock-solid reliable
- Text streaming UX is excellent

### What Could Be Improved

- Could optimize: detect tool calls earlier in stream to abort sooner
- Could cache: avoid re-sending full message history on fallback
- Could add: metrics on stream vs. sync usage patterns

### Lessons Learned

- **Uniform != Better**: Different response types have different requirements
- **UX where it matters**: Stream text (user-visible), sync tools (internal)
- **Pragmatism wins**: Perfect streaming not worth complexity/reliability cost

---

## Related Decisions

- **ADR-0004**: Fail-Open Cost Estimation (handles edge cases in cost tracking)
- **ADR-0005**: Phased Chat Development (streaming added in coordination with observability)
- **Future**: If cl-llm-provider adds incremental tool parsing, revisit this decision

---

## References

- **Commits:**
  - [d9b899d](commit:d9b899d) - "feat(agent): implement streaming agent loop with tool support"
  - [35bcc82](commit:35bcc82) - "fix(agent): prevent double token counting on tool call fallback"
- **Planning Doc:** `specs/plans/2026-01-13-streaming-observability-upgrade.md`
- **Code:** `src/agent.lisp:187-245`, `src/streaming.lisp`
- **Library:** cl-llm-provider streaming API documentation

---

## Notes

This decision was **recovered via git archaeology** during Canon initiation (Pass 5: Rationale Recovery). The implementation commits clearly show the hybrid approach and the token counting fix.

**Recovery Confidence:** 0.95
**Decision Quality:** Pragmatic engineering—optimizes for different response types

**Future Considerations:**
- Monitor LLM provider finish-reason reliability
- Consider caching to reduce fallback latency
- Track metrics: stream vs. sync usage ratio

---

**ADR Status:** ✅ Accepted and Implemented
**Last Reviewed:** 2026-01-17
**Next Review:** If streaming API changes or tool parsing becomes incremental
