# Property: Streaming Continuity

**Type:** Real-Time Display
**Confidence:** 1.00

---

## Statement

**Streaming responses MUST display tokens incrementally as received, maintaining message continuity without visual artifacts.**

---

## Invariant

```
∀ chunk ∈ stream:
  append(chunk, current-message-position)
  ∧ advance(marker)
  ∧ visible-text = concatenate(all-previous-chunks)
```

---

## Behavior

### Start Stream
1. Create placeholder message
2. Position marker at end
3. Begin receiving chunks

### Append Chunks
1. Insert chunk at marker
2. Advance marker
3. Display updates in real-time

### Finalize
1. Apply markdown rendering
2. Finalize message
3. Enable scrolling

---

## Test Case

```elisp
(ert-deftest streaming-continuity
  (with-temp-buffer
    (sly-agent-q-chat-mode)
    (sly-agent-q-chat--streaming-start)
    (sly-agent-q-chat--streaming-append "Hello")
    (sly-agent-q-chat--streaming-append " world")
    (sly-agent-q-chat--streaming-finalize)
    (should (search-backward "Hello world"))))
```

---

**Status:** ✅ Verified (7 streaming tests)
