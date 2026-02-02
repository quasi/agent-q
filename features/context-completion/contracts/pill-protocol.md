# Contract: Pill Protocol

**Confidence:** 1.00
**Source:** sly-agent-q-context.el, 29 pill tests

---

## Purpose

Visual representation of attached context as clickable pills in chat input.

---

## Format

**Text:** `[@name]` where name is file/symbol/buffer

**Example:** `@src/agent.lisp` becomes `[@src/agent.lisp]`

---

## Text Properties

| Property | Value | Purpose |
|----------|-------|---------|
| `sly-agent-q-context-pill` | name (string) | Identifies as pill, stores name |
| `face` | `sly-agent-q-context-pill-face` | Visual styling |
| `rear-nonsticky` | `t` | Typing after pill doesn't extend properties |
| `keymap` | `sly-agent-q-context-pill-map` | Click/delete bindings |

---

## Operations

### Create Pill

```elisp
(sly-agent-q-context--insert-pill "src/agent.lisp")
→ Inserts [@src/agent.lisp] with properties
→ Adds to buffer-local context list
```

### Remove Pill

```elisp
;; User presses DEL on pill
→ Removes [@name] text
→ Removes from context list
```

### Visit Source

```elisp
;; User clicks pill
→ Opens file/buffer/symbol definition
```

---

## Invariants

1. **Uniqueness:** Pill text MUST have `sly-agent-q-context-pill` property
2. **Brackets:** Pills MUST be wrapped in `[@ ]`
3. **Rear-nonsticky:** Properties MUST NOT extend to following text
4. **Context Sync:** Pill presence ↔ context list membership

---

## Lifecycle

```
User types @file
  ↓ completion
User selects completion
  ↓ create pill
[@file] inserted
  ↓ (persists)
User sends message
  ↓ content fetched
Context added to LLM message
```

---

**Status:** ✅ Tested (29 tests)
