---
type: contract
name: pill-protocol
version: 1.0.0
feature: context-completion
depends_on: []
---

# Contract: Pill Protocol

**Confidence:** 1.00
**Source:** sly-agent-q-context.el, 29 pill tests

---

## Purpose

Visual representation of attached context as clickable pills in chat input.

---

## JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Context Pill",
  "description": "Visual pill representing attached context",
  "properties": {
    "name": {
      "type": "string",
      "description": "Display name (file/symbol/buffer)",
      "examples": ["src/agent.lisp", "make-context-item", "*scratch*"]
    },
    "type": {
      "type": "string",
      "enum": ["file", "symbol", "buffer"],
      "description": "Type of context being attached"
    },
    "text_properties": {
      "type": "object",
      "properties": {
        "pill": {"type": "string"},
        "face": {"type": "string"},
        "rear_nonsticky": {"type": "boolean"},
        "keymap": {"type": "string"}
      }
    },
    "content": {
      "type": "string",
      "description": "Full content of the attached context"
    }
  },
  "required": ["name", "type"]
}
```

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

## Error Conditions

| Error | Cause | Recovery |
|-------|-------|----------|
| Invalid pill name | Name contains invalid characters or is empty | Validate name before pill creation; reject invalid names |
| Duplicate pill | Same context attached twice | Check existing pills before insertion; update if already exists |
| Text property corruption | Pill properties lost or modified | Recreate pill with correct properties |
| Context load failure | Referenced file/symbol no longer exists | Display warning; allow user to remove pill or update reference |
| Keymap not found | Pill keymap undefined | Fallback to default behavior; log warning |

**Prevention:**
- Validate all pill names before creation
- Check for duplicates in context list
- Apply text properties atomically using `propertize` to prevent partial property states

**Handling:**
- Invalid pills: Show error message, don't create pill
- Missing content: Warn user, allow removal
- Property corruption: Recreate pill on next buffer initialization

**Error Messages:**
- `"Cannot attach context: file not found"` → File doesn't exist
- `"Context already attached"` → Duplicate detected
- `"Invalid context name"` → Name validation failed

---

**Status:** ✅ Tested (29 tests)
