# Property: Tool Name Uniqueness

**Type:** Registry Integrity
**Confidence:** 1.00
**Source:** src/tools/registry.lisp

---

## Statement

**Tool names MUST be unique within the registry. Registering a tool with an existing name overwrites the previous tool.**

---

## Invariant

```
∀ name ∈ tool-names:
  (length (filter (λ tool → (tool-name tool) = name) registry)) ≤ 1
```

**In words:** At most one tool with any given name exists in the registry at any time.

---

## Behavior

### First Registration

```lisp
(register-tool *agent-q-registry*
  (make-instance 'tool :name "my_tool" ...))

(get-tool "my_tool") → #<TOOL my_tool>  ; Found
```

### Duplicate Registration (Overwrites)

```lisp
;; Register original tool
(register-tool *agent-q-registry*
  (make-instance 'tool :name "my_tool" :description "Original"))

;; Register replacement tool with same name
(register-tool *agent-q-registry*
  (make-instance 'tool :name "my_tool" :description "Replacement"))

;; Only replacement exists
(let ((tool (get-tool "my_tool")))
  (tool-description tool)) → "Replacement"  ; Not "Original"
```

---

## Rationale

**Why allow overwrites?**
1. **Development workflow**: REPL-based development means reloading tool definitions
2. **Hot reload**: Can update tools without restarting Lisp image
3. **Simplicity**: No need for explicit unregister function

**Why not versioning?**
- Tools are implementation details, not public API
- LLM sees current version only
- Versioning would complicate schema generation

---

## Test Case

```lisp
(test tool-name-uniqueness
  "Verify that registering duplicate name overwrites previous tool."

  (let ((registry (make-hash-table :test 'equal)))
    ;; Register original
    (register-tool registry
      (make-instance 'tool :name "test_tool" :description "Original"))

    (is (string= "Original"
                 (tool-description (gethash "test_tool" registry))))

    ;; Register duplicate (overwrites)
    (register-tool registry
      (make-instance 'tool :name "test_tool" :description "Replacement"))

    (is (string= "Replacement"
                 (tool-description (gethash "test_tool" registry))))

    ;; Only one tool with this name
    (is (= 1 (hash-table-count registry)))))
```

---

## Consequences

**Positive:**
- ✅ Simple mental model (one name = one tool)
- ✅ Easy to update tools during development
- ✅ No stale tools lingering in registry

**Negative:**
- ⚠️ No warning when overwriting (silent replacement)
- ⚠️ Cannot have tool versioning (v1 vs. v2)
- ⚠️ Accidental name collision overwrites tool

---

## Related Properties

- **safety-enforcement.md** - How tools are protected
- Registry is global and persistent (for image lifetime)

---

**Property Status:** ✅ Enforced by hash table semantics
**Last Updated:** 2026-01-20
