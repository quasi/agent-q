# Contract: Tool Registry

**Version:** 0.2.0
**Status:** Stable
**Confidence:** 0.95
**Source:** specs/PHASE-2-SPEC.md:163-199, src/tools/registry.lisp

---

## Purpose

Central hash table storing all available tools, providing registration, retrieval, and schema generation for LLM integration.

---

## Global Registry

```lisp
(defvar *agent-q-registry* (make-hash-table :test 'equal)
  "Central registry of all tools. Keys: tool names (strings). Values: tool objects.")
```

**Invariants:**
- Keys MUST be strings (tool names in snake_case)
- Values MUST be tool objects
- Registry persists for the Lisp image lifetime
- Tools registered once, never removed (no unregister function)

---

## Core Operations

### register-tool

```lisp
(register-tool registry tool) → tool
```

**Purpose:** Add a tool to the registry.

**Parameters:**
- `registry` - hash table (*agent-q-registry*)
- `tool` - tool object to register

**Behavior:**
- Stores tool with `(tool-name tool)` as key
- Overwrites if tool with same name exists (idempotent)
- Returns the registered tool object

**Example:**
```lisp
(register-tool *agent-q-registry*
  (make-instance 'tool :name "describe_symbol" ...))
```

**Invariants:**
- Tool name MUST be non-empty string
- Tool object MUST be valid (all required slots filled)

---

### get-tool

```lisp
(get-tool name) → tool or NIL
```

**Purpose:** Retrieve a tool by name.

**Parameters:**
- `name` - string, tool name (e.g., "describe_symbol")

**Returns:**
- Tool object if found
- NIL if no tool with that name

**Example:**
```lisp
(get-tool "describe_symbol") → #<TOOL describe_symbol>
(get-tool "nonexistent") → NIL
```

---

### list-registered-tools

```lisp
(list-registered-tools) → list of tool names
```

**Purpose:** Get all registered tool names.

**Returns:** List of strings (tool names)

**Example:**
```lisp
(list-registered-tools)
→ ("describe_symbol" "apropos_search" "eval_form" ...)
```

---

### get-agent-q-tools

```lisp
(get-agent-q-tools &key max-safety-level categories) → list of tools
```

**Purpose:** Get tools filtered by safety level and/or categories.

**Parameters:**
- `max-safety-level` - keyword (`:safe`, `:cautious`, or `:dangerous`)
- `categories` - list of keywords (e.g., `(:introspection :execution)`)

**Returns:** List of tool objects matching criteria

**Behavior:**
- If `max-safety-level` provided: only tools at or below that level
  - `:safe` → only safe tools
  - `:cautious` → safe + cautious tools
  - `:dangerous` → all tools
- If `categories` provided: only tools with at least one matching category
- Both filters applied if both provided (AND logic)

**Example:**
```lisp
;; Get only safe introspection tools
(get-agent-q-tools :max-safety-level :safe
                   :categories '(:introspection))
→ (describe_symbol apropos_search function_arglist ...)
```

---

## Schema Generation

### tools-to-schema

```lisp
(tools-to-schema tools) → list of JSON schema plists
```

**Purpose:** Convert tools to LLM-compatible schema format.

**Parameters:**
- `tools` - list of tool objects (from `get-agent-q-tools`)

**Returns:** List of plists suitable for cl-llm-provider

**Format:**
```lisp
(:type "function"
 :function (:name "describe_symbol"
            :description "Get detailed information about a Lisp symbol..."
            :parameters (:type "object"
                        :properties (:symbol (:type "string"
                                             :description "Symbol name...")
                                    :package (:type "string"
                                             :description "Package name..."))
                        :required ("symbol"))))
```

**Example:**
```lisp
(tools-to-schema (get-agent-q-tools :max-safety-level :safe))
→ ((:type "function" :function (:name "describe_symbol" ...))
   (:type "function" :function (:name "apropos_search" ...))
   ...)
```

---

## Initialization

### register-agent-q-tools

```lisp
(register-agent-q-tools) → integer (count of tools registered)
```

**Purpose:** Load all tool modules and register their tools.

**Behavior:**
- Loads introspection.lisp, execution.lisp, buffer.lisp, diff.lisp
- Each file registers its tools to *agent-q-registry*
- Idempotent: safe to call multiple times

**Returns:** Number of tools in registry after loading

**Example:**
```lisp
(register-agent-q-tools) → 18
```

**Called by:** Agent initialization (`ensure-agent`)

---

## Tool Discovery

Tools organized by category for discovery:

| Category | Count | Examples |
|----------|-------|----------|
| `:introspection` | 9 | describe_symbol, apropos_search, who_calls |
| `:execution` | 4 | eval_form, compile_form, get_repl_history |
| `:buffer` | 4 | read_file, read_buffer, write_file |
| `:diff` | 1 | propose_file_edit |
| `:xref` | 2 | who_calls, who_references |
| `:clos` | 2 | class_slots, class_hierarchy |
| `:packages` | 1 | list_package_symbols |

Note: Some tools have multiple categories (e.g., who_calls is both `:introspection` and `:xref`)

---

## Safety Level Distribution

| Level | Count | Auto-Execute | Approval Required |
|-------|-------|--------------|-------------------|
| `:safe` | 13 | Yes | No |
| `:cautious` | 3 | Yes (logged) | No |
| `:dangerous` | 2 | No | Yes |

---

## Invariants

1. **Uniqueness**: Tool names MUST be unique across all categories
2. **Persistence**: Registry persists for image lifetime (not serialized)
3. **Initialization**: `register-agent-q-tools` MUST be called before agent use
4. **Idempotency**: Re-registering a tool (same name) overwrites previous

---

## Integration Points

**With Agent Loop:**
```lisp
;; Agent sends tools to LLM when starting conversation
(let ((tools (get-agent-q-tools :max-safety-level :cautious)))
  (cl-llm-provider:chat *provider* messages :tools (tools-to-schema tools)))
```

**With Tool Executor:**
```lisp
;; LLM returns tool call, executor retrieves tool
(let ((tool (get-tool (getf tool-call :name))))
  (when tool
    (execute-tool-call tool tool-call)))
```

---

## Error Handling

**Missing Tool:**
```lisp
(get-tool "nonexistent") → NIL  ; Not an error, just returns nil
```

**Executor checks for nil:**
```lisp
(unless tool
  (make-instance 'tool-result :success nil
                 :error (format nil "Unknown tool: ~A" name)))
```

---

## References

- **PHASE-2-SPEC.md:163-199** - Registry specification
- **src/tools/registry.lisp** - Implementation
- **tool-protocol.md** - Tool structure
- **tool-execution.md** - How tools are invoked

---

**Contract Status:** ✅ Stable, 18 tools registered
**Last Updated:** 2026-01-20
