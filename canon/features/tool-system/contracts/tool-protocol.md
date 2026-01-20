# Contract: Tool Protocol

**Version:** 0.2.0
**Status:** Stable
**Confidence:** 0.90
**Source:** specs/PHASE-2-SPEC.md:110-160, src/tools/package.lisp

---

## Purpose

Defines the protocol for creating tools that extend Agent-Q's capabilities. Tools are executable functions the LLM can invoke to perform actions in the running Lisp image.

---

## Tool Definition Macro

### Signature

```lisp
(define-tool name description parameters
  &key required safety-level categories handler)
  → tool object
```

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `name` | string | Yes | Tool name in snake_case (e.g., "describe_symbol") |
| `description` | string | Yes | Human-readable description for LLM to understand when to use this tool |
| `parameters` | list of plists | Yes | Parameter schema: `(:name "param" :type :string :description "...")` |
| `required` | list of strings | No | List of required parameter names (default: nil) |
| `safety-level` | keyword | No | `:safe`, `:cautious`, or `:dangerous` (default: `:safe`) |
| `categories` | list of keywords | No | E.g., `(:introspection :xref)` for categorization |
| `handler` | lambda | Yes | Function that receives hash table of arguments → result |

### Safety Levels

| Level | Behavior | Examples | User Approval |
|-------|----------|----------|---------------|
| `:safe` | Read-only, auto-execute | describe-symbol, apropos-search | No |
| `:cautious` | Modifies state, logged | eval-form, compile-form | No, but logged |
| `:dangerous` | Permanent changes, requires approval | write-file, propose-file-edit | Yes (via approval handler) |

### Parameter Types

Supported types for tool parameters:

- `:string` - Text parameter
- `:boolean` - True/false flag
- `:number` - Numeric value (integer or float)
- `:object` - Nested object (not commonly used)
- `:array` - List of values (not commonly used)

---

## Tool Structure

### Tool Object Slots

```lisp
(defclass tool ()
  ((name          :type string            ; "describe_symbol"
                  :accessor tool-name)

   (description   :type string            ; "Get detailed info about symbol"
                  :accessor tool-description)

   (parameters    :type list              ; List of parameter plists
                  :accessor tool-parameters)

   (required      :type list              ; List of required param names
                  :accessor tool-required)

   (safety-level  :type keyword           ; :safe | :cautious | :dangerous
                  :accessor tool-safety-level
                  :initform :safe)

   (categories    :type list              ; (:introspection :xref)
                  :accessor tool-categories
                  :initform nil)

   (handler       :type function          ; (lambda (args-hash-table) ...)
                  :accessor tool-handler)))
```

### Handler Function Contract

**Input:** Hash table with string keys (parameter names) → values

**Output:** Any of:
- `string` - Direct result (formatted for LLM)
- `list` - Pretty-printed as s-expression
- `nil` - Formatted as "nil"
- `(values result error-msg)` - Result with optional error

**Error Handling:**
- Tools MAY signal errors (condition system)
- Executor catches all errors → formats as tool-result with `:success nil`
- Recommended: Return formatted error strings rather than signaling

---

## Example Definitions

### Simple Safe Tool (Read-Only)

```lisp
(let ((tool (define-tool
              "describe_symbol"
              "Get detailed information about a Lisp symbol including type, value, documentation."
              '((:name "symbol" :type :string :description "Symbol name to describe")
                (:name "package" :type :string :description "Package name (optional)"))
              :required '("symbol")
              :safety-level :safe
              :categories '(:introspection)
              :handler (lambda (args)
                         (let* ((sym-name (gethash "symbol" args))
                                (pkg-name (gethash "package" args))
                                (pkg (if pkg-name (find-package pkg-name) *package*))
                                (sym (and pkg (find-symbol (string-upcase sym-name) pkg))))
                           (if sym
                               (with-output-to-string (s)
                                 (describe sym s))
                               (format nil "Symbol ~A not found" sym-name)))))))
  (register-tool *agent-q-registry* tool))
```

### Cautious Tool (Executes Code)

```lisp
(let ((tool (define-tool
              "eval_form"
              "Evaluate a Lisp form in the running image. Returns result or error."
              '((:name "form" :type :string :description "Lisp form to evaluate")
                (:name "package" :type :string :description "Package context (optional)"))
              :required '("form")
              :safety-level :cautious  ; Modifies state, logged
              :categories '(:execution)
              :handler (lambda (args)
                         (let* ((form-string (gethash "form" args))
                                (pkg (find-package (or (gethash "package" args) *package*))))
                           (handler-case
                               (let ((*package* pkg))
                                 (format nil "~S" (eval (read-from-string form-string))))
                             (error (e)
                               (format nil "Error: ~A" e))))))))
  (register-tool *agent-q-registry* tool))
```

### Dangerous Tool (Requires Approval)

```lisp
(let ((tool (define-tool
              "write_file"
              "Write content to a file. DESTRUCTIVE - requires user approval."
              '((:name "path" :type :string :description "File path to write")
                (:name "content" :type :string :description "Content to write"))
              :required '("path" "content")
              :safety-level :dangerous  ; Requires approval
              :categories '(:buffer :filesystem)
              :handler (lambda (args)
                         (let ((path (gethash "path" args))
                               (content (gethash "content" args)))
                           (with-open-file (stream path :direction :output
                                                  :if-exists :supersede)
                             (write-string content stream))
                           (format nil "Wrote ~D bytes to ~A"
                                  (length content) path))))))
  (register-tool *agent-q-registry* tool))
```

---

## Invariants

1. **Name uniqueness**: Tool names MUST be unique in the registry
2. **Snake_case naming**: Tool names MUST use snake_case (LLM convention)
3. **Required parameters**: Parameters in `:required` list MUST exist in `:parameters`
4. **Safety level**: MUST be one of `:safe`, `:cautious`, or `:dangerous`
5. **Handler signature**: Handler MUST accept hash table, MAY return any value
6. **Registration**: Tools MUST be registered to *agent-q-registry* to be usable

---

## Registration Pattern

All tool definitions follow this pattern:

```lisp
(let ((tool (define-tool ...)))
  (register-tool *agent-q-registry* tool))
```

**Rationale:**
- `let` binding ensures tool object only exists to register it
- No global variable pollution
- Tool stored in central registry only

---

## Integration with LLM Provider

Tools are converted to LLM-compatible schema:

```lisp
(tool-to-schema tool)
→ (:type "function"
   :function (:name "describe_symbol"
              :description "Get detailed information..."
              :parameters (:type "object"
                          :properties (:symbol (:type "string" ...)
                                      :package (:type "string" ...))
                          :required ("symbol"))))
```

This schema is sent to cl-llm-provider which communicates it to the LLM (Claude, GPT-4, etc.). The LLM then knows:
- What tools are available
- What parameters each tool accepts
- When to use each tool (via description)

---

## Error Handling Contract

### Tool Handler Errors

**Option 1: Return error string**
```lisp
:handler (lambda (args)
           (if (valid-input? args)
               (do-work args)
               "Error: Invalid input"))  ; Return error as string
```

**Option 2: Signal condition**
```lisp
:handler (lambda (args)
           (unless (valid-input? args)
             (error "Invalid input"))    ; Signal error
           (do-work args))
```

### Executor Behavior

- Executor wraps handler in `handler-case`
- All errors caught → formatted into tool-result with `:success nil`
- Error message extracted from condition
- LLM receives error message, can retry or adjust approach

---

## Best Practices

### Naming

✅ **Good:** `describe_symbol`, `function_arglist`, `read_file`
❌ **Bad:** `describeSymbol`, `function-arglist`, `readFile`

**Rationale:** LLMs trained on snake_case function names (Python, etc.)

### Descriptions

✅ **Good:** "Get detailed information about a Lisp symbol including its type, value, and documentation."
❌ **Bad:** "Describes symbols"

**Rationale:** LLM uses description to decide when to use tool. More detail = better selection.

### Parameters

✅ **Good:** Specific types, clear descriptions, mark optional vs. required
❌ **Bad:** Vague descriptions, no type hints, unclear optionality

### Safety Levels

✅ **Safe for read-only:** `:safe`
✅ **Safe for logged execution:** `:cautious`
✅ **Dangerous for filesystem/network:** `:dangerous`

**Rationale:** Prevents accidental destructive operations

### Error Messages

✅ **Good:** "Symbol FOO not found in package BAR (status: EXTERNAL)"
❌ **Bad:** "Error"

**Rationale:** LLM can understand and retry with corrected input

---

## Testing Contract

### Manual Testing (Current Approach)

1. Register tool in REPL
2. Call `(get-tool "tool_name")`
3. Manually invoke handler: `(funcall (tool-handler tool) (hash-table ...))`
4. Verify result format and error handling

### Automated Testing (Planned)

```lisp
(test describe-symbol-tool
  (let ((tool (get-tool "describe_symbol")))
    (is (not (null tool)))
    (let ((result (funcall (tool-handler tool)
                          (make-hash-table-args "symbol" "car"))))
      (is (search "function" result)))))
```

---

## Related Contracts

- **tool-registry.md**: How tools are stored and retrieved
- **tool-execution.md**: How tools are called by the agent
- **diff-approval**: Special handling for dangerous tools

---

## References

- **PHASE-2-SPEC.md:110-160** - Original tool system specification
- **src/tools/package.lisp** - `define-tool` macro implementation
- **src/tools/introspection.lisp** - 9 example tool implementations
- **src/tools/execution.lisp** - Cautious tool examples

---

**Contract Status:** ✅ Stable and implemented across 18 tools
**Last Updated:** 2026-01-20
**Next Review:** When adding new tool categories or safety levels
