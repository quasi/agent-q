# Scenario: Context Types

**Feature:** context-management
**User Story:** As a user, I want to add different types of context (code, errors, text, files) so the LLM understands what kind of information each item represents.
**Test Coverage:** agent-q-context/struct/type-variants (sly-agent-q-context-test.el:34)
**Confidence:** 1.00

---

## Context

Context items are classified by type to help the LLM understand the semantic meaning of each piece of information. Types influence how content is interpreted and referenced in LLM responses.

---

## Actors

- **User**: Provides various types of information
- **Context Manager**: Stores items with type metadata
- **LLM** (external): Interprets types for better responses

---

## Supported Types

### Type 1: :code (Default)

**Purpose:** Source code snippets

**Usage:**
```lisp
(add-context manager "(defun foo () 42)" :type :code)
```

**Typical Sources:**
- Marked regions in Emacs buffers
- Function definitions
- Code examples

**LLM Interpretation:** Treatable as executable code, can be analyzed for bugs

---

### Type 2: :error

**Purpose:** Error messages, warnings, stack traces

**Usage:**
```lisp
(add-context manager "ERROR: Division by zero at line 42" :type :error)
```

**Typical Sources:**
- Compiler error output
- Runtime exceptions
- SLIME/SLY REPL errors

**LLM Interpretation:** Diagnostic information, needs debugging

---

### Type 3: :text

**Purpose:** Plain text, notes, documentation

**Usage:**
```lisp
(add-context manager "This function should handle edge cases" :type :text)
```

**Typical Sources:**
- User annotations
- Documentation snippets
- Requirements text

**LLM Interpretation:** Descriptive information, not executable

---

### Type 4: :file

**Purpose:** Entire file contents

**Usage:**
```lisp
(let ((content (with-open-file (f "src/example.lisp") (read-file-to-string f))))
  (add-context manager content :type :file
               :metadata '(:filename "src/example.lisp")))
```

**Typical Sources:**
- `C-c C-q f` (add-file-to-context) in Emacs
- Bulk context addition

**LLM Interpretation:** Complete file, may contain multiple definitions

---

### Type 5: :repl-history

**Purpose:** Previous REPL interactions

**Usage:**
```lisp
(add-context manager "(+ 1 2) ; → 3" :type :repl-history)
```

**Typical Sources:**
- SLIME/SLY REPL session history
- Evaluation results

**LLM Interpretation:** Interactive session context, shows what user tried

---

### Type 6: :custom

**Purpose:** Extension point for future types

**Usage:**
```lisp
(add-context manager "..." :type :custom
             :metadata '(:custom-type "profile-data"))
```

**Status:** ⚠️ Undocumented, intended for future extensions

**LLM Interpretation:** Unknown (requires additional metadata)

---

## Type Selection Flow

### Decision Tree

```
What kind of information?
│
├─ Source code snippet → :code
├─ Error/warning message → :error
├─ Plain text note → :text
├─ Entire file → :file
├─ REPL interaction → :repl-history
└─ Other/future → :custom
```

---

## Main Flow: Type-Aware Addition

### Step 1: User Provides Content and Type

**Emacs Action:** User selects content and specifies type

**Examples:**
```elisp
;; Code region (default)
(agent-q--add-context-region)  ; Infers :code from context

;; Error from REPL
(agent-q--add-error-to-context "ERROR: ...")  ; Explicitly :error

;; Text note
(agent-q-add-context "note text" :type :text)  ; Explicit :text
```

### Step 2: Type Validated

**CLOS Enforcement:**
```lisp
(defclass context-item ()
  ((item-type :type (member :code :text :file :repl-history :error :custom))))
```

**Result:**
- Valid type → Item created
- Invalid type → `TYPE-ERROR` signaled

### Step 3: Type Stored

**Item Structure:**
```lisp
#<CONTEXT-ITEM
  id: "ctx-42"
  type: :CODE      ← Type stored
  content: "..."
  timestamp: ...>
```

### Step 4: Type Used in Formatting

**Markdown Header:**
```markdown
### Code        ← Capitalized type name
```lisp
(defun foo () 42)
```
```

---

## Type-Specific Behaviors

### Behavior 1: Markdown Heading

| Type | Markdown Heading |
|------|------------------|
| `:code` | `### Code` |
| `:error` | `### Error` |
| `:text` | `### Text` |
| `:file` | `### File` |
| `:repl-history` | `### Repl-History` |
| `:custom` | `### Custom` |

### Behavior 2: LLM Interpretation

**Example Prompt:**
```markdown
### Code (from src/agent.lisp:42)
```lisp
(defun foo () (/ x 0))
```

### Error
```lisp
ERROR: Division by zero
```

<user>Why am I getting this error?</user>
```

**LLM Response:** "The error occurs in the `foo` function at line 42 where you divide by zero: `(/ x 0)`. The **Code** context shows the problematic function, and the **Error** context confirms the division by zero..."

**Observation:** LLM uses type to understand relationship between items

### Behavior 3: Filtering

```lisp
;; Get only errors for error analysis
(get-context manager :types '(:error))

;; Get code and files (exclude errors/text)
(get-context manager :types '(:code :file))
```

---

## Verification

### Test: All Types Accepted

**Test:** `agent-q-context/struct/type-variants` (sly-agent-q-context-test.el:34)

```elisp
(ert-deftest agent-q-context/struct/type-variants ()
  "Test that all context types work."
  (should (context-item-valid-type-p :code))
  (should (context-item-valid-type-p :text))
  (should (context-item-valid-type-p :file))
  (should (context-item-valid-type-p :repl-history))
  (should (context-item-valid-type-p :error))
  (should (context-item-valid-type-p :custom)))
```

**Status:** ✅ Pass (all 6 types validated)

### Test: Invalid Type Rejected

```lisp
;; Manual verification (no explicit test)
(handler-case
    (make-context-item "test" :type :invalid)
  (type-error (e)
    (format t "✓ Invalid type rejected: ~A" e)))
```

**Result:** ✅ TYPE-ERROR signaled as expected

---

## Use Cases by Type

### Use Case 1: Debugging (Error + Code)

```
User adds:
1. Error message (type :error)
2. Problematic function (type :code)

LLM receives both and explains:
"The error at line X occurs because function Y..."
```

### Use Case 2: Feature Request (Text + Code)

```
User adds:
1. Feature description (type :text)
2. Existing implementation (type :code)

LLM receives both and suggests:
"To add this feature to the existing implementation..."
```

### Use Case 3: REPL Session Review (REPL-History)

```
User adds:
1. (+ 1 2) → 3 (type :repl-history)
2. (+ 1 "2") → ERROR (type :repl-history)

LLM explains:
"In your first attempt, the addition worked because both arguments were numbers. The second failed because..."
```

---

## Type Selection Best Practices

### Best Practice 1: Use Specific Types

❌ **Bad:** Everything as `:code`
```lisp
(add-context manager "ERROR: ..." :type :code)  ; Wrong type
```

✅ **Good:** Appropriate types
```lisp
(add-context manager "ERROR: ..." :type :error)  ; Correct type
```

**Rationale:** LLM better understands semantic meaning

### Best Practice 2: Annotate Custom Types

When using `:custom`, add metadata:
```lisp
(add-context manager "..." :type :custom
             :metadata '(:custom-type "benchmark-results"
                        :format "json"))
```

### Best Practice 3: Group Related Types

Add related items in sequence:
```lisp
;; Error debugging: error first, then code
(add-context manager error-msg :type :error)
(add-context manager function-def :type :code)

;; LLM sees chronological relationship
```

---

## Related Scenarios

- **add-context-item.md**: Shows how types are specified during addition
- **get-filtered-context.md**: Shows how to filter by type
- **format-for-llm.md**: Shows how types affect markdown headers

---

## Related Properties

- **type-safety.md**: CLOS enforcement of valid types
- **markdown-formatting.md**: Type capitalization in headers

---

## Known Limitations

### L-001: :custom Type Undocumented

**Problem:** `:custom` exists but no documentation on usage
**Impact:** Users don't know when to use it
**Status:** Documentation gap (see canon/README.md)

### L-002: Fixed Type Set

**Problem:** Cannot add project-specific types without modifying source
**Workaround:** Use `:custom` with metadata to distinguish subtypes
**Example:**
```lisp
;; Emulate :benchmark type
(add-context manager data :type :custom
             :metadata '(:custom-type :benchmark))
```

### L-003: No Type Coercion

**Problem:** Must explicitly specify type, no auto-detection
**Example:**
```lisp
;; No auto-detection of error strings
(add-context manager "ERROR: ..." :type :code)  ; Accepted but wrong
```

**Recommendation:** Add heuristic type detection:
```lisp
(defun infer-type (content)
  (cond
    ((search "ERROR:" content) :error)
    ((search "WARNING:" content) :error)
    (t :code)))  ; Default
```

---

## Future Enhancements

### Enhancement 1: Type Hierarchy

Support subtypes:
```lisp
:code → :code/lisp, :code/python, etc.
:error → :error/compile-time, :error/runtime
```

### Enhancement 2: Type-Specific Formatting

Different markdown for different types:
```lisp
;; :error uses ``` text ``` instead of ``` lisp ```
(defmethod format-item ((item context-item) (type (eql :error)))
  (format nil "```text~%~A~%```" (context-item-content item)))
```

### Enhancement 3: Auto-Detection

Infer type from content:
```lisp
(add-context-smart manager "ERROR: ...")  ; Automatically detects :error
```

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | 6 types defined | Phase 1 foundation |
| 2026-01-17 | Scenario documented | Canon extraction |

---

**Scenario Status:** ✅ Verified (tested)
**Confidence:** 1.00
**Test Coverage:** ✅ All types explicitly tested
