# Property: Markdown Formatting

**Feature:** context-management
**Category:** LLM Integration
**Source:** src/context.lisp:104-125
**Confidence:** 1.00

---

## Statement

**Context managers format all context items as markdown for LLM consumption via the `context-to-string` method, using fenced code blocks with language tags and metadata annotations.**

---

## Formal Definition

### Format Structure

```
Output = Header + ItemBlock₁ + ItemBlock₂ + ... + ItemBlockₙ

Where:
  Header = "## Context\n\n"
  ItemBlock = TypeHeader + MetadataAnnotation? + CodeFence + Content + CloseFence + "\n\n"
  TypeHeader = "### " + Capitalize(item-type)
  MetadataAnnotation = " (from " + filename + [":" + start-line + "-" + end-line] + ")"
  CodeFence = "```lisp\n"
  Content = item.content
  CloseFence = "```"
```

### Example Output

```markdown
## Context

### Code (from src/agent.lisp:42-58)
```lisp
(defun send-to-agent (instruction)
  (process-with-llm instruction))
```

### Error
```lisp
SIMPLE-ERROR: Unbound variable FOO
  Backtrace: ...
```
```

---

## Implementation

### Method Definition

```lisp
(defmethod context-to-string ((manager context-manager))
  "Format all context items as markdown for LLM."
  (with-output-to-string (s)
    (format s "## Context~%~%")
    (loop for item across (context-items manager)
          for type = (context-item-type item)
          for content = (context-item-content item)
          for metadata = (context-item-metadata item)
          do
          (format s "### ~A"
                  (string-capitalize (symbol-name type)))
          ;; Add metadata to header if present
          (when metadata
            (let ((filename (getf metadata :filename))
                  (start-line (getf metadata :start-line))
                  (end-line (getf metadata :end-line)))
              (when filename
                (format s " (from ~A" filename)
                (when (and start-line end-line)
                  (format s ":~D-~D" start-line end-line))
                (format s ")"))))
          (format s "~%```lisp~%~A~%```~%~%" content))))
```

---

## Formatting Rules

### Rule 1: Markdown Section Structure

**Pattern:** Each context item becomes a markdown section
**Header Level:** Level 3 (`###`)
**Rationale:** Allows "## Context" as parent section

### Rule 2: Type Capitalization

**Transform:** `:code` → "Code", `:error` → "Error", `:repl-history` → "Repl-History"
**Implementation:** `(string-capitalize (symbol-name type))`
**Rationale:** Human-readable headers in generated prompts

### Rule 3: Metadata Annotation

**When Present:**
- Filename only: `### Code (from example.lisp)`
- Filename + lines: `### Code (from example.lisp:10-15)`

**When Absent:**
- No annotation: `### Code`

**Rationale:** Provides source context for LLM to understand provenance

### Rule 4: Language Tag

**Fixed Tag:** Always uses `` ```lisp ``
**Applies To:** All item types (:code, :error, :text, :repl-history, etc.)
**Rationale:** Agent-Q is Lisp-focused; syntax highlighting optimized for Lisp

### Rule 5: Content Preservation

**Transform:** None - content inserted verbatim
**Escaping:** No special escaping (assumes content doesn't contain `` ``` ``)
**Rationale:** Preserve exact code/error text for LLM analysis

---

## Verification

### Test Evidence

**Test:** Implicit in integration tests that verify LLM prompts
**Status:** ⚠️ No explicit unit test for `context-to-string` output format
**Coverage Gap:** Format changes could silently break LLM integration

### Specification Evidence

**Source:** specs/PHASE-1-SPEC.md:165-167
```
Format as markdown with code fences for LLM consumption.
```

**Alignment:** ✅ Code implements markdown formatting as specified

---

## Rationale

### Why Markdown?

1. **LLM-friendly**: Modern LLMs trained on markdown understand structure
2. **Human-readable**: Developers can inspect prompts easily
3. **Structured**: Clear delimiters between context items
4. **Syntax highlighting**: Code fences enable LLM syntax awareness

### Why `` ```lisp `` for All Types?

- **Consistency**: All content treated uniformly
- **Simple**: No logic to map types to language tags
- **Acceptable trade-off**: errors and text content still render readably in `` ```lisp `` fences, and Lisp is the dominant content type

**Trade-off:** Suboptimal for :text type (plain text in `` ```lisp `` fence)

### Why Include Metadata?

- **Provenance**: LLM can reference source files in responses
- **Context**: Line numbers help LLM understand code location
- **Debugging**: Easier to trace where context originated

---

## Behavior Examples

### Example 1: Code Item with Full Metadata

```lisp
(let ((manager (make-context-manager)))
  (add-context manager "(defun foo () 42)"
               :type :code
               :metadata '(:filename "src/example.lisp" :start-line 10 :end-line 12))
  (context-to-string manager))
```

**Output:**
```markdown
## Context

### Code (from src/example.lisp:10-12)
```lisp
(defun foo () 42)
```
```

### Example 2: Error Item Without Metadata

```lisp
(let ((manager (make-context-manager)))
  (add-context manager "ERROR: Division by zero" :type :error)
  (context-to-string manager))
```

**Output:**
```markdown
## Context

### Error
```lisp
ERROR: Division by zero
```
```

### Example 3: Multiple Items

```lisp
(let ((manager (make-context-manager)))
  (add-context manager "(+ 1 2)" :type :code)
  (add-context manager "Result: 3" :type :text)
  (context-to-string manager))
```

**Output:**
```markdown
## Context

### Code
```lisp
(+ 1 2)
```

### Text
```lisp
Result: 3
```
```

---

## Consequences

### Positive

✅ **LLM-optimized**: Markdown structure improves LLM comprehension
✅ **Human-readable**: Developers can inspect prompts
✅ **Provenance tracking**: Metadata preserves source information
✅ **Consistent format**: Predictable structure simplifies testing

### Negative

⚠️ **Lisp-only language tag**: Suboptimal for non-Lisp content
⚠️ **No escaping**: Content with `` ``` `` would break formatting
⚠️ **Token overhead**: Markdown structure adds ~30 tokens per item
⚠️ **Fixed format**: Cannot customize per-item or per-LLM

---

## Edge Cases

### Edge Case 1: Content with Code Fences

```lisp
;; Content contains ``` (would break formatting)
(add-context manager "Example: ```lisp (+ 1 2)```" :type :text)
```

**Behavior:** ⚠️ Undefined - likely breaks markdown parsing
**Workaround:** Escape or sanitize `` ``` `` in content before adding
**Status:** Known limitation, no current handling

### Edge Case 2: Very Long Content

```lisp
;; Item with 10,000 lines of code
(add-context manager very-long-string :type :code)
```

**Behavior:** ✅ Formatted normally, but may exceed LLM token limits
**Impact:** LLM request may fail or truncate
**Workaround:** Limit context item size (50KB limit exists in UI layer)

### Edge Case 3: Empty Manager

```lisp
(let ((manager (make-context-manager)))
  (context-to-string manager))
```

**Output:**
```markdown
## Context

```

**Behavior:** ✅ Valid markdown with empty body

### Edge Case 4: Metadata with Special Characters

```lisp
(add-context manager "code" :type :code
             :metadata '(:filename "path/with spaces.lisp"))
```

**Output:**
```markdown
### Code (from path/with spaces.lisp)
```

**Behavior:** ✅ Special characters preserved (no escaping needed)

---

## LLM Integration

### Typical Prompt Structure

```
<system>
You are a helpful AI assistant for Common Lisp development.
</system>

<context-from-manager>
## Context

### Code (from src/agent.lisp:10-15)
```lisp
(defun foo () 42)
```
</context-from-manager>

<user>
What does this function do?
</user>
```

**Token Estimation:**
- Header: ~2 tokens
- Per item: ~10-20 tokens (structure) + content tokens
- 50 items ≈ 500-1000 structural tokens + content

---

## Related Properties

- **sliding-window.md**: Limits total items formatted (max 50)
- **type-safety.md**: Ensures valid types for capitalization

---

## Known Limitations

### L-001: Fixed Language Tag

**Problem:** Always uses `` ```lisp `` regardless of item type
**Impact:** Suboptimal for :text (plain text), :error (error output)
**Workaround:** Accept suboptimal highlighting for non-code types
**Recommendation:** Map types to language tags:
  - `:code` → `` ```lisp ``
  - `:error` → `` ```text ``
  - `:text` → `` ```text ``

### L-002: No Content Escaping

**Problem:** Content with `` ``` `` breaks markdown structure
**Impact:** Rare but possible formatting corruption
**Workaround:** Sanitize content before adding
**Recommendation:** Escape `` ``` `` as `` `\`\` `` in content

### L-003: No Format Customization

**Problem:** Cannot customize markdown format per LLM provider
**Impact:** May not be optimal for all LLMs (e.g., Claude vs GPT-4)
**Status:** Acceptable - markdown is universal enough

### L-004: Token Overhead

**Problem:** Markdown structure adds 10-20 tokens per item
**Impact:** Reduces effective context capacity
**Calculation:** 50 items × 20 tokens = 1000 tokens overhead
**Status:** Acceptable trade-off for structure

---

## Future Enhancements

### Enhancement 1: Dynamic Language Tags

Map context types to language tags: :code → `` ```lisp ``, :error → `` ```text ``, :file → auto-detect from extension, :text → `` ```markdown ``:
```lisp
(defun type-to-lang-tag (type)
  (case type
    (:code "lisp")
    (:error "text")
    (:text "text")
    (:repl-history "lisp")
    (otherwise "text")))
```

### Enhancement 2: Token-Aware Truncation

Truncate content to fit LLM context limits:
```lisp
(when (> (token-count content) max-tokens-per-item)
  (setf content (truncate-content content max-tokens-per-item)))
```

### Enhancement 3: Alternative Formats

Support XML or JSON format for LLMs that prefer structured data:
```lisp
(defgeneric context-to-format (manager format))
(defmethod context-to-format ((manager context-manager) (format (eql :markdown))) ...)
(defmethod context-to-format ((manager context-manager) (format (eql :xml))) ...)
```

---

## Change History

| Date | Change | Rationale |
|------|--------|-----------|
| 2025-12-31 | Markdown formatting implemented | Phase 1 foundation |
| 2026-01-17 | Property documented | Canon extraction |

---

**Property Status:** ✅ Verified (code inspection + spec alignment)
**Confidence:** 1.00
**Test Coverage:** ⚠️ Implicit (no explicit format tests)
