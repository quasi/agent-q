# Per-Hunk Diff Approval Implementation

**Version**: 0.3.0
**Component**: sly-agent-q-diff.el + tools/diff.lisp
**Purpose**: Granular review and application of file changes proposed by LLM agents

---

## Terminology

**MUST** use these terms consistently. No synonyms.

- **hunk**: A contiguous block of changes in unified diff format. Delimited by `@@` header lines.
- **state**: One of `applied`, `rejected`, `nil` (pending). Tracked per hunk position.
- **overlay**: Emacs overlay object providing visual feedback on hunk text.
- **diff buffer**: Temporary buffer displaying unified diff in `sly-agent-q-diff-mode`.
- **decision**: String returned to Lisp side: `"accepted"`, `"rejected"`, or error message.
- **tool call**: LLM requests execution of `propose_file_edit` with path/original/modified/description.
- **recursive-edit**: Emacs mechanism blocking execution until user exits with decision.
- **diff-mode infrastructure**: Built-in Emacs diff-mode.el functions for parsing and applying diffs.
- **blocking call**: Synchronous RPC from Lisp → Emacs that waits for user input.

---

## Architecture

### Component Interaction

```
LLM Agent (via cl-llm-provider)
  ↓ tool_use request
Common Lisp Tool (tools/diff.lisp)
  ↓ eval-in-emacs (sly-agent-q-show-diff-and-wait ...)
Emacs Entry Point (sly-agent-q-diff.el)
  ↓ generate unified diff
  ↓ create diff buffer
  ↓ enter recursive-edit (BLOCKS)
User Reviews Hunks
  ↓ press 'a' or 'r' on each hunk
  ↓ press 'q' to finish
Emacs Exit Point
  ↓ return "accepted" or "rejected"
Common Lisp Tool
  ↓ format success/rejection message
LLM Agent
  ↓ observe result, continue or stop
```

### Data Flow: Tool Call to Result

1. **Lisp invokes tool**: `(propose-file-edit :path "foo.lisp" :original "..." :modified "..." :description "Add docstrings")`
2. **Tool executes**: Calls `(eval-in-emacs '(sly-agent-q-show-diff-and-wait ...))`
3. **Emacs generates diff**: `(sly-agent-q-diff--generate-unified-diff original modified path)`
4. **Emacs creates UI**: Buffer in `sly-agent-q-diff-mode`, enters `recursive-edit`
5. **User acts**: Navigates hunks with `n`/`p`, accepts with `a`, rejects with `r`, quits with `q`
6. **Emacs returns**: Decision string flows back through `eval-in-emacs` to Lisp
7. **Tool formats result**: Returns formatted message to LLM based on decision

---

## Data Structures

### Buffer-Local State Variables

All state is `defvar-local` in the diff buffer:

| Variable | Type | Purpose |
|----------|------|---------|
| `sly-agent-q-diff--path` | string | Absolute path to file being edited |
| `sly-agent-q-diff--original` | string | Expected original content (verification) |
| `sly-agent-q-diff--modified` | string | Proposed new content |
| `sly-agent-q-diff--description` | string | Explanation of changes |
| `sly-agent-q-diff--decision` | string | Decision to return: "accepted" or "rejected" |
| `sly-agent-q-diff--hunk-states` | alist | Maps hunk position → state (applied/rejected/nil) |

### Hunk State Machine

```
nil (pending)
  ↓ user presses 'a'
applied
  ↓ user presses SPC (toggle)
nil (pending)

nil (pending)
  ↓ user presses 'r'
rejected
  ↓ user presses SPC (toggle)
applied (attempts to apply)
```

**State transitions**:
- `nil → applied`: Press `a`, executes `diff-apply-hunk`
- `nil → rejected`: Press `r`, marks state only (no file change)
- `applied → nil`: Press `SPC` (visual toggle, does NOT unapply from file)
- `rejected → applied`: Press `SPC`, attempts `diff-apply-hunk`

**CRITICAL**: Applied hunks CANNOT be unapplied. File changes are immediate and persistent.

---

## Tool Protocol

### Lisp Side: propose_file_edit

**Tool Definition** (src/tools/diff.lisp):

```lisp
(define-tool
  "propose_file_edit"
  "Propose changes to a file as a diff for user review."
  '((:name "path" :type :string :description "File path")
    (:name "original" :type :string :description "Expected original content")
    (:name "modified" :type :string :description "Proposed new content")
    (:name "description" :type :string :description "What changed and why"))
  :required '("path" "original" "modified" "description")
  :safety-level :moderate)
```

**Handler Flow**:

1. Extract args: `path`, `original`, `modified`, `description`
2. Call: `(eval-in-emacs '(sly-agent-q-show-diff-and-wait ,path ,original ,modified ,description))`
3. Block until Emacs returns decision string
4. Match decision:
   - `"accepted"` → Return success message
   - `"rejected"` → Return rejection message with **STOP** directive
   - Other → Return error message

**Decision String Semantics**:

| Decision | Meaning | LLM Instruction |
|----------|---------|-----------------|
| `"accepted"` | User applied all or some hunks | Continue, file modified |
| `"rejected"` | User rejected all hunks | **STOP**, do NOT retry |
| Other | Error or unexpected | Report issue, do not proceed |

**CRITICAL**: `"rejected"` includes explicit instruction for LLM to STOP and ask user for direction. Prevents retry loops.

### Emacs Side: Entry and Exit

**Entry Point**: `sly-agent-q-show-diff-and-wait`

```elisp
(defun sly-agent-q-show-diff-and-wait (path original modified description)
  "Display diff buffer, enter recursive-edit, return decision."
  (let ((diff-text (sly-agent-q-diff--generate-unified-diff
                    original modified (expand-file-name path))))
    (with-current-buffer (get-buffer-create "*Agent-Q Diff*")
      ;; Insert header and diff
      ;; Set buffer-local state
      ;; Enter sly-agent-q-diff-mode
      ;; Call recursive-edit (BLOCKS)
      ;; Return sly-agent-q-diff--decision)))
```

**Exit Points**:

| Command | Function | Sets Decision | Exits Recursive-Edit |
|---------|----------|---------------|----------------------|
| `C-c C-c` | `sly-agent-q-diff-accept-all` | `"accepted"` | Yes |
| `C-c C-k` | `sly-agent-q-diff-reject-all` | `"rejected"` | Yes |
| `q` | `sly-agent-q-diff-finish` | Based on states | Yes |

**`sly-agent-q-diff-finish` Decision Logic**:

```elisp
(cond
  ((> applied 0) "accepted")  ; At least one hunk applied
  (t "rejected"))             ; Zero hunks applied
```

---

## Function Catalog

### Core Functions

| Function | Purpose | Side Effects |
|----------|---------|--------------|
| `sly-agent-q-show-diff-and-wait` | Entry point from Lisp | Opens diff buffer, blocks |
| `sly-agent-q-diff--generate-unified-diff` | Create diff text | None (pure) |
| `sly-agent-q-diff-accept-hunk` | Apply current hunk | Modifies target file buffer |
| `sly-agent-q-diff-reject-hunk` | Mark hunk rejected | Updates state alist |
| `sly-agent-q-diff-toggle-hunk` | Toggle hunk state | May apply hunk to file |
| `sly-agent-q-diff-preview-hunk` | Jump to source | Switches windows |
| `sly-agent-q-diff-finish` | Exit with decision | Exits recursive-edit |
| `sly-agent-q-diff--update-header` | Refresh progress | Modifies buffer text |
| `sly-agent-q-diff--apply-visual-feedback` | Add overlay | Creates/deletes overlays |

### Diff Generation

**Signature**:
```elisp
(sly-agent-q-diff--generate-unified-diff original modified file-path)
  → string (unified diff format)
```

**Implementation**:
1. Create temp file with original content
2. Create temp buffer with modified content
3. Call `diff-no-select` (generates unified diff)
4. Extract diff text from buffer
5. Clean up temp file

**Output Format**:
```diff
--- /abs/path/to/file.lisp
+++ /abs/path/to/file.lisp
@@ -10,3 +10,4 @@
 (defun foo ()
+  "Docstring."
   42)
```

### Hunk Application

**Function**: `sly-agent-q-diff-accept-hunk`

**Flow**:
1. Find file buffer (open with `find-file-noselect` if needed)
2. Position point on hunk header (`@@`)
3. Call `diff-apply-hunk` (built-in from diff-mode.el)
4. Check result (success/failure)
5. Update `sly-agent-q-diff--hunk-states` → `'applied`
6. Call `sly-agent-q-diff--apply-visual-feedback`
7. Update header with `sly-agent-q-diff--update-header`
8. Save target buffer silently (suppress auto-save prompts)

**Error Handling**:
- `diff-apply-hunk` signals error if context doesn't match → Catch with `condition-case`
- Display error message: `"Failed to apply hunk: <reason>"`
- Hunk remains in `nil` (pending) state

### State Tracking

**Alist Structure**: `sly-agent-q-diff--hunk-states`

```elisp
;; Example:
((450 . applied)    ; Hunk at position 450 was applied
 (720 . rejected)   ; Hunk at position 720 was rejected
 (990 . nil))       ; Hunk at position 990 is pending
```

**Key**: Buffer position of `@@` hunk header
**Value**: `'applied`, `'rejected`, or `nil`

**Lookup**:
```elisp
(alist-get hunk-start sly-agent-q-diff--hunk-states)
```

**Update**:
```elisp
(setf (alist-get hunk-start sly-agent-q-diff--hunk-states) 'applied)
```

---

## Integration with diff-mode.el

### Functions Used

| diff-mode Function | Purpose | Called By |
|--------------------|---------|-----------|
| `diff-apply-hunk` | Apply single hunk to target file | `sly-agent-q-diff-accept-hunk` |
| `diff-apply-buffer` | Apply all hunks in buffer | `sly-agent-q-diff-accept-all` |
| `diff-hunk-next` | Navigate to next hunk | `sly-agent-q-diff-next-hunk` |
| `diff-hunk-prev` | Navigate to previous hunk | `sly-agent-q-diff-previous-hunk` |
| `diff-goto-source` | Jump to source location | `sly-agent-q-diff-preview-hunk` |
| `diff-no-select` | Generate diff text | `sly-agent-q-diff--generate-unified-diff` |

### Why diff-mode Infrastructure?

**Replaced**: Manual string-matching and line-offset calculation (fragile, error-prone)

**Advantages**:
- Handles whitespace variations automatically
- Adjusts for context shifts (nearby code changes)
- Robust hunk parsing (no regex brittleness)
- Built-in error handling for mismatched context
- Standard unified diff format compatibility

---

## Invariants

These MUST hold at all times:

### INV-001: Path Absoluteness
`sly-agent-q-diff--path` MUST be an absolute path.

**Check**: `(file-name-absolute-p sly-agent-q-diff--path)`

**Enforcement**: `sly-agent-q-show-diff-and-wait` calls `(expand-file-name path)` on entry.

### INV-002: Decision Values
`sly-agent-q-diff--decision` ∈ `{"accepted", "rejected"}`

**Check**: `(member sly-agent-q-diff--decision '("accepted" "rejected") :test #'string=)`

**Enforcement**: Only set by accept-all, reject-all, or finish functions.

### INV-003: Hunk State Validity
`∀ (pos . state) ∈ sly-agent-q-diff--hunk-states: state ∈ {applied, rejected, nil}`

**Check**: `(every (lambda (pair) (member (cdr pair) '(applied rejected nil))) sly-agent-q-diff--hunk-states)`

### INV-004: Applied Hunks Are Irreversible
Once `state = 'applied`, the target file buffer MUST contain the hunk's changes.

**Note**: Toggling applied → nil only changes visual state; file remains modified.

### INV-005: Buffer Locality
All `sly-agent-q-diff--*` variables are buffer-local to the diff buffer.

**Enforcement**: `defvar-local` declarations.

### INV-006: Diff Buffer Name
Diff buffer MUST be named `"*Agent-Q Diff*"`.

**Check**: `(string= (buffer-name) "*Agent-Q Diff*")`

---

## Common Patterns

### PATTERN-001: Accept Hunk

**Scenario**: User wants to apply a specific hunk to the file.

**Complete Example**:

```elisp
(defun sly-agent-q-diff-accept-hunk ()
  "Apply current hunk to target file."
  (interactive)
  (let ((hunk-start (save-excursion
                      (diff-beginning-of-hunk)
                      (point))))
    ;; Open target file buffer (or find existing)
    (let ((file-buffer (find-file-noselect
                        (expand-file-name sly-agent-q-diff--path))))
      (condition-case err
          (progn
            ;; Apply hunk using diff-mode infrastructure
            (diff-apply-hunk)
            ;; Update state
            (setf (alist-get hunk-start sly-agent-q-diff--hunk-states) 'applied)
            ;; Visual feedback
            (sly-agent-q-diff--apply-visual-feedback hunk-start 'applied)
            ;; Save file buffer (suppress auto-save prompts)
            (with-current-buffer file-buffer
              (let ((buffer-save-without-query t))
                (save-buffer)))
            ;; Update progress header
            (sly-agent-q-diff--update-header)
            (message "Hunk applied"))
        (error
         (message "Failed to apply hunk: %s" (error-message-string err)))))))
```

**Rules Satisfied**: INV-001 (absolute path), INV-004 (applied is irreversible)

**Why This Shape**:
- `find-file-noselect` opens buffer without switching windows
- `condition-case` catches `diff-apply-hunk` failures gracefully
- `buffer-save-without-query` prevents interactive prompts during batch operations
- Visual feedback called AFTER successful application (consistent state)

### PATTERN-002: Navigation with State Awareness

**Scenario**: User navigates to next hunk, wants visual indication of state.

**Complete Example**:

```elisp
(defun sly-agent-q-diff-next-hunk (arg)
  "Move to next hunk and show its state."
  (interactive "p")
  (diff-hunk-next arg)
  (let* ((hunk-start (save-excursion
                       (diff-beginning-of-hunk)
                       (point)))
         (state (alist-get hunk-start sly-agent-q-diff--hunk-states)))
    (pcase state
      ('applied (message "Hunk applied"))
      ('rejected (message "Hunk rejected"))
      (_ (message "Hunk pending")))))
```

**Why This Shape**:
- Delegates navigation to `diff-hunk-next` (don't reimplement)
- Reads state AFTER navigation completes (current position is stable)
- Uses `pcase` for clean state matching

### PATTERN-003: Header Update

**Scenario**: After state change, update progress display.

**Complete Example**:

```elisp
(defun sly-agent-q-diff--update-header ()
  "Update diff header with current progress."
  (let* ((states (mapcar #'cdr sly-agent-q-diff--hunk-states))
         (total (length states))
         (applied (cl-count 'applied states))
         (rejected (cl-count 'rejected states))
         (pending (- total applied rejected)))
    (save-excursion
      (goto-char (point-min))
      ;; Find progress line
      (when (re-search-forward "^Progress:" nil t)
        (let ((inhibit-read-only t))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (format "Progress: %d/%d applied, %d rejected, %d pending  |  %s"
                          applied total rejected pending
                          "[a]ccept [r]eject [n]ext [p]rev [q]uit")))))))
```

**Why This Shape**:
- Derive counts from `sly-agent-q-diff--hunk-states` (single source of truth)
- Use `inhibit-read-only` only within narrowest scope (safety)
- Search for existing line instead of assuming position (robust to header format changes)

---

## Edge Cases

### EDGE-001: Concurrent File Modification

**Scenario**: User edits the target file in another buffer while reviewing diff.

**What happens**:
1. Diff generated at time T0 with original content
2. User switches to file buffer, makes edit at time T1
3. User returns to diff buffer, tries to apply hunk at position P
4. `diff-apply-hunk` attempts to match context lines
5. Context mismatch → error signaled

**Idiomatic handling**:

```elisp
;; In sly-agent-q-diff-accept-hunk
(condition-case err
    (diff-apply-hunk)
  (error
   (message "Failed to apply hunk: %s" (error-message-string err))))
```

**Why not**:
- Lock file during review: Prevents legitimate concurrent work
- Auto-refresh diff: Confusing UX, invalidates reviewed hunks
- Retry silently: Hides error from user

### EDGE-002: Empty Hunks or Whitespace-Only Changes

**Scenario**: Diff contains hunk with only whitespace changes.

**What happens**:
1. `diff-no-select` generates hunk (whitespace is significant in unified diff)
2. User accepts hunk
3. `diff-apply-hunk` applies whitespace changes normally
4. Target buffer modified (whitespace changed)

**Idiomatic handling**:
No special case needed. Whitespace changes are valid edits. `diff-apply-hunk` handles correctly.

**Why not**:
- Filter whitespace hunks: User may want to normalize whitespace
- Warn on whitespace-only: Noisy, not actually an error

### EDGE-003: Unapply Limitation

**Scenario**: User accepts hunk (applied), then changes mind.

**What happens**:
1. Hunk applied to file buffer (file content changed)
2. User presses `SPC` to toggle state → state changes to `nil`
3. Visual overlay removed
4. **File content REMAINS modified** (no unapply operation)

**Why this limitation**:
- `diff-apply-hunk` is one-way (no inverse operation in diff-mode.el)
- Computing inverse is complex (would need to generate reverse diff)
- User has standard undo: `C-x b <file> C-/` works normally

**Documented behavior**:
> "Applied hunks cannot be unapplied. Use regular Emacs undo in the target buffer."

### EDGE-004: Multi-File Diff Sequence

**Scenario**: Agent proposes changes to multiple files in sequence.

**What happens**:
1. Agent calls `propose_file_edit` for file A
2. Emacs displays diff for A, blocks
3. User reviews, presses `q`
4. Emacs returns decision for A
5. Agent observes result, calls `propose_file_edit` for file B
6. Emacs displays diff for B (reuses `*Agent-Q Diff*` buffer)
7. [repeat]

**Buffer reuse**:
`(get-buffer-create "*Agent-Q Diff*")` reuses existing buffer. Old content is erased on new diff display.

**State isolation**:
Each diff session has fresh buffer-local variables (set on entry to `sly-agent-q-show-diff-and-wait`).

### EDGE-005: Single-Hunk Diff

**Scenario**: Diff contains only one hunk.

**What happens**:
- Navigation commands (`n`/`p`) move within single hunk (no-op if already positioned)
- Accept/reject works normally
- Progress shows "1/1 applied" or "0/1 pending"
- Legacy `C-c C-c` equivalent to `a` (both apply all hunks)

**No special case needed**. Single-hunk is just `total=1`.

---

## Anti-Patterns

### ANTI-001: Calling diff-apply-hunk Outside diff-mode

**Description**: Attempting to use `diff-apply-hunk` in a buffer not derived from `diff-mode`.

**Symptoms**:
- Error: `diff-find-source-location: Wrong type argument: stringp, nil`
- Hunk not applied
- Corrupt state

**Why harmful**: `diff-apply-hunk` relies on diff-mode's syntax parsing and buffer-local variables.

**Remediation**: Always ensure `(derived-mode-p 'diff-mode)` before calling `diff-apply-hunk`.

**Agent action**: Flag for human review. Auto-fix forbidden.

### ANTI-002: Modifying diff--hunk-states Without Visual Update

**Description**: Changing `sly-agent-q-diff--hunk-states` directly without calling `sly-agent-q-diff--apply-visual-feedback`.

**Symptoms**:
- State is correct internally
- Visual display (overlays) out of sync
- User confusion (hunk looks pending but state says applied)

**Why harmful**: Visual feedback is primary UX signal. Desynced state is user-hostile.

**Remediation**: ALWAYS call `sly-agent-q-diff--apply-visual-feedback` after state change.

**Agent action**: Auto-fix allowed (add visual update call).

### ANTI-003: Returning Non-String Decision

**Description**: Setting `sly-agent-q-diff--decision` to non-string value (e.g., symbol, nil).

**Symptoms**:
- Lisp side receives unexpected type
- Tool handler crashes or returns malformed result
- LLM receives error instead of decision

**Why harmful**: Breaks tool protocol contract. Lisp side expects string.

**Remediation**: Always use `"accepted"` or `"rejected"` (strings with quotes).

**Agent action**: Auto-fix allowed (wrap in `(format nil "~A" decision)`).

---

## Testing Strategy

### Unit Tests (6 tests)

**Scope**: Individual functions in isolation.

**Approach**: Mock dependencies, verify return values and state changes.

**Example Test**:

```elisp
(ert-deftest sly-agent-q-diff-test-accept-hunk ()
  "Test hunk acceptance updates state correctly."
  (with-temp-buffer
    (insert "--- a/test.lisp\n"
            "+++ b/test.lisp\n"
            "@@ -1,1 +1,2 @@\n"
            " line1\n"
            "+line2\n")
    (sly-agent-q-diff-mode)
    (goto-char (point-min))
    (re-search-forward "^@@")
    (beginning-of-line)
    (let ((pos (point)))
      ;; Simulate state change (no real file operation in unit test)
      (setf (alist-get pos sly-agent-q-diff--hunk-states) 'applied)
      (should (eq 'applied (alist-get pos sly-agent-q-diff--hunk-states))))))
```

**Coverage**:
- Generate diff
- Count hunks
- Accept hunk (state update)
- Reject hunk (state update)
- Toggle hunk (state transitions)
- Finish (decision logic)

### Integration Tests (4 tests)

**Scope**: End-to-end workflows with real temp files.

**Approach**: Create temp file, generate diff, apply hunks, verify file contents.

**Example Test**:

```elisp
(ert-deftest sly-agent-q-diff-integration-multi-hunk ()
  "Integration test: Apply first hunk only, verify selective application."
  (let* ((test-file (make-temp-file "agent-q-test-" nil ".lisp"))
         (original ";;;; test.lisp\n\n(defun foo ()\n  42)\n\n;; filler...\n\n(defun bar ()\n  99)\n")
         (modified ";;;; test.lisp\n\n(defun foo ()\n  \"Docstring.\"\n  42)\n\n;; filler...\n\n(defun bar ()\n  \"Another.\"\n  99)\n"))
    (unwind-protect
        (progn
          (with-temp-file test-file (insert original))
          (let ((diff-text (sly-agent-q-diff--generate-unified-diff
                            original modified test-file)))
            (with-temp-buffer
              (insert diff-text)
              (sly-agent-q-diff-mode)
              (setq sly-agent-q-diff--path test-file)
              ;; Apply first hunk only
              (goto-char (point-min))
              (re-search-forward "^@@")
              (diff-apply-hunk)
              ;; Verify file has first change, not second
              (with-current-buffer (find-buffer-visiting test-file)
                (should (string-match-p "\"Docstring.\"" (buffer-string)))
                (should-not (string-match-p "\"Another.\"" (buffer-string)))))))
      (delete-file test-file))))
```

**Coverage**:
- Multi-hunk selective application
- All-accept legacy mode
- Toggle hunk state
- Header update with real counts

### Manual Testing (TESTING-CHECKLIST.md)

**Scope**: UX workflows, edge cases, performance.

**Approach**: Human tester follows checklist with real Emacs session.

**Categories**:
- Single-hunk diff
- Multi-hunk accept/reject
- Preview source location
- Toggle state transitions
- Help display
- Unreviewed hunks warning
- Legacy all-or-nothing commands
- Apply failure handling
- Multi-file diff sequence
- Visual feedback
- Performance (10-hunk, 50-hunk diffs)
- Regression tests

---

## Verification Checklist

Before claiming diff implementation complete:

- [ ] All 10 ERT tests passing
- [ ] Diff generates valid unified format
- [ ] Hunks apply correctly via `diff-apply-hunk`
- [ ] State tracked per hunk position
- [ ] Visual feedback (overlays) match state
- [ ] Progress header updates in real-time
- [ ] Keybindings respond correctly (a/r/SPC/n/p/RET/q/?)
- [ ] Decision string ("accepted"/"rejected") returned to Lisp
- [ ] Tool handler formats success/rejection messages
- [ ] LLM receives STOP directive on rejection
- [ ] Backward compatibility: `C-c C-c` / `C-c C-k` still work
- [ ] No regression: Single-hunk diffs still functional
- [ ] Documentation: DIFF-REVIEW-GUIDE.md complete
- [ ] Manual testing checklist completed

---

## References

**Implementation Files**:
- `contrib/sly-agent-q/sly-agent-q-diff.el` (495 lines)
- `src/tools/diff.lisp` (60 lines)

**Documentation**:
- `docs/DIFF-REVIEW-GUIDE.md` (user-facing guide)
- `contrib/sly-agent-q/TESTING-CHECKLIST.md` (manual testing procedures)
- `CHANGELOG.md` (version 0.3.0 entry)

**Tests**:
- `contrib/sly-agent-q/test/sly-agent-q-diff-test.el` (6 unit tests)
- `contrib/sly-agent-q/test/sly-agent-q-diff-integration-test.el` (4 integration tests)

**Dependencies**:
- Emacs 27.1+ (built-in diff-mode.el)
- SLY (for eval-in-emacs RPC)
