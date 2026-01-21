# Manual Testing Guide: File Lifecycle Tools

**Purpose**: Verify that `create_file`, `move_file`, and `delete_file` tools work correctly in Agent-Q.

**Time required**: ~15 minutes

**Target audience**: Developers and testers verifying the Phase 4 implementation.

---

## Prerequisites

Before testing, ensure you have:

- [ ] Agent-Q loaded in Emacs with SLY connection active
- [ ] Git repository with a clean working directory
- [ ] Write permissions in your project directory
- [ ] Test project root set (verify with `get_project_root` tool)

### Setup Verification

**Step 1**: Start Agent-Q session in Emacs

```elisp
M-x sly
;; Wait for REPL to start
(ql:quickload "agent-q")
(agent-q:configure :provider :anthropic
                   :model "claude-sonnet-4-20250514")
```

**Step 2**: Verify tools are registered

```lisp
;; In the SLY REPL
(length (agent-q.tools:get-agent-q-tools :max-safety-level :moderate))
;; Expected: Should show >= 10 tools
```

**Step 3**: Create test directory

```bash
# In terminal, in your project root
mkdir -p test-lifecycle-tools/subdir
cd test-lifecycle-tools
```

✅ **Checkpoint**: You should now have an empty `test-lifecycle-tools/` directory.

---

## Test Suite

### Test 1: create_file - Basic File Creation

**Goal**: Verify `create_file` creates a new file with content.

**Steps**:

1. Ask Agent-Q to create a test file:
   ```
   Use the create_file tool to create a file at "test-lifecycle-tools/test1.txt"
   with content "Hello from create_file test"
   ```

2. **Expected behavior**:
   - Tool should succeed
   - Response includes: "✓ Created file: test-lifecycle-tools/test1.txt"
   - Response shows content size and line count

3. **Verify**:
   ```bash
   cat test-lifecycle-tools/test1.txt
   # Expected output: Hello from create_file test
   ```

✅ **Pass criteria**: File exists with correct content.

---

### Test 2: create_file - Overwrite Protection

**Goal**: Verify `create_file` prevents accidental overwrites.

**Steps**:

1. Try to create the same file again:
   ```
   Use create_file to create "test-lifecycle-tools/test1.txt"
   with content "This should fail"
   ```

2. **Expected behavior**:
   - Tool should fail
   - Error message: "Error: File 'test-lifecycle-tools/test1.txt' already exists."
   - Error suggests: "Use allow_overwrite: true to replace it."

3. **Verify file unchanged**:
   ```bash
   cat test-lifecycle-tools/test1.txt
   # Expected: Still says "Hello from create_file test"
   ```

✅ **Pass criteria**: File NOT overwritten, clear error message received.

---

### Test 3: create_file - Allow Overwrite

**Goal**: Verify explicit overwrite works.

**Steps**:

1. Create file with overwrite flag:
   ```
   Use create_file to create "test-lifecycle-tools/test1.txt"
   with content "Overwritten content" and allow_overwrite set to true
   ```

2. **Expected behavior**:
   - Tool should succeed
   - Response shows file was created

3. **Verify**:
   ```bash
   cat test-lifecycle-tools/test1.txt
   # Expected output: Overwritten content
   ```

✅ **Pass criteria**: File successfully overwritten with new content.

---

### Test 4: create_file - Parent Directory Creation

**Goal**: Verify automatic parent directory creation.

**Steps**:

1. Create file in non-existent directory:
   ```
   Use create_file to create "test-lifecycle-tools/deep/nested/dirs/test2.txt"
   with content "Testing parent creation"
   ```

2. **Expected behavior**:
   - Tool should succeed (creates parent directories automatically)
   - Response confirms file creation

3. **Verify directory structure**:
   ```bash
   ls -R test-lifecycle-tools/deep/
   # Expected: Shows nested/dirs/ structure with test2.txt

   cat test-lifecycle-tools/deep/nested/dirs/test2.txt
   # Expected output: Testing parent creation
   ```

✅ **Pass criteria**: File created with full directory path auto-created.

---

### Test 5: move_file - Basic Rename

**Goal**: Verify `move_file` renames files correctly.

**Steps**:

1. Rename a file:
   ```
   Use move_file to move "test-lifecycle-tools/test1.txt"
   to "test-lifecycle-tools/renamed.txt"
   ```

2. **Expected behavior**:
   - Tool should succeed
   - Response: "✓ Moved: test-lifecycle-tools/test1.txt → test-lifecycle-tools/renamed.txt"

3. **Verify**:
   ```bash
   ls test-lifecycle-tools/
   # Expected: renamed.txt exists, test1.txt does NOT exist

   cat test-lifecycle-tools/renamed.txt
   # Expected: Contains "Overwritten content" from Test 3
   ```

✅ **Pass criteria**: File renamed, original name gone, content preserved.

---

### Test 6: move_file - Move to Different Directory

**Goal**: Verify `move_file` moves files between directories.

**Steps**:

1. Move file to subdirectory:
   ```
   Use move_file to move "test-lifecycle-tools/renamed.txt"
   to "test-lifecycle-tools/subdir/moved.txt"
   ```

2. **Expected behavior**:
   - Tool should succeed
   - Confirmation message shown

3. **Verify**:
   ```bash
   ls test-lifecycle-tools/
   # Expected: renamed.txt does NOT exist

   ls test-lifecycle-tools/subdir/
   # Expected: moved.txt exists

   cat test-lifecycle-tools/subdir/moved.txt
   # Expected: Same content as before
   ```

✅ **Pass criteria**: File successfully moved to new directory.

---

### Test 7: move_file - Overwrite Protection

**Goal**: Verify `move_file` prevents overwriting existing files.

**Steps**:

1. Create a target file that will conflict:
   ```
   Use create_file to create "test-lifecycle-tools/target.txt"
   with content "I should not be overwritten"
   ```

2. Create a source file:
   ```
   Use create_file to create "test-lifecycle-tools/source.txt"
   with content "Trying to overwrite"
   ```

3. Try to move source to target:
   ```
   Use move_file to move "test-lifecycle-tools/source.txt"
   to "test-lifecycle-tools/target.txt"
   ```

4. **Expected behavior**:
   - Tool should fail
   - Error: "Error: Destination file 'test-lifecycle-tools/target.txt' already exists."
   - Suggests using `allow_overwrite: true`

5. **Verify both files unchanged**:
   ```bash
   cat test-lifecycle-tools/source.txt
   # Expected: Trying to overwrite

   cat test-lifecycle-tools/target.txt
   # Expected: I should not be overwritten
   ```

✅ **Pass criteria**: Move blocked, both files preserved, clear error.

---

### Test 8: move_file - Source Validation

**Goal**: Verify `move_file` checks source file exists.

**Steps**:

1. Try to move non-existent file:
   ```
   Use move_file to move "test-lifecycle-tools/does-not-exist.txt"
   to "test-lifecycle-tools/anywhere.txt"
   ```

2. **Expected behavior**:
   - Tool should fail
   - Error: "Error: Source file 'test-lifecycle-tools/does-not-exist.txt' does not exist"

✅ **Pass criteria**: Clear error for missing source file.

---

### Test 9: delete_file - Basic Deletion

**Goal**: Verify `delete_file` removes files.

**Steps**:

1. Delete a file:
   ```
   Use delete_file to delete "test-lifecycle-tools/source.txt"
   ```

2. **Expected behavior**:
   - Tool should succeed
   - Response: "✓ Deleted: test-lifecycle-tools/source.txt"
   - Shows size freed

3. **Verify**:
   ```bash
   ls test-lifecycle-tools/source.txt
   # Expected: "No such file or directory"
   ```

✅ **Pass criteria**: File successfully deleted.

---

### Test 10: delete_file - Non-existent File

**Goal**: Verify `delete_file` handles missing files gracefully.

**Steps**:

1. Try to delete non-existent file:
   ```
   Use delete_file to delete "test-lifecycle-tools/never-existed.txt"
   ```

2. **Expected behavior**:
   - Tool should fail
   - Error: "Error: File 'test-lifecycle-tools/never-existed.txt' does not exist"

✅ **Pass criteria**: Clear error for missing file.

---

### Test 11: delete_file - Directory Rejection

**Goal**: Verify `delete_file` rejects directories.

**Steps**:

1. Try to delete a directory:
   ```
   Use delete_file to delete "test-lifecycle-tools/subdir"
   ```

2. **Expected behavior**:
   - Tool should fail
   - Error: "Error: 'test-lifecycle-tools/subdir' is a directory. Use a directory deletion tool instead."

3. **Verify directory still exists**:
   ```bash
   ls -d test-lifecycle-tools/subdir
   # Expected: Directory still exists
   ```

✅ **Pass criteria**: Directory deletion blocked with helpful error.

---

### Test 12: Security - Path Validation (All Tools)

**Goal**: Verify all tools reject paths outside project root.

**Steps**:

1. Try to create file outside project:
   ```
   Use create_file to create "../../etc/malicious.txt"
   with content "This should fail"
   ```

2. **Expected behavior**:
   - Tool should fail
   - Error: "Error: Path '../../etc/malicious.txt' is outside project root"

3. **Repeat for other tools**:
   ```
   Use move_file to move "test-lifecycle-tools/target.txt" to "/tmp/evil.txt"
   # Expected: "Error: Destination path '/tmp/evil.txt' is outside project root"

   Use delete_file to delete "../../important-file.txt"
   # Expected: "Error: Path '../../important-file.txt' is outside project root"
   ```

✅ **Pass criteria**: All three tools reject external paths with clear errors.

---

### Test 13: Buffer Synchronization (If Using Emacs)

**Goal**: Verify tools update open Emacs buffers correctly.

**Steps**:

1. Open a file in Emacs:
   ```elisp
   C-x C-f test-lifecycle-tools/buffer-test.txt
   ;; Type some content: "Original buffer content"
   C-x C-s  ;; Save
   ```

2. Modify file via Agent-Q:
   ```
   Use create_file with allow_overwrite true to create
   "test-lifecycle-tools/buffer-test.txt" with content "Modified by Agent-Q"
   ```

3. **Check Emacs buffer**:
   - Switch to the `buffer-test.txt` buffer in Emacs
   - Expected: Buffer should show "Modified by Agent-Q"
   - Buffer should NOT show as modified (no asterisk)

4. Test move operation:
   ```
   Use move_file to move "test-lifecycle-tools/buffer-test.txt"
   to "test-lifecycle-tools/moved-buffer.txt"
   ```

5. **Check Emacs buffer**:
   - Buffer name should update to `moved-buffer.txt`
   - Content should be preserved

6. Test deletion:
   ```
   Use delete_file to delete "test-lifecycle-tools/moved-buffer.txt"
   ```

7. **Check Emacs**:
   - Buffer should be killed (closed)
   - Attempting to switch to it should fail

✅ **Pass criteria**: All buffer operations synchronized correctly.

---

## Cleanup

After testing, remove the test directory:

```bash
rm -rf test-lifecycle-tools/
```

Or use Agent-Q:
```
Use delete_file to clean up all files in test-lifecycle-tools/,
then manually remove the directory
```

---

## Troubleshooting

### Issue: "Tool not found"

**Cause**: Tools not registered or wrong safety level queried.

**Fix**:
```lisp
;; Verify tools are loaded
(agent-q.tools:get-agent-q-tools :max-safety-level :moderate)
;; Should include create_file, move_file, delete_file
```

### Issue: "Path is outside project root"

**Cause**: `*project-root*` not set correctly.

**Fix**:
```lisp
;; Check current project root
(agent-q:get-project-root)

;; Set manually if needed
(setf agent-q:*project-root* #P"/path/to/your/project/")
```

### Issue: Tools succeed but files don't change

**Cause**: Emacs connection may not be working.

**Fix**:
```lisp
;; Test Emacs connection
(agent-q.tools::eval-in-emacs '(message "Connection works!"))
;; Should see "Connection works!" in Emacs echo area
```

### Issue: Buffer not updating after file changes

**Cause**: Buffer may need manual revert.

**Fix**: In Emacs, run `M-x revert-buffer` on the affected buffer.

---

## Test Results Template

Use this checklist to track your testing:

```
File Lifecycle Tools - Manual Test Results
Date: ___________
Tester: ___________

[ ] Test 1: create_file - Basic Creation
[ ] Test 2: create_file - Overwrite Protection
[ ] Test 3: create_file - Allow Overwrite
[ ] Test 4: create_file - Parent Directory Creation
[ ] Test 5: move_file - Basic Rename
[ ] Test 6: move_file - Move to Different Directory
[ ] Test 7: move_file - Overwrite Protection
[ ] Test 8: move_file - Source Validation
[ ] Test 9: delete_file - Basic Deletion
[ ] Test 10: delete_file - Non-existent File
[ ] Test 11: delete_file - Directory Rejection
[ ] Test 12: Security - Path Validation (All Tools)
[ ] Test 13: Buffer Synchronization

Total Passed: ___ / 13
Issues Found: _______________
Notes: _______________
```

---

## What's Next

After manual testing:

- **If all tests pass**: Implementation is verified working ✅
- **If tests fail**: Document failures and check implementation
- **Automated testing**: See `tests/filesystem-tests.lisp` for unit tests
- **Integration testing**: Use with real Agent-Q workflows

For questions or issues, refer to:
- `canon/features/file-system-tools/contracts/` - Tool specifications
- `tests/filesystem-tests.lisp` - Automated test suite
- `src/tools/filesystem.lisp` - Implementation source code
