# Manual Testing Guide: File System Tools (Phase 2 & 4)

**Purpose**: Verify all recently implemented filesystem tools work correctly in Agent-Q.

**Tools covered**: `directory_tree`, `search_files`, `insert_at_line`, `create_file`, `move_file`, `delete_file`

**Time required**: ~25 minutes

**Target audience**: Developers and testers verifying Phase 2 and Phase 4 implementations.

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

**Step 2**: Verify all tools are registered

```lisp
;; In the SLY REPL
(length (agent-q.tools:get-agent-q-tools :max-safety-level :moderate))
;; Expected: Should show >= 10 tools
```

**Step 3**: Create test directory structure

```bash
# In terminal, in your project root
mkdir -p test-fs-tools/{src,tests,docs,build}
cd test-fs-tools

# Create some test files
echo "Main application code" > src/main.lisp
echo "Helper utilities" > src/utils.lisp
echo "Configuration" > src/config.lisp
echo "Unit tests" > tests/test-main.lisp
echo "Integration tests" > tests/test-utils.lisp
echo "Project README" > docs/README.md
echo "User guide" > docs/guide.md
echo "Build output" > build/output.fasl
echo ".DS_Store" > .DS_Store
```

✅ **Checkpoint**: You should now have a test directory with multiple files and subdirectories.

---

## Part 1: Phase 2 Tools - Navigation & Search

### Test 1: directory_tree - Basic Tree View

**Goal**: Verify `directory_tree` shows hierarchical structure.

**Steps**:

1. Ask Agent-Q to show directory tree:
   ```
   Use directory_tree to show the structure of "test-fs-tools"
   ```

2. **Expected output format**:
   ```
   Directory tree: /path/to/test-fs-tools

   [DIR] test-fs-tools
     [DIR] build
       [FILE] output.fasl (XXX bytes)
     [DIR] docs
       [FILE] README.md (XXX bytes)
       [FILE] guide.md (XXX bytes)
     [DIR] src
       [FILE] config.lisp (XXX bytes)
       [FILE] main.lisp (XXX bytes)
       [FILE] utils.lisp (XXX bytes)
     [DIR] tests
       [FILE] test-main.lisp (XXX bytes)
       [FILE] test-utils.lisp (XXX bytes)
     [FILE] .DS_Store (XXX bytes)
   ```

3. **Verify**:
   - All directories shown with [DIR] prefix
   - All files shown with [FILE] prefix and size
   - Hierarchical indentation is correct
   - Order is alphabetical or consistent

✅ **Pass criteria**: Tree structure matches actual filesystem, proper formatting.

---

### Test 2: directory_tree - With Exclusions

**Goal**: Verify `directory_tree` can exclude patterns.

**Steps**:

1. Ask Agent-Q to show tree excluding build artifacts:
   ```
   Use directory_tree on "test-fs-tools" with exclude_patterns set to
   [".DS_Store", "*.fasl", "build"]
   ```

2. **Expected behavior**:
   - `.DS_Store` file NOT shown
   - `build/` directory NOT shown
   - `output.fasl` NOT shown
   - All other files and directories shown normally

3. **Verify**:
   ```bash
   # Tree should NOT include these
   # - .DS_Store
   # - build/ (entire directory)
   # Should include: src/, tests/, docs/, and all .lisp and .md files
   ```

✅ **Pass criteria**: Excluded patterns successfully filtered from output.

---

### Test 3: search_files - Simple Wildcard Search

**Goal**: Verify `search_files` finds files by pattern.

**Steps**:

1. Search for all Lisp files:
   ```
   Use search_files with pattern "*.lisp" in "test-fs-tools"
   ```

2. **Expected output**:
   ```
   Found 5 files matching '*.lisp':

     src/config.lisp
     src/main.lisp
     src/utils.lisp
     tests/test-main.lisp
     tests/test-utils.lisp
   ```

3. **Verify count and paths**:
   - Should find exactly 5 .lisp files
   - Paths are relative to search root
   - All .lisp files found, no .md or .fasl files

✅ **Pass criteria**: All .lisp files found, count correct, no false positives.

---

### Test 4: search_files - Recursive Pattern

**Goal**: Verify `search_files` handles recursive `**` pattern.

**Steps**:

1. Search recursively for markdown files:
   ```
   Use search_files with pattern "**/*.md" in "test-fs-tools"
   ```

2. **Expected output**:
   ```
   Found 2 files matching '**/*.md':

     docs/README.md
     docs/guide.md
   ```

3. **Verify**:
   - Finds .md files in any subdirectory (docs/)
   - Recursive pattern works correctly
   - Count matches actual files

✅ **Pass criteria**: Recursive search finds files in subdirectories.

---

### Test 5: search_files - With Exclusions

**Goal**: Verify `search_files` respects exclusion patterns.

**Steps**:

1. Search for all files excluding tests:
   ```
   Use search_files with pattern "*" in "test-fs-tools"
   with exclude_patterns set to ["tests", "*.fasl", ".DS_Store"]
   ```

2. **Expected behavior**:
   - Files in `tests/` directory NOT shown
   - `.fasl` files NOT shown
   - `.DS_Store` NOT shown
   - Files in `src/` and `docs/` shown

3. **Verify exclusions working**:
   - test-main.lisp NOT in results
   - test-utils.lisp NOT in results
   - src/main.lisp IS in results

✅ **Pass criteria**: Exclusions properly filter search results.

---

### Test 6: search_files - Question Mark Wildcard

**Goal**: Verify single-character `?` wildcard works.

**Steps**:

1. Create files for testing:
   ```bash
   cd test-fs-tools/src
   echo "test" > a.lisp
   echo "test" > b.lisp
   echo "test" > ab.lisp
   ```

2. Search with question mark pattern:
   ```
   Use search_files with pattern "src/?.lisp" in "test-fs-tools"
   ```

3. **Expected output**:
   ```
   Found 2 files matching 'src/?.lisp':

     src/a.lisp
     src/b.lisp
   ```

   Note: `ab.lisp` should NOT match (2 chars, not 1)

4. **Verify**:
   ```bash
   # Only single-letter files should match
   # a.lisp ✓
   # b.lisp ✓
   # ab.lisp ✗ (too long)
   # main.lisp ✗ (too long)
   ```

✅ **Pass criteria**: Only single-character filenames match `?` pattern.

---

## Part 2: Phase 4 Tools - File Lifecycle

### Test 7: insert_at_line - Insert at Beginning

**Goal**: Verify `insert_at_line` can insert at file start.

**Steps**:

1. Check current file content:
   ```bash
   cat test-fs-tools/src/main.lisp
   # Shows: Main application code
   ```

2. Insert at beginning (line 0):
   ```
   Use insert_at_line to insert ";;; Header comment" at line 0 in
   "test-fs-tools/src/main.lisp"
   ```

3. **Expected behavior**:
   - Tool succeeds with confirmation message
   - Shows content inserted

4. **Verify**:
   ```bash
   cat test-fs-tools/src/main.lisp
   # Expected output:
   # ;;; Header comment
   # Main application code
   ```

✅ **Pass criteria**: New line inserted at beginning, original content preserved.

---

### Test 8: insert_at_line - Insert at End

**Goal**: Verify `insert_at_line` can append to file.

**Steps**:

1. Insert at end (line -1):
   ```
   Use insert_at_line to insert ";;; Footer comment" at line -1 in
   "test-fs-tools/src/main.lisp"
   ```

2. **Verify**:
   ```bash
   cat test-fs-tools/src/main.lisp
   # Expected output:
   # ;;; Header comment
   # Main application code
   # ;;; Footer comment
   ```

✅ **Pass criteria**: New line appended to end of file.

---

### Test 9: insert_at_line - Insert at Specific Line

**Goal**: Verify `insert_at_line` handles 1-indexed line numbers.

**Steps**:

1. Insert after first line (line 1):
   ```
   Use insert_at_line to insert ";;; After header" at line 1 in
   "test-fs-tools/src/main.lisp"
   ```

2. **Verify**:
   ```bash
   cat test-fs-tools/src/main.lisp
   # Expected output:
   # ;;; Header comment
   # ;;; After header
   # Main application code
   # ;;; Footer comment
   ```

✅ **Pass criteria**: Line inserted at correct position (1-indexed).

---

### Test 10: create_file - Basic Creation

**Goal**: Verify `create_file` creates new files.

**Steps**:

1. Create a new file:
   ```
   Use create_file to create "test-fs-tools/src/new-module.lisp"
   with content:
   ";;; New Module

   (in-package :agent-q)

   (defun hello ()
     \"Say hello\"
     (format t \"Hello!\"))"
   ```

2. **Expected behavior**:
   - Tool succeeds: "✓ Created file: test-fs-tools/src/new-module.lisp"
   - Shows content size and line count

3. **Verify**:
   ```bash
   cat test-fs-tools/src/new-module.lisp
   # Should show the exact content above

   ls test-fs-tools/src/
   # Should include new-module.lisp
   ```

✅ **Pass criteria**: File created with correct content.

---

### Test 11: create_file - Overwrite Protection

**Goal**: Verify `create_file` prevents accidental overwrites.

**Steps**:

1. Try to create existing file:
   ```
   Use create_file to create "test-fs-tools/src/new-module.lisp"
   with content "This should fail"
   ```

2. **Expected behavior**:
   - Tool fails with clear error
   - Error: "Error: File 'test-fs-tools/src/new-module.lisp' already exists."
   - Suggests: "Use allow_overwrite: true to replace it."

3. **Verify file unchanged**:
   ```bash
   cat test-fs-tools/src/new-module.lisp
   # Should still show original content, NOT "This should fail"
   ```

✅ **Pass criteria**: Overwrite blocked, original file preserved.

---

### Test 12: create_file - Parent Directory Creation

**Goal**: Verify automatic parent directory creation.

**Steps**:

1. Create file in non-existent path:
   ```
   Use create_file to create "test-fs-tools/new/deep/path/file.lisp"
   with content "Testing parent creation"
   ```

2. **Expected behavior**:
   - Tool succeeds (creates parent directories automatically)
   - Confirmation message shown

3. **Verify directory structure**:
   ```bash
   ls -R test-fs-tools/new/
   # Expected: Shows deep/path/ structure

   cat test-fs-tools/new/deep/path/file.lisp
   # Expected: Testing parent creation
   ```

✅ **Pass criteria**: File created with full directory path auto-created.

---

### Test 13: move_file - Simple Rename

**Goal**: Verify `move_file` renames files in same directory.

**Steps**:

1. Rename a file:
   ```
   Use move_file to move "test-fs-tools/src/new-module.lisp"
   to "test-fs-tools/src/renamed-module.lisp"
   ```

2. **Expected behavior**:
   - Tool succeeds
   - Response: "✓ Moved: test-fs-tools/src/new-module.lisp → test-fs-tools/src/renamed-module.lisp"

3. **Verify**:
   ```bash
   ls test-fs-tools/src/
   # Expected: renamed-module.lisp exists, new-module.lisp does NOT

   cat test-fs-tools/src/renamed-module.lisp
   # Content should be preserved
   ```

✅ **Pass criteria**: File renamed, content preserved, old name gone.

---

### Test 14: move_file - Move to Different Directory

**Goal**: Verify `move_file` moves files between directories.

**Steps**:

1. Move file to different directory:
   ```
   Use move_file to move "test-fs-tools/src/renamed-module.lisp"
   to "test-fs-tools/docs/archived-module.lisp"
   ```

2. **Verify**:
   ```bash
   ls test-fs-tools/src/
   # Expected: renamed-module.lisp NOT present

   ls test-fs-tools/docs/
   # Expected: archived-module.lisp present
   ```

✅ **Pass criteria**: File moved to new directory successfully.

---

### Test 15: move_file - Overwrite Protection

**Goal**: Verify `move_file` prevents overwriting existing files.

**Steps**:

1. Try to move to existing file:
   ```
   Use move_file to move "test-fs-tools/docs/guide.md"
   to "test-fs-tools/docs/README.md"
   ```

2. **Expected behavior**:
   - Tool fails
   - Error: "Error: Destination file 'test-fs-tools/docs/README.md' already exists."
   - Suggests using `allow_overwrite: true`

3. **Verify both files unchanged**:
   ```bash
   # Both files should still exist with original content
   cat test-fs-tools/docs/guide.md
   cat test-fs-tools/docs/README.md
   ```

✅ **Pass criteria**: Move blocked, both files preserved.

---

### Test 16: delete_file - Basic Deletion

**Goal**: Verify `delete_file` removes files.

**Steps**:

1. Delete a file:
   ```
   Use delete_file to delete "test-fs-tools/build/output.fasl"
   ```

2. **Expected behavior**:
   - Tool succeeds
   - Response: "✓ Deleted: test-fs-tools/build/output.fasl"
   - Shows size freed

3. **Verify**:
   ```bash
   ls test-fs-tools/build/output.fasl
   # Expected: "No such file or directory"
   ```

✅ **Pass criteria**: File successfully deleted.

---

### Test 17: delete_file - Directory Rejection

**Goal**: Verify `delete_file` rejects directories.

**Steps**:

1. Try to delete a directory:
   ```
   Use delete_file to delete "test-fs-tools/build"
   ```

2. **Expected behavior**:
   - Tool fails
   - Error: "Error: 'test-fs-tools/build' is a directory. Use a directory deletion tool instead."

3. **Verify directory still exists**:
   ```bash
   ls -d test-fs-tools/build
   # Expected: Directory still present
   ```

✅ **Pass criteria**: Directory deletion blocked with helpful error.

---

### Test 18: delete_file - Non-existent File

**Goal**: Verify `delete_file` handles missing files gracefully.

**Steps**:

1. Try to delete non-existent file:
   ```
   Use delete_file to delete "test-fs-tools/never-existed.txt"
   ```

2. **Expected behavior**:
   - Tool fails
   - Error: "Error: File 'test-fs-tools/never-existed.txt' does not exist"

✅ **Pass criteria**: Clear error for missing file.

---

## Part 3: Integration Scenarios

### Test 19: Workflow - Search, Create, Move

**Goal**: Verify tools work together in realistic workflow.

**Steps**:

1. **Search**: Find all test files
   ```
   Use search_files with pattern "test-*.lisp" in "test-fs-tools"
   ```

2. **Create**: Add a new test file
   ```
   Use create_file to create "test-fs-tools/tests/test-integration.lisp"
   with content:
   ";;; Integration Tests

   (in-package :agent-q-tests)

   (def-suite integration-tests)"
   ```

3. **Verify creation**:
   ```bash
   cat test-fs-tools/tests/test-integration.lisp
   ```

4. **Move**: Reorganize test file
   ```
   Use move_file to move "test-fs-tools/tests/test-integration.lisp"
   to "test-fs-tools/tests/integration/test-suite.lisp"
   ```

5. **Final verification**:
   ```bash
   ls test-fs-tools/tests/integration/
   # Expected: test-suite.lisp

   # Verify original location empty
   ls test-fs-tools/tests/test-integration.lisp
   # Expected: No such file
   ```

✅ **Pass criteria**: All three tools work in sequence without errors.

---

### Test 20: Workflow - Tree, Edit, Verify

**Goal**: Verify navigation + editing workflow.

**Steps**:

1. **Tree**: View structure before changes
   ```
   Use directory_tree on "test-fs-tools/src"
   ```

2. **Create**: Add multiple modules
   ```
   Use create_file to create "test-fs-tools/src/module-a.lisp"
   with content ";;; Module A"

   Use create_file to create "test-fs-tools/src/module-b.lisp"
   with content ";;; Module B"
   ```

3. **Search**: Verify new files found
   ```
   Use search_files with pattern "module-*.lisp" in "test-fs-tools/src"
   ```

   Expected: Should find both module-a.lisp and module-b.lisp

4. **Tree**: View structure after changes
   ```
   Use directory_tree on "test-fs-tools/src"
   ```

   Expected: Should show module-a.lisp and module-b.lisp in tree

✅ **Pass criteria**: All tools reflect changes consistently.

---

## Part 4: Security & Boundary Testing

### Test 21: Path Validation - All Tools

**Goal**: Verify all tools enforce project root boundary.

**Steps**:

1. Test `directory_tree` with external path:
   ```
   Use directory_tree on "/etc"
   ```
   **Expected**: Error about path outside project root

2. Test `search_files` with traversal:
   ```
   Use search_files with pattern "*" in "../../etc"
   ```
   **Expected**: Error about path outside project root

3. Test `create_file` with absolute path:
   ```
   Use create_file to create "/tmp/malicious.txt"
   with content "This should fail"
   ```
   **Expected**: Error about path outside project root

4. Test `move_file` source validation:
   ```
   Use move_file to move "../../etc/passwd" to "test-fs-tools/evil.txt"
   ```
   **Expected**: Error about source path outside project root

5. Test `move_file` destination validation:
   ```
   Use move_file to move "test-fs-tools/src/utils.lisp" to "/tmp/stolen.lisp"
   ```
   **Expected**: Error about destination path outside project root

6. Test `delete_file` with external path:
   ```
   Use delete_file to delete "../../important-file.txt"
   ```
   **Expected**: Error about path outside project root

7. Test `insert_at_line` with external path:
   ```
   Use insert_at_line to insert "evil" at line 0 in "/etc/hosts"
   ```
   **Expected**: Error about path outside project root

✅ **Pass criteria**: ALL tools reject external paths with clear errors.

---

### Test 22: Exclusion Pattern Edge Cases

**Goal**: Verify exclusion patterns handle edge cases correctly.

**Steps**:

1. Create edge case files:
   ```bash
   cd test-fs-tools
   touch "file with spaces.txt"
   touch "special@chars#.txt"
   touch ".hidden-file"
   ```

2. Test excluding files with spaces:
   ```
   Use directory_tree on "test-fs-tools"
   with exclude_patterns ["file with spaces.txt"]
   ```
   **Expected**: File with spaces NOT shown

3. Test excluding hidden files:
   ```
   Use search_files with pattern "*"
   with exclude_patterns [".*"]
   ```
   **Expected**: `.hidden-file` and `.DS_Store` NOT in results

✅ **Pass criteria**: Exclusions handle special characters and patterns correctly.

---

## Part 5: Buffer Synchronization (Emacs Integration)

### Test 23: Buffer Sync - Create and Edit

**Goal**: Verify buffer synchronization with file operations.

**Steps**:

1. **Create file via Agent-Q**:
   ```
   Use create_file to create "test-fs-tools/buffer-sync.lisp"
   with content ";;; Original content"
   ```

2. **Open in Emacs**:
   ```elisp
   C-x C-f test-fs-tools/buffer-sync.lisp
   ;; Buffer should show: ;;; Original content
   ```

3. **Modify via Agent-Q**:
   ```
   Use create_file with allow_overwrite true to create
   "test-fs-tools/buffer-sync.lisp" with content ";;; Modified by Agent-Q"
   ```

4. **Check Emacs buffer**:
   - Switch to buffer in Emacs
   - Expected: Buffer shows ";;; Modified by Agent-Q"
   - Expected: Buffer NOT marked as modified (no asterisk)

✅ **Pass criteria**: Buffer content updated, no unsaved changes indicator.

---

### Test 24: Buffer Sync - Move File

**Goal**: Verify buffer tracks file moves.

**Steps**:

1. **Move file via Agent-Q**:
   ```
   Use move_file to move "test-fs-tools/buffer-sync.lisp"
   to "test-fs-tools/src/buffer-moved.lisp"
   ```

2. **Check Emacs**:
   - Buffer name should update to `buffer-moved.lisp`
   - Buffer should visit new path: `test-fs-tools/src/buffer-moved.lisp`
   - Content should be preserved

✅ **Pass criteria**: Buffer name and path updated automatically.

---

### Test 25: Buffer Sync - Delete File

**Goal**: Verify buffer cleanup on file deletion.

**Steps**:

1. **Delete file via Agent-Q**:
   ```
   Use delete_file to delete "test-fs-tools/src/buffer-moved.lisp"
   ```

2. **Check Emacs**:
   - Buffer should be killed (closed)
   - Attempting to switch to it should fail
   - File should be gone from filesystem

✅ **Pass criteria**: Buffer automatically closed after deletion.

---

## Cleanup

After testing, remove the test directory:

```bash
rm -rf test-fs-tools/
```

---

## Troubleshooting

### Issue: "Tool not found"

**Cause**: Tools not registered or wrong safety level.

**Fix**:
```lisp
;; List all moderate-level tools
(mapcar #'cl-llm-provider:tool-name
        (agent-q.tools:get-agent-q-tools :max-safety-level :moderate))
;; Should include: directory_tree, search_files, insert_at_line,
;;                 create_file, move_file, delete_file
```

### Issue: "Path is outside project root"

**Cause**: Project root not configured correctly.

**Fix**:
```lisp
;; Check current project root
(agent-q::ensure-project-root)

;; Set manually if needed
(setf agent-q:*project-root* #P"/path/to/your/project/")
```

### Issue: Glob patterns not working

**Cause**: Pattern syntax may be incorrect.

**Supported patterns**:
- `*` - Matches any characters (wildcard)
- `**` - Recursive directory matching
- `?` - Matches single character
- **NOT supported**: Character classes `[abc]`, brace expansion `{a,b}`

**Fix**: Use simpler patterns or multiple searches.

### Issue: Tools succeed but files don't change

**Cause**: Emacs connection may be broken.

**Fix**:
```lisp
;; Test Emacs connection
(agent-q.tools::eval-in-emacs '(message "Test message"))
;; Should see "Test message" in Emacs echo area

;; If broken, restart SLY connection
M-x sly-quit
M-x sly
```

### Issue: Buffer not updating

**Cause**: Buffer may need manual revert.

**Fix**:
```elisp
M-x revert-buffer
;; Or set auto-revert:
M-x auto-revert-mode
```

---

## Test Results Template

```
File System Tools - Manual Test Results
Date: ___________
Tester: ___________
Agent-Q Version: ___________

PART 1: Phase 2 Tools (Navigation & Search)
[ ] Test 1: directory_tree - Basic Tree View
[ ] Test 2: directory_tree - With Exclusions
[ ] Test 3: search_files - Simple Wildcard Search
[ ] Test 4: search_files - Recursive Pattern
[ ] Test 5: search_files - With Exclusions
[ ] Test 6: search_files - Question Mark Wildcard

PART 2: Phase 4 Tools (File Lifecycle)
[ ] Test 7: insert_at_line - Insert at Beginning
[ ] Test 8: insert_at_line - Insert at End
[ ] Test 9: insert_at_line - Insert at Specific Line
[ ] Test 10: create_file - Basic Creation
[ ] Test 11: create_file - Overwrite Protection
[ ] Test 12: create_file - Parent Directory Creation
[ ] Test 13: move_file - Simple Rename
[ ] Test 14: move_file - Move to Different Directory
[ ] Test 15: move_file - Overwrite Protection
[ ] Test 16: delete_file - Basic Deletion
[ ] Test 17: delete_file - Directory Rejection
[ ] Test 18: delete_file - Non-existent File

PART 3: Integration Scenarios
[ ] Test 19: Workflow - Search, Create, Move
[ ] Test 20: Workflow - Tree, Edit, Verify

PART 4: Security & Boundary Testing
[ ] Test 21: Path Validation - All Tools
[ ] Test 22: Exclusion Pattern Edge Cases

PART 5: Buffer Synchronization
[ ] Test 23: Buffer Sync - Create and Edit
[ ] Test 24: Buffer Sync - Move File
[ ] Test 25: Buffer Sync - Delete File

Summary:
Total Passed: ___ / 25
Issues Found: _______________
Critical Issues: _______________
Notes: _______________
```

---

## What's Next

After manual testing:

- **All tests pass**: Implementation verified working ✅
- **Tests fail**: Document failures, check implementation
- **Automated tests**: Run `(asdf:test-system :agent-q)` for unit tests
- **Real usage**: Try tools in actual Agent-Q development workflow

For detailed specifications:
- **Contracts**: `canon/features/file-system-tools/contracts/`
- **Unit tests**: `tests/filesystem-tests.lisp`
- **Implementation**: `src/tools/filesystem.lisp`
