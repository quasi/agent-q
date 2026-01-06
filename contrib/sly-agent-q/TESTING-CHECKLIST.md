# Manual Testing Checklist: Per-Hunk Diff Approval

## Prerequisites

- [ ] Agent-Q loaded in Emacs with SLY connection
- [ ] Test project with multiple Lisp files available
- [ ] `sly-agent-q-diff.el` loaded and compiled

## Test Cases

### TC1: Single Hunk Diff

**Setup:**
1. Create simple file: `(defun foo () 42)`
2. Ask agent to add docstring

**Expected:**
- [ ] Diff buffer opens with 1 hunk
- [ ] Header shows "Progress: 0/1 applied, 0 rejected, 1 pending"
- [ ] Hunk shows docstring addition
- [ ] Press `a` - Green overlay, "Hunk applied"
- [ ] Header updates to "1/1 applied, 0 rejected, 0 pending"
- [ ] Press `q` - Buffer closes, file modified
- [ ] Source file contains docstring

### TC2: Multi-Hunk Diff - Accept Some, Reject Others

**Setup:**
1. File with 3 functions
2. Ask agent to add docstrings to all 3

**Expected:**
- [ ] Diff opens with 3 hunks
- [ ] Navigate with `n` between hunks
- [ ] Accept hunk 1 with `a` - Applied
- [ ] Accept hunk 3 with `a` - Applied
- [ ] Reject hunk 2 with `r` - Rejected
- [ ] Press `q` - Summary: "2 applied, 1 rejected"
- [ ] Source file has docstrings for functions 1 and 3, not 2

### TC3: Preview Source Location

**Setup:**
1. Multi-hunk diff

**Expected:**
- [ ] Position on hunk 2
- [ ] Press `RET` - Jumps to source file at hunk location
- [ ] Press `C-x o` - Returns to diff buffer
- [ ] Still on same hunk

### TC4: Toggle Hunk State

**Setup:**
1. Multi-hunk diff

**Expected:**
- [ ] Position on pending hunk
- [ ] Press `SPC` - Hunk accepted
- [ ] Press `n` to next hunk
- [ ] Reject with `r` - Rejected
- [ ] Press `p` to go back
- [ ] Press `SPC` on rejected hunk - Attempts to apply

### TC5: Help Display

**Expected:**
- [ ] Press `?` - Help buffer opens
- [ ] Help shows all keybindings
- [ ] Press `q` - Help closes

### TC6: Unreviewed Hunks Warning

**Setup:**
1. 3-hunk diff

**Expected:**
- [ ] Accept hunk 1 only
- [ ] Press `q` - Warning: "2 unreviewed hunk(s) remaining"
- [ ] Answer `no` - Returns to diff
- [ ] Review remaining hunks
- [ ] Press `q` - Closes without warning

### TC7: Legacy All-or-Nothing Mode

**Expected:**
- [ ] Multi-hunk diff opens
- [ ] Press `C-c C-c` - All hunks applied
- [ ] Buffer closes immediately
- [ ] File has all changes

### TC8: Apply Failure Handling

**Setup:**
1. Create diff
2. Manually edit source file to change context
3. Try to apply hunk

**Expected:**
- [ ] Press `a` - Error: "Failed to apply hunk: <reason>"
- [ ] Hunk marked with error state (not applied)
- [ ] Can continue reviewing other hunks

### TC9: Multi-File Diff Sequence

**Setup:**
1. Ask agent to modify 2 different files

**Expected:**
- [ ] First file's diff opens
- [ ] Review and accept/reject
- [ ] Press `q` - First diff closes
- [ ] Second file's diff opens automatically
- [ ] Review second file
- [ ] Both files modified as reviewed

### TC10: Visual Feedback

**Expected:**
- [ ] Accepted hunks: green background, [APPLIED] marker
- [ ] Rejected hunks: red background, [REJECTED] marker
- [ ] Pending hunks: normal diff colors
- [ ] Header shows real-time progress

## Performance Tests

- [ ] 10-hunk diff: Smooth navigation with `n`/`p`
- [ ] 50-hunk diff: No lag when accepting/rejecting
- [ ] Large file diff (1000+ lines): diff-apply-hunk completes < 1s

## Regression Tests

- [ ] Single-hunk diff still works with `C-c C-c`
- [ ] Reject all with `C-c C-k` still works
- [ ] Diff generation still produces valid unified diffs

## Edge Cases

- [ ] Empty file - single hunk adding content
- [ ] Delete entire file content - single hunk removing all
- [ ] Whitespace-only changes - diff-apply-hunk handles gracefully
- [ ] Overlapping hunks - apply in order, each updates context
- [ ] Non-existent file - graceful error
- [ ] File deleted during review - graceful error
