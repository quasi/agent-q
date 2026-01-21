# Enhancement 3: Multi-File Change Strategies with Checkpointing

**Date**: 2025-01-31
**Status**: Research
**Category**: Architecture / Change Management
**Goal**: Enable safe, reviewable, undoable multi-file modifications

---

## Executive Summary

Multi-file refactoring is where AI coding assistants often fail catastrophically. Research shows **41-86.7% failure rates** for multi-agent systems, with coordination failures (36.94%) and context window limitations causing most breakdowns.

This document analyzes proven strategies from leading tools (aider, Cursor, macher, Claude Code, Replit) and proposes a checkpointing architecture for Agent-Q that enables:

1. **Atomic multi-file changes** - All-or-nothing transactions
2. **Checkpoint-to-checkpoint undo** - Revert to any save point
3. **Preview before apply** - Dry-run mode for safety
4. **Incremental review** - Accept/reject changes per file
5. **Isolation strategies** - Git worktrees, ephemeral workspaces

**Key Insight**: The most successful tools combine **transaction semantics** (atomicity, rollback) with **incremental human approval** (preview, per-file review) to balance safety and flexibility.

---

## The Multi-File Change Problem

### Why Multi-File Changes Fail

**Context Window Limitations** ([Augment Code - Enterprise Multi-File](https://www.augmentcode.com/guides/enterprise-multi-file-refactoring-why-ai-breaks-at-scale)):
> "AI coding assistants struggle with enterprise refactoring because context window limitations prevent them from tracking dependencies across multiple files simultaneously. Tools with 4-8K token windows can only process 2-3 files at once, creating coordination failures in codebases with 400,000+ files."

**Dependency Tracking Failures** ([Cursor Limitations](https://www.augmentcode.com/tools/cursor-ai-limitations-why-multi-file-refactors-fail-in-enterprise)):
> "When refactoring touches file A, the system needs to maintain a dependency graph showing that files B, C, and D import from A, that file E uses a type defined in C, and that changing A's signature requires updating test files F through J."

**Catastrophic Example** ([Automate Multi-File Refactoring](https://www.augmentcode.com/guides/automate-multi-file-code-refactoring-with-ai-agents-a-step-by-step-guide)):
> "Within minutes, an AI had changed 31 files, introduced 260+ type errors, and created circular dependencies that broke the build system."

**Real Production Failure** ([Multi-Agent Failures](https://www.augmentcode.com/guides/why-multi-agent-llm-systems-fail-and-how-to-fix-them)):
> "In July 2025, Replit's AI agent deleted Jason Lemkin's entire production database—1,206 executive records and data from nearly 1,200 companies—despite explicit instructions for a 'code and action freeze.'"

### Success Rates

- **Multi-agent LLM systems**: 41-86.7% failure rate in production ([arXiv research](https://www.augmentcode.com/guides/why-multi-agent-llm-systems-fail-and-how-to-fix-them))
- **Specification problems**: 41.77% of failures
- **Coordination failures**: 36.94% of failures
- **Total**: Nearly 79% of breakdowns from these two causes

---

## Strategy 1: Repository Map + Explicit File Selection (aider)

### How It Works

**Repository Map** ([aider Usage](https://aider.chat/docs/usage.html)):
> "Aider uses a repository map to help the LLM understand the layout, code structure, and content of a git repo. The repo map is created through static analysis of the code's abstract syntax tree and call graph to provide a compact and powerful summary of the entire code base."

**Explicit File Addition** ([aider Complex Change](https://aider.chat/examples/complex-change.html)):
> "You can use aider with multiple source files at once, so aider can make coordinated code changes across all of them in a single changeset/commit. To edit files, you need to 'add them to the chat'. Do this by naming them on the aider command line. Or, you can use the in-chat `/add` command."

**Automatic Git Commits** ([Blott Aider Review](https://www.blott.com/blog/post/aider-review-a-developers-month-with-this-terminal-based-code-assistant)):
> "One neat thing about Aider is that it will apply edits directly to the source files, and automatically create Git commits with the right commit messages so you can easily undo any changes it has done. Every change Aider makes is committed to git with a descriptive message. Use the `/undo` command to immediately revert the last commit."

### Workflow

```
User: "Refactor authentication to use new JWT library"
  ↓
Aider builds repo map (AST + call graph)
  ↓
User adds relevant files:
  /add src/auth/login.lisp
  /add src/auth/token.lisp
  /add src/middleware/auth-check.lisp
  /add tests/auth-tests.lisp
  ↓
LLM proposes changes across all 4 files
  ↓
Aider applies changes + auto-commits with message:
  "Refactor auth to use JWT library (4 files)"
  ↓
User can /undo if issues found
```

### Pros & Cons

**Pros:**
- ✅ Explicit control over which files are in scope
- ✅ Repo map provides context without full file loading
- ✅ Automatic Git commits = built-in checkpoints
- ✅ `/undo` command = instant rollback
- ✅ Clean Git history for review

**Cons:**
- ❌ User must know which files to add (cognitive load)
- ❌ Single undo granularity (entire commit, not per-file)
- ❌ No preview before apply (changes happen immediately)
- ❌ Repo map construction can be slow for huge codebases

**Complexity**: 4 (repo map + AST analysis + Git integration)
**Benefit**: 5 (proven approach, 26% SWE-Bench Lite score)

---

## Strategy 2: Ephemeral Workspace with Original/Modified Versions (macher)

### How It Works

**Dual-Version Architecture** ([macher GitHub](https://github.com/kmontag/macher)):
> "The macher-context maintains two versions of each file: **Original content** (read-only snapshots at first-access time) and **Modified content** (editable copies where the LLM makes changes using tools)."

**Preset System**:
- `@macher-notools`: Workspace context only (file listings)
- `@macher-ro`: + Read-only tools (LLM can read files)
- `@macher`: + Editing tools (LLM can propose changes)

**Patch Generation** ([macher README](https://github.com/kmontag/macher)):
> "If changes are made, the default `macher-process-request-function` will generate a patch at the end of the request, and display it in the workspace's patch buffer. You can review the proposed patch and apply changes using standard diff-mode commands."

### Workflow

```
User: "Add error handling to database module"
  ↓
macher creates ephemeral workspace:
  - original/db.lisp (snapshot)
  - original/db-utils.lisp (snapshot)
  - modified/db.lisp (editable copy)
  - modified/db-utils.lisp (editable copy)
  ↓
LLM reads originals, makes changes to modified versions
  ↓
macher generates unified diff:
  diff -u original/db.lisp modified/db.lisp
  diff -u original/db-utils.lisp modified/db-utils.lisp
  ↓
Diff buffer opens with both changes
  ↓
User reviews:
  - Hunk 1 (db.lisp): C-c C-a (accept)
  - Hunk 2 (db-utils.lisp): C-c C-a (accept)
  ↓
Changes applied to actual files
```

### Pros & Cons

**Pros:**
- ✅ Complete isolation (ephemeral workspace)
- ✅ Original files never touched until approval
- ✅ Standard diff-mode commands (familiar UX)
- ✅ Per-file or per-hunk review possible
- ✅ No Git commits until user decides

**Cons:**
- ❌ Ephemeral workspace overhead (copy files)
- ❌ No built-in checkpointing (relies on Git)
- ❌ Workspace state lost if session crashes
- ❌ Not persistent across Emacs restarts

**Complexity**: 3 (simpler than repo map, just file shadowing)
**Benefit**: 4 (good isolation, leverages Emacs infrastructure)

---

## Strategy 3: Composer + Git Worktrees (Cursor 2.0)

### How It Works

**Composer Model** ([Cursor Composer AI](https://skywork.ai/blog/vibecoding/cursor-composer-ai-model/)):
> "Composer is a specialized chat interface designed for large-scale code refactoring. It can generate diffs across multiple files, allowing software engineers to preview and approve changes systematically. Composer often ships cleaner, consistent multi-file changes because it plans edits and aligns with your lint/build tools."

**Git Worktree Isolation** ([Cursor 2.0 Architecture](https://www.digitalapplied.com/blog/cursor-2-0-agent-first-architecture-guide)):
> "The isolation mechanism relies on Git worktrees or remote machines. Git worktrees create separate workspace copies while sharing the same repository, so each agent modifies files in its own space without triggering conflicts. Changes stay isolated until deliberately merged into the main codebase."

**Parallel Agents** ([Cursor 2.0 Guide](https://www.digitalapplied.com/blog/cursor-2-0-agent-first-architecture-guide)):
> "Run up to 8 agents simultaneously using Git worktree isolation. One worktree for an AI writing tests, another for an AI implementing the code to pass those tests."

### Workflow

```
User: "Implement user profile feature"
  ↓
Cursor creates git worktree:
  main/              (original codebase)
  trees/profile/     (isolated workspace)
  ↓
Composer plans multi-file changes:
  1. Create src/profile/controller.ts
  2. Update src/routes.ts (add /profile route)
  3. Create views/profile.html
  4. Update tests/routes.test.ts
  ↓
Composer generates diffs for all 4 changes
  ↓
User reviews plan in UI:
  [✓] Add controller
  [✓] Update routes
  [✓] Add view
  [✗] Reject test changes (want to write manually)
  ↓
Changes applied to worktree:
  trees/profile/ now has 3 of 4 changes
  ↓
User tests in worktree, then:
  cd main
  git merge trees/profile
```

### Pros & Cons

**Pros:**
- ✅ Complete isolation (separate directory tree)
- ✅ Parallel development (multiple agents/features)
- ✅ Plan-first approach (review before execution)
- ✅ Lint integration catches errors early
- ✅ No risk to main codebase

**Cons:**
- ❌ High complexity (worktree management)
- ❌ Disk space overhead (multiple copies)
- ❌ Merge conflicts when combining worktrees
- ❌ Worktrees can become "forgotten" and stale
- ❌ Steep learning curve for Git worktrees

**Complexity**: 5 (worktree management + multi-agent orchestration)
**Benefit**: 5 (enterprise-grade, parallel development)

---

## Strategy 4: Checkpoint System (Claude Code, Replit)

### How It Works

**Claude Code Checkpoints** ([Skywork Claude Code](https://skywork.ai/skypage/en/claude-code-checkpoints-ai-coding/1976917740735229952)):
> "Claude Code automatically captures a snapshot of your code's state *before* it makes any edit. This creates a safety net, allowing you to pursue ambitious refactors or explore new features with the confidence that you can always return to a prior, known-good state."

**Replit's System** ([Replit Checkpoints](https://docs.replit.com/replitai/checkpoints-and-rollbacks)):
> "A checkpoint is a complete snapshot of your Replit App state created automatically by Agent and Assistant at key development milestones. Unlike traditional version control that only tracks code changes, Replit checkpoints capture your entire development context. Think of checkpoints as save points in a video game."

**IBM STRATUS Research** ([IBM Undo-Retry](https://research.ibm.com/blog/undo-agent-for-cloud)):
> "When STRATUS's remediation agent makes an unsuccessful move, an 'undo' maneuver reverts the system to the last checkpoint, so alternate solutions can be explored. After the agent takes a series of steps (a 'transaction'), the agent assesses the severity level of the system. If the new state is worse off, the agent aborts the transaction and the system reverts to its initial, checkpointed state. Our core assumption is that every action must be undoable."

### Transaction Semantics

From database research ([ARIES Algorithm](https://www.cs.rpi.edu/~sibel/csci4380/fall2023/course_notes/transactions_durability.html)):

**ACID Properties for Code Changes:**
- **Atomicity**: All files in changeset succeed or all fail (no partial state)
- **Consistency**: Code remains valid (no circular deps, type errors)
- **Isolation**: Changes don't affect other sessions/agents
- **Durability**: Checkpoints persist (survive crashes, restarts)

**Write-Ahead Logging (WAL)** ([WAL Redo/Undo](https://medium.com/@moali314/database-logging-wal-redo-and-undo-mechanisms-58c076fbe36e)):
> "WAL is also called a 'Redo Log' — its primary function during initial recovery phases is to redo changes to ensure durability. The undo log serves as a critical component for maintaining transactional consistency, allowing for the rollback of changes."

### Workflow

```
User starts session
  ↓
Auto-checkpoint: CP-0 (initial state)
  ↓
User: "Add JWT authentication"
  ↓
Agent proposes multi-file changes
  ↓
User accepts → changes applied
  ↓
Auto-checkpoint: CP-1 "Added JWT auth (3 files)"
  ↓
User: "Add rate limiting middleware"
  ↓
Agent modifies 2 files
  ↓
User accepts
  ↓
Auto-checkpoint: CP-2 "Added rate limiting (2 files)"
  ↓
User notices auth broke
  ↓
User: claude restore CP-1
  ↓
System reverts to state after JWT, before rate limiting
  ↓
User: "Add rate limiting, but preserve auth headers"
  ↓
Agent tries again with better context
```

### Checkpoint Metadata

```lisp
;; Checkpoint structure
(defstruct checkpoint
  (id "CP-1")
  (timestamp (get-universal-time))
  (description "Added JWT authentication")
  (files '("src/auth/jwt.lisp"
           "src/middleware/auth.lisp"
           "tests/auth-tests.lisp"))
  (file-hashes '(("src/auth/jwt.lisp" . "a3f5c9...")
                 ("src/middleware/auth.lisp" . "b7d2e4...")
                 ("tests/auth-tests.lisp" . "c1a8f3...")))
  (conversation-context "messages-1-through-15")
  (test-results :passing)
  (parent-checkpoint "CP-0"))
```

### Pros & Cons

**Pros:**
- ✅ Automatic safety net (no user intervention)
- ✅ Multiple restore points (not just last change)
- ✅ Capture full context (files + conversation)
- ✅ Enables experimentation ("just try it, can undo")
- ✅ Transaction semantics (all-or-nothing)

**Cons:**
- ❌ Storage overhead (multiple snapshots)
- ❌ Checkpoint granularity tradeoffs (too many vs too few)
- ❌ Complex implementation (state tracking)
- ❌ Restore can be confusing (which checkpoint?)

**Complexity**: 4 (state management + persistence + WAL)
**Benefit**: 5 (game changer for safety and experimentation)

---

## Strategy 5: Git Worktree Multi-Agent Isolation

### Community Insights

**Why Worktrees for AI** ([Nick Mitchinson](https://www.nrmitchi.com/2025/10/using-git-worktrees-for-multi-feature-development-with-ai-agents/)):
> "Git worktrees transform how you manage parallel development, particularly powerful when working with AI coding agents. By providing isolated, persistent working directories for each branch, worktrees eliminate the friction of context switching while giving AI agents clear, bounded workspaces."

**Isolation Benefits** ([Mike Welsh](https://medium.com/@mike-welsh/supercharging-development-using-git-worktree-ai-agents-4486916435cb)):
> "Each worktree maintains completely independent file states, eliminating any possibility of cross-contamination between AI sessions. All worktrees share the same Git object database, making operations like commits, pulls, and merges seamless across all instances."

**Parallel AI Coding** ([Agent Interviews](https://docs.agentinterviews.com/blog/parallel-ai-coding-with-gitworktrees/)):
> "Parallel AI coding is an advanced development technique where you run multiple AI agents simultaneously on isolated copies of your codebase to implement the same feature. Each agent works independently on its own branch, producing different implementations of the same specification."

### Directory Structure

```
my-project/
├── .git/              (shared Git database)
├── main/              (main branch worktree)
│   ├── src/
│   └── tests/
├── .trees/            (all feature worktrees)
│   ├── agent-1-auth/  (feature/jwt-auth branch)
│   │   ├── src/       (modified for auth)
│   │   └── tests/
│   ├── agent-2-api/   (feature/graphql-api branch)
│   │   ├── src/       (modified for API)
│   │   └── tests/
│   └── agent-3-ui/    (feature/react-ui branch)
│       ├── src/
│       └── tests/
└── .gitignore         (contains ".trees")
```

### Multi-Agent Workflow

```
User: "Implement user authentication, GraphQL API, and React UI"
  ↓
Spawn 3 agents in parallel:
  Agent 1: git worktree add .trees/agent-1-auth -b feature/jwt-auth
  Agent 2: git worktree add .trees/agent-2-api -b feature/graphql-api
  Agent 3: git worktree add .trees/agent-3-ui -b feature/react-ui
  ↓
Each agent works independently:
  Agent 1: Implements JWT auth in .trees/agent-1-auth/
  Agent 2: Implements GraphQL in .trees/agent-2-api/
  Agent 3: Implements React UI in .trees/agent-3-ui/
  ↓
Agents commit to their branches:
  Agent 1: git commit -m "Add JWT authentication"
  Agent 2: git commit -m "Add GraphQL API"
  Agent 3: git commit -m "Add React UI components"
  ↓
User reviews each worktree/branch independently:
  cd .trees/agent-1-auth && test auth
  cd .trees/agent-2-api && test API
  cd .trees/agent-3-ui && test UI
  ↓
Merge approved changes:
  git checkout main
  git merge feature/jwt-auth
  git merge feature/graphql-api
  git merge feature/react-ui
  ↓
Clean up worktrees:
  git worktree remove .trees/agent-1-auth
  git worktree remove .trees/agent-2-api
  git worktree remove .trees/agent-3-ui
```

### Pros & Cons

**Pros:**
- ✅ Perfect isolation (separate file trees)
- ✅ Parallel development (3-8 agents)
- ✅ No context switching (each agent has own dir)
- ✅ Shared Git database (efficient)
- ✅ Standard Git workflow (branch/merge)

**Cons:**
- ❌ Disk space (multiple copies of codebase)
- ❌ Merge conflicts (when combining branches)
- ❌ Complexity (managing multiple worktrees)
- ❌ Stale worktrees (forgotten branches)
- ❌ Learning curve (Git worktree commands)

**Complexity**: 5 (very high)
**Benefit**: 5 (enables true parallel development)
**Best For**: Large features split into independent subtasks

---

## Strategy 6: Preview/Dry-Run Mode

### Importance

**Incremental Approach** ([Avoiding AI Fix Loop](https://byldd.com/tips-to-avoid-ai-fix-loop/)):
> "Build or debug in bite-sized pieces. If you ask the AI to implement a huge feature all at once, it might introduce multiple bugs at once, making it harder to pinpoint issues. Instead, have it add or fix one thing at a time."

### Tools with Preview

**Cline** ([Cline Features](https://www.qodo.ai/blog/best-ai-coding-assistant-tools/)):
> "Cline parsed the request, created a multi-step plan, and presented each action for approval before making any changes. Cline displayed a detailed preview of file writes and modifications before applying them."

**CodeGPT** ([CodeGPT Features](https://www.codegpt.co/)):
> "Before coding, CodeGPT studies your codebase and co-creates a detailed plan, ensuring it grasps your project's full scope and intent. You can see CodeGPT in action—reading files, weighing options, and suggesting edits. You review every step before anything changes."

**MCP-Sync** ([MCP-Sync](https://github.com/ztripez/mcp-sync)):
> "Features a dry-run mode to preview changes before applying them."

### Workflow Stages

```
┌─────────────────────────────────────┐
│ 1. PLAN                             │
│    - LLM proposes changes           │
│    - User reviews plan              │
│    - [Accept Plan] or [Revise]     │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│ 2. DRY RUN                          │
│    - Show diffs for all files       │
│    - Syntax check                   │
│    - Lint check                     │
│    - Dependency analysis            │
│    - [Preview OK] or [Back to Plan] │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│ 3. APPLY                            │
│    - Create checkpoint              │
│    - Apply changes to files         │
│    - Run tests (optional)           │
│    - Auto-commit (optional)         │
└─────────────────────────────────────┘
              ↓
┌─────────────────────────────────────┐
│ 4. VALIDATE                         │
│    - Run full test suite            │
│    - Check build                    │
│    - [Success] or [Rollback]        │
└─────────────────────────────────────┘
```

**Complexity**: 3 (requires state machine + diff generation)
**Benefit**: 5 (prevents catastrophic failures)

---

## Proposed Architecture for Agent-Q

### Design Goals

1. **Safety First**: No changes without user approval
2. **Granular Control**: Per-file or per-hunk review
3. **Checkpoint Everything**: Auto-save points for rollback
4. **Preview Before Apply**: Dry-run mode shows all impacts
5. **Lisp-Native**: Leverage REPL, packages, conditions

### Component Architecture

```
┌──────────────────────────────────────────────────────────┐
│                   AGENT-Q CORE                           │
│                                                          │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────┐│
│  │  Conversation  │  │  LLM Provider  │  │    Tool    ││
│  │    Manager     │  │   (OpenAI,     │  │  Registry  ││
│  │                │  │   Claude, etc) │  │            ││
│  └────────────────┘  └────────────────┘  └────────────┘│
└──────────────────────────────────────────────────────────┘
                          ↓
┌──────────────────────────────────────────────────────────┐
│           CHANGE MANAGEMENT LAYER (NEW)                  │
│                                                          │
│  ┌─────────────────────────────────────────────────────┐│
│  │ CHECKPOINT MANAGER                                  ││
│  │  - Create checkpoints before changes               ││
│  │  - Store file hashes + conversation context        ││
│  │  - Restore to any checkpoint                       ││
│  │  - Persist to ~/.agent-q/checkpoints/              ││
│  └─────────────────────────────────────────────────────┘│
│                                                          │
│  ┌─────────────────────────────────────────────────────┐│
│  │ CHANGE PLANNER                                      ││
│  │  - Collect all file changes from LLM                ││
│  │  - Build dependency graph (imports, references)     ││
│  │  - Detect conflicts (circular deps, type errors)   ││
│  │  - Generate unified diffs for preview              ││
│  └─────────────────────────────────────────────────────┘│
│                                                          │
│  ┌─────────────────────────────────────────────────────┐│
│  │ CHANGE COORDINATOR                                  ││
│  │  - Stage 1: Plan review (user approves intent)     ││
│  │  - Stage 2: Dry-run (show diffs, check syntax)     ││
│  │  - Stage 3: Apply (checkpoint → change → validate) ││
│  │  - Stage 4: Test (optional test suite run)         ││
│  └─────────────────────────────────────────────────────┘│
│                                                          │
│  ┌─────────────────────────────────────────────────────┐│
│  │ FILE TRACKER                                        ││
│  │  - Track which files in scope (like aider /add)    ││
│  │  - Maintain file hashes for change detection       ││
│  │  - Support @ mentions (@src/auth.lisp)             ││
│  └─────────────────────────────────────────────────────┘│
└──────────────────────────────────────────────────────────┘
                          ↓
┌──────────────────────────────────────────────────────────┐
│              EMACS INTEGRATION LAYER                     │
│                                                          │
│  ┌─────────────────────────────────────────────────────┐│
│  │ DIFF VIEWER (Enhanced)                              ││
│  │  - Multi-file diff buffer                          ││
│  │  - Per-file or per-hunk approval                   ││
│  │  - [a]ccept [r]eject [s]kip [q]uit navigation     ││
│  │  - Integration with Enhancement-1 (per-hunk)       ││
│  └─────────────────────────────────────────────────────┘│
│                                                          │
│  ┌─────────────────────────────────────────────────────┐│
│  │ CHECKPOINT UI                                       ││
│  │  - C-c q C (create checkpoint manually)            ││
│  │  - C-c q L (list checkpoints)                      ││
│  │  - C-c q R (restore checkpoint with preview)       ││
│  │  - Checkpoint browser (like magit log)             ││
│  └─────────────────────────────────────────────────────┘│
└──────────────────────────────────────────────────────────┘
```

### Data Structures

```lisp
;;; Checkpoint representation
(defstruct checkpoint
  (id "CP-20250131-143022")         ; Unique ID with timestamp
  (timestamp 3945678912)             ; Universal time
  (description "Added JWT auth")     ; User/auto description
  (type :auto)                       ; :auto or :manual
  (files '("src/auth.lisp"           ; List of affected files
           "src/middleware.lisp"))
  (file-states                       ; File content snapshots
   '(("src/auth.lisp" .
      (:hash "a3f5c9d2..."
       :content "..."
       :path "/abs/path/src/auth.lisp"))
     ("src/middleware.lisp" .
      (:hash "b7d2e4f8..."
       :content "..."
       :path "/abs/path/src/middleware.lisp"))))
  (conversation-state                ; Conversation snapshot
   (:message-count 15
    :last-user-message "Add JWT authentication"
    :last-assistant-message "I'll add JWT..."))
  (test-results :passing)            ; Test status at checkpoint
  (parent-checkpoint "CP-20250131-142015")  ; Previous checkpoint
  (metadata                          ; Additional info
   (:agent-version "0.2.5"
    :model "gpt-4o"
    :context-size 1234)))

;;; Change plan representation
(defstruct change-plan
  (id "PLAN-123")
  (intent "Refactor auth to use JWT")
  (files '("src/auth.lisp"
           "src/middleware.lisp"
           "tests/auth-tests.lisp"))
  (changes                           ; Per-file changes
   '(("src/auth.lisp" .
      (:action :modify
       :original "..."
       :modified "..."
       :diff "...unified diff..."))
     ("src/middleware.lisp" .
      (:action :modify
       :original "..."
       :modified "..."))
     ("tests/auth-tests.lisp" .
      (:action :create
       :content "..."))))
  (dependencies                      ; Dependency analysis
   '(("src/auth.lisp" imports-from "src/middleware.lisp")
     ("tests/auth-tests.lisp" tests "src/auth.lisp")))
  (conflicts                         ; Detected issues
   '((:type :circular-import
      :files ("src/auth.lisp" "src/middleware.lisp")
      :description "Circular import detected")))
  (status :pending)                  ; :pending :approved :applied :rolled-back
  (approved-by-user nil))            ; T after user approves plan
```

### Core Workflows

#### Workflow 1: Simple Single-File Change

```lisp
;; User adds file to context
(agent-q-add-file "src/auth.lisp")

;; User sends request
(agent-q-send "Add docstring to login function")

;; Agent loop
;; 1. LLM proposes single file change
;; 2. Change planner creates change-plan (1 file)
;; 3. Checkpoint manager creates CP-N (auto)
;; 4. Diff viewer shows change
;; 5. User accepts with C-c C-c
;; 6. Change applied using diff-apply-hunk
;; 7. Success → checkpoint remains
```

#### Workflow 2: Multi-File Change with Review

```lisp
;; User adds multiple files
(agent-q-add-files '("src/auth.lisp"
                     "src/middleware.lisp"
                     "tests/auth-tests.lisp"))

;; User sends request
(agent-q-send "Refactor auth to use JWT tokens")

;; Agent loop
;; 1. LLM proposes multi-file changes (3 files)
;; 2. Change planner:
;;    - Builds dependency graph
;;    - Detects no conflicts
;;    - Generates unified diffs for all 3 files
;;    - Creates change-plan-123
;; 3. STAGE 1: Plan Review
;;    - Show summary:
;;      "Will modify 3 files:
;;        - src/auth.lisp (add JWT support)
;;        - src/middleware.lisp (update auth check)
;;        - tests/auth-tests.lisp (add JWT tests)"
;;    - User: [A]pprove plan, [R]evise, [C]ancel
;;    - User presses 'A'
;; 4. STAGE 2: Dry-Run
;;    - Show all diffs in multi-file diff buffer
;;    - Syntax check all changes (compile-form with :dry-run)
;;    - User: [A]pply changes, [B]ack to plan, [C]ancel
;;    - User presses 'A'
;; 5. STAGE 3: Apply
;;    - Checkpoint manager creates CP-N with all 3 file states
;;    - Apply changes per-file:
;;      File 1: src/auth.lisp → diff-apply-hunk → Success
;;      File 2: src/middleware.lisp → diff-apply-hunk → Success
;;      File 3: tests/auth-tests.lisp → diff-apply-hunk → Success
;;    - All succeeded → mark change-plan as :applied
;; 6. STAGE 4: Validate (optional)
;;    - Run test suite
;;    - Tests pass → keep changes
;;    - Tests fail → offer rollback
```

#### Workflow 3: Checkpoint Rollback

```lisp
;; User realizes recent changes broke something
(agent-q-list-checkpoints)
;; →
;; CP-10  2025-01-31 14:30  Added JWT auth (3 files)  [current]
;; CP-9   2025-01-31 14:15  Fixed login bug (1 file)
;; CP-8   2025-01-31 14:00  Initial session

;; User wants to go back to CP-9
(agent-q-restore-checkpoint "CP-9")
;; → Shows diff preview:
;;   "This will revert 3 files to state at CP-9:
;;     - src/auth.lisp (remove JWT code)
;;     - src/middleware.lisp (restore old auth check)
;;     - tests/auth-tests.lisp (remove JWT tests)
;;   Continue? [Y/n]"
;; User: Y
;; → Restores all 3 files to CP-9 state
;; → Creates new checkpoint CP-11 "Rolled back to CP-9"
;; → Updates conversation context to match CP-9
```

---

## Implementation Roadmap

### Phase 1: Foundation (2-3 weeks)

**Checkpoint Manager** (Complexity: 3)
```lisp
;; File: src/checkpoint.lisp

(defun create-checkpoint (&key description type files)
  "Create checkpoint of current file states."
  (let ((cp (make-checkpoint
             :id (generate-checkpoint-id)
             :timestamp (get-universal-time)
             :description description
             :type (or type :auto)
             :files files
             :file-states (snapshot-files files)
             :conversation-state (snapshot-conversation)
             :parent-checkpoint *current-checkpoint-id*)))
    (persist-checkpoint cp)
    (setf *current-checkpoint-id* (checkpoint-id cp))
    cp))

(defun restore-checkpoint (checkpoint-id &key preview)
  "Restore files to checkpoint state."
  (let ((cp (load-checkpoint checkpoint-id)))
    (when preview
      (show-restore-preview cp))
    (unless preview
      (dolist (file-state (checkpoint-file-states cp))
        (restore-file file-state))
      (restore-conversation-context
       (checkpoint-conversation-state cp))
      (create-checkpoint
       :description (format nil "Rolled back to ~A" checkpoint-id)
       :type :manual))))
```

**File Tracker** (Complexity: 2)
```lisp
;; File: src/file-tracker.lisp

(defvar *tracked-files* nil
  "List of files in current change scope.")

(defun agent-q-add-file (path)
  "Add file to tracked set (like aider /add)."
  (push (make-file-entry :path path
                         :hash (file-hash path)
                         :added-at (get-universal-time))
        *tracked-files*))

(defun agent-q-add-files (paths)
  "Add multiple files."
  (mapc #'agent-q-add-file paths))

(defun parse-at-mentions (text)
  "Parse @src/auth.lisp mentions in user message."
  (let ((mentions '()))
    (ppcre:do-matches (start end "@([\\w/.\\-]+)" text)
      (push (subseq text (1+ start) end) mentions))
    (nreverse mentions)))
```

### Phase 2: Change Planning (3-4 weeks)

**Change Planner** (Complexity: 4)
```lisp
;; File: src/change-planner.lisp

(defun build-change-plan (llm-response)
  "Build change plan from LLM tool calls."
  (let ((plan (make-change-plan
               :id (generate-plan-id)
               :intent (extract-intent llm-response))))
    ;; Collect all file changes from tool calls
    (dolist (tool-call (response-tool-calls llm-response))
      (when (tool-modifies-file-p tool-call)
        (add-file-change plan tool-call)))
    ;; Analyze dependencies
    (setf (change-plan-dependencies plan)
          (analyze-dependencies (change-plan-files plan)))
    ;; Detect conflicts
    (setf (change-plan-conflicts plan)
          (detect-conflicts plan))
    plan))

(defun detect-conflicts (plan)
  "Detect circular imports, type errors, etc."
  (append
   (detect-circular-imports (change-plan-changes plan))
   (detect-undefined-symbols (change-plan-changes plan))
   (detect-type-mismatches (change-plan-changes plan))))
```

**Multi-File Diff Viewer** (Complexity: 3)
```elisp
;; File: contrib/sly-agent-q/sly-agent-q-multi-diff.el

(defun sly-agent-q-show-multi-file-diff (plan)
  "Show diffs for multiple files with per-file review."
  (let ((buffer (get-buffer-create "*Agent-Q Multi-File Diff*")))
    (with-current-buffer buffer
      (sly-agent-q-multi-diff-mode)
      (erase-buffer)

      ;; Header with plan summary
      (insert (format "Change Plan: %s\n" (plan-description plan)))
      (insert (format "Files: %d\n\n" (length (plan-files plan))))

      ;; Insert diff for each file
      (dolist (file (plan-files plan))
        (insert (format "━━━ %s ━━━\n" file))
        (insert (generate-unified-diff
                 (file-original-content file)
                 (file-modified-content file)
                 file))
        (insert "\n\n"))

      ;; Footer with commands
      (insert "[a] Accept file  [r] Reject file  [n] Next  [q] Quit\n"))

    (pop-to-buffer buffer)))
```

### Phase 3: Staged Application (2-3 weeks)

**Change Coordinator** (Complexity: 4)
```lisp
;; File: src/change-coordinator.lisp

(defun apply-change-plan (plan)
  "Apply multi-file changes with staged workflow."
  ;; Stage 1: Plan Review
  (unless (plan-approved-p plan)
    (when (not (user-approve-plan plan))
      (return-from apply-change-plan :plan-rejected)))

  ;; Stage 2: Dry-Run
  (let ((dry-run-results (dry-run-changes plan)))
    (when (has-errors-p dry-run-results)
      (show-dry-run-errors dry-run-results)
      (when (not (user-confirm "Apply anyway?"))
        (return-from apply-change-plan :dry-run-failed))))

  ;; Stage 3: Apply with Checkpoint
  (let ((checkpoint (create-checkpoint
                     :description (change-plan-intent plan)
                     :files (change-plan-files plan))))
    (handler-case
        (progn
          (dolist (file-change (change-plan-changes plan))
            (apply-file-change file-change))
          (setf (change-plan-status plan) :applied)
          :success)
      (error (e)
        ;; Rollback on any error
        (restore-checkpoint (checkpoint-id checkpoint))
        (error "Apply failed, rolled back: ~A" e))))

  ;; Stage 4: Validate (optional)
  (when (and *run-tests-after-apply*
             (change-plan-has-tests-p plan))
    (let ((test-results (run-test-suite)))
      (unless (tests-passed-p test-results)
        (when (user-confirm "Tests failed. Rollback?")
          (restore-checkpoint (checkpoint-id checkpoint))
          (return-from apply-change-plan :tests-failed))))))
```

### Phase 4: Polish & Integration (2 weeks)

- Checkpoint browser UI (like magit log)
- Integration with git auto-commit
- Conflict resolution UI
- Repository map (simple version)
- Documentation & examples

**Total Estimated Effort**: 9-12 weeks

---

## Comparison Matrix

| Strategy | Isolation | Undo | Preview | Complexity | Benefit | Best For |
|----------|-----------|------|---------|------------|---------|----------|
| **Repo Map + Git** (aider) | Git commits | Single undo | ❌ No | 4 | 5 | Large codebases |
| **Ephemeral Workspace** (macher) | File shadowing | Git-based | ✅ Diff | 3 | 4 | Exploratory changes |
| **Git Worktrees** (Cursor) | Separate trees | Merge revert | ✅ Plan | 5 | 5 | Parallel features |
| **Checkpoints** (Claude Code) | State snapshots | Multi-point | ❌ No | 4 | 5 | Experimentation |
| **Agent-Q Proposal** | Checkpoints | Multi-point | ✅ Staged | 4 | 5 | All scenarios |

---

## Risk Mitigation

### Risk 1: Storage Overhead

**Problem**: Checkpoints consume disk space (snapshot every change)

**Mitigation**:
- Store only diffs, not full file contents
- Compress checkpoint data
- Auto-cleanup old checkpoints (keep last 20, or 7 days)
- User-configurable retention policy

### Risk 2: Checkpoint Proliferation

**Problem**: Too many auto-checkpoints confuse users

**Mitigation**:
- Smart checkpoint creation (only on multi-file changes)
- Merge consecutive small changes
- Show only "important" checkpoints in UI by default
- Allow filtering: show all / manual only / multi-file only

### Risk 3: Restore Conflicts

**Problem**: File changed externally since checkpoint

**Mitigation**:
- Detect external changes via file hash comparison
- Show diff: checkpoint version vs current version vs restore target
- Offer three-way merge UI
- Allow partial restore (select which files)

### Risk 4: Performance Degradation

**Problem**: Checkpoint operations slow down workflow

**Mitigation**:
- Lazy snapshot (copy-on-write when possible)
- Async checkpoint persistence
- In-memory cache for recent checkpoints
- Indexed storage for fast lookup

### Risk 5: Context Window Still Limited

**Problem**: Even with checkpoints, can't handle 100-file changes

**Mitigation**:
- Break into sub-plans (Phase 1: files 1-10, Phase 2: files 11-20)
- Use repo map to reduce context needs
- Leverage agent memory to track "what we've done so far"
- Multi-agent with worktrees for truly massive changes

---

## Success Metrics

### Quantitative

1. **Change Success Rate**: % of multi-file changes that apply without errors
   - Target: >85% (vs. current ~40-60% for multi-agent systems)

2. **Rollback Usage**: How often users restore checkpoints
   - Baseline: Track in first month
   - Goal: <20% of changes need rollback (indicates good planning)

3. **Time to Apply**: Average time from plan approval to applied changes
   - Target: <30 seconds for typical 3-file change

4. **Storage Usage**: Disk space for checkpoints
   - Target: <100MB for 50 checkpoints (with compression)

### Qualitative

5. **User Confidence**: Survey "I feel safe experimenting with multi-file changes"
   - Target: 4.5+/5.0 rating

6. **Error Recovery**: "When changes fail, I can easily recover"
   - Target: 4.5+/5.0 rating

7. **Clarity**: "I understand what will change before approving"
   - Target: 4.5+/5.0 rating

---

## Future Extensions

### Multi-Agent Orchestration
Once checkpoints are stable, add:
- Spawn sub-agents for independent file groups
- Each agent gets own checkpoint branch
- Merge sub-agent results with conflict resolution

### Semantic Conflict Detection
Beyond syntax:
- Detect breaking API changes
- Identify test coverage gaps
- Check for race conditions in concurrent code

### Collaborative Checkpoints
For team settings:
- Share checkpoints with team members
- Checkpoint branching (multiple developers from same base)
- Merge checkpoint histories

### Time-Travel Debugging
Leverage checkpoint history:
- Bisect to find when bug was introduced
- Replay conversation from any checkpoint
- "What if" scenarios (fork from checkpoint)

---

## References

### Multi-File Change Strategies
- [aider Complex Change Example](https://aider.chat/examples/complex-change.html)
- [aider Usage Documentation](https://aider.chat/docs/usage.html)
- [macher GitHub](https://github.com/kmontag/macher)
- [Cursor Composer AI Model](https://skywork.ai/blog/vibecoding/cursor-composer-ai-model/)
- [Cursor 2.0 Architecture Guide](https://www.digitalapplied.com/blog/cursor-2-0-agent-first-architecture-guide)

### Checkpointing Systems
- [Claude Code Checkpoints](https://skywork.ai/skypage/en/claude-code-checkpoints-ai-coding/1976917740735229952)
- [Replit Checkpoints Documentation](https://docs.replit.com/replitai/checkpoints-and-rollbacks)
- [IBM STRATUS Undo-Retry](https://research.ibm.com/blog/undo-agent-for-cloud)
- [GitHub Issue: Native Undo/Checkpoint](https://github.com/anthropics/claude-code/issues/6001)
- [Kiro Checkpointing](https://kiro.dev/blog/introducing-checkpointing/)

### Git Worktrees
- [Using Git Worktrees for Multi-Feature Development](https://www.nrmitchi.com/2025/10/using-git-worktrees-for-multi-feature-development-with-ai-agents/)
- [Supercharging Development with Worktrees](https://medium.com/@mike-welsh/supercharging-development-using-git-worktree-ai-agents-4486916435cb)
- [Git Worktrees Changed My AI Workflow](https://nx.dev/blog/git-worktrees-ai-agents)
- [Parallel AI Coding with Worktrees](https://docs.agentinterviews.com/blog/parallel-ai-coding-with-gitworktrees/)

### Multi-File Failures & Coordination
- [Enterprise Multi-File Refactoring Failures](https://www.augmentcode.com/guides/enterprise-multi-file-refactoring-why-ai-breaks-at-scale)
- [Cursor Limitations at Scale](https://www.augmentcode.com/tools/cursor-ai-limitations-why-multi-file-refactors-fail-in-enterprise)
- [Why Multi-Agent Systems Fail](https://www.augmentcode.com/guides/why-multi-agent-llm-systems-fail-and-how-to-fix-them)
- [When AI Tools Fight Each Other](https://medium.com/@techdigesthq/when-ai-tools-fight-each-other-the-hidden-chaos-of-multi-agent-workflows-83169e8dcc6f)

### Transaction & State Management
- [Database ARIES Algorithm](https://www.cs.rpi.edu/~sibel/csci4380/fall2023/course_notes/transactions_durability.html)
- [WAL Redo/Undo Mechanisms](https://medium.com/@moali314/database-logging-wal-redo-and-undo-mechanisms-58c076fbe36e)
- [Memento Pattern for Undo](https://curatepartners.com/tech-skills-tools-platforms/mastering-the-memento-pattern-powering-undo-redo-and-state-restoration-in-software/)

### Emacs Undo Systems
- [undo-tree Package](https://www.dr-qubit.org/undo-tree.html)
- [undo-fu-session](https://github.com/emacsmirror/undo-fu-session)
- [EmacsWiki: UndoTree](https://www.emacswiki.org/emacs/UndoTree)

### Preview & Dry-Run Modes
- [Tips to Avoid AI Fix Loop](https://byldd.com/tips-to-avoid-ai-fix-loop/)
- [VS Code Chat Checkpoints](https://code.visualstudio.com/docs/copilot/chat/chat-checkpoints)
- [CodeGPT Features](https://www.codegpt.co/)

### Atomic Commits
- [Git Atomic Commits Best Practices](https://www.compilenrun.com/docs/devops/git/git-best-practices/git-atomic-commits/)
- [Atommit - AI Commit Assistant](https://atommit.com/)
- [Git Interactive Staging](https://git-scm.com/book/en/v2/Git-Tools-Interactive-Staging)

---

**End of Enhancement 3**
