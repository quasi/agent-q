# How-to: Manage Persistent Sessions

Save your conversations, switch between multiple projects, and never lose work.

## Problem

You want to preserve conversations across Emacs restarts, organize work into separate projects, and easily switch between different tasks.

## Solution

Agent-Q provides **sessions**: persistent containers for conversations with metadata (name, timestamps, token usage). Sessions automatically save to disk and restore when you restart Emacs.

---

## Quickstart (2 minutes)

### 1. Your First Session

When you first use Agent-Q, a default session is created automatically:

```
Session: session-20260122-143000-A3F2
```

This is already saving your conversation.

### 2. Rename Your Session

Give it a meaningful name:

```
M-x sly-agent-q-rename-session
Enter name: feature-authentication
```

Now your mode line shows:

```
[feature-authentication:5 ↑1000↓500]
```

### 3. Create a Second Session

Start a new project:

```
C-c q n   (or M-x sly-agent-q-create-session)
Enter name: bug-fixing
```

You now have two separate conversations.

### 4. Switch Between Sessions

```
M-x sly-agent-q-switch-session
```

Select `feature-authentication` from completion. The conversation loads instantly.

That's it! Your work is organized and preserved.

---

## Understanding Sessions

### What is a Session?

A session is a **container** that holds:

- **Conversation** (all messages)
- **Context** (attached files, symbols)
- **Metadata** (model, timestamps, token counts)
- **Name** (your label)

### Session ID Format

Each session has a unique ID:

```
session-YYYYMMDD-HHMMSS-XXXX

Example: session-20260122-143000-A3F2
         └──────┬──────┘ └─┬─┘ └─┬─┘
            ISO date    Time   Random hex
```

### Storage Location

Sessions are saved to disk automatically:

```
~/.emacs.d/agent-q-sessions/
├── session-20260122-143000-A3F2.lisp
├── session-20260121-091500-B7D1.lisp
└── session-20260120-153000-C2E9.lisp
```

Each file is a readable S-expression (you can edit it manually if needed).

### Mode Line Display

The mode line shows session status:

```
[feature-auth:5 ↑1000↓500]
  └────┬─────┘│  └──┬─┘└─┬─┘
      Name    Msgs  In  Out
```

- **Name or ID**: Session name (if set) or ID
- **Message Count**: Number of messages in conversation
- **↑ Input**: Total input tokens consumed
- **↓ Output**: Total output tokens generated

---

## Creating Sessions

### Create New Session

```
C-c q n   Create new session
```

**Prompts for:**
- **Name (optional)**: Give it a descriptive name

**What happens:**
1. Current session auto-saves
2. New session created with unique ID
3. Empty conversation starts
4. Mode line updates

### Quick Create Without Name

Press `C-c q n` and just hit `RET` without entering a name. The session uses its ID as the identifier.

### When to Create New Sessions

Create separate sessions for:

- **Different projects** ("backend-api", "frontend-ui")
- **Different tasks** ("feature-auth", "bug-fixing", "refactoring")
- **Different experiments** ("try-approach-A", "try-approach-B")
- **Different contexts** ("learning-alexandria", "implementing-tests")

---

## Switching Sessions

### Switch Command

```
M-x sly-agent-q-switch-session
```

Shows completion with all sessions:

```
feature-authentication (2 hours ago, 25 messages)
bug-fixing (yesterday, 12 messages)
learning-clos (3 days ago, 8 messages)
```

Select one and press `RET`.

**What happens:**
1. Current session saves automatically
2. Selected session loads from disk
3. Conversation history displays
4. Context restores
5. Mode line updates

### Switch via Keyboard

If you have the session ID, you can type it directly in the completion prompt.

---

## Renaming Sessions

### Rename Current Session

```
M-x sly-agent-q-rename-session
Current: session-20260122-143000-A3F2
New name: feature-authentication
```

The session file on disk remains the same, but the name field updates.

### Why Rename?

Session IDs are hard to remember:
- `session-20260122-143000-A3F2` ← Hard to identify
- `feature-authentication` ← Easy to find

Rename sessions as soon as you know what you're working on.

---

## Deleting Sessions

### Delete Command

```
M-x sly-agent-q-delete-session
```

Shows completion with all sessions. Select one and confirm.

**Warning:** This permanently deletes the session file from disk. Cannot be undone.

### When to Delete

Delete sessions when:
- Work is complete and merged
- Experiment failed and isn't needed
- Cleaning up old sessions

**Tip:** Don't delete too quickly—old sessions can have useful conversation history.

---

## Auto-Save

### How Auto-Save Works

Sessions save automatically:

1. **Every 5 minutes** (timer-based)
2. **When you close Emacs** (buffer kill hook)
3. **Before switching sessions** (explicit save)
4. **When you send a message** (after agent responds)

### Disable Auto-Save

```elisp
(setq sly-agent-q-auto-save-interval nil)
```

**Not recommended.** You risk losing work if Emacs crashes.

### Manual Save

```elisp
M-x sly-agent-q-save-session
```

Forces an immediate save. Rarely needed with auto-save enabled.

---

## Searching Sessions

### Search Command

```
M-x sly-agent-q-search-sessions
Query: authentication
```

**Searches:**
- Session names
- Message content

**Returns:** List of matching sessions with context.

### Use Cases

- **Find old discussion:** "What did I learn about macros?"
- **Locate project:** "Where did I work on the auth system?"
- **Revisit solution:** "How did I fix that bug?"

---

## Session Metadata

### What's Stored

Each session tracks:

| Field | Description | Example |
|-------|-------------|---------|
| **ID** | Unique identifier | session-20260122-143000-A3F2 |
| **Name** | User-friendly label | feature-authentication |
| **Created** | When created | 2026-01-22 14:30:00 |
| **Updated** | Last modification | 2026-01-22 16:45:12 |
| **Model** | LLM model used | claude-sonnet-4-20250514 |
| **Input Tokens** | Total consumed | 1000 |
| **Output Tokens** | Total generated | 500 |
| **Messages** | All conversation turns | [User: ..., Agent-Q: ...] |
| **Context** | Attached items | [@src/agent.lisp] |

### Viewing Metadata

The mode line shows abbreviated metadata. For full details, open the session file:

```
M-x find-file ~/.emacs.d/agent-q-sessions/session-20260122-143000-A3F2.lisp
```

The file is readable Common Lisp S-expressions.

---

## Session Lifecycle

### Full Lifecycle Example

```
1. Create session
   C-c q n → Enter name: "implement-cache"

2. Work on task
   [Have conversation with Agent-Q...]

3. Auto-save (every 5 minutes)
   [Saves in background]

4. Switch to another project
   M-x sly-agent-q-switch-session → "bug-fixing"
   [Previous session saves automatically]

5. Return later
   M-x sly-agent-q-switch-session → "implement-cache"
   [Conversation and context restored]

6. Complete task
   [Session remains on disk]

7. Clean up (optional)
   M-x sly-agent-q-delete-session → "implement-cache"
   [Session deleted from disk]
```

---

## Advanced Features

### Session Migration (v1 → v2)

If you used an older version of Agent-Q, sessions stored in Elisp format (v1) automatically convert to the new Common Lisp format (v2) when loaded.

**No action needed.** Migration happens transparently.

### Manual Session Editing

Session files are readable S-expressions. You can edit them manually:

```lisp
;; ~/.emacs.d/agent-q-sessions/session-20260122-143000-A3F2.lisp
(:version 2
 :id "session-20260122-143000-A3F2"
 :name "feature-authentication"
 :created-at 3945234600
 :updated-at 3945237200
 :model "claude-sonnet-4-20250514"
 :metadata (:total-input-tokens 1000 :total-output-tokens 500)
 :messages ((:role :user
             :content "How do I implement authentication?"
             :timestamp 3945234600)
            (:role :assistant
             :content "You can use JWT tokens..."
             :timestamp 3945234650)))
```

**Use cases:**
- Fix corrupted sessions
- Merge sessions manually
- Export/import conversations

### Session Export/Import

Currently, copying session files works:

```bash
# Export
cp ~/.emacs.d/agent-q-sessions/session-20260122-143000-A3F2.lisp ~/backup/

# Import (on another machine)
cp ~/backup/session-20260122-143000-A3F2.lisp ~/.emacs.d/agent-q-sessions/
```

Restart Emacs or reload sessions:

```elisp
M-x sly-agent-q-list-sessions
```

### Session Size

Sessions have no size limit. Large conversations (100+ messages) may slow down loading slightly.

**Tip:** Create new sessions for new projects instead of continuing indefinitely.

---

## Keyboard Reference

| Key / Command | Action |
|---------------|--------|
| `C-c q n` | Create new session |
| `M-x sly-agent-q-switch-session` | Switch to different session |
| `M-x sly-agent-q-rename-session` | Rename current session |
| `M-x sly-agent-q-delete-session` | Delete a session |
| `M-x sly-agent-q-search-sessions` | Search sessions by name/content |
| `M-x sly-agent-q-list-sessions` | List all sessions |
| `M-x sly-agent-q-save-session` | Manually save current session |

---

## Tips

### Organize by Project

Create one session per project:

```
backend-api
frontend-components
database-schema
deployment-scripts
```

Easier to find conversations later.

### Use Descriptive Names

Good names:
- `implement-user-auth`
- `debug-memory-leak`
- `learn-clos-patterns`

Bad names:
- `session1`
- `test`
- `temp`

### Start Fresh When Context Changes

If you switch topics dramatically, create a new session. Don't force unrelated conversations into one session.

### Clean Up Periodically

Delete sessions you no longer need:

```
M-x sly-agent-q-list-sessions
[Review list]
M-x sly-agent-q-delete-session → [Select old sessions]
```

Keeps your session list manageable.

---

## Troubleshooting

### "Session list is empty"

**Check storage directory:**
```bash
ls ~/.emacs.d/agent-q-sessions/
```

If empty, sessions haven't been created yet. Use `C-c q n` to create one.

### "Session won't save"

**Check permissions:**
```bash
ls -ld ~/.emacs.d/agent-q-sessions/
```

Must be writable. Fix permissions:
```bash
chmod 755 ~/.emacs.d/agent-q-sessions/
```

### "Session file is corrupted"

**Symptoms:** Error when loading, garbled content

**Fix:**
1. Open the session file manually
2. Fix the S-expression syntax
3. Or delete the file and start fresh

### "Can't find old session"

**Use search:**
```
M-x sly-agent-q-search-sessions
Query: [Enter keywords from conversation]
```

### "Mode line doesn't update"

**Cause:** SLY disconnected

**Fix:**
```
M-x sly    Reconnect to Lisp
```

Mode line updates via RPC, requires active connection.

### "Auto-save is too frequent"

**Adjust interval:**
```elisp
(setq sly-agent-q-auto-save-interval 600)  ; 10 minutes
```

Default is 300 seconds (5 minutes).

---

## What Happens Behind the Scenes

### Session Structure (Common Lisp)

```lisp
(defclass session ()
  ((id :initarg :id)
   (name :initarg :name)
   (created-at :initarg :created-at)
   (updated-at :initarg :updated-at)
   (conversation :initarg :conversation)  ; Messages + context
   (model :initarg :model)
   (metadata :initarg :metadata)))
```

### Serialization Format

**v2 (Current):**
- Common Lisp S-expressions
- Human-readable, can edit manually
- Header comments for fast metadata parsing
- Universal-time timestamps

**v1 (Legacy):**
- Emacs Lisp format
- Automatically converted on load

### Session Manager

Global manager maintains:
- **Cache:** Hash table (session-id → session object)
- **Directory:** `~/.emacs.d/agent-q-sessions/`
- **Current Session:** Pointer to active session

### RPC Architecture

**Elisp side (UI):**
- Commands (create, switch, rename, delete)
- Mode line display
- Completion

**Common Lisp side (Persistence):**
- Save/load operations
- Serialization
- File management
- Session cache

**Communication:** SLY RPC calls connect the two.

---

## Next Steps

- **Chat Interface**: See `chat-interface.md` for using sessions with chat
- **Context**: See `context-completion.md` for managing context within sessions
- **Cost Tracking**: See `cost-estimation.md` for understanding token usage
