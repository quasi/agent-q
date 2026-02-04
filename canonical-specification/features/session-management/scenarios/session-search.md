---
type: scenario
name: session-search
version: 1.0.0
feature: session-management
covers:
  - session-persistence
tags:
  - search
  - user-story
---

# Scenario: Session Search

**Flow:** Search and restore sessions by name or content
**Confidence:** 0.90

---

## Preconditions

- Multiple sessions exist with varied content
- User wants to find specific past conversation
- SLY connected

---

## Flow

### 1. User Initiates Search

**Action:** `C-c C-f` (or `M-x agent-q-search-sessions`)

**Prompt:** `Search sessions: ` → User enters "authentication"

**Elisp:**

```elisp
(defun agent-q-search-sessions (query)
  "Search past sessions for QUERY."
  (interactive "sSearch sessions: ")
  (agent-q--check-sly-connection)
  (let ((matches (agent-q--search-sessions-sync query)))
    (if matches
        (let* ((choices (mapcar (lambda (meta)
                                  (cons (format "%s - %s"
                                                (plist-get meta :id)
                                                (or (plist-get meta :name) "unnamed"))
                                        (plist-get meta :id)))
                                matches))
               (choice (completing-read
                        (format "Found %d matches: " (length matches))
                        choices nil t)))
          (when choice
            (let ((session-id (cdr (assoc choice choices))))
              (agent-q--switch-session-rpc session-id))))
      (message "No sessions found matching: %s" query))))
```

**RPC Call:**

```elisp
(sly-eval `(agent-q:agent-q-search-sessions "authentication"))
```

---

### 2. System Searches Sessions

**CL Side:**

```lisp
(defun search-sessions (query &optional (manager (ensure-session-manager)))
  "Search sessions by name and message content.
QUERY is a string to search for (case-insensitive).
Returns list of session metadata plists for matching sessions."
  (let* ((query-lower (string-downcase query))
         (all-sessions (list-sessions manager))
         (matches nil))
    (dolist (meta all-sessions)
      (let ((session-id (getf meta :id)))
        ;; Check name first (fast)
        (if (and (getf meta :name)
                 (search query-lower (string-downcase (getf meta :name))))
            (push meta matches)
            ;; Check message content (requires loading session)
            (let ((session (load-session session-id manager)))
              (when (and session (session-contains-query-p session query-lower))
                (push meta matches))))))
    (nreverse matches)))
```

**Optimization:** Name check is fast (metadata only), content check requires full load.

**Content Check:**

```lisp
(defun session-contains-query-p (session query-lower)
  "Check if SESSION contains QUERY-LOWER in any message content."
  (some (lambda (msg)
          (search query-lower (string-downcase (message-content msg))))
        (session-messages session)))
```

---

## Search Examples

### Search by Name

**Query:** `"debug"`

**Matches:**
- `session-20260120-143022-A4F2` - "Debug Feature X" (name match)
- `session-20260115-100000-D5E6` - "Debug API issue" (name match)

**No Content Search:** Name matches found immediately (fast).

---

### Search by Content

**Query:** `"authentication"`

**Process:**

1. List all sessions (10 sessions)
2. Check names for "authentication" (0 matches)
3. Load each session and check message content:
   - Session 1: No match
   - Session 2: Match found in message: "The **authentication** token is invalid"
   - Session 3: No match
   - Session 4: Match found in message: "User **authentication** failed"
   - ... (continue for all sessions)

**Matches:**
- `session-20260120-143022-A4F2` - "Debug Feature X" (content: "authentication token")
- `session-20260118-091500-G7H8` - "API Testing" (content: "authentication failed")

**Performance:** Slower (requires loading all sessions), but thorough.

---

### Mixed Search

**Query:** `"auth"` (short substring)

**Matches:**
- Name match: "Authorization System" (contains "auth")
- Content match: "The **auth**entication bug..." (contains "auth")
- Content match: "User **auth**orization check" (contains "auth")

**Case Insensitive:** "auth", "Auth", "AUTH" all match.

---

### 3. System Displays Matches

**Completion Interface:**

```
Found 3 matches: session-20260120-143022-A4F2 - Debug Feature X
                session-20260118-091500-G7H8 - API Testing
                session-20260115-100000-D5E6 - Debug API issue
```

**User selects:** "Debug Feature X"

---

### 4. System Switches to Selected Session

**Same as Switch Scenario:**

1. Save current session
2. Load selected session
3. Restore UI with messages
4. Update mode line

**Result:** User finds and restores conversation about authentication.

---

## Postconditions

**Session Restored:**

```
╭──────────────────────────────────────────────────────────╮
│ Agent-Q Chat - Debug Feature X                           │
│ [Debug Feature X:5 ↑2000↓1000]                          │
╰──────────────────────────────────────────────────────────╯

[User] Explain the authentication bug

[Assistant] The authentication token validation is...

[User] How do we fix it?

[Assistant] We can update the validator to...

───────────────────────────────────────────────────────────
> _
```

**User Found Target Conversation:** Successfully located authentication discussion.

---

## Timing

**Performance varies by search strategy:**

### Name-Only Search (Fast)

- List sessions: 10-50ms
- Check names (10 sessions): < 5ms
- **Total:** 15-55ms

### Content Search (Slow)

- List sessions: 10-50ms
- Load all sessions (10 sessions, cache miss): 500-2000ms
- Search all messages (100 messages avg): 50-100ms
- **Total:** 560-2150ms (0.5-2 seconds for 10 sessions)

**Scalability:**

| Sessions | Name Search | Content Search |
|----------|-------------|----------------|
| 10       | 15-55ms     | 500-2000ms     |
| 100      | 50-100ms    | 5-20 seconds   |
| 1000     | 200-500ms   | 50-200 seconds |

**Bottleneck:** Content search requires loading all sessions.

---

## Edge Cases

### No Matches

**Behavior:**

```elisp
(if matches
    ...
    (message "No sessions found matching: %s" query))
```

**Result:** User informed, no action taken.

### Empty Query

**Behavior:** Searches for empty string (matches all sessions).

**Result:** All sessions returned (effectively same as list).

### Special Characters in Query

**Behavior:** CL `search` handles special characters literally (no regex).

**Example:**

- Query: `"(defun"` → Matches sessions with "(defun" in content
- Query: `"$var"` → Matches sessions with "$var" in content

**No Escaping Needed:** Plain string search, not regex.

### Very Long Query

**Behavior:** Search string length unbounded, but unlikely to match.

**Performance:** Negligible impact (string search is fast).

---

## Search Patterns

### Find Session by Topic

**Query:** `"API"`

**Use Case:** User worked on API integration last week, wants to resume.

**Result:** Sessions mentioning "API" in name or content.

---

### Find Session by Error Message

**Query:** `"NullPointerException"`

**Use Case:** User debugged specific error, wants to review solution.

**Result:** Session containing error message in conversation.

---

### Find Session by Library Name

**Query:** `"cl-llm-provider"`

**Use Case:** User discussed integration with library, wants reference.

**Result:** Sessions mentioning library in messages.

---

## Known Limitations

### No Full-Text Index

**Current:** Linear scan through all sessions

**Impact:** Slow for large session collections (100+ sessions)

**Future:** Could add inverted index for fast content search

### No Regex Support

**Current:** Plain substring matching (case-insensitive)

**Impact:** Can't search patterns like "auth.*token" or "bug-\d+"

**Workaround:** Use simpler substring queries

### No Date Range Filter

**Current:** Searches all sessions regardless of date

**Impact:** Can't limit to "sessions from last month"

**Workaround:** Use temporal ordering in completion (most recent first)

### No Fuzzy Matching

**Current:** Exact substring match required

**Impact:** Typos prevent matches ("authetication" ≠ "authentication")

**Workaround:** Check spelling before search

---

## Performance Optimization

**Name-First Strategy:**

```lisp
;; Check name first (fast)
(if (and (getf meta :name)
         (search query-lower (string-downcase (getf meta :name))))
    (push meta matches)
    ;; Only check content if name didn't match
    (let ((session (load-session session-id manager)))
      (when (and session (session-contains-query-p session query-lower))
        (push meta matches))))
```

**Benefit:** Sessions with matching names found immediately (no load needed).

**Cache Utilization:**

Loaded sessions cached in `(session-cache manager)`.

Subsequent searches reuse cached sessions (faster).

---

**Status:** ✅ Implemented with optimization
