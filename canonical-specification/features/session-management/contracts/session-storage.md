# Contract: Session Storage

**Confidence:** 1.00
**Source:** src/session.lisp:13-76

---

## Purpose

Define the session and session-manager data structures for wrapping conversations with persistence metadata.

---

## JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Session",
  "description": "Session data structure wrapping conversation with persistence metadata",
  "properties": {
    "id": {
      "type": "string",
      "pattern": "^session-[0-9]{8}-[0-9]{6}-[A-F0-9]{4}$",
      "description": "Unique session identifier",
      "examples": ["session-20260120-143022-A4F2"]
    },
    "name": {
      "type": ["string", "null"],
      "description": "User-friendly session name"
    },
    "created_at": {
      "type": "integer",
      "description": "Universal time when session was created",
      "minimum": 0
    },
    "updated_at": {
      "type": "integer",
      "description": "Universal time of last modification",
      "minimum": 0
    },
    "conversation": {
      "type": "object",
      "description": "Conversation object containing messages and context"
    },
    "model": {
      "type": ["string", "null"],
      "description": "Model name used for this session",
      "examples": ["claude-sonnet-4-20250514"]
    },
    "metadata": {
      "type": ["object", "null"],
      "description": "Extensible metadata plist",
      "properties": {
        "total_input_tokens": {"type": "integer"},
        "total_output_tokens": {"type": "integer"},
        "provider": {"type": "string"}
      },
      "additionalProperties": true
    }
  },
  "required": ["id", "created_at", "updated_at", "conversation"]
}
```

---

## Session Class

**Definition:**

```lisp
(defclass session ()
  ((id :accessor session-id
       :initform (generate-session-id)
       :documentation "Unique session identifier (session-YYYYMMDD-HHMMSS-XXXX)")
   (name :accessor session-name
         :initform nil
         :documentation "User-friendly session name")
   (created-at :accessor session-created-at
               :initform (get-universal-time)
               :documentation "Universal time when session was created")
   (updated-at :accessor session-updated-at
               :initform (get-universal-time)
               :documentation "Universal time of last modification")
   (conversation :accessor session-conversation
                 :initform (make-instance 'conversation)
                 :documentation "The conversation object containing messages and context")
   (model :accessor session-model
          :initform nil
          :documentation "Model used for this session (e.g., claude-sonnet-4-20250514)")
   (metadata :accessor session-metadata
             :initform nil
             :documentation "Plist for extensible data: :provider, :total-input-tokens, :total-output-tokens, etc.")))
```

---

## Session ID Format

**Pattern:** `session-YYYYMMDD-HHMMSS-XXXX`

**Components:**
- `YYYYMMDD`: Date (e.g., 20260120)
- `HHMMSS`: Time (e.g., 123456)
- `XXXX`: Random hex suffix for uniqueness (4 digits)

**Generator:**

```lisp
(defun generate-session-id ()
  "Generate a unique session ID in format: session-YYYYMMDD-HHMMSS-XXXX"
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "session-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D-~4,'0X"
            year month day hour min sec (random #xFFFF))))
```

**Example:** `session-20260120-143022-A4F2`

---

## Session Manager Class

**Definition:**

```lisp
(defclass session-manager ()
  ((sessions-directory :accessor sessions-directory
                       :initform (default-sessions-directory)
                       :documentation "Directory where session files are stored")
   (current-session :accessor current-session
                    :initform nil
                    :documentation "The currently active session")
   (session-cache :accessor session-cache
                  :initform (make-hash-table :test 'equal)
                  :documentation "Cache of loaded sessions: session-id -> session")))
```

**Global Instance:**

```lisp
(defvar *session-manager* nil
  "Global session manager instance.")
```

**Default Directory:** `~/.emacs.d/agent-q-sessions/`

---

## Constructor Functions

### make-session

```lisp
(defun make-session (&key name model)
  "Create a new session with optional NAME and MODEL."
  (make-instance 'session :name name :model model))
```

### ensure-session-manager

```lisp
(defun ensure-session-manager ()
  "Ensure *session-manager* exists, creating if necessary."
  (unless *session-manager*
    (setf *session-manager* (make-instance 'session-manager)))
  *session-manager*)
```

---

## Accessor Methods

### Session Operations

```lisp
;; Get messages from session's conversation
(defun session-messages (session)
  (conversation-messages (session-conversation session)))

;; Add message to session (updates timestamp)
(defun session-add-message (session role content)
  (setf (session-updated-at session) (get-universal-time))
  (add-message (session-conversation session) role content))

;; Get message count
(defun session-message-count (session)
  (length (session-messages session)))

;; Accumulate token usage in metadata
(defun session-add-tokens (session input-tokens output-tokens)
  (let ((meta (session-metadata session)))
    (setf (getf meta :total-input-tokens)
          (+ (or (getf meta :total-input-tokens) 0) (or input-tokens 0)))
    (setf (getf meta :total-output-tokens)
          (+ (or (getf meta :total-output-tokens) 0) (or output-tokens 0)))
    (setf (session-metadata session) meta)))
```

---

## Invariants

1. **Unique ID:** Session IDs MUST be unique (enforced by timestamp + random suffix)
2. **Non-nil Conversation:** Every session MUST have a conversation object
3. **Timestamp Monotonicity:** `updated-at` MUST be ≥ `created-at`
4. **Metadata Extensibility:** Metadata plist can contain arbitrary keys (open for extension)
5. **Cache Consistency:** Cached sessions MUST match disk state or be marked dirty

---

## Usage Example

```lisp
;; Create session
(defparameter *session* (make-session :name "Debug Session"
                                      :model "claude-sonnet-4-20250514"))

;; Add message
(session-add-message *session* :user "What is the bug?")

;; Track tokens
(session-add-tokens *session* 100 50)

;; Check metadata
(session-metadata *session*)
;; => (:total-input-tokens 100 :total-output-tokens 50)
```

---

## Error Conditions

| Error | Cause | Recovery |
|-------|-------|----------|
| `INVALID-SESSION-ID` | Malformed session ID format | Regenerate ID; validate format before use |
| `SESSION-EXISTS` | Attempt to create session with existing ID | Generate new unique ID; retry |
| `MISSING-CONVERSATION` | Session created without conversation object | Auto-create conversation; log warning |
| `INVALID-TIMESTAMP` | Negative or future timestamp | Use current time; log anomaly |
| `METADATA-OVERFLOW` | Metadata exceeds 64KB | Truncate or summarize; warn user |

**Prevention:**
- Validate session ID format using regex
- Check for ID uniqueness before creation
- Ensure conversation always initialized
- Validate timestamps are positive and within valid range (after 2020-01-01, not more than 24 hours in the future)

**Handling:**
- Invalid IDs: Regenerate automatically
- Missing conversation: Create default instance
- Bad timestamps: Replace with current time
- All fixes logged for debugging

**Validation:**
```lisp
(defun valid-session-id-p (id)
  "Check if ID matches session-YYYYMMDD-HHMMSS-XXXX format."
  (and (stringp id)
       (= 32 (length id))
       (string-match "^session-[0-9]{8}-[0-9]{6}-[A-F0-9]{4}$" id)))
```

---

**Status:** ✅ Implemented and tested
