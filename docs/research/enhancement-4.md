# Enhancement 4: Interactive Chat Interface for Agent-Q

**Status:** Research Phase
**Created:** 2025-01-31
**Author:** Research compiled from multiple sources

---

## Executive Summary

This document analyzes how to convert Agent-Q's current readonly conversation buffer into a fully interactive chat interface similar to Claude Code, Cursor, Copilot Chat, and other modern AI coding assistants. The goal is to create an Emacs-native chat experience that leverages Emacs' strengths (extensibility, keybindings, major modes) while incorporating best practices from contemporary AI assistant UIs.

**Key Findings:**
- Modern AI assistants provide 3-5 different chat interaction modes (dedicated panel, inline, quick overlay, voice)
- Context management via @-mentions and #-symbols is now standard (not just a nice-to-have)
- Action buttons (Apply/Reject/Copy/Retry) must be inline with messages, not buried in menus
- Session persistence and conversation threading are critical for multi-turn workflows
- Emacs-specific patterns (comint-mode, gptel, ellama) offer proven architectures to build upon

**Current State:** Agent-Q uses a readonly buffer that displays `[AGENT-Q]` responses but lacks:
- Multi-line input editing
- Context attachment controls (@file, @function)
- Interactive action buttons
- Session/conversation management
- Message threading
- Markdown/code block rendering with syntax highlighting

**Proposed Solution:** Transform the conversation buffer into a comint-mode-derived interactive chat interface with dedicated input area, rich message rendering, context controls, and inline action buttons.

---

## Table of Contents

1. [Problem Analysis](#problem-analysis)
2. [Chat Interface Patterns from Leading Tools](#chat-interface-patterns-from-leading-tools)
3. [Emacs Chat Interface Patterns](#emacs-chat-interface-patterns)
4. [Core Chat Interface Features](#core-chat-interface-features)
5. [Proposed Architecture for Agent-Q](#proposed-architecture-for-agent-q)
6. [Implementation Roadmap](#implementation-roadmap)
7. [Comparison Matrix](#comparison-matrix)
8. [Risk Mitigation](#risk-mitigation)
9. [Success Metrics](#success-metrics)
10. [References](#references)

---

## Problem Analysis

### Current Agent-Q Limitations

Agent-Q's current conversation buffer is **readonly** with minimal interaction:

```elisp
;; Current state: readonly buffer with basic rendering
(with-current-buffer (get-buffer-create "*Agent-Q*")
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "[AGENT-Q]\n" 'face 'font-lock-keyword-face))
    (insert response)
    (insert "\n\n")))
```

**Problems:**
1. **No inline editing:** Users can't edit their input after sending (must re-type)
2. **No multi-line input:** Entering code snippets is awkward
3. **No context controls:** Can't easily attach files, functions, or regions
4. **No action buttons:** Can't Apply/Reject/Copy responses directly
5. **No session management:** Can't switch between conversations or restore history
6. **Poor code rendering:** No syntax highlighting in code blocks
7. **No threading:** Lost in long conversations, can't see branches
8. **No interactive elements:** Everything requires manual copy-paste

### Why This Matters

Multi-file refactorings fail 41-86.7% of the time (from enhancement-3.md). A better chat interface can reduce failures by:
- Making context explicit and visible (@file mentions)
- Providing granular approval controls (per-message action buttons)
- Enabling iterative refinement (edit + re-send)
- Preserving conversation history for debugging

---

## Chat Interface Patterns from Leading Tools

### Claude Code

**Interface Modes:**
- **Terminal CLI (default):** Interactive command-line chat with streaming responses
- **VS Code Extension:** Beautiful chat panel with no terminal required
- **Slack Integration:** @Claude mentions in threads trigger coding sessions

**Key Features ([source](https://www.builder.io/blog/claude-code), [source](https://marketplace.visualstudio.com/items?itemName=AndrePimenta.claude-code-chat)):**
- **@-mentions for files:** `@src/auth.lisp` adds file to context
- **Slash commands:** `/help`, `/commit`, `/review-pr` for common tasks
- **Checkpoint system:** Restore previous states, undo changes with git-based backups
- **Conversation history:** ↑ arrow navigates past chats, even from previous sessions
- **Plan & Thinking modes:** Toggle to see agent's reasoning process
- **Model selection:** Choose Opus/Sonnet/Haiku per-conversation
- **MCP server support:** Extend with custom tools via Model Context Protocol

**VS Code Extension Features:**
- Real-time streaming responses with visual feedback
- Syntax highlighting in code blocks
- Auto-resizing input area
- Native VS Code theming integration
- Zero terminal dependency

**Insight:** Claude Code's strength is **simplicity with power**. The terminal version is just a chat, but @-mentions and /commands make it incredibly efficient. The VS Code extension adds visual polish without sacrificing the terminal workflow.

---

### OpenCode

**Interface:** Terminal-based TUI (Text User Interface) implemented in Go ([source](https://opencode.ai/), [source](https://deepwiki.com/opencode-ai/opencode/5-chat-interface))

**Architecture:**
```
┌─────────────────────────────────────┐
│        Messages Display             │  ← Main chat view
├─────────────────────────────────────┤
│        Input Editor                 │  ← Multi-line input box
├─────────────────────────────────────┤
│        Status Bar                   │  ← Session info
└─────────────────────────────────────┘
```

**Key Features:**
- **Split-pane layout:** Messages above, input below, sidebar for context
- **@ completion dialog:** Type `@` to get file/folder completion overlay
- **Agent system:** Specialized assistants with custom prompts, models, and tool access
- **Context retention:** Remembers codebase, tracks related files across turns
- **Non-interactive mode:** `opencode "fix the bug"` for scripting/automation
- **Free models included:** Connect Claude, GPT, Gemini, or use built-in free models

**Insight:** OpenCode proves that **terminal-based chat can be beautiful**. The completion dialog pattern (@ triggers overlay) is more discoverable than requiring users to remember syntax.

---

### Cursor

**Interface Modes ([source](https://docs.cursor.com/composer), [source](https://forum.cursor.com/t/cursor-2-0-composer-in-app-browser-voice-more/139132)):**
1. **Ask Mode:** Read-only interface for questions and exploration
2. **Edit Mode:** Single-turn edits with workspace for precise changes
3. **Agent Mode (default):** Combines Ask + Edit + additional tools for complex tasks

**Composer Features:**
- **⌘I to open:** Instant access from any context
- **⌘N for new chat:** Fresh conversation without losing current one
- **Automatic context suggestions:** Embeddings-based relevance (no manual @ required)
- **File creation autonomy:** Composer creates files; chat requires manual creation
- **Checkpoint per iteration:** Click "checkout" near any checkpoint to revert
- **Multi-agent parallelism:** Run up to 8 agents simultaneously (Cursor 2.0)
- **Dedicated agent sidebar:** Agents, plans, runs are first-class objects
- **Browser integration:** Fetch docs, search web, preview output
- **Voice control:** Speak prompts instead of typing
- **4x faster performance:** Composer model completes tasks in under 30 seconds

**Composer vs Chat:**
> "A big difference is that Composer can create files autonomously. With the AI chat you have to create them manually, making Composer a lot faster to use."

**Agent Layout (Cursor 2.0):**
- Multiple agents working in parallel (one refactoring, one fixing tests, one polishing UI)
- Switch between agents like switching terminals or git branches
- Each agent has its own thread, plan, and execution history

**Insight:** Cursor shows that **mode separation matters**. Ask mode for learning, Edit mode for precision, Agent mode for autonomy. The checkpoint system is critical for safe experimentation.

---

### GitHub Copilot Chat (VS Code)

**Interface Modes ([source](https://code.visualstudio.com/docs/copilot/chat/copilot-chat), [source](https://code.visualstudio.com/docs/copilot/overview)):**
1. **Chat View (⌃⌘I):** Persistent chat panel in sidebar
2. **Inline Chat (⌘I):** In-editor overlay for flow state
3. **Quick Chat (⇧⌥⌘L):** Lightweight overlay for quick questions

**Context Management ([source](https://code.visualstudio.com/docs/copilot/chat/copilot-chat-context)):**
- **# symbols (not @):** `#file`, `#folder`, `#symbol`, `#terminal`, `#git`, etc.
- **Context picker:** Click "Add Context" button to select items visually
- **Smart context:** Copilot automatically includes relevant code using embeddings
- **Explicit control:** Remove past Q&A from history if irrelevant

**Customization Features ([source](https://code.visualstudio.com/docs/copilot/customization/overview)):**
- **Custom instructions:** `.github/copilot-instructions.md` for project-wide conventions
- **Prompt files:** `.prompt.md` files for reusable task templates
- **Custom agents:** `.agent.md` files with instructions + tools, shareable across workspace
- **Handoffs:** Guided workflows that transition between agents with pre-filled prompts

**Action Buttons:**
- **Accept All / Discard All** for code snippets
- **Revert button** in gutter for specific changes
- **Copy / Insert / Run** for generated code

**Insight:** Copilot's **handoff system** is brilliant for complex workflows. "Fix bug → write test → update docs" becomes three agent transitions with preserved context. The # vs @ distinction (# for VS Code entities, @ for agents) reduces confusion.

---

### Continue.dev

**Interface:** Sidebar chat panel for VS Code and JetBrains ([source](https://docs.continue.dev/chat/how-to-use-it), [source](https://docs.continue.dev/actions/how-to-use-it/))

**Keyboard Shortcuts:**
- **Cmd/Ctrl + L:** Send selected code to chat
- **Cmd/Ctrl + J:** Alternative binding
- **Cmd/Ctrl + Shift + R:** Copy terminal contents to chat for debugging

**Slash Commands ([source](https://docs.continue.dev/customize/slash-commands)):**
- **/share:** Generate shareable markdown transcript (specify `outputDir`)
- **/cmd:** Generate shell command from natural language, auto-paste to terminal (VS Code only)
- **/commit:** Show git diff, ask LLM for commit message
- **/onboard:** Analyze project structure, READMEs, dependencies for new developers

**Custom Slash Commands:**
- Write `.prompt` files (plain text with templating)
- Refer to files, URLs, highlighted code, etc.
- Supports MCP "prompts" via Model Context Protocol

**Quick Actions ([source](https://docs.continue.dev/actions/how-to-use-it/)):**
- **Buttons above classes/functions:** One-click actions
- **Lightbulb integration:** Error/warning underlines → Cmd/Ctrl + . → "Ask Continue"
- **Apply button:** Update existing code in editor with suggested changes

**Insight:** Continue's **/cmd** feature (natural language → shell command → auto-paste) is a game-changer for terminal workflows. The lightbulb integration shows how to embed AI assistance into existing IDE affordances.

---

## Emacs Chat Interface Patterns

### comint-mode (Command Interpreter Mode)

**Purpose:** Low-level library for all process and network interactions in Emacs ([source](https://www.masteringemacs.org/article/comint-writing-command-interpreter))

**Architecture:**
- Base class for Shell mode, IELM (Inferior Emacs Lisp Mode), IRC clients, REPL modes
- Handles prompt detection, input filtering, output processing, history management
- Customizable via hooks: `comint-input-filter-functions`, `comint-input-sender`, `comint-get-old-input`

**Key Variables:**
- `comint-prompt-regexp`: Regex for matching prompts (makes them read-only)
- `comint-prompt-read-only`: Make prompts immutable
- `comint-process-echoes`: Handle echo from subprocess
- `comint-scroll-to-bottom-on-input`: Auto-scroll behavior
- `comint-output-filter-functions`: Process output chunks

**Derived Mode Pattern:**
```elisp
(define-derived-mode my-chat-mode comint-mode "MyChat"
  "Major mode for interactive chat with AI."
  (setq comint-prompt-regexp "^[^>]*> ")
  (setq comint-prompt-read-only t)
  (setq comint-input-sender #'my-chat-send-input)
  (setq comint-get-old-input #'comint-get-old-input-default))

(defun my-chat-send-input (proc string)
  "Send STRING to PROC (AI backend)."
  ;; Custom send logic here
  )
```

**Challenges:**
> "comint-mode is a real mess right now. That last 20% truly is a pain. I used ielm as my implementation guide."

**Output Chunking Issue:**
> "When we send the string via comint-send-string, part of its bookkeeping is to apply these filter functions to the output. However, the output can come in chunks, so simply accepting the process output is not sufficient."

**Example: ChatGPT Shell ([source](https://xenodium.com/a-chatgpt-emacs-shell)):**
> "I set out to wire ChatGPT with Emacs's general command interpreter (comint). Having no previous comint experience, I figured I could just take a peek at an existing comint-derived mode. inferior-emacs-lisp-mode (ielm) seemed to fit the bill just fine, so I borrowed quite a bit to assemble a basic shell experience."

**Insight:** comint-mode provides the **foundation** (prompt handling, history, input/output routing) but requires significant customization. The chunked output problem means we need careful filter implementation for streaming responses.

---

### gptel vs ellama

Two popular Emacs LLM clients with different philosophies ([source](https://github.com/karthink/gptel), [source](https://blog.aheymans.xyz/post/llm_in_emacs/)):

**gptel ([source](https://github.com/karthink/gptel)):**
- **Philosophy:** "Works in the spirit of Emacs, available at any time and uniformly in any buffer"
- **Interface:** Buffer-as-chat. Send text up to cursor as prompt, insert response below cursor
- **Context:** Limited to active buffer or selected region
- **Backend:** Own code for talking to multiple LLM providers (not llm.el)
- **Usage:** More manual, feels like "real chat"
- **Streaming:** Supported
- **Integration:** Recently added to Doom Emacs as official llm module

**ellama:**
- **Philosophy:** Full-fledged LLM client with task-specific commands
- **Interface:** Separate commands for dozens of tasks (chat, summarize, refactor, translate, etc.)
- **Context:** Can add arbitrary regions, buffers, files to conversation context (key advantage)
- **Backend:** Built on llm.el package (supports Ollama, OpenAI, Vertex, GPT4All)
- **Usage:** Task-oriented workflow
- **Input:** Takes input from echo area (not super edit-friendly)
- **Streaming:** Native support

**Comparison Table:**

| Feature | gptel | ellama |
|---------|-------|--------|
| Input method | Any buffer | Echo area |
| Context scope | Buffer/region only | Regions + buffers + files |
| Task commands | Generic chat | Dozens of specialized commands |
| Backend | Custom | llm.el |
| Chat feel | More natural | More task-oriented |
| Editing input | Full buffer editing | Minibuffer limitations |

**Insight:** gptel's "buffer is chat" approach is elegant for Emacs but lacks ellama's rich context system. For Agent-Q, we want **gptel's editing flexibility + ellama's context richness**.

---

### ement.el (Matrix Client)

Modern chat client showing Emacs UI best practices ([source](https://github.com/alphapapa/ement.el)):

**UI Patterns:**
- **Default style:** IRC-like with username prefixes (enables `occur` to show all messages from a user)
- **Alternative styles:** Element-like (usernames above message groups), classic no-margins IRC
- **Timestamp headers:** Displayed when time passes between events or date changes
- **User colors:** Optional unique colors per user (customizable contrast)
- **magit-section integration:** Uses `magit-section` for UI components (collapsible regions, navigation)

**Insight:** Ement.el proves that **Emacs chat UIs can be modern** while leveraging Emacs strengths (occur, magit-section). The magit-section integration is key for collapsible message threads.

---

### ERC (IRC Client)

Built-in Emacs IRC client with modular design ([source](https://www.gnu.org/software/emacs/erc.html)):

**Modular Architecture:**
- **Core + 24+ loadable modules:** autoaway, fill (line wrapping), log (save buffers), spelling, bbdb (contact management)
- **Hooks everywhere:** Extensible at every interaction point
- **Timestamp support:** Built-in message timestamping
- **Flood control:** Rate limiting for sending messages
- **Nick completion:** Tab-complete usernames

**Insight:** ERC's **modular design** is a model for extensibility. Agent-Q should have core chat + optional modules (context enrichment, checkpoint system, semantic search).

---

## Core Chat Interface Features

### 1. Input Methods

Modern AI assistants provide multiple input modalities:

#### Dedicated Input Buffer (Recommended for Agent-Q)

**Pattern:**
```elisp
(defvar-local agent-q-input-start-marker nil
  "Marker for start of input area.")

(defun agent-q-create-input-area ()
  "Create dedicated input area at bottom of chat buffer."
  (goto-char (point-max))
  (insert (propertize "\n────────────────────────────────\n"
                     'face 'font-lock-comment-face
                     'read-only t))
  (setq agent-q-input-start-marker (point-marker))
  (insert (propertize "> " 'face 'comint-highlight-prompt
                      'read-only t 'rear-nonsticky t))
  (set-marker-insertion-type agent-q-input-start-marker t))
```

**Benefits:**
- Full Emacs editing (multi-line, undo, kill/yank)
- Familiar keybindings (C-a, C-e, M-f, M-b)
- Easy integration with yasnippet, abbrev-mode
- Can show context attachments inline

**Example (VS Code Copilot Chat):**
- Auto-resizing input area
- Syntax highlighting for code snippets in input
- Context pills (# tags) rendered as chips

---

#### Multi-line Input in Minibuffer

**Pattern ([source](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-Edit.html)):**
```elisp
;; C-q C-j inserts newline in minibuffer
;; C-o also works (open-line)
;; Minibuffer auto-expands with resize-mini-windows = grow-only

(setq resize-mini-windows 'grow-only)  ; Auto-expand, don't shrink
(setq max-mini-window-height 0.25)     ; Max 25% of frame height
```

**Limitations:**
- Can't split minibuffer
- Limited to `max-mini-window-height`
- Loses focus when switching windows
- Poor UX for large code snippets

**Use Case:** Quick questions, not multi-paragraph prompts.

---

#### Inline Input (Cursor/Copilot Style)

**Pattern:** Overlay input area at cursor position in source buffer

**Benefits:**
- Context is implicit (current file/function)
- Fast for flow state (⌘I → type → enter)
- No window switching

**Drawbacks:**
- Not suitable for long conversations
- Harder to review chat history
- Input obscures code

**Use Case:** Quick edits, single-turn questions.

---

### 2. Message Rendering

#### Markdown with GFM Support

**Required Library:** `markdown-mode` with `gfm-mode` ([source](https://jblevins.org/projects/markdown-mode/))

```elisp
(require 'markdown-mode)

(defun agent-q-render-message (content role)
  "Render CONTENT as markdown with ROLE-specific styling."
  (let ((start (point)))
    (insert content)
    (let ((end (point)))
      ;; Apply markdown rendering
      (save-excursion
        (goto-char start)
        (markdown-mode)  ; Temporarily enable for rendering
        (font-lock-fontify-region start end))

      ;; Add role-specific face
      (put-text-property start end 'agent-q-role role)
      (put-text-property start end 'face
                        (pcase role
                          ('user 'font-lock-string-face)
                          ('assistant 'font-lock-keyword-face))))))
```

**GFM Features to Support:**
- **Fenced code blocks:** ```language ... ```
- **Syntax highlighting:** Language-specific colors (via `markdown-mode`)
- **Strikethrough:** ~~text~~
- **Task lists:** - [ ] unchecked, - [x] checked (render as buttons)
- **Tables:** GitHub-style table rendering
- **Autolinked URLs:** Clickable without angle brackets

---

#### Code Block Syntax Highlighting

**Two Approaches:**

**1. Server-side (Build-time):**
- Shiki (VSCode TextMate Grammar) ([source](https://www.librechat.ai/docs/documentation/syntax_highlighting))
- Prism (language-xxx classes)
- Requires HTML rendering (not practical for Emacs)

**2. Client-side (Emacs):**
```elisp
(defun agent-q-highlight-code-block (start end lang)
  "Apply syntax highlighting to code block from START to END in LANG."
  (let ((mode (intern (concat lang "-mode"))))
    (when (fboundp mode)
      (with-current-buffer (current-buffer)
        (save-excursion
          (let ((string (buffer-substring-no-properties start end)))
            (with-temp-buffer
              (insert string)
              (funcall mode)
              (font-lock-ensure)
              (let ((props (text-properties-at (point-min))))
                ;; Copy text properties back to original buffer
                (with-current-buffer (other-buffer)
                  (set-text-properties start end props))))))))))
```

**Alternative:** Use `markdown-mode`'s built-in code block fontification:
```elisp
(setq markdown-fontify-code-blocks-natively t)
```

**Insight:** Emacs' `font-lock-mode` + native major modes give us free syntax highlighting. No need for external libraries like Prism/Shiki.

---

### 3. Context Management

#### @-Mentions for Files/Symbols

**Pattern:**

```elisp
(defvar agent-q-context-items nil
  "List of context items attached to current message.
Each item is a plist: (:type :file :path \"/path/to/file\" :display \"file.lisp\")")

(defun agent-q-insert-context-mention (type data)
  "Insert @-mention for TYPE with DATA."
  (let* ((display (pcase type
                   (:file (file-name-nondirectory (plist-get data :path)))
                   (:symbol (plist-get data :name))
                   (:buffer (buffer-name (plist-get data :buffer)))))
         (mention (format "@%s" display)))

    ;; Insert with special properties
    (insert (propertize mention
                       'face 'font-lock-constant-face
                       'agent-q-context-type type
                       'agent-q-context-data data
                       'mouse-face 'highlight
                       'help-echo (format "Context: %s" display)
                       'keymap agent-q-context-mention-map))

    ;; Track in context list
    (push (list :type type :data data :display display) agent-q-context-items)))

;; Keymap for mentions
(defvar agent-q-context-mention-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'agent-q-visit-context-item)
    (define-key map (kbd "RET") #'agent-q-visit-context-item)
    (define-key map (kbd "DEL") #'agent-q-remove-context-item)
    map))
```

**Completion Interface:**

```elisp
(defun agent-q-complete-context-mention ()
  "Complete @-mention using completion framework."
  (interactive)
  (when (looking-back "@\\([^ ]*\\)" (line-beginning-position))
    (let* ((prefix (match-string 1))
           (candidates (agent-q-gather-context-candidates prefix))
           (choice (completing-read "Context: " candidates)))
      (delete-region (match-beginning 1) (match-end 1))
      (agent-q-insert-context-mention (plist-get choice :type)
                                      (plist-get choice :data)))))

(defun agent-q-gather-context-candidates (prefix)
  "Gather context candidates matching PREFIX."
  (append
   ;; Files in project
   (mapcar (lambda (file)
             (list :type :file :path file :display (file-name-nondirectory file)))
           (project-files (project-current)))

   ;; Symbols in current buffer
   (mapcar (lambda (sym)
             (list :type :symbol :name (car sym) :location (cdr sym)))
           (agent-q-gather-symbols))

   ;; Open buffers
   (mapcar (lambda (buf)
             (list :type :buffer :buffer buf :display (buffer-name buf)))
           (buffer-list))))
```

**Visual Representation:**

```
You: Fix the bug in @auth.lisp related to @jwt-decode
     ─────────────────────┬──────────────────┬──────────
                          │                  │
                    [file context]      [symbol context]
```

---

#### Context Panel/Sidebar

**Pattern (inspired by Continue.dev):**

```elisp
(defun agent-q-show-context-panel ()
  "Show context panel in side window."
  (interactive)
  (let ((buf (get-buffer-create "*Agent-Q Context*")))
    (with-current-buffer buf
      (agent-q-context-panel-mode)
      (agent-q-refresh-context-panel))
    (display-buffer buf
                   '(display-buffer-in-side-window
                     (side . right)
                     (window-width . 40)))))

(defun agent-q-refresh-context-panel ()
  "Refresh context panel contents."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Context Items\n" 'face 'bold))
    (insert (propertize "─────────────\n" 'face 'shadow))

    (dolist (item agent-q-context-items)
      (insert (propertize (format "[%s] " (plist-get item :type))
                         'face 'font-lock-type-face))
      (insert (propertize (plist-get item :display)
                         'face 'link
                         'agent-q-context-item item
                         'mouse-face 'highlight))
      (insert "  ")
      (insert-button "✕"
                     'action (lambda (_) (agent-q-remove-context-item item))
                     'face 'error)
      (insert "\n"))))
```

---

### 4. Action Buttons

#### Inline Button Implementation

**Pattern:**

```elisp
(defun agent-q-insert-action-buttons ()
  "Insert action buttons after assistant message."
  (insert "\n")
  (let ((button-start (point)))

    ;; Apply button
    (insert-button "Apply"
                   'action #'agent-q-apply-response
                   'face 'success
                   'help-echo "Apply changes to buffer")
    (insert "  ")

    ;; Reject button
    (insert-button "Reject"
                   'action #'agent-q-reject-response
                   'face 'error
                   'help-echo "Reject changes")
    (insert "  ")

    ;; Copy button
    (insert-button "Copy"
                   'action #'agent-q-copy-response
                   'face 'font-lock-function-name-face
                   'help-echo "Copy to clipboard")
    (insert "  ")

    ;; Retry button
    (insert-button "Retry"
                   'action #'agent-q-retry-request
                   'face 'warning
                   'help-echo "Retry with same prompt")

    (put-text-property button-start (point) 'agent-q-action-buttons t))
  (insert "\n\n"))

(defun agent-q-apply-response (&optional button)
  "Apply response to target buffer."
  (interactive)
  (let* ((msg-start (agent-q-message-start-position button))
         (msg-end (agent-q-message-end-position button))
         (content (buffer-substring-no-properties msg-start msg-end))
         (code-blocks (agent-q-extract-code-blocks content)))

    (if (null code-blocks)
        (message "No code blocks to apply")

      ;; Show diff and apply
      (agent-q-show-diff-and-apply code-blocks))))
```

**Button States:**

```elisp
(defun agent-q-set-button-state (button state)
  "Set BUTTON to STATE (active, disabled, loading)."
  (pcase state
    ('active
     (button-put button 'face 'success))
    ('disabled
     (button-put button 'face 'shadow)
     (button-put button 'action nil))
    ('loading
     (button-put button 'face 'warning)
     (button-put button 'help-echo "Processing..."))))
```

**Insight:** Buttons should be **first-class UI elements** with states, not just text with keybindings. Emacs' `insert-button` provides mouse + keyboard interaction out of the box.

---

#### Code Block Extraction

```elisp
(defun agent-q-extract-code-blocks (content)
  "Extract code blocks from markdown CONTENT.
Returns list of plists with :language, :code, :start, :end."
  (let ((blocks nil))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))

      ;; Find fenced code blocks: ```lang ... ```
      (while (re-search-forward "^```\\([a-z]*\\)\n\\(.*?\\)^```" nil t)
        (push (list :language (match-string 1)
                   :code (match-string 2)
                   :start (match-beginning 0)
                   :end (match-end 0))
              blocks)))
    (nreverse blocks)))
```

---

### 5. Session Persistence and History

#### Session Storage

**Data Structure:**

```elisp
(defstruct agent-q-session
  (id (format "session-%s" (format-time-string "%Y%m%d-%H%M%S")))
  (created-at (current-time))
  (updated-at (current-time))
  (messages nil)  ; List of message plists
  (context-items nil)
  (model "claude-sonnet-4-5")
  (metadata nil))  ; User-defined metadata

(defstruct agent-q-message
  (id nil)
  (role nil)  ; 'user or 'assistant
  (content nil)
  (timestamp nil)
  (context-items nil)  ; Context at time of message
  (tool-calls nil)
  (metadata nil))
```

**Storage Backend:**

```elisp
(defcustom agent-q-sessions-directory
  (expand-file-name "agent-q-sessions" user-emacs-directory)
  "Directory for storing Agent-Q sessions."
  :type 'directory
  :group 'agent-q)

(defun agent-q-save-session (session)
  "Save SESSION to disk."
  (let ((file (expand-file-name
               (format "%s.el" (agent-q-session-id session))
               agent-q-sessions-directory)))
    (with-temp-file file
      (insert ";; -*- lexical-binding: t; -*-\n")
      (insert (format ";; Agent-Q Session: %s\n" (agent-q-session-id session)))
      (insert (format ";; Created: %s\n\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S"
                                       (agent-q-session-created-at session))))
      (pp (list 'agent-q-session
               :id (agent-q-session-id session)
               :created-at (agent-q-session-created-at session)
               :updated-at (agent-q-session-updated-at session)
               :messages (agent-q-session-messages session)
               :context-items (agent-q-session-context-items session)
               :model (agent-q-session-model session)
               :metadata (agent-q-session-metadata session))
          (current-buffer)))))

(defun agent-q-load-session (id)
  "Load session with ID from disk."
  (let ((file (expand-file-name (format "%s.el" id) agent-q-sessions-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (read (current-buffer))))))
```

**Session Switcher:**

```elisp
(defun agent-q-switch-session ()
  "Switch to a different session."
  (interactive)
  (let* ((sessions (agent-q-list-sessions))
         (choices (mapcar (lambda (s)
                           (cons (format "%s - %s"
                                       (agent-q-session-id s)
                                       (format-time-string "%Y-%m-%d %H:%M"
                                                          (agent-q-session-created-at s)))
                                s))
                         sessions))
         (choice (completing-read "Switch to session: " choices)))
    (when choice
      (agent-q-restore-session (cdr (assoc choice choices))))))

(defun agent-q-list-sessions ()
  "Return list of all saved sessions."
  (let ((files (directory-files agent-q-sessions-directory t "\\.el$")))
    (mapcar #'agent-q-load-session
            (mapcar #'file-name-base files))))
```

**Insight:** Store sessions as readable Lisp files (not JSON/binary). Enables version control, manual editing, and easy debugging.

---

#### Conversation History Navigation

**Pattern (inspired by Claude Code):**

```elisp
(defvar agent-q-history-position 0
  "Current position in message history (0 = most recent).")

(defun agent-q-history-previous ()
  "Navigate to previous message in history."
  (interactive)
  (when (< agent-q-history-position (1- (length agent-q-message-history)))
    (setq agent-q-history-position (1+ agent-q-history-position))
    (agent-q-insert-history-message agent-q-history-position)))

(defun agent-q-history-next ()
  "Navigate to next message in history."
  (interactive)
  (when (> agent-q-history-position 0)
    (setq agent-q-history-position (1- agent-q-history-position))
    (agent-q-insert-history-message agent-q-history-position)))

(defun agent-q-insert-history-message (position)
  "Insert message at POSITION in history into input area."
  (let ((msg (nth position agent-q-message-history)))
    (agent-q-clear-input)
    (insert (agent-q-message-content msg))

    ;; Restore context items
    (setq agent-q-context-items (agent-q-message-context-items msg))))

;; Bind to M-p and M-n (like comint-mode)
(define-key agent-q-mode-map (kbd "M-p") #'agent-q-history-previous)
(define-key agent-q-mode-map (kbd "M-n") #'agent-q-history-next)
```

**Insight:** M-p/M-n for history navigation is muscle memory for Emacs users (shell-mode, comint-mode, minibuffer). Don't reinvent.

---

### 6. Conversation Threading

**Thread Structure:**

```elisp
(defstruct agent-q-thread
  (id nil)
  (parent-message-id nil)  ; Message that started this thread
  (messages nil)           ; Messages in this thread
  (status 'active)         ; 'active, 'archived, 'resolved
  (metadata nil))

(defun agent-q-create-thread-from-message (message-id)
  "Create a new thread branching from MESSAGE-ID."
  (let ((thread (make-agent-q-thread
                 :id (agent-q-generate-id)
                 :parent-message-id message-id
                 :messages (list message-id)
                 :status 'active)))
    (push thread agent-q-threads)
    thread))
```

**Thread Visualization:**

```
┌─ Thread 1: "Fix auth bug" [active]
│  ├─ User: Fix the JWT decode issue
│  ├─ Agent: Here's the fix... [Apply] [Reject]
│  └─ User: Now add tests
│     └─ Agent: Added tests... [Apply] [Reject]
│
└─ Thread 2: "Refactor DB layer" [archived]
   ├─ User: Refactor connection pool
   └─ Agent: Refactored... [Applied ✓]
```

**Thread Switcher:**

```elisp
(defun agent-q-switch-thread ()
  "Switch to a different conversation thread."
  (interactive)
  (let* ((choices (mapcar (lambda (thread)
                           (cons (format "[%s] %s - %d messages"
                                       (agent-q-thread-status thread)
                                       (agent-q-thread-summary thread)
                                       (length (agent-q-thread-messages thread)))
                                thread))
                         agent-q-threads))
         (choice (completing-read "Switch to thread: " choices)))
    (when choice
      (agent-q-display-thread (cdr (assoc choice choices))))))
```

**Insight:** Threads prevent "conversation drift" where a single chat tries to tackle multiple unrelated topics. Each thread maintains focused context.

---

## Proposed Architecture for Agent-Q

### High-Level Component Design

```
┌─────────────────────────────────────────────────────────────┐
│                   Agent-Q Chat Buffer                       │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  Message Area (comint-mode derived)                 │   │
│  │  ──────────────────────────────────────────────     │   │
│  │                                                      │   │
│  │  [USER] Fix the bug in @auth.lisp                   │   │
│  │                                                      │   │
│  │  [AGENT-Q] Here's the fix:                          │   │
│  │  ```lisp                                            │   │
│  │  (defun jwt-decode (token)                          │   │
│  │    ...)                                             │   │
│  │  ```                                                │   │
│  │  [Apply] [Reject] [Copy] [Retry]                   │   │
│  │                                                      │   │
│  └─────────────────────────────────────────────────────┘   │
│                                                             │
│  ──────────────────────────────────────────────────────    │
│  Input Area (multi-line editing)                           │
│  ──────────────────────────────────────────────────────    │
│  > Add tests for jwt-decode                                │
│  > @tests/auth-test.lisp                                   │
│  >█                                                         │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│  [Session: main] [Thread: 1/3] [Model: sonnet-4-5]         │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────┐
│   Context Panel (side)  │
├─────────────────────────┤
│ Context Items           │
│ ─────────────           │
│ [file] auth.lisp      ✕ │
│ [sym] jwt-decode      ✕ │
│ [buf] *scratch*       ✕ │
│                         │
│ [+ Add Context]         │
└─────────────────────────┘
```

### Core Data Structures

```elisp
;;; Message representation
(defstruct agent-q-message
  (id (agent-q-generate-id))
  (role nil)  ; 'user or 'assistant
  (content nil)
  (timestamp (current-time))
  (context-items nil)
  (tool-calls nil)
  (parent-id nil)  ; For threading
  (metadata nil))

;;; Context item representation
(defstruct agent-q-context-item
  (type nil)  ; :file, :symbol, :buffer, :region
  (display-name nil)
  (data nil)  ; Type-specific data (path, position, etc.)
  (content nil))  ; Actual content to send to LLM

;;; Session representation
(defstruct agent-q-session
  (id (format "session-%s" (format-time-string "%Y%m%d-%H%M%S")))
  (created-at (current-time))
  (updated-at (current-time))
  (messages nil)
  (threads nil)
  (context-items nil)
  (model "claude-sonnet-4-5")
  (metadata nil))

;;; Thread representation
(defstruct agent-q-thread
  (id (agent-q-generate-id))
  (parent-message-id nil)
  (messages nil)
  (status 'active)  ; 'active, 'archived, 'resolved
  (summary nil)
  (metadata nil))
```

### Major Mode Definition

```elisp
(define-derived-mode agent-q-chat-mode comint-mode "Agent-Q-Chat"
  "Major mode for interactive chat with Agent-Q.

Key bindings:
\\{agent-q-chat-mode-map}"
  :group 'agent-q

  ;; comint configuration
  (setq comint-prompt-regexp "^> ")
  (setq comint-prompt-read-only t)
  (setq comint-input-sender #'agent-q-send-input)
  (setq comint-get-old-input #'agent-q-get-input)
  (setq comint-process-echoes nil)

  ;; Scrolling behavior
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-move-point-for-output t)

  ;; Local variables
  (setq-local agent-q-current-session (make-agent-q-session))
  (setq-local agent-q-current-thread nil)
  (setq-local agent-q-context-items nil)
  (setq-local agent-q-message-history nil)

  ;; Markdown rendering
  (setq-local markdown-fontify-code-blocks-natively t)

  ;; Line wrapping
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)

  ;; Hooks
  (add-hook 'kill-buffer-hook #'agent-q-save-session nil t))

(defvar agent-q-chat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Input control
    (define-key map (kbd "RET") #'agent-q-send-or-newline)
    (define-key map (kbd "C-c RET") #'agent-q-send-input)
    (define-key map (kbd "C-c C-c") #'agent-q-send-input)

    ;; History navigation
    (define-key map (kbd "M-p") #'agent-q-history-previous)
    (define-key map (kbd "M-n") #'agent-q-history-next)

    ;; Context management
    (define-key map (kbd "C-c @") #'agent-q-add-context-file)
    (define-key map (kbd "C-c #") #'agent-q-add-context-symbol)
    (define-key map (kbd "C-c C-x") #'agent-q-clear-context)

    ;; Session management
    (define-key map (kbd "C-c C-s") #'agent-q-switch-session)
    (define-key map (kbd "C-c C-n") #'agent-q-new-session)

    ;; Thread management
    (define-key map (kbd "C-c C-t") #'agent-q-switch-thread)
    (define-key map (kbd "C-c C-b") #'agent-q-branch-thread)

    map)
  "Keymap for `agent-q-chat-mode'.")
```

### Input Handling

```elisp
(defun agent-q-send-or-newline ()
  "Send input if at end of buffer and not holding Shift, else insert newline."
  (interactive)
  (if (and (eobp)
           (not (member 'shift (event-modifiers last-input-event))))
      (agent-q-send-input)
    (insert "\n")))

(defun agent-q-send-input ()
  "Send current input to Agent-Q."
  (interactive)
  (let ((input (agent-q-get-input)))
    (when (string-blank-p input)
      (user-error "Input is empty"))

    ;; Create user message
    (let ((msg (make-agent-q-message
                :role 'user
                :content input
                :context-items (copy-sequence agent-q-context-items))))

      ;; Add to session
      (agent-q-add-message msg)

      ;; Render user message
      (agent-q-render-user-message msg)

      ;; Clear input area
      (agent-q-clear-input)

      ;; Send to agent (async)
      (agent-q-process-message msg))))

(defun agent-q-get-input ()
  "Get current input text."
  (buffer-substring-no-properties agent-q-input-start-marker (point-max)))

(defun agent-q-clear-input ()
  "Clear input area."
  (delete-region agent-q-input-start-marker (point-max))
  (goto-char (point-max)))
```

### Message Rendering

```elisp
(defun agent-q-render-user-message (message)
  "Render user MESSAGE in chat buffer."
  (let ((inhibit-read-only t))
    (goto-char agent-q-input-start-marker)
    (backward-char 1)  ; Before separator line

    (insert "\n")
    (let ((start (point)))
      ;; Message header
      (insert (propertize "[USER] " 'face 'font-lock-string-face))
      (insert (propertize (format-time-string "%H:%M:%S"
                                             (agent-q-message-timestamp message))
                         'face 'shadow))
      (insert "\n")

      ;; Message content
      (insert (agent-q-message-content message))
      (insert "\n")

      ;; Context pills
      (when (agent-q-message-context-items message)
        (insert (propertize "Context: " 'face 'shadow))
        (dolist (item (agent-q-message-context-items message))
          (agent-q-insert-context-pill item))
        (insert "\n"))

      ;; Mark region as user message
      (put-text-property start (point) 'agent-q-message message)
      (put-text-property start (point) 'read-only t))

    (insert "\n")))

(defun agent-q-render-assistant-message (message)
  "Render assistant MESSAGE in chat buffer."
  (let ((inhibit-read-only t))
    (goto-char agent-q-input-start-marker)
    (backward-char 1)

    (insert "\n")
    (let ((start (point)))
      ;; Message header
      (insert (propertize "[AGENT-Q] " 'face 'font-lock-keyword-face))
      (insert (propertize (format-time-string "%H:%M:%S"
                                             (agent-q-message-timestamp message))
                         'face 'shadow))
      (insert "\n")

      ;; Message content (with markdown rendering)
      (let ((content-start (point)))
        (insert (agent-q-message-content message))
        (agent-q-render-markdown content-start (point)))
      (insert "\n")

      ;; Action buttons
      (agent-q-insert-action-buttons message)

      ;; Mark region
      (put-text-property start (point) 'agent-q-message message)
      (put-text-property start (point) 'read-only t))

    (insert "\n")))

(defun agent-q-render-markdown (start end)
  "Apply markdown rendering from START to END."
  (save-excursion
    (goto-char start)

    ;; Find and highlight code blocks
    (while (re-search-forward "^```\\([a-z]*\\)\n" end t)
      (let* ((lang (match-string 1))
             (code-start (point))
             (code-end (when (re-search-forward "^```" end t)
                        (match-beginning 0))))
        (when code-end
          ;; Apply syntax highlighting
          (agent-q-highlight-code-block code-start code-end lang)

          ;; Make block stand out
          (put-text-property (match-beginning 0) code-end
                           'face 'markdown-code-face))))))
```

### Context Management

```elisp
(defun agent-q-add-context-file ()
  "Add file to context via completion."
  (interactive)
  (let* ((file (read-file-name "Add file to context: "))
         (content (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
         (item (make-agent-q-context-item
                :type :file
                :display-name (file-name-nondirectory file)
                :data (list :path file)
                :content content)))
    (push item agent-q-context-items)
    (agent-q-insert-context-pill item)
    (message "Added %s to context" (file-name-nondirectory file))))

(defun agent-q-add-context-symbol ()
  "Add symbol at point to context."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (bounds (bounds-of-thing-at-point 'defun))
         (content (when bounds
                   (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (item (make-agent-q-context-item
                :type :symbol
                :display-name symbol
                :data (list :name symbol :position (point))
                :content content)))
    (push item agent-q-context-items)
    (agent-q-insert-context-pill item)
    (message "Added symbol '%s' to context" symbol)))

(defun agent-q-insert-context-pill (item)
  "Insert visual 'pill' for context ITEM."
  (insert (propertize (format "[@%s] " (agent-q-context-item-display-name item))
                     'face 'font-lock-constant-face
                     'agent-q-context-item item
                     'mouse-face 'highlight
                     'help-echo (format "Context: %s" (agent-q-context-item-display-name item)))))
```

---

## Implementation Roadmap

### Phase 1: Foundation (2-3 weeks)

**Goal:** Convert readonly buffer to basic interactive chat

**Tasks:**
1. **Create `agent-q-chat-mode` derived from `comint-mode`**
   - Define major mode with keybindings
   - Configure comint variables (prompt, input sender, output filters)
   - Set up message area vs input area separation

2. **Implement input handling**
   - Multi-line input with RET vs C-c RET behavior
   - Input history (M-p/M-n navigation)
   - Clear input after sending

3. **Implement basic message rendering**
   - User message rendering (timestamp, content)
   - Assistant message rendering (timestamp, content)
   - Proper text properties (read-only, message boundaries)

4. **Integrate with existing agent backend**
   - Async message processing (don't block UI)
   - Streaming response rendering (chunked output)
   - Error handling and display

**Deliverables:**
- `contrib/sly-agent-q/sly-agent-q-chat.el` (new file)
- Working chat interface replacing current readonly buffer
- Input/output working with existing Agent-Q backend
- Documentation of keybindings

**Success Criteria:**
- Can send multi-line messages
- Responses stream in real-time
- Input history works (M-p/M-n)
- No regressions in existing functionality

---

### Phase 2: Rich Rendering (2-3 weeks)

**Goal:** Add markdown, code blocks, syntax highlighting

**Tasks:**
1. **Integrate markdown-mode rendering**
   - Enable GFM (GitHub Flavored Markdown)
   - Fenced code block detection
   - Native syntax highlighting in code blocks

2. **Implement code block extraction**
   - Parse code blocks from responses
   - Extract language, content, position
   - Prepare for Apply button functionality

3. **Add visual polish**
   - Message separators (subtle lines)
   - Timestamp formatting (relative vs absolute)
   - User vs assistant message styling (different faces)
   - Link detection and buttonization

4. **Improve scrolling behavior**
   - Auto-scroll to latest message
   - Preserve scroll position when typing
   - Jump to latest message command

**Deliverables:**
- Markdown rendering working
- Syntax-highlighted code blocks
- Polished visual appearance
- Smooth scrolling UX

**Success Criteria:**
- Code blocks render with correct syntax highlighting
- Markdown tables, lists, emphasis all work
- Links are clickable
- Chat remains readable with 100+ messages

---

### Phase 3: Context Management (3-4 weeks)

**Goal:** Implement @-mentions and context panel

**Tasks:**
1. **Implement @-mention completion**
   - Detect @ prefix in input
   - Show completion candidates (files, symbols, buffers)
   - Insert context pill with properties
   - Track attached context items

2. **Build context panel**
   - Side window showing current context
   - Visual list of attached items (file, symbol, buffer)
   - Remove button (✕) per item
   - Add context button

3. **Implement context serialization**
   - Convert context items to LLM prompt format
   - Include file contents, symbol definitions, buffer text
   - Truncate large files (preview first N lines)
   - Smart context prioritization (relevance scoring)

4. **Add context commands**
   - C-c @ add file
   - C-c # add symbol
   - C-c C-x clear all context
   - Show context panel toggle

**Deliverables:**
- @-mention completion working
- Context panel functional
- Context included in LLM requests
- Documentation of context system

**Success Criteria:**
- Can @-mention files and auto-complete
- Context panel shows all attached items
- Removing context item works
- LLM receives correct context in prompt

---

### Phase 4: Action Buttons (2-3 weeks)

**Goal:** Add Apply/Reject/Copy/Retry buttons

**Tasks:**
1. **Implement button infrastructure**
   - Insert buttons after assistant messages
   - Button states (active, disabled, loading)
   - Button callbacks with message context

2. **Implement Apply button**
   - Extract code blocks from message
   - Show diff (reuse existing diff UI from enhancement-1)
   - On accept: apply changes to buffer
   - Update button to "Applied ✓"

3. **Implement other buttons**
   - Reject: mark as rejected, log decision
   - Copy: copy message/code to clipboard
   - Retry: re-send previous user message

4. **Add keyboard shortcuts for buttons**
   - Navigate between buttons (Tab/S-Tab)
   - Activate button (RET/Space)
   - Quick access (1-4 keys for Apply/Reject/Copy/Retry)

**Deliverables:**
- All action buttons functional
- Diff integration working
- Keyboard navigation of buttons
- Button state management

**Success Criteria:**
- Apply button shows diff and updates buffer
- Reject button marks message appropriately
- Copy button works for code and full messages
- Retry button re-sends with same context

---

### Phase 5: Session Management (2-3 weeks)

**Goal:** Implement sessions, persistence, history

**Tasks:**
1. **Implement session data structures**
   - Session storage format (Lisp files)
   - Message history tracking
   - Context item persistence

2. **Build session persistence**
   - Auto-save on buffer kill
   - Save on interval (5 minutes)
   - Load session on startup

3. **Create session switcher**
   - List all sessions with metadata
   - Completion-based selection
   - Create new session
   - Delete old sessions

4. **Implement conversation threading**
   - Thread data structure
   - Branch thread command
   - Switch thread command
   - Thread visualization

**Deliverables:**
- Sessions saved to `~/.emacs.d/agent-q-sessions/`
- Session switcher (C-c C-s)
- Thread support
- Auto-save working

**Success Criteria:**
- Sessions persist across Emacs restarts
- Can switch between sessions without data loss
- Threads allow branching conversations
- Old sessions can be archived/deleted

---

### Phase 6: Polish & Extensions (2-3 weeks)

**Goal:** Final UX improvements and extensibility

**Tasks:**
1. **Add slash commands**
   - /help, /commit, /review
   - Custom command registration
   - MCP prompt integration

2. **Implement quick actions**
   - Lightbulb-style error fixing
   - Quick buttons in source buffers
   - Context menu integration

3. **Add voice/audio input (optional)**
   - Whisper.el integration
   - Microphone input toggle
   - Audio transcription

4. **Performance optimization**
   - Large conversation handling (1000+ messages)
   - Lazy rendering (only visible messages)
   - Message compaction (summarize old messages)

5. **Documentation and examples**
   - User guide with screenshots
   - Video walkthrough
   - Example workflows
   - Customization guide

**Deliverables:**
- Slash command system
- Quick actions working
- Performance improvements
- Complete documentation

**Success Criteria:**
- Chat remains responsive with 1000+ messages
- Slash commands discoverable and useful
- Documentation comprehensive
- Users can extend with custom commands

---

## Comparison Matrix

### Chat Interface Modes

| Feature | Claude Code | OpenCode | Cursor | Copilot | Continue.dev | Agent-Q (Proposed) |
|---------|-------------|----------|--------|---------|--------------|-------------------|
| **Terminal CLI** | ✓ | ✓ | ✗ | ✗ | ✗ | ✗ |
| **IDE Panel** | ✓ (VS Code) | ✗ | ✓ | ✓ | ✓ | ✓ (Emacs) |
| **Inline Overlay** | ✗ | ✗ | ✓ (⌘I) | ✓ (⌘I) | ✓ | Phase 6 |
| **Quick Overlay** | ✗ | ✗ | ✗ | ✓ (⇧⌥⌘L) | ✗ | Phase 6 |
| **Multi-line Input** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Input History** | ✓ (↑ arrow) | ✓ | ✓ | ✓ | ✓ | ✓ (M-p/M-n) |

---

### Context Management

| Feature | Claude Code | OpenCode | Cursor | Copilot | Continue.dev | Agent-Q (Proposed) |
|---------|-------------|----------|--------|---------|--------------|-------------------|
| **@-mentions** | ✓ | ✓ | ✓ | ✗ (#-symbols) | ✓ | ✓ |
| **Auto-complete** | ✓ | ✓ (@ overlay) | ✓ (embeddings) | ✓ | ✓ | ✓ |
| **Context Panel** | ✗ | ✓ (sidebar) | ✗ | ✗ | ✓ | ✓ |
| **File Context** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Symbol Context** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Terminal Context** | ✓ | ✓ | ✗ | ✓ | ✓ (C-S-r) | Phase 6 |
| **External Docs** | ✓ (MCP) | ✗ | ✓ (browser) | ✗ | ✓ (OpenCtx) | Phase 6 (MCP) |

---

### Message Rendering

| Feature | Claude Code | OpenCode | Cursor | Copilot | Continue.dev | Agent-Q (Proposed) |
|---------|-------------|----------|--------|---------|--------------|-------------------|
| **Markdown** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Code Blocks** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Syntax Highlight** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ (native) |
| **Streaming** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Timestamps** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **User Colors** | ✗ | ✗ | ✗ | ✗ | ✗ | Optional |

---

### Action Buttons

| Feature | Claude Code | OpenCode | Cursor | Copilot | Continue.dev | Agent-Q (Proposed) |
|---------|-------------|----------|--------|---------|--------------|-------------------|
| **Apply** | ✓ | ? | ✓ | ✓ | ✓ | ✓ |
| **Reject** | ✓ | ? | ✓ | ✓ | ✗ | ✓ |
| **Copy** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Retry** | ✓ | ? | ✓ | ✓ | ✓ | ✓ |
| **Inline Buttons** | ✓ | ? | ✓ | ✓ | ✓ | ✓ |
| **Keyboard Nav** | ✓ | ✓ | ✓ | ✓ | ? | ✓ |

---

### Session Management

| Feature | Claude Code | OpenCode | Cursor | Copilot | Continue.dev | Agent-Q (Proposed) |
|---------|-------------|----------|--------|---------|--------------|-------------------|
| **Persistence** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Session Switch** | ✓ (--continue) | ✓ | ✓ | ✓ | ✓ | ✓ |
| **History Search** | ✓ | ? | ✓ | ✓ | ✓ | Phase 6 |
| **Threading** | ✗ | ? | ✓ (branches) | ✗ | ✗ | ✓ |
| **Checkpoints** | ✓ (git-based) | ? | ✓ (per iteration) | ✗ | ✗ | ✓ (integration) |

---

### Advanced Features

| Feature | Claude Code | OpenCode | Cursor | Copilot | Continue.dev | Agent-Q (Proposed) |
|---------|-------------|----------|--------|---------|--------------|-------------------|
| **Slash Commands** | ✓ | ✓ | ✗ | ✗ | ✓ | ✓ |
| **Custom Agents** | ✓ (MCP) | ✓ | ✓ | ✓ (.agent.md) | ✗ | Phase 6 |
| **Voice Input** | ✗ | ✗ | ✓ (Cursor 2.0) | ✗ | ✗ | Phase 6 |
| **Browser Integration** | ✗ | ✗ | ✓ (Cursor 2.0) | ✗ | ✗ | Phase 6 |
| **Multi-Agent Parallel** | ✗ | ✓ | ✓ (8 agents) | ✗ | ✗ | Phase 6 |
| **Model Selection** | ✓ | ✓ | ✓ | ✗ | ✓ | ✓ |

---

## Risk Mitigation

### Technical Risks

**Risk 1: comint-mode complexity**
- **Mitigation:** Start with simple ielm-based implementation, iterate
- **Fallback:** Use plain buffer with manual input/output handling (no comint inheritance)

**Risk 2: Streaming response rendering (chunked output)**
- **Mitigation:** Use `comint-output-filter-functions` carefully, test with slow connections
- **Fallback:** Batch output (wait for complete response before rendering)

**Risk 3: Large conversation performance (1000+ messages)**
- **Mitigation:** Lazy rendering (only fontify visible region), message compaction
- **Fallback:** Archive old messages, warn user at 500 messages

**Risk 4: Markdown rendering conflicts with comint**
- **Mitigation:** Apply markdown as text properties post-insertion, don't modify comint flow
- **Fallback:** Simplified rendering (no full markdown, just code block highlighting)

**Risk 5: Context extraction breaking with large files**
- **Mitigation:** Truncate files > 10KB, show preview + "expand" button
- **Fallback:** Refuse to add files > 50KB, warn user

---

### UX Risks

**Risk 1: Confusing keybindings (too many shortcuts)**
- **Mitigation:** Follow Emacs conventions (M-p/M-n for history, C-c prefix for mode commands)
- **Fallback:** Provide menu-bar menu with all commands listed

**Risk 2: Context pills cluttering input area**
- **Mitigation:** Collapse context pills (show count, expand on click)
- **Fallback:** Move context to dedicated panel only (no inline pills)

**Risk 3: Action buttons not discoverable**
- **Mitigation:** Add help-echo tooltips, document in mode line
- **Fallback:** Always show "Press 1-4 for actions" hint after assistant messages

**Risk 4: Session switching losing unsaved input**
- **Mitigation:** Auto-save draft messages, restore on session switch
- **Fallback:** Warn before switching if input non-empty

**Risk 5: Threading too complex for users**
- **Mitigation:** Make threading opt-in (default to single linear conversation)
- **Fallback:** Remove threading, focus on session management only

---

## Success Metrics

### User Experience Metrics

1. **Input Efficiency**
   - Time to compose multi-line prompt: < 30 seconds (target)
   - Context attachment: < 5 seconds per item (target)
   - History navigation: instant (< 100ms)

2. **Response Clarity**
   - Code block readability: 95% of users can distinguish code from text
   - Action button discoverability: 90% of users find Apply button without docs
   - Markdown rendering: All GFM features render correctly

3. **Session Management**
   - Session persistence: 100% of sessions survive Emacs restart
   - Session switch time: < 2 seconds
   - Context preservation: No data loss on switch

4. **Performance**
   - Chat with 100 messages: No perceptible lag
   - Chat with 1000 messages: < 1 second to scroll to bottom
   - Streaming response: < 100ms latency per chunk

---

### Feature Adoption Metrics

1. **Context Management**
   - % of messages with @-mentions: target > 50%
   - Average context items per message: target 2-3
   - Context panel usage: target > 70% of users

2. **Action Buttons**
   - Apply button usage: target > 80% of code responses
   - Reject button usage: target > 20% of responses (indicates critical review)
   - Copy button usage: target > 30% of messages

3. **Session Management**
   - % of users with multiple sessions: target > 60%
   - Average session length: target > 20 messages
   - Session restoration rate: target > 80%

4. **Advanced Features** (Phase 6)
   - Slash command usage: target > 40% of messages
   - Threading usage: target > 30% of users
   - Custom command creation: target > 10% of users

---

## References

### Claude Code
- [How I use Claude Code + tips](https://www.builder.io/blog/claude-code)
- [Claude Code Chat UI for VS Code](https://marketplace.visualstudio.com/items?itemName=AndrePimenta.claude-code-chat)
- [Claude Code quietly upgrades: VS Code plugin](https://ai.plainenglish.io/claude-code-quietly-upgrades-the-official-vs-code-chat-plugin-is-worth-trying-813512df3a8c)
- [Claude Code in Slack integration](https://techcrunch.com/2025/12/08/claude-code-is-coming-to-slack-and-thats-a-bigger-deal-than-it-sounds/)
- [Claude Agent in JetBrains IDEs](https://blog.jetbrains.com/ai/2025/09/introducing-claude-agent-in-jetbrains-ides/)

### OpenCode
- [OpenCode GitHub Repository](https://github.com/opencode-ai/opencode)
- [OpenCode Official Website](https://opencode.ai/)
- [OpenCode Documentation](https://opencode.ai/docs/)
- [Chat Interface Deep Dive](https://deepwiki.com/opencode-ai/opencode/5-chat-interface)

### Cursor
- [Cursor Features Overview](https://cursor.com/features)
- [Cursor Composer Documentation](https://docs.cursor.com/composer)
- [Cursor 2.0 Release Announcement](https://forum.cursor.com/t/cursor-2-0-composer-in-app-browser-voice-more/139132)
- [What's Cursor Composer?](https://prototypr.io/post/cursor-composer-cmdi)
- [The 3 Cursor AI Modes](https://www.thepromptwarrior.com/p/the-3-cursor-ai-modes)

### GitHub Copilot Chat
- [GitHub Copilot in VS Code](https://code.visualstudio.com/docs/copilot/overview)
- [Get started with Copilot Chat](https://code.visualstudio.com/docs/copilot/chat/copilot-chat)
- [Manage context for AI](https://code.visualstudio.com/docs/copilot/chat/copilot-chat-context)
- [Customize chat to your workflow](https://code.visualstudio.com/docs/copilot/customization/overview)
- [Chat modes in VS Code](https://code.visualstudio.com/docs/copilot/chat/chat-modes)

### Continue.dev
- [Continue.dev Chat Documentation](https://docs.continue.dev/chat/how-to-use-it)
- [Using Slash Commands](https://docs.continue.dev/actions/how-to-use-it/)
- [Custom Slash Commands](https://docs.continue.dev/customize/slash-commands)

### Emacs Chat Clients
- [Comint Mode: Writing Command Interpreter (Mastering Emacs)](https://www.masteringemacs.org/article/comint-writing-command-interpreter)
- [EmacsWiki: Comint Mode](https://www.emacswiki.org/emacs/ComintMode)
- [ChatGPT Emacs Shell](https://xenodium.com/a-chatgpt-emacs-shell)
- [gptel GitHub Repository](https://github.com/karthink/gptel)
- [Using LLMs in Emacs](https://blog.aheymans.xyz/post/llm_in_emacs/)
- [ement.el (Matrix Client)](https://github.com/alphapapa/ement.el)
- [GNU Emacs ERC](https://www.gnu.org/software/emacs/erc.html)

### Markdown in Emacs
- [Markdown Mode for Emacs](https://jblevins.org/projects/markdown-mode/)
- [Markdown Mode GitHub](https://github.com/jrblevin/markdown-mode)
- [GitHub-like Markdown preview](https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/)

### Minibuffer and Input Patterns
- [Minibuffer Edit (GNU Emacs Manual)](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-Edit.html)
- [Text from Minibuffer (Elisp Manual)](https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-from-Minibuffer.html)
- [EmacsWiki: Dedicated Minibuffer Frame](https://www.emacswiki.org/emacs/Dedicated_Minibuffer_Frame)

### AI Assistant Features
- [AI Chat | JetBrains AI Assistant](https://www.jetbrains.com/help/ai-assistant/ai-chat.html)
- [Vercel AI Elements](https://blog.logrocket.com/vercel-ai-elements/)
- [Shadcn AI Actions](https://www.shadcn.io/ai/actions)
- [Beyond the Chatbot: A2UI Design](https://medium.com/@dhairyabadiyani/beyond-the-chatbot-why-a2ui-agent-to-user-interface-is-the-next-design-frontier-e125b0094a85)

### Session Management
- [AI SDK UI: Chatbot Message Persistence](https://ai-sdk.dev/docs/ai-sdk-ui/chatbot-message-persistence)
- [Manage chat sessions in VS Code](https://code.visualstudio.com/docs/copilot/chat/chat-sessions)
- [Claude Code Session Management](https://stevekinney.com/courses/ai-development/claude-code-session-management)
- [Creating chat history object (Azure)](https://learn.microsoft.com/en-us/semantic-kernel/concepts/ai-services/chat-completion/chat-history)

### Threading and Context
- [Microsoft Agent Framework Multi-Turn Conversations](https://learn.microsoft.com/en-us/agent-framework/user-guide/agents/multi-turn-conversation)
- [OpenAI Assistants API Threads Guide](https://dzone.com/articles/openai-assistants-api-threads-guide)
- [Conversation state | OpenAI API](https://platform.openai.com/docs/guides/conversation-state)
- [Implementing Context-Aware AI Responses](https://getstream.io/blog/ai-chat-memory/)

---

**End of Enhancement 4**
