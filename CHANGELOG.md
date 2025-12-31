# Changelog

All notable changes to Agent-Q will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.5] - 2025-12-31

### Added
- **Auto-load agent-q on SLY connection**
  - Agent-Q now automatically loads when SLY connects to a Lisp image
  - Runs `(ql:quickload :agent-q)` and `(cl-llm-provider:load-configuration-from-file)` automatically
  - Shows success/failure message in minibuffer
  - No manual loading required!

### Fixed
- **Critical: Fixed SBCL 2.5+ compatibility issue**
  - SBCL 2.5+ changed `fboundp` to return the function object instead of T
  - This unserializable return value broke SLY communication
  - Fixed by wrapping check in `(if ... t nil)` to ensure T/NIL return
  - Error manifested as: `Invalid read syntax: "#<"` when using Agent-Q commands

- **Improved error handling in send commands**
  - Added type checking for responses from Lisp side
  - Non-string responses now display helpful error messages

## [0.1.4] - 2025-12-31

### Fixed
- **Improved agent-q loading detection**
  - Check now verifies both package exists AND function is bound
  - Uses `(cl:fboundp 'agent-q:agent-q-send)` for more reliable detection
  - Should correctly detect when system is loaded in REPL

## [0.1.3] - 2025-12-31

### Fixed
- **Critical**: Fixed keymap structure to require C-c q prefix
  - Completely rewrote keymap setup with proper two-level structure
  - Created `sly-agent-q-command-map` for Agent-Q commands
  - Created `sly-agent-q-mode-map` that binds C-c q to command map
  - Commands now ONLY trigger after pressing C-c q (not just 'q')
  - Removed MREPL exclusion - Agent-Q now works in all SLY buffers
  - SLY uses C-c C-* pattern, so C-c q is conflict-free
  - REPL keybindings (especially 'q') no longer interfered with

## [0.1.2] - 2025-12-31

### Fixed
- **Critical**: Attempted fix for MREPL keybinding clobbering (incomplete)
  - Agent-Q minor mode restricted to source file buffers only
  - This was reverted in v0.1.3 in favor of proper keymap fix

## [0.1.1] - 2025-12-31

### Fixed
- **Critical**: Fixed debugger loop when SLY is started before agent-q is loaded
  - Added `sly-agent-q--check-loaded` function to verify agent-q package exists
  - All interactive commands now check if agent-q is loaded before making RPC calls
  - Shows helpful message: "Agent-Q not loaded. Load it with: (asdf:load-system :agent-q)"
  - Prevents 'q' key clobbering and debugger errors in REPL

- **UX**: Made Agent-Q conversation buffer read-only and dismissable
  - Created `sly-agent-q-conversation-mode` derived from `special-mode`
  - Buffer is now read-only by default (edits via `inhibit-read-only`)
  - Press 'q' to hide/dismiss the conversation buffer
  - Press 'n' to start a new conversation directly from buffer
  - Added helpful header text explaining keybindings

### Changed
- Conversation buffer now shows helpful keybinding hints at the top
- Improved buffer initialization with proper major mode setup

## [0.1.0] - 2025-12-31

### Added
- Initial release of Agent-Q Phase 1 (Foundation)
- Context management system
  - Add code regions, buffers, defuns to context
  - Sliding window with 50-item capacity
  - Metadata tracking (filename, line numbers)
  - Markdown formatting for LLM

- Conversation management
  - Multi-turn conversation history
  - Message role tracking (user/assistant)
  - Project-specific conversations

- LLM integration via cl-llm-provider
  - Support for Anthropic, OpenAI, Ollama, OpenRouter
  - Real-time LLM communication
  - Error handling (auth, rate limits, API errors)
  - Token usage tracking

- System prompt management
  - Base system prompt for Common Lisp assistance
  - Project-specific prompt loading from `.agent-q/system-prompt.md`
  - Prompt composition and templating

- SLY/Emacs integration
  - Full minor mode (`sly-agent-q-mode`)
  - Comprehensive keybindings under `C-c q` prefix
  - Context commands (add region/buffer/defun, clear, show)
  - Conversation commands (send, send with context, new conversation)
  - Response handling (insert at point, copy to kill ring)
  - Quick actions (document, explain, fix errors)
  - Conversation buffer with syntax highlighting

### Documentation
- Complete README.md with installation and usage instructions
- QUICKSTART.md for 5-minute setup
- IMPLEMENTATION-SUMMARY.md with architecture details
- Comprehensive inline documentation and docstrings

[0.1.1]: https://github.com/yourusername/agent-q/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/yourusername/agent-q/releases/tag/v0.1.0
