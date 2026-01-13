# Agent-Q

**AI-powered agentic extension for Common Lisp development**

Agent-Q is an intelligent assistant that integrates with SLY/Emacs to provide context-aware help with Common Lisp development. It can understand your code, answer questions, generate documentation, suggest fixes, and act as an autonomous development partner.

## Features

### Phase 1 (Complete) ✅
- 🧠 **Context-aware assistance** - Accumulate code snippets from your buffers for targeted help
- 💬 **Conversational interface** - Multi-turn conversations with full history
- 🔧 **SLY integration** - Seamless integration with your existing Lisp workflow
- 🎨 **Multiple providers** - Support for Anthropic, OpenAI, Ollama, and OpenRouter
- ⚡ **Quick actions** - Document functions, explain code, debug errors with a keystroke

### Phase 2 (In Progress) 🔄
- 🛠️ **Tool system** - Extensible tool registry for agent capabilities
- 🔍 **Introspection tools** - describe, apropos, documentation, who-calls
- ⚙️ **Execution tools** - eval, compile, macroexpand in running image
- 📊 **Diff approval** - Review and approve code changes before application

### Phase 3 (Partial) 🔄
- 💾 **Session management** - Persistent sessions with SQLite storage
- 📝 **Rich markdown** - Beautiful rendering with syntax highlighting in chat
- 🌊 **Streaming responses** - Real-time token-by-token display
- 🎯 **@-mention completion** - Inline completion for files, symbols, and buffers
- 📎 **Context pills** - Visual indicators for attached context with hover previews
- 📋 **Context panel** - Sidebar showing all attached context items
- ⏳ **Planned**: Condition system integration, test framework integration, knowledge base

## Prerequisites

- **Emacs** with **SLY** installed
- **Common Lisp** implementation (SBCL, CCL, etc.)
- **Quicklisp** for package management
- **cl-llm-provider** library
- **API key** for your chosen LLM provider (Anthropic, OpenAI, etc.)

## Installation

### 1. Install cl-llm-provider

```lisp
(ql:quickload "cl-llm-provider")
```

### 2. Clone Agent-Q

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/yourusername/agent-q.git
```

### 3. Load Agent-Q in Common Lisp

```lisp
(ql:quickload "agent-q")
```

### 4. Set up Emacs integration

Add to your Emacs init file (e.g., `~/.emacs` or `~/.emacs.d/init.el`):

```elisp
(with-eval-after-load 'sly
  (add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")
  (require 'sly-agent-q)
  (sly-agent-q-setup))
```

Restart Emacs or evaluate the above code.

## Configuration

### API Key Setup

Agent-Q uses environment variables for API keys. Set the appropriate variable for your provider:

**For Anthropic (Claude):**
```bash
export ANTHROPIC_API_KEY="sk-ant-..."
```

**For OpenAI:**
```bash
export OPENAI_API_KEY="sk-..."
```

**For Ollama (local, no key needed):**
```bash
# No API key required for local Ollama
```

Add these to your shell configuration file (`~/.bashrc`, `~/.zshrc`, etc.) to make them permanent.

### Optional: Config File

Create `~/.config/agent-q/config.lisp` for custom settings:

```lisp
(in-package :agent-q)

;; Configure provider and model
(configure :provider :anthropic
           :model "claude-sonnet-4-20250514")

;; Or use OpenAI
;; (configure :provider :openai
;;            :model "gpt-4-turbo")

;; Or use Ollama (local)
;; (configure :provider :ollama
;;            :model "llama2"
;;            :base-url "http://localhost:11434")
```

### Optional: Project-Specific Prompts

Create `.agent-q/system-prompt.md` in your project root to add custom instructions:

```markdown
This project uses a custom macro system for DSL generation.
Prefer using `define-dsl-element` over manual defmacro.
Follow the naming convention: operation-noun (e.g., parse-expression, emit-code).
```

Agent-Q will automatically include these instructions when working in that project.

## Usage

### Basic Workflow

1. **Start SLY** and connect to your Lisp image
2. **Enable Agent-Q** mode (automatic if you added setup to init file)
3. **Add context** - Mark code regions you want the agent to know about
4. **Ask questions** - Send messages to get help
5. **Insert responses** - Paste agent suggestions into your code

### @-Mention Completion (New!)

Agent-Q now supports inline completion for attaching context to your messages. In the chat input, type `@` followed by the beginning of a filename, symbol, or buffer name:

**Attaching Files:**
```
> @src/agen TAB
```
Completes to file candidates: `@src/agent.lisp`, `@src/agent-q.asd`, etc.

**Attaching Symbols:**
```
> @defun fib TAB
```
Completes to Lisp symbols: `@fibonacci`, `@fib-helper`, etc.

**Attaching Buffers:**
```
> @buffer *mes TAB
```
Completes to open buffers: `@*messages*`, `@*scratch*`, etc.

**Visual Pills:**
When you select a completion, it creates a visual pill `[@name]` in your message. These pills are:
- **Clickable** - Press `RET` to visit the file/symbol/buffer
- **Removable** - Press `DEL` to remove from context
- **Hoverable** - Tooltip shows the first few lines of content

**Context Panel:**
Press `C-c @` to toggle the context panel sidebar, which shows:
- All attached context items
- Item types (file, symbol, buffer, region, url)
- Quick actions (visit, remove)
- Total context size

**Content Limits:**
Each context item is limited to 50KB to prevent overwhelming the LLM. Large files are truncated with a note indicating the size.

### Keybindings

All Agent-Q commands are under the `C-c q` prefix:

#### Context Management
- `C-c q c r` - Add region to context
- `C-c q c b` - Add entire buffer to context
- `C-c q c d` - Add current defun to context
- `C-c q c c` - Clear all context
- `C-c q c s` - Show context summary
- `C-c @` - Toggle context panel sidebar (in chat buffer)
- `C-c C-x` - Clear all context items (in chat buffer)
- `@` - Start @-mention completion (in chat input)

#### Conversation
- `C-c q s` - Send message to agent
- `C-c q S` - Send message with context (capital S)
- `C-c q r` - Send region with custom instruction
- `C-c q n` - Start new conversation
- `C-c q v` - View conversation buffer

#### Response Handling
- `C-c q i` - Insert last response at point
- `C-c q w` - Copy last response to kill ring

#### Quick Actions
- `C-c q q d` - Document current defun
- `C-c q q e` - Explain selected region
- `C-c q q f` - Help fix recent error

### Example Session

```elisp
;; 1. Write some code
(defun fibonacci (n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

;; 2. Mark the defun and add to context
;; M-x sly-agent-q-add-defun-to-context (or C-c q c d)

;; 3. Ask the agent to optimize it
;; M-x sly-agent-q-send-with-context: "Make this tail-recursive"
;; (or C-c q S)

;; 4. Agent responds with optimized version
;; Use C-c q i to insert at point
```

## API Reference (Common Lisp)

### Configuration

```lisp
;; Configure provider
(agent-q:configure :provider :anthropic
                   :model "claude-sonnet-4-20250514"
                   :api-key "sk-ant-...")  ; optional, reads from env

;; Check configuration
agent-q:*default-provider*    ; => :anthropic
agent-q:*default-model*       ; => "claude-sonnet-4-20250514"
agent-q:*provider-instance*   ; => #<CL-LLM-PROVIDER:PROVIDER>
```

### Context Management

```lisp
;; Add context programmatically
(agent-q:agent-q-add-context "(defun foo () ...)"
                             :type :code
                             :metadata '(:filename "example.lisp"
                                        :start-line 10
                                        :end-line 15))

;; Clear context
(agent-q:agent-q-clear-context)

;; Get context summary
(agent-q:agent-q-get-context-summary)
;; => (:count 3 :types (:code :code :error) :preview "3 items: Code, Code, Error")
```

### Sending Messages

```lisp
;; Send without context
(agent-q:agent-q-send "What's the best way to handle errors in CL?")

;; Send with context
(agent-q:agent-q-send "Optimize this function" :include-context t)

;; Start new conversation
(agent-q:agent-q-new-conversation :project "my-app")
```

## Troubleshooting

### "Authentication failed" error

- Check that your API key is set correctly in the environment
- Verify the key is valid by testing it with curl or the provider's CLI
- Restart your Lisp image after setting environment variables

### "Rate limited" error

- You've exceeded your API rate limit
- Wait a few minutes and try again
- Consider using a local model with Ollama for development

### "Provider not found" error

- Make sure cl-llm-provider is loaded: `(ql:quickload "cl-llm-provider")`
- Check that you're using a supported provider: `:anthropic`, `:openai`, `:ollama`, `:openrouter`

### Emacs can't find sly-agent-q

- Verify the path in your init file matches where you cloned agent-q
- Make sure you've evaluated the configuration or restarted Emacs
- Check for errors in `*Messages*` buffer

### "No agent initialized" message

- The agent initializes automatically on first use
- Try calling `(agent-q:agent-q-configure)` from the REPL
- Check that `agent-q:*provider-instance*` is not nil

## Architecture

Agent-Q is organized into four phases, with Phases 1-2 substantially complete:

- **Phase 1 (Complete ✅)**: Foundation - Context management, LLM integration, basic Emacs UI
- **Phase 2 (In Progress 🔄)**: REPL-aware - Tool system for introspection and code execution
- **Phase 3 (Partial 🔄)**: Autonomous - Session persistence complete; condition system, testing, knowledge base pending
- **Phase 4 (Planned ⏳)**: Intelligent - Semantic indexing, profiling, refactoring, pattern detection

See `specs/` directory for detailed specifications and `specs/PHASE1-IMPLEMENTATION-SUMMARY.md` for Phase 1 completion details.

## Development

### Running Tests

```lisp
;; Load the system
(ql:quickload "agent-q")

;; Manual tests
(in-package :agent-q)

;; Test context accumulation
(let ((mgr (make-instance 'context-manager)))
  (add-context mgr "(defun test () ...)")
  (add-context mgr "(defun test2 () ...)")
  (context-to-string mgr))

;; Test conversation
(let ((conv (new-conversation)))
  (add-message conv :user "Hello")
  (get-messages conv))

;; Test LLM integration (requires API key)
(agent-q-send "What is the purpose of CLOS?")
```

### Project Structure

```
agent-q/
├── agent-q.asd                    # ASDF system definition
├── src/                           # Common Lisp source
│   ├── package.lisp               # Core package definition
│   ├── config.lisp                # Configuration management
│   ├── context.lisp               # Context accumulation
│   ├── conversation.lisp          # Message history
│   ├── prompts.lisp               # System prompts
│   ├── agent.lisp                 # Core agent loop
│   ├── llm-integration.lisp       # LLM provider integration
│   ├── session.lisp               # Session persistence (SQLite)
│   ├── sly-interface.lisp         # SLY RPC endpoints
│   └── tools/                     # Tool system (Phase 2)
│       ├── package.lisp
│       ├── registry.lisp
│       ├── introspection.lisp
│       ├── execution.lisp
│       ├── buffer.lisp
│       └── diff.lisp
├── contrib/
│   └── sly-agent-q/               # Emacs integration
│       ├── sly-agent-q.el         # Core minor mode
│       ├── sly-agent-q-chat.el    # Chat interface with markdown
│       ├── sly-agent-q-context.el # @-mention completion and context management
│       ├── sly-agent-q-sessions.el # Session management UI
│       ├── sly-agent-q-tools.el   # Tool execution UI
│       ├── sly-agent-q-diff.el    # Diff approval interface
│       └── test/                  # Comprehensive test suite
├── specs/                         # Design specifications
│   ├── PHASE1-IMPLEMENTATION-SUMMARY.md
│   └── plans/                     # Feature implementation plans
└── README.md
```

## Contributing

Contributions are welcome! Please:

1. Check existing issues or create a new one
2. Fork the repository
3. Create a feature branch
4. Make your changes with tests
5. Submit a pull request

## License

MIT License - see LICENSE file for details

## Acknowledgments

- Built on [cl-llm-provider](https://github.com/user/cl-llm-provider) for LLM integration
- Inspired by AI-powered development tools and the Common Lisp community
- Uses [SLY](https://github.com/joaotavora/sly) for Emacs integration

## Links

- Documentation: See `specs/` directory
- Issues: https://github.com/yourusername/agent-q/issues
- cl-llm-provider: https://github.com/user/cl-llm-provider

---

**Status**: Phase 1 Complete ✅ | Phase 2 In Progress 🔄 | Phase 3 Partial 🔄

**Recent Additions:**
- @-mention completion for inline context attachment (files, symbols, buffers)
- Context pills with visual indicators and hover previews
- Context panel sidebar for managing attached context
- Comprehensive chat interface with markdown rendering and streaming
- Session management with SQLite persistence
- Tool system with introspection, execution, and diff approval
- Full test suite for Elisp components (155 tests, 150 passing)
