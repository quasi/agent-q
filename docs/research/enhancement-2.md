# Enhancement 2: Best-in-Class AI Coding Assistant Features

**Date**: 2025-01-31
**Status**: Research
**Category**: Feature Comparison & Roadmap
**Goal**: Make Agent-Q the best coding assistant for Common Lisp development

---

## Executive Summary

This document analyzes the features of leading AI coding assistants (gptel, macher, aider, Goose, OpenCode, Cursor, GitHub Copilot) to identify must-have capabilities for Agent-Q. Each feature is scored for **implementation complexity** (1-5) and **user benefit** (1-5), with citations to user feedback and industry research.

**Key Finding**: The best coding assistants share these priorities:
1. **Context management** - Full codebase understanding with persistent memory
2. **Granular control** - Per-hunk diffs, surgical edits, clear approval workflows
3. **Git integration** - Automatic commits, branch management, reviewable history
4. **Multi-model flexibility** - Support for multiple LLM providers
5. **Agent capabilities** - Tool use, autonomous iteration, task decomposition

---

## Tool Overview

### gptel
- **Type**: Emacs LLM client library (chat + programmatic API)
- **Philosophy**: "Works in the spirit of Emacs, available at any time and uniformly in any buffer"
- **Key Strength**: Context engineering - effortless model/tool switching, conversation branching, content editing
- **Users love**: Flexibility, multi-backend support, native Emacs integration
- **Sources**: [gptel GitHub](https://github.com/karthink/gptel), [Ben's Journal Review](https://www.blogbyben.com/2024/08/gptel-mindblowing-integration-between.html), [Hacker News Discussion](https://news.ycombinator.com/item?id=42034675)

### macher
- **Type**: Project-aware multi-file LLM editing (built on gptel)
- **Philosophy**: Ephemeral file-editing environment with two versions (original/modified)
- **Key Strength**: Multi-file edits with standard diff-mode workflow
- **Users love**: Patch-based workflow, read-only vs editing tool separation
- **Sources**: [macher GitHub](https://github.com/kmontag/macher)

### aider
- **Type**: Terminal-based AI pair programmer
- **Philosophy**: Git-native simplicity with surgical code changes
- **Key Strength**: Automatic Git integration + unified diff approach
- **Users love**: "Hands down, the best AI coding assistant tool so far" - IndyDevDan
- **Benchmark**: 26.33% on SWE-Bench Lite (SOTA at release), 88% code editing accuracy (Aider polyglot)
- **Sources**: [Blott Aider Review](https://www.blott.com/blog/post/aider-review-a-developers-month-with-this-terminal-based-code-assistant), [aider GitHub](https://github.com/Aider-AI/aider), [SWE-Bench Results](https://aider.chat/2024/05/22/swe-bench-lite.html), [netnerds review](https://blog.netnerds.net/2024/10/aider-is-awesome/)

### Goose
- **Type**: Open-source AI agent framework (CLI + desktop app)
- **Philosophy**: "Build system for agent behavior, not better IDE integration"
- **Key Strength**: Recipe system for composable workflows, terminal integration, MCP-UI support
- **Users love**: Model-agnostic, transparency (see exact commands), ambient terminal assistance
- **Sources**: [Goose GitHub](https://github.com/block/goose), [What Makes Goose Different](https://dev.to/nickytonline/what-makes-goose-different-from-other-ai-coding-agents-2edc), [Why Goose Won Us Over](https://medium.com/onion-creative/why-goose-is-the-ai-coding-agent-that-finally-won-us-over-8835c662d152)

### OpenCode
- **Type**: Open-source terminal-based AI coding agent
- **Philosophy**: Privacy-first (no code storage), LSP integration for multi-language support
- **Key Strength**: Plan/Build modes, LSP integration, parallel agent sessions
- **Users love**: Open source (MIT), privacy, multi-session support
- **Sources**: [OpenCode GitHub](https://github.com/opencode-ai/opencode), [OpenCode Docs](https://opencode.ai/docs/)

### Industry Context
- **76% of developers** use or plan to use AI coding tools ([Stack Overflow 2024](https://stackoverflow.blog/2024/05/29/developers-get-by-with-a-little-help-from-ai-stack-overflow-knows-code-assistant-pulse-survey-results/))
- **99% say AI tools save time**, 68% save 10+ hours/week ([Atlassian State of DevEx 2025](https://www.shakudo.io/blog/best-ai-coding-assistants))
- **41% of all code** is AI-generated or AI-assisted in 2025 ([Second Talent Stats](https://www.secondtalent.com/resources/ai-coding-assistant-statistics/))
- Major companies (Google, Microsoft) report **30-50% of code AI-generated**

---

## Feature Matrix with Scores

**Scoring Legend:**
- **Complexity**: 1=Trivial, 2=Easy, 3=Moderate, 4=Hard, 5=Very Hard
- **Benefit**: 1=Nice to have, 2=Useful, 3=Valuable, 4=Important, 5=Game changer
- **Priority**: (Benefit - Complexity) = Higher is better ROI

| Feature | Tools | Complexity | Benefit | Priority | Notes |
|---------|-------|------------|---------|----------|-------|
| **CORE EDITING** |
| Per-hunk diff approval | macher, gptel | 3 | 5 | +2 | See Enhancement-1; granular control over changes |
| Unified diff application | aider, macher | 2 | 4 | +2 | Use `diff-apply-hunk` (built-in Emacs) |
| Inline diff preview | gptel (addon) | 3 | 4 | +1 | Show changes inline with accept/reject |
| Conflict marker workflow | gptel-proof | 2 | 3 | +1 | Use VC conflict markers for review |
| Whole-file replacement | All | 1 | 2 | +1 | Current Agent-Q approach |
| Search/replace blocks | aider | 2 | 3 | +1 | Precise block-level edits |
| Multi-file editing | macher, aider | 4 | 5 | +1 | Edit multiple files in single request |
| Surgical string replacement | OpenCode | 2 | 4 | +2 | Exact string match edits |
| **GIT INTEGRATION** |
| Auto-commit changes | aider ⭐ | 3 | 5 | +2 | Most praised feature! Clear commit messages |
| Git status awareness | aider | 2 | 4 | +2 | Know what's staged/unstaged before acting |
| Branch management | aider, Cursor | 3 | 4 | +1 | Create/switch branches for features |
| Pre-commit hook support | aider | 2 | 3 | +1 | Run linters/tests before commit |
| Commit message generation | aider, Copilot | 2 | 3 | +1 | AI-generated meaningful messages |
| Reviewable history | aider | 1 | 5 | +4 | Clean Git log for easy rollback |
| Git worktree support | Cursor, Goose | 4 | 4 | 0 | Parallel feature development |
| **CONTEXT MANAGEMENT** |
| Repository map/indexing | aider ⭐, Cursor | 4 | 5 | +1 | AST + call graph for codebase understanding |
| @ file/symbol mentions | Cursor, gptel | 2 | 4 | +2 | Precise context targeting |
| Multi-repository indexing | Cursor, Copilot Enterprise | 5 | 4 | -1 | For large multi-service systems |
| Context window tracking | Claude Code, Cursor | 2 | 4 | +2 | Warn before hitting limits |
| Conversation summarization | Claude Code, gptel | 3 | 4 | +1 | Automatic compaction to save tokens |
| Session persistence | All | 2 | 5 | +3 | Resume conversations across restarts |
| Named sessions | Goose, OpenCode | 2 | 4 | +2 | Organize by task/feature |
| Context branching | gptel (Org mode) | 3 | 3 | 0 | Multiple conversation paths |
| Long-term memory | Task Orchestrator (MCP) | 4 | 5 | +1 | Remember across sessions (days/weeks) |
| Project-specific context | macher | 3 | 4 | +1 | Workspace-aware tool invocations |
| **TOOL USE & AGENTIC** |
| Tool execution loop | Agent-Q ✓, All agentic | 3 | 5 | +2 | LLM requests tools → execute → iterate |
| Read-only tools | macher, Agent-Q ✓ | 2 | 4 | +2 | Introspection without side effects |
| File editing tools | All | 2 | 5 | +3 | Core functionality |
| Shell command execution | OpenCode, Goose | 3 | 4 | +1 | Run tests, build, install deps |
| LSP integration | OpenCode | 4 | 4 | 0 | Code intelligence (diagnostics, symbols) |
| Linter/test integration | aider | 3 | 5 | +2 | Auto-fix lint errors, run tests |
| MCP server support | Goose ⭐, Claude Code | 3 | 4 | +1 | Extensible tool ecosystem |
| MCP-UI support | Goose | 4 | 3 | -1 | Render UI components (rare need) |
| Autonomous iteration | All agentic | 2 | 5 | +3 | Try-fix-verify loops without user |
| Max iteration limits | Agent-Q ✓, All | 1 | 4 | +3 | Prevent infinite loops |
| **MULTI-MODEL SUPPORT** |
| Model switching | gptel ⭐, macher, aider | 2 | 5 | +3 | Pick best model per task |
| Local model support | gptel, aider, Goose | 3 | 4 | +1 | Ollama, Llama.cpp, GPT4All |
| Multi-model workflows | aider (GPT-4o + Opus) | 3 | 4 | +1 | Retry with different model on failure |
| Bring-your-own API key | Goose, aider, gptel | 1 | 5 | +4 | No vendor lock-in |
| Provider-agnostic | Goose, OpenCode, gptel | 2 | 4 | +2 | Works with 75+ LLM providers |
| **WORKFLOW & UX** |
| In-buffer editing | gptel, Cursor | 2 | 4 | +2 | Work directly in source buffers |
| Dedicated chat buffer | Agent-Q ✓, gptel | 1 | 4 | +3 | Separate conversation view |
| Transient menus | gptel | 2 | 3 | +1 | Emacs-native UI for options |
| Voice input | aider ⭐ | 3 | 3 | 0 | `/voice` command for hands-free coding |
| Rewrite/refactor region | gptel, Cursor | 2 | 4 | +2 | Select code → AI rewrites it |
| Add context from anywhere | gptel, Agent-Q ✓ | 2 | 4 | +2 | Mark regions/buffers for context |
| Context editing | gptel | 2 | 3 | +1 | Edit past prompts/responses |
| Code comment integration | aider | 2 | 3 | +1 | Add comments → aider implements |
| Image/web page context | aider, Cursor | 3 | 3 | 0 | Screenshots, reference docs |
| Plan mode | OpenCode, Cursor | 3 | 4 | +1 | Outline steps before coding |
| Terminal integration | Goose ⭐ | 3 | 4 | +1 | @goose "do this" in regular terminal |
| **MULTI-AGENT & PARALLEL** |
| Sub-agent spawning | Claude Code, Cursor | 4 | 4 | 0 | Delegate subtasks to isolated agents |
| Parallel task execution | Goose, Cursor (8-agent) | 5 | 5 | 0 | 36-40% speed gains, 5-8x productivity |
| Orchestrator-worker pattern | Anthropic Research system | 5 | 4 | -1 | Lead agent + specialized workers |
| Specialized agents | Agentwise (Frontend, Backend, etc.) | 5 | 4 | -1 | Role-based agent coordination |
| Shared interface definitions | Best practice | 3 | 4 | +1 | Prevent integration nightmares |
| Task isolation | Claude Code (worktrees) | 4 | 4 | 0 | Each agent gets own context |
| **REPL & LANGUAGE-SPECIFIC** |
| REPL interaction | Agent-Q ✓ | 3 | 5 | +2 | Evaluate code in running image |
| Live introspection | Agent-Q ✓ | 3 | 5 | +2 | Inspect running system (Lisp-specific) |
| Condition system integration | Agent-Q (Phase 3) | 4 | 5 | +1 | Handle errors interactively (Lisp-specific) |
| Symbol/function lookup | Agent-Q ✓ | 2 | 4 | +2 | Describe, apropos, who-calls |
| Macroexpansion | Agent-Q ✓ | 2 | 3 | +1 | Understand macro transformations |
| CLOS introspection | Agent-Q ✓ | 3 | 4 | +1 | Class slots, hierarchy |
| Package-aware operations | Agent-Q ✓ | 2 | 4 | +2 | Handle CL package system |
| **DEBUGGING & TESTING** |
| Error tracking | Agent-Q ✓ | 2 | 4 | +2 | Store last error + backtrace |
| REPL history | Agent-Q ✓ | 2 | 3 | +1 | Review eval history |
| Test framework integration | aider, OpenCode | 3 | 5 | +2 | Run tests, fix failures |
| Test generation | Copilot, Cursor | 3 | 4 | +1 | Generate test cases |
| Debugger integration | Future | 5 | 5 | 0 | Step through code with AI help |
| Lint auto-fix | aider | 3 | 4 | +1 | Fix lint errors automatically |
| **DOCUMENTATION & LEARNING** |
| Docstring generation | Agent-Q (working!) | 2 | 4 | +2 | Add comprehensive docstrings |
| Code explanation | All | 1 | 3 | +2 | Understand unfamiliar code |
| API documentation lookup | Cursor, Copilot | 3 | 3 | 0 | Pull from online docs |
| Generate README/docs | Cursor, Copilot | 2 | 3 | +1 | Project documentation |
| Comment generation | All | 1 | 2 | +1 | Inline code comments |
| **PERFORMANCE & EFFICIENCY** |
| Repository-wide search | OpenCode, Cursor | 3 | 4 | +1 | Fast grep/ripgrep integration |
| File watching | OpenCode LSP | 3 | 3 | 0 | Detect external changes |
| Incremental indexing | Cursor | 4 | 3 | -1 | Re-index only changed files |
| Streaming responses | All modern tools | 2 | 4 | +2 | See output as it generates |
| Token usage tracking | Most tools | 2 | 3 | +1 | Monitor API costs |
| Local caching | Some tools | 3 | 3 | 0 | Cache responses for speed |
| **COLLABORATION & ENTERPRISE** |
| Shareable sessions | OpenCode, Goose | 3 | 3 | 0 | Send conversation links |
| Team knowledge base | Copilot Enterprise | 5 | 4 | -1 | Org-wide code understanding |
| Code review integration | Some tools | 4 | 4 | 0 | PR review assistance |
| Security scanning | Some tools | 4 | 4 | 0 | Vulnerability detection |
| Audit logs | Enterprise tools | 3 | 3 | 0 | Track AI actions |
| **SAFETY & CONTROL** |
| Safety levels per tool | Agent-Q ✓ | 2 | 5 | +3 | Safe/moderate/dangerous classification |
| Approval workflows | Agent-Q (diff review) | 2 | 5 | +3 | Human-in-the-loop for edits |
| Dry-run mode | Some tools | 2 | 4 | +2 | Preview without executing |
| Undo operations | Cursor, Copilot | 3 | 5 | +2 | Revert AI changes |
| Sandbox execution | Future | 5 | 4 | -1 | Isolated environment for code |
| **KNOWLEDGE & MEMORY** |
| Recipe/skill system | Goose ⭐ | 4 | 5 | +1 | Reusable workflow definitions |
| Custom tool definitions | All extensible | 3 | 4 | +1 | Add project-specific tools |
| Learning from corrections | Future | 5 | 5 | 0 | Improve from user edits |
| Project conventions | Some tools | 4 | 4 | 0 | Learn coding standards |
| Decision memory | Task Orchestrator | 4 | 5 | +1 | Remember past choices |

---

## Top 20 Features by Priority Score

*Priority = Benefit - Complexity (higher is better ROI)*

| Rank | Feature | Priority | Complexity | Benefit | Quick Win? |
|------|---------|----------|------------|---------|------------|
| 1 | Reviewable Git history | +4 | 1 | 5 | ⭐ YES |
| 2 | Bring-your-own API key | +4 | 1 | 5 | ⭐ YES |
| 3 | Session persistence | +3 | 2 | 5 | ⭐ YES |
| 4 | File editing tools | +3 | 2 | 5 | ✓ (Have) |
| 5 | Autonomous iteration | +3 | 2 | 5 | ✓ (Have) |
| 6 | Max iteration limits | +3 | 1 | 4 | ✓ (Have) |
| 7 | Safety levels per tool | +3 | 2 | 5 | ✓ (Have) |
| 8 | Approval workflows | +3 | 2 | 5 | ✓ (Have) |
| 9 | Model switching | +3 | 2 | 5 | ⭐ YES |
| 10 | Dedicated chat buffer | +3 | 1 | 4 | ✓ (Have) |
| 11 | Per-hunk diff approval | +2 | 3 | 5 | (Enhancement-1) |
| 12 | Auto-commit changes | +2 | 3 | 5 | High value! |
| 13 | Unified diff application | +2 | 2 | 4 | (Enhancement-1) |
| 14 | Git status awareness | +2 | 2 | 4 | Easy win |
| 15 | Repository map/indexing | +1 | 4 | 5 | Hard but critical |
| 16 | @ file/symbol mentions | +2 | 2 | 4 | ⭐ YES |
| 17 | Context window tracking | +2 | 2 | 4 | ⭐ YES |
| 18 | Named sessions | +2 | 2 | 4 | ⭐ YES |
| 19 | Read-only tools | +2 | 2 | 4 | ✓ (Have) |
| 20 | Linter/test integration | +2 | 3 | 5 | High value! |

---

## User Feedback Themes

### What Users Love ❤️

**Git Integration (aider):**
> "Git integration stands out as a thoughtful feature. Aider commits any pending changes with clear messages before making its own edits, meaning you never lose work if you need to undo an AI change." - [Blott Review](https://www.blott.com/blog/post/aider-review-a-developers-month-with-this-terminal-based-code-assistant)

> "It's really like having your senior developer live right in your Git repo - truly amazing!" - rappster ([Gerry Pass review](https://www.gerrypass.com/articles/aider-top-tier-ai-coding-assistant-and-best-ai-pair-programming-tool))

**Granular Control:**
> "Aider is the precision tool of LLM code gen... Minimal, thoughtful and capable of surgical changes ... while keeping the developer in control." - Reilly Sweetland

**Context Engineering (gptel):**
> "With gptel you can effortlessly switch models and tools, edit past conversation content, or branch the context. Users have tried various LLM clients, but haven't encountered another that makes it so easy to control all three key aspects of LLM usage." - [gptel docs](https://gptel.org/)

**Productivity Gains:**
> "Aider ... has easily quadrupled my coding productivity." - SOLAR_FIELDS ([Gerry Pass](https://www.gerrypass.com/articles/aider-top-tier-ai-coding-assistant-and-best-ai-pair-programming-tool))

> "99% of developers say AI tools save them time, and 68% clock more than ten hours saved per week." - [Atlassian State of DevEx 2025](https://www.shakudo.io/blog/best-ai-coding-assistants)

**Flexibility (Goose):**
> "You can bring your existing AI subscriptions to Goose - GitHub Copilot, Cursor, OpenAI, Anthropic, or any OpenAI-compatible provider. Goose is model-agnostic." - [What Makes Goose Different](https://dev.to/nickytonline/what-makes-goose-different-from-other-ai-coding-agents-2edc)

**Transparency:**
> "Goose emphasizes transparency: you can see exactly what the agent is doing, which commands it runs, etc." - [Goose Review](https://medium.com/onion-creative/why-goose-is-the-ai-coding-agent-that-finally-won-us-over-8835c662d152)

### What Users Want 🎯

**Context Awareness ([Best AI Coding Assistants 2025](https://www.shakudo.io/blog/best-ai-coding-assistants)):**
> "The most powerful assistants are those that understand your codebase, coding standards, and compliance requirements, making their recommendations truly context-aware."

**Multi-Agent Systems:**
> "The future of AI coding assistants is in multi-agent systems: specialized agents that communicate with each other, each handling distinct tasks under safe guardrails." - [20 Best AI Coding Assistants](https://www.qodo.ai/blog/best-ai-coding-assistant-tools/)

**Privacy & Security:**
> "In 2025, the importance of security and privacy in AI coding assistants can't be overstated. Many tools now support local deployment options." - [Best AI Coding Assistants](https://www.shakudo.io/blog/best-ai-coding-assistants)

**Persistent Memory:**
> "Users report spending 5-10 minutes per session re-establishing context that should persist automatically." - [Task Orchestrator](https://github.com/jpicklyk/task-orchestrator)

### Pain Points 😤

**Context Window Limits:**
> "By task 10-15, the context window fills with 200k+ tokens. The model loses focus, forgets earlier decisions, and eventually fails." - [Task Orchestrator](https://github.com/jpicklyk/task-orchestrator)

**Prompt Misinterpretation:**
> "Context and variable usage aren't always well handled; instructions may be misunderstood, especially if your request is vague or spans multiple files." - [Blott Aider Review](https://www.blott.com/blog/post/aider-review-a-developers-month-with-this-terminal-based-code-assistant)

**Security Concerns:**
> "48% of AI-generated code contains potential security vulnerabilities, highlighting the ongoing need for human review." - [Second Talent Stats](https://www.secondtalent.com/resources/ai-coding-assistant-statistics/)

**Coordination Overhead:**
> "The 2024 DORA report found that a 25% increase in AI adoption triggered a 7.2% decrease in delivery stability. Multi-agent coding amplifies both productivity gains and coordination risks." - [Multi-Agent Parallel Development](https://www.digitalapplied.com/blog/multi-agent-coding-parallel-development)

---

## Recommendations for Agent-Q

### Immediate Wins (Complexity ≤ 2, Benefit ≥ 4)

**Priority 1: Context & Session Management**
1. **Named sessions** (Complexity: 2, Benefit: 4)
   - Store conversations in `~/.agent-q/sessions/project-name/session-name.lisp`
   - `agent-q-resume-session` command with completion
   - Auto-save on exit

2. **Context window tracking** (Complexity: 2, Benefit: 4)
   - Show "15% remaining" warning in modeline
   - Offer `/compact` command to summarize
   - Auto-compact at 10% remaining

3. **@ mentions for files/symbols** (Complexity: 2, Benefit: 4)
   - Parse `@filename.lisp` → add to context
   - Parse `@package:symbol` → describe and add
   - Integrate with existing context system

**Priority 2: Git Integration**
4. **Git status awareness** (Complexity: 2, Benefit: 4)
   - Run `git status` before proposing changes
   - Show "3 unstaged files, 2 staged" in agent response
   - Warn if proposing edits to unstaged changes

5. **Auto-commit changes** (Complexity: 3, Benefit: 5)
   - After successful diff application, offer to commit
   - Generate commit message from change description
   - Add `Co-authored-by: Agent-Q` trailer

**Priority 3: Model Flexibility**
6. **Model switching** (Complexity: 2, Benefit: 5)
   - `C-c q m` to select model interactively
   - Store per-session (GPT-4o for exploration, Claude Opus for coding)
   - Retry failed tool calls with alternative model

### Medium-Term Enhancements (Complexity 3-4, High Benefit)

**Repository Understanding**
7. **Simple repo map** (Complexity: 4, Benefit: 5)
   - Phase 1: File listing with short descriptions (comments)
   - Phase 2: AST parsing for function/class signatures
   - Phase 3: Call graph for dependencies
   - Don't try to compete with Cursor's full indexing initially

**Testing Integration**
8. **Test runner integration** (Complexity: 3, Benefit: 5)
   - `run_tests` tool: execute test suite, return results
   - `fix_test_failure` workflow: analyze failure → propose fix → rerun
   - Integration with FiveAM, Parachute, etc.

**Diff Workflow (see Enhancement-1)**
9. **Per-hunk approval** (Complexity: 3, Benefit: 5)
   - Detailed in Enhancement-1
   - Use built-in `diff-apply-hunk`

### Long-Term Vision (Complexity 5, Game-Changers)

**Multi-Agent Capabilities**
10. **Parallel task execution** (Complexity: 5, Benefit: 5)
    - Spawn sub-agents for independent tasks
    - Example: Agent 1 writes tests, Agent 2 implements, Agent 3 documents
    - Coordinate via shared context/state
    - **Caution**: Start with 2-3 agents max, not 8+

**Persistent Memory**
11. **Long-term project memory** (Complexity: 4, Benefit: 5)
    - Store decisions, conventions, gotchas in `.agent-q/memory.db`
    - Auto-load on session start: "Remembered: Use Alexandria for utilities"
    - Learn from user corrections over time

---

## Anti-Patterns to Avoid

### Don't Reinvent Diff Application ❌
**Lesson**: Use Emacs built-ins (`diff-apply-hunk`), don't write custom string matching.
**Source**: Enhancement-1 research

### Don't Over-Agent 🤖❌
> "Three to five agents usually beats eight to ten. Beyond that, merge complexity eats the gains." - [Multi-Agent Parallel Execution](https://skywork.ai/blog/agent/multi-agent-parallel-execution-running-multiple-ai-agents-simultaneously/)

**Recommendation**: Start with simple sub-agent spawning (Phase 3), not full orchestration.

### Don't Break Terminal Workflows 💻❌
> "Devs won't switch editors or abandon dev containers. Assistants must work inline without breaking keymaps or pre-commit hooks." - [Best AI Coding Assistants](https://www.shakudo.io/blog/best-ai-coding-assistants)

**Recommendation**: Agent-Q stays in Emacs, leverages SLY integration.

### Don't Ignore Security 🔒❌
> "48% of AI-generated code contains potential security vulnerabilities" - [Second Talent](https://www.secondtalent.com/resources/ai-coding-assistant-statistics/)

**Recommendation**: Keep safety levels (`:safe`, `:moderate`, `:dangerous`), require approval for dangerous operations.

### Don't Lose Context 🧠❌
> "Users report spending 5-10 minutes per session re-establishing context that should persist automatically." - [Task Orchestrator](https://github.com/jpicklyk/task-orchestrator)

**Recommendation**: Session persistence (Immediate Win #1) is critical.

---

## Competitive Positioning

### What Makes Agent-Q Unique

**Lisp-Native Advantage:**
- Live REPL interaction (no other tool has this for CL)
- Condition system integration (Phase 3)
- Macro-aware introspection
- Package system understanding
- CLOS introspection (slots, hierarchy, MOP)

**Emacs-Native Advantage:**
- Works where Lisp developers live (SLY/SLIME users)
- No context switch to different IDE
- Leverage decades of Emacs diff/version-control infrastructure
- Integration with Org-mode for literate programming

**Best-of-Breed Approach:**
- Git integration (from aider)
- Per-hunk diffs (from macher/gptel)
- Context engineering (from gptel)
- Tool safety (unique to Agent-Q)
- Model flexibility (from Goose/gptel)

### Where Agent-Q Can Lead

**Common Lisp Development:**
- **Only** AI coding assistant purpose-built for CL
- Deep understanding of Lisp idioms, conventions
- Integration with Quicklisp, ASDF
- Macro expansion and debugging assistance

**Research & Experimentation:**
- Small, hackable codebase
- Clear phases (Foundation → Agentic → Autonomous → Intelligent)
- Specs-first development
- Community-driven feature prioritization

---

## Implementation Roadmap Sketch

### Phase 2.5: Quick Wins (2-3 weeks)
- Named sessions with auto-save
- Context window tracking
- @ mentions for files/symbols
- Git status awareness
- Model switching UI

### Phase 2.75: Git Integration (2-3 weeks)
- Auto-commit workflow
- Commit message generation
- Branch management commands
- Pre-commit hook support

### Phase 3: Enhanced (per original plan)
- Condition system integration
- Testing framework integration
- Knowledge base (project memory)
- Per-hunk diff approval (Enhancement-1)

### Phase 4: Advanced (future)
- Simple repository map
- Multi-agent task delegation
- Advanced debugging integration
- Persistent long-term memory

---

## Metrics for Success

### User Adoption
- Weekly active users
- Session length (should increase with persistence)
- Feature usage (which tools are called most?)

### Code Quality
- % of AI-proposed changes accepted (target: >70%)
- Time saved per session (user survey)
- Code generated vs code reviewed/edited ratio

### Developer Satisfaction
- "Would you recommend Agent-Q?" (NPS score)
- "Agent-Q is better than [alternative]" rating
- Feature request themes

### Technical Performance
- Average tool execution time
- Context window utilization (before/after compaction)
- Token costs per session

---

## References

### Tools Analyzed
- [gptel](https://github.com/karthink/gptel) - Simple LLM client for Emacs
- [macher](https://github.com/kmontag/macher) - Multi-file LLM editing
- [aider](https://github.com/Aider-AI/aider) - Terminal AI pair programmer
- [Goose](https://github.com/block/goose) - Open-source AI agent framework
- [OpenCode](https://github.com/opencode-ai/opencode) - Terminal AI coding agent

### User Reviews & Testimonials
- [Blott: Aider Review](https://www.blott.com/blog/post/aider-review-a-developers-month-with-this-terminal-based-code-assistant)
- [Ben's Journal: gptel Review](https://www.blogbyben.com/2024/08/gptel-mindblowing-integration-between.html)
- [Why Goose Won Us Over](https://medium.com/onion-creative/why-goose-is-the-ai-coding-agent-that-finally-won-us-over-8835c662d152)
- [What Makes Goose Different](https://dev.to/nickytonline/what-makes-goose-different-from-other-ai-coding-agents-2edc)

### Industry Research
- [Stack Overflow Developer Survey 2024](https://stackoverflow.blog/2024/05/29/developers-get-by-with-a-little-help-from-ai-stack-overflow-knows-code-assistant-pulse-survey-results/)
- [Atlassian State of Developer Experience 2025](https://www.shakudo.io/blog/best-ai-coding-assistants)
- [Second Talent: AI Coding Assistant Statistics](https://www.secondtalent.com/resources/ai-coding-assistant-statistics/)
- [Best AI Coding Assistants 2025](https://www.shakudo.io/blog/best-ai-coding-assistants)
- [20 Best AI Coding Assistant Tools](https://www.qodo.ai/blog/best-ai-coding-assistant-tools/)

### Technical Deep Dives
- [aider SWE-Bench Results](https://aider.chat/2024/05/22/swe-bench-lite.html)
- [aider Code Editing Benchmarks](https://aider.chat/docs/benchmarks.html)
- [Unified Diffs Make GPT-4 3X Less Lazy](https://aider.chat/docs/unified-diffs.html)
- [gptel-rewrite addons](https://github.com/karthink/gptel/wiki/gptel%E2%80%90rewrite-addons)
- [Code Surgery: How AI Assistants Edit Files](https://fabianhertwig.com/blog/coding-assistants-file-edits/)

### Multi-Agent & Architecture
- [Multi-Agent Parallel Execution](https://skywork.ai/blog/agent/multi-agent-parallel-execution-running-multiple-ai-agents-simultaneously/)
- [How Anthropic Built Their Multi-Agent Research System](https://www.anthropic.com/engineering/multi-agent-research-system)
- [Parallelizing AI Coding Agents](https://ainativedev.io/news/how-to-parallelize-ai-coding-agents)
- [AI Agent Orchestration Patterns](https://learn.microsoft.com/en-us/azure/architecture/ai-ml/guide/ai-agent-design-patterns)

### Context Management
- [Task Orchestrator](https://github.com/jpicklyk/task-orchestrator) - Persistent AI memory MCP server
- [Managing Claude Code's Context](https://www.cometapi.com/managing-claude-codes-context/)
- [Amazon Bedrock AgentCore Memory](https://aws.amazon.com/blogs/machine-learning/amazon-bedrock-agentcore-memory-building-context-aware-agents/)

### Voice & Accessibility
- [aider Voice-to-Code](https://aider.chat/docs/usage/voice.html)
- [Vibe Coding: AI + Voice](https://wisprflow.ai/vibe-coding)

---

**End of Enhancement 2**
