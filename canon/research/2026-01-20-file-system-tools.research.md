# Feature Research: Comprehensive File System Tools for AI Agents

## Executive Summary

**Domain**: File system manipulation tools for AI coding agents
**Date**: 2026-01-20
**Primary Reference**: Claude's `str_replace_based_edit_tool` (Anthropic API) - cleanest command structure, battle-tested
**Secondary References**: MCP Filesystem Server (standard protocol), Aider's edit formats (multi-format flexibility)

### Key Findings

1. **Three dominant edit paradigms exist**: (a) str_replace with exact matching, (b) search/replace blocks with markers, (c) full unified diff - each with different tradeoffs for accuracy vs. token efficiency vs. human review.

2. **Directory awareness is essential**: All mature implementations provide project root concepts, directory listing, tree navigation, and path validation. Without this, agents cannot understand codebase structure.

3. **Safety requires multiple layers**: Read-only operations (`:safe`), logged mutations (`:cautious`), and approval-gated destructive operations (`:dangerous`) with user review - Agent-Q already has this foundation.

### Recommendation

Implement a **multi-tier file system tool suite** building on Agent-Q's existing infrastructure:
- **Tier 1 (Safe)**: Directory listing, file info, tree navigation, glob search
- **Tier 2 (Cautious)**: str_replace edits with automatic diff generation + logging
- **Tier 3 (Dangerous)**: File creation, full overwrites, moves/deletes - via existing diff approval UI

---

## 1. Problem Domain

### Core Functionality

File system tools for AI agents enable autonomous code editing by providing:
- **Reading**: Examine files, directories, metadata
- **Writing**: Create, modify, overwrite files
- **Navigation**: Understand project structure, find files
- **Editing**: Make targeted changes without full file replacement

### Adjacent Functionality

Mature implementations also include:
- **Project root management**: Bounded workspace for safety
- **Multi-file operations**: Edit across files atomically
- **Search/grep integration**: Find code before editing
- **Version control awareness**: Git status, staging integration
- **Backup/undo capabilities**: Recovery from mistakes

### Out of Scope for This Research

- Git operations (commit, push, branch) - separate tool category
- Remote file systems (SFTP, S3) - future extension
- Binary file manipulation - not applicable to code editing
- IDE-specific integrations (LSP, debugger) - separate concern

---

## 2. Landscape Survey

### Implementations Reviewed

| Name | Ecosystem | Approach | Last Active | Notes |
|------|-----------|----------|-------------|-------|
| Claude str_replace_editor | Anthropic API | Exact str_replace | 2025-07 | Primary reference, clean design |
| MCP Filesystem Server | Node.js/MCP | Full CRUD + edit_file | 2025-12 | Protocol standard, security-focused |
| Aider | Python CLI | Multi-format (diff, whole, udiff) | 2026-01 | Most flexible, model-adaptive |
| Cursor Composer | VS Code fork | Multi-file agent | 2026-01 | IDE-integrated, semantic index |
| Agent-Q (current) | CL + Emacs | propose_file_edit (diff) | Current | Has diff approval, needs expansion |

### Ecosystem Observations

**Anthropic/Claude**: The `str_replace_based_edit_tool` defines the API standard:
- Commands: `view`, `str_replace`, `create`, `insert`
- Exact string matching prevents ambiguity
- Line numbers for view ranges and insert positions
- `max_characters` parameter for large files

**MCP Protocol**: Establishes filesystem server pattern:
- Allowed directories for access control
- Separate tools for read vs write operations
- `edit_file` with pattern matching and dry-run mode
- Directory tree and search capabilities

**Aider**: Proves multi-format approach works:
- Different LLMs prefer different edit formats
- Search/replace blocks (`<<<<<<< SEARCH`) intuitive
- Architect/Editor separation for complex changes
- Model-specific format selection

**Common Lisp / Agent-Q Current State**:
- ✅ `read_file` - basic file reading via Emacs
- ✅ `write_file` - dangerous full overwrite
- ✅ `read_buffer` - Emacs buffer reading
- ✅ `search_in_buffer` - buffer search
- ✅ `propose_file_edit` - diff approval workflow
- ❌ No directory listing
- ❌ No project root concept
- ❌ No str_replace editing (only full diff)
- ❌ No file creation (new files)
- ❌ No file metadata

---

## 3. Deep Dive: Reference Implementations

### Claude str_replace_based_edit_tool (Anthropic API)

**Documentation**: https://platform.claude.com/docs/en/agents-and-tools/tool-use/text-editor-tool

#### API Overview

```json
{
  "type": "text_editor_20250728",
  "name": "str_replace_based_edit_tool",
  "max_characters": 10000
}
```

Commands:

```json
// View file or directory
{"command": "view", "path": "src/main.py", "view_range": [1, 50]}

// Replace exact text
{"command": "str_replace", "path": "src/main.py",
 "old_str": "def foo():", "new_str": "def foo(x):"}

// Create new file
{"command": "create", "path": "tests/test_foo.py", "file_text": "..."}

// Insert at line
{"command": "insert", "path": "src/main.py", "insert_line": 10, "new_str": "# comment"}
```

#### Architecture

```
User Request → Claude → Tool Call → Your Implementation → File System
                 ↑                           │
                 └───── Tool Result ─────────┘
```

- Claude provides the tool schema (built-in, not customizable)
- Your application implements the actual file operations
- Results returned as tool_result messages

#### Complexity Analysis

- **Tool Schema**: ~700 tokens overhead per request
- **Implementation**: ~200 lines for complete handler
- **Key Constraint**: `str_replace` must match EXACTLY once
- **Error Cases**: No match, multiple matches, file not found

**What makes it simple**:
- Single command per call (no batch operations)
- Exact string matching (no regex complexity)
- Line numbers make positioning unambiguous
- Built-in schema reduces implementation burden

**What makes it complex**:
- Exact matching can fail on whitespace differences
- No undo in latest version (must track externally)
- Large files need view_range management
- Directory listing is basic (no tree)

#### Strengths
- Clean, well-documented API
- Battle-tested across Claude users
- Minimal token overhead for edits
- Clear error semantics

#### Weaknesses
- No glob/search for finding files
- No file metadata (size, modified date)
- No move/copy operations
- Single-file edits only

---

### MCP Filesystem Server (Model Context Protocol)

**Repository**: https://github.com/modelcontextprotocol/servers/tree/main/src/filesystem

#### API Overview

```typescript
// Read operations
read_text_file({ path: string, head?: number, tail?: number })
list_directory({ path: string })
directory_tree({ path: string, excludePatterns?: string[] })
search_files({ path: string, pattern: string, excludePatterns?: string[] })
get_file_info({ path: string })

// Write operations
write_file({ path: string, content: string })  // destructive hint
edit_file({ path: string, edits: [{oldText, newText}], dryRun?: boolean })
create_directory({ path: string })
move_file({ source: string, destination: string })
```

#### Architecture

```
MCP Client (Claude, GPT, etc.)
         │
         │ JSON-RPC over stdio/HTTP
         ▼
┌─────────────────────────┐
│   MCP Filesystem Server │
│  ┌───────────────────┐  │
│  │  Access Control   │  │  ← Allowed directories
│  │  Path Validation  │  │  ← Prevent traversal
│  │  Operation Hints  │  │  ← destructiveHint, readOnlyHint
│  └───────────────────┘  │
└────────────┬────────────┘
             │
             ▼
         File System
```

#### Complexity Analysis

- **Total LoC**: ~1500 (TypeScript)
- **Tools Count**: 12 tools
- **Dependencies**: Node.js, MCP SDK

**What makes it simple**:
- Clear separation of read vs write
- Declarative access control
- Standardized protocol

**What makes it complex**:
- Many tools to implement
- Pattern matching in edit_file
- Tree traversal with exclusions

#### Strengths
- Comprehensive coverage (12 tools)
- Security-first design (allowed directories)
- Protocol standard (works with any MCP client)
- edit_file with dryRun for preview

#### Weaknesses
- No approval workflow (just hints)
- Requires running separate server
- No Emacs/editor integration

---

### Aider Edit Formats

**Documentation**: https://aider.chat/docs/more/edit-formats.html

#### Format Comparison

| Format | Token Efficiency | Accuracy | Best For |
|--------|------------------|----------|----------|
| whole | Low (full file) | High | Small files |
| diff | High | Medium | Most models |
| diff-fenced | High | Medium | Gemini |
| udiff | High | High | GPT-4 Turbo |

#### Diff Format Example

```
src/main.py
<<<<<<< SEARCH
def foo():
    return 42
=======
def foo(x):
    return x * 2
>>>>>>> REPLACE
```

#### Architecture

```
User Prompt → [Architect Model] → High-level plan
                    │
                    ▼
            [Editor Model] → Edit instructions in chosen format
                    │
                    ▼
            [Aider Parser] → Apply to files
```

#### Complexity Analysis

- **Formats Supported**: 5 (whole, diff, diff-fenced, udiff, editor-*)
- **Model Adaptation**: Auto-selects format per model
- **Parsing Complexity**: Each format needs dedicated parser

**What makes it simple**:
- Search/replace blocks are human-readable
- Format selection can be automatic
- Multiple edits in single response

**What makes it complex**:
- Parser for each format
- Edge cases in marker detection
- Whitespace handling varies by format

#### Strengths
- Model-adaptive (picks best format)
- Intuitive markers (git merge conflict style)
- Supports multi-file edits in one response
- Widely adopted pattern (Cline, RooCode use similar)

#### Weaknesses
- Parser complexity for each format
- No built-in approval workflow
- No directory operations

---

## 4. Feature Analysis

### Core Features (Must Have)

| Feature | Complexity | Rationale |
|---------|------------|-----------|
| `list_directory` | Low | Essential for navigation |
| `read_file` (enhanced) | Low | Already exists, add line ranges |
| `edit_file` (str_replace) | Medium | Targeted edits without full diff |
| `create_file` | Low | New file creation |
| `project_root` | Low | Bounded workspace |
| `get_file_info` | Low | Size, timestamps |

### Expected Features (Should Have)

| Feature | Complexity | Rationale |
|---------|------------|-----------|
| `directory_tree` | Medium | Hierarchical view |
| `search_files` (glob) | Medium | Find files by pattern |
| `move_file` | Low | Rename/relocate |
| `delete_file` | Low | Cleanup |
| `multi_file_edit` | High | Atomic cross-file changes |

### Advanced Features (Could Have)

| Feature | Complexity | Rationale |
|---------|------------|-----------|
| `edit_file` with dry-run | Medium | Preview before apply |
| `watch_file` | High | React to external changes |
| `diff_files` | Medium | Compare two files |
| Git integration | High | Stage, commit from tools |

### Explicitly Out of Scope

- Remote filesystem access - different security model
- Binary file editing - not code editing
- Database/API writes - different tool category
- Real-time collaboration - requires sync protocol

---

## 5. Complexity vs. Benefit Matrix

```
                    │ Low Complexity     │ Medium             │ High              │
────────────────────┼────────────────────┼────────────────────┼───────────────────┤
Essential           │ list_directory     │ edit_file          │                   │
                    │ project_root       │ (str_replace)      │                   │
                    │ get_file_info      │                    │                   │
                    │ create_file        │                    │                   │
────────────────────┼────────────────────┼────────────────────┼───────────────────┤
Important           │ move_file          │ directory_tree     │ multi_file_edit   │
                    │ delete_file        │ search_files       │                   │
────────────────────┼────────────────────┼────────────────────┼───────────────────┤
Useful              │ enhanced read_file │ diff_files         │ watch_file        │
                    │ (line ranges)      │ dry_run preview    │ git integration   │
────────────────────┼────────────────────┼────────────────────┼───────────────────┤
Nice-to-have        │ (defer to v2)      │ (defer to v2)      │ (defer to v2)     │
```

---

## 6. Design Patterns Observed

### Universal Patterns

1. **Path validation**: All implementations validate paths against allowed directories
2. **Error semantics**: Structured error responses (not found, permission denied, multiple matches)
3. **Read/write separation**: Clear distinction between safe reads and dangerous writes
4. **Idempotent reads**: Reading same file twice gives same result

### Common Patterns

1. **Line numbers for context**: View ranges, insert positions use 1-indexed lines
2. **Exact matching for edits**: Prevents accidental changes
3. **Directory as first-class**: Not just files, but directory structure matters
4. **Metadata access**: Size, timestamps for informed decisions

### Divergent Approaches

| Aspect | Claude API | MCP Server | Aider |
|--------|------------|------------|-------|
| Edit mechanism | str_replace exact | edit_file patterns | format-dependent |
| Approval | External | Hints only | None |
| Multi-file | No | No | Yes |
| Project root | Implicit | Explicit config | Working directory |

---

## 7. Implementation Insights

### What Works Well

1. **str_replace with exact matching**: Prevents ambiguous edits, Claude is trained for it
2. **Separate tools per operation**: Clear semantics, easier to reason about
3. **Safety levels**: Agent-Q's `:safe/:cautious/:dangerous` maps well to these operations
4. **Diff approval for destructive ops**: Agent-Q already has this via `propose_file_edit`

### Common Pitfalls

1. **Whitespace sensitivity**: Exact matching fails if user's file has different indentation
2. **Large file handling**: Need view_range / head/tail to avoid context overflow
3. **Missing file creation**: `write_file` to nonexistent path creates implicitly (unexpected)
4. **No transaction semantics**: Multi-step edits can leave partial state

### Opportunities for Agent-Q

1. **Emacs integration advantage**: Can leverage Emacs' file handling, undo system, dired
2. **Existing diff approval**: `propose_file_edit` provides user control
3. **SLY RPC**: Already have the bridge for file operations
4. **CL condition system**: Better error handling than most implementations

---

## 8. Recommendations

### Recommended Approach

Build a **layered file system tool suite** with three tiers:

#### Tier 1: Safe Read Operations
- `list_directory`: List files/dirs with metadata
- `directory_tree`: Recursive JSON tree
- `get_file_info`: Size, timestamps, permissions
- `search_files`: Glob pattern matching
- `read_file`: Enhanced with line ranges

#### Tier 2: Cautious Edit Operations
- `edit_file`: str_replace style with automatic diff generation
  - If single match: apply directly (logged)
  - If multiple matches or complex: fallback to `propose_file_edit`
- `insert_at_line`: Insert text at specific line
- All edits logged, reversible via Emacs undo

#### Tier 3: Dangerous Destructive Operations
- `create_file`: New file with content (via `propose_file_edit` for approval)
- `write_file`: Full overwrite (existing, via approval)
- `move_file`: Rename/relocate (via approval)
- `delete_file`: Remove file (via approval)

### Reference to Study Closely

**Primary**: Claude's `str_replace_based_edit_tool` - clean API, well-documented
**For directory ops**: MCP Filesystem Server - comprehensive toolset
**For architecture**: Agent-Q's existing tool system - already proven

### Suggested MVP Scope

Phase 1 (Core):
1. `list_directory` tool
2. `get_file_info` tool
3. `edit_file` with str_replace semantics
4. Project root configuration

Phase 2 (Navigation):
1. `directory_tree` tool
2. `search_files` tool (glob)
3. Enhanced `read_file` with line ranges

Phase 3 (Lifecycle):
1. `create_file` tool
2. `move_file` tool
3. `delete_file` tool

### Future Expansion Path

- v2: Multi-file atomic edits
- v2: Git-aware operations (status, stage, unstage)
- v3: Watch mode for external changes
- v3: Semantic code search integration

---

## 9. Open Questions

1. **Project root mechanism**: Should we use SLY's `(slynk:default-directory)`, a config variable, or detect from `.git`?

2. **str_replace vs propose_file_edit**: When should `edit_file` auto-apply vs show diff? Threshold on number of changes? Always show for first edit?

3. **Emacs buffer vs file**: Should tools work on files or buffers? Current `read_file` uses temp buffer - should `edit_file` modify open buffers?

4. **Path format**: Relative to project root, or absolute? How to handle paths outside project?

5. **Large file strategy**: What's the right `max_characters` default? How to handle files larger than context window?

---

## 10. References

### Repositories
- Claude Text Editor Tool: https://platform.claude.com/docs/en/agents-and-tools/tool-use/text-editor-tool
- MCP Filesystem Server: https://github.com/modelcontextprotocol/servers/tree/main/src/filesystem
- Aider: https://github.com/paul-gauthier/aider

### Documentation
- Aider Edit Formats: https://aider.chat/docs/more/edit-formats.html
- MCP Protocol: https://publicapis.io/blog/mcp-model-context-protocol-guide
- Claude Code Overview: https://code.claude.com/docs/en/overview

### Articles & Comparisons
- Code Surgery: How AI Assistants Make Precise Edits: https://fabianhertwig.com/blog/coding-assistants-file-edits/
- Cursor vs VS Code Copilot: https://www.digitalocean.com/resources/articles/github-copilot-vs-cursor
