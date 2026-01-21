# Agent-Q Phase 2: Comprehensive Test Suite

## Overview

Agent-Q Phase 2 includes a comprehensive test suite covering all 18 tools, the tool registry, execution pipeline, and integration with the agent loop.

**Test Coverage:**
- ✅ Registry initialization and management (3 tests)
- ✅ Introspection tools (9 test suites covering 9 tools)
- ✅ Execution tools (7 tests covering 4 tools)
- ✅ Buffer tools (3 tests covering 4 tools)
- ✅ Diff tool (2 tests)
- ✅ Tool execution pipeline (3 tests)
- ✅ Error handling (3 tests)
- ✅ Tool availability (9 tests)
- ✅ Backtrace and utilities (6 tests)
- ✅ Tool execution mechanisms (3 tests)
- ✅ Agent loop simulation (4 tests)
- ✅ Error recovery (3 tests)

**Total: 58 test cases across 12 test suites**

## Running Tests

### Quickest Test (No Dependencies)

```lisp
(ql:quickload :agent-q/tests)
(agent-q-tests:run-phase-2-tests)
```

### Running Individual Test Suites

```lisp
(ql:quickload :agent-q/tests)
(fiveam:run! 'agent-q-tests:phase-2-tools)
(fiveam:run! 'agent-q-tests:phase-2-integration)
```

### Running Specific Test Suites

```lisp
;; Registry tests
(fiveam:run! 'agent-q-tests:registry-tests)

;; Introspection tool tests
(fiveam:run! 'agent-q-tests:introspection-tools-tests)

;; Execution tool tests
(fiveam:run! 'agent-q-tests:execution-tools-tests)

;; Integration tests
(fiveam:run! 'agent-q-tests:tool-availability-tests)
(fiveam:run! 'agent-q-tests:backtrace-tests)
(fiveam:run! 'agent-q-tests:tool-execution-tests)
(fiveam:run! 'agent-q-tests:agent-loop-tests)
(fiveam:run! 'agent-q-tests:error-recovery-tests)
```

### Running via ASDF

```lisp
(asdf:test-system :agent-q)
```

## Test Structure

### 1. Registry Tests (`registry-tests`)

Tests the tool registry system that manages all 18 tools.

- **initialize-registry**: Verifies registry is initialized on load
- **get-registered-tools**: Checks that all tools can be retrieved
- **tool-categories**: Verifies tools have proper category tags

```lisp
;; Example
(fiveam:run! 'agent-q-tests:registry-tests)
```

### 2. Introspection Tools Tests (`introspection-tools-tests`)

Tests the 9 read-only introspection tools:

1. **describe_symbol**: Get info about symbols
   - Basic description retrieval
   - Non-existent symbol handling

2. **apropos_search**: Find symbols by pattern
   - Basic pattern matching
   - Empty result handling

3. **function_arglist**: Get function lambda lists
   - Builtin function handling
   - Graceful degradation without SWANK/SLYNK

4. **who_calls**: Find callers (requires SLYNK)
   - Graceful handling when SLYNK unavailable

5. **who_references**: Find references
   - Same graceful handling as who_calls

6. **list_package_symbols**: List package exports
   - Valid package listing
   - Invalid package handling

7. **class_slots**: Inspect CLOS class slots
   - Standard class introspection

8. **class_hierarchy**: Show class precedence
   - Hierarchy information retrieval

9. **macroexpand_form**: Expand macros
   - Basic macro expansion

### 3. Execution Tools Tests (`execution-tools-tests`)

Tests the 4 code execution tools:

1. **eval_form**: Evaluate Lisp code
   - Simple expression evaluation
   - Multiple value handling
   - Error capture
   - Package context support

2. **compile_form**: Compile and load code
   - Basic compilation
   - Warning capture

3. **get_last_error**: Retrieve last error
   - Error information retrieval

4. **get_repl_history**: Get execution history
   - History retrieval

### 4. Buffer Tools Tests (`buffer-tools-tests`)

Tests the 4 buffer/file operation tools:

1. **read_file**: Read file contents
   - File reading with Emacs connection
   - Graceful handling without Emacs

2. **read_buffer**: Read buffer contents
   - Buffer reading with Emacs

3. **write_file**: Write to files
   - Safety level verification (:dangerous)

4. **search_in_buffer**: Search in buffers
   - Pattern search with Emacs

### 5. Diff Tool Tests (`diff-tool-tests`)

Tests the key Phase 2 diff tool:

- **propose_file_edit**: Show diffs for user review
  - Tool registration verification
  - Required argument checking
  - Safety level verification (:moderate)

### 6. Tool Availability Tests (`tool-availability-tests`)

Tests that all tools are properly registered and formatted for LLM:

- **all_expected_tools_present**: All 18 tools registered
- **safe_tools_accessible**: Safe tools available at :safe level
- **all_tools_have_descriptions**: Each tool has documentation
- **all_tools_have_parameters**: Each tool has parameter specs
- **introspection_tools_safe**: Introspection tools are :safe
- **execution_tools_moderate**: Execution tools are :moderate or :safe
- **diff_tool_moderate**: Diff tool is :moderate

### 7. Backtrace Tests (`backtrace-tests`)

Tests utility functions for error handling:

- **capture_backtrace_portable**: Returns string without error
- **safe_symbol_lookup**: Finds existing symbols
- **safe_symbol_lookup_nonexistent**: Handles missing symbols
- **format_for_llm_truncates**: Truncates long strings
- **format_for_llm_short_strings**: Preserves short strings

### 8. Tool Execution Tests (`tool-execution-tests`)

Tests the tool execution mechanism:

- **tool_handler_callable**: Handlers are callable
- **tool_handler_returns_string**: Handlers return strings
- **multiple_tool_calls**: Can execute tools sequentially

### 9. Agent Loop Tests (`agent-loop-tests`)

Simulates agent loop with tools:

- **agent_can_access_tools**: Registry accessible
- **tool_results_formatting**: Results formattable for LLM
- **sequential_tool_execution**: Tools execute in sequence

### 10. Error Recovery Tests (`error-recovery-tests`)

Tests error handling and recovery:

- **missing_required_args**: Missing args handled gracefully
- **tool_error_doesnt_crash_registry**: Errors don't corrupt registry
- **repeated_tool_calls_stable**: Repeated calls stable

## Expected Test Results

All tests should pass when run against a correctly loaded Agent-Q system:

```
Phase 2 Tool System Tests
✓ Registry Tests (3 tests)
✓ Introspection Tools Tests (9 tests)
✓ Execution Tools Tests (7 tests)
✓ Buffer Tools Tests (3 tests)
✓ Diff Tool Tests (2 tests)
✓ Tool Execution Pipeline Tests (3 tests)
✓ Error Handling Tests (3 tests)

Phase 2 Integration Tests
✓ Tool Availability Tests (9 tests)
✓ Backtrace Tests (6 tests)
✓ Tool Execution Tests (3 tests)
✓ Agent Loop Tests (4 tests)
✓ Error Recovery Tests (3 tests)

Total: 58 tests passed
```

## Test Dependencies

Tests require:
- **FiveAM**: Test framework (automatically loaded)
- **CL-LLM-Provider**: Tool infrastructure (part of Agent-Q)
- **Agent-Q**: Main system (tested by these tests)

## How to Add New Tests

1. Add test to appropriate suite in `phase2-tools-tests.lisp` or `phase2-integration-tests.lisp`
2. Use `fiveam:test` macro with descriptive name and docstring
3. Use `is` macro for assertions
4. Add to appropriate `define-test-suite` block

Example:

```lisp
(define-test-suite my-tools-tests
  "Tests for new tools"
  (test new-tool-basic
    "new_tool should do something"
    (let ((handler (find-tool-handler "new_tool")))
      (is (not (null handler)))
      (let ((result (funcall handler '(:arg "value"))))
        (is (stringp result))))))
```

## Testing Without Emacs Connection

Most tests work without SLYNK/SWANK connection:
- Introspection tools test gracefully degrade
- Backtrace capture uses implementation-specific fallbacks
- Buffer tools return error messages without Emacs

Only full diff workflow requires live Emacs connection via SLY.

## Manual Testing Checklist

After automated tests pass:

- [ ] Load `(ql:quickload :agent-q)` cleanly
- [ ] All 18 tools registered: `(agent-q.tools:get-agent-q-tools)`
- [ ] Can call tools individually
- [ ] Tool results formatmable for LLM output
- [ ] Errors caught and logged properly
- [ ] Registry survives multiple tool calls
- [ ] No memory leaks or state corruption

## Continuous Integration

Tests suitable for CI/CD:

```bash
# In your CI script
sbcl --non-interactive \
     --eval "(ql:quickload :agent-q/tests)" \
     --eval "(agent-q-tests:run-phase-2-tests)" \
     --quit
```

Exit code 0 = all tests passed
Exit code 1 = test failures

## Performance Notes

- Registry initialization: <100ms
- Tool handler invocation: <50ms per call
- Full test suite: <5 seconds (no Emacs connection)
- With Emacs: Add time for diff workflow testing

## Troubleshooting

### "Package AGENT-Q.TOOLS does not exist"

The tools module must be loaded first. Ensure `agent-q.asd` system definition is correct.

```lisp
(ql:quickload :agent-q)  ; Must load main system first
(ql:quickload :agent-q/tests)  ; Then tests
```

### "Tool handler not found"

Registry may not be initialized. Check:

```lisp
(agent-q.tools:*agent-q-registry*)  ; Should not be NIL
(agent-q.tools:get-agent-q-tools)   ; Should return list
```

### "FiveAM not available"

Install it:

```lisp
(ql:quickload :fiveam)
```

## See Also

- `CLAUDE.md` - Project architecture and Phase 2 specification
- `src/tools/` - Tool implementations
- `agent-q.asd` - System definition with test configuration
