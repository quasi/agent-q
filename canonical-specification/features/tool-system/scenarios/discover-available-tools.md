---
type: scenario
name: discover-available-tools
version: 1.0.0
feature: tool-system
covers:
  - tool-registry
tags:
  - introspection
  - initialization
---

# Scenario: Discover Available Tools

**Feature:** tool-system
**User Story:** As the system, I want to register and discover available tools so the LLM knows what capabilities are available.
**Test Coverage:** Integration tests verify registry operations
**Confidence:** 0.90

---

## Context

The tool registry maintains a catalog of all available tools. At initialization, tools are registered, and the registry provides tool definitions to the LLM.

---

## Main Flow

### Step 1: System Loads Tool Modules

**Initialization:**
```lisp
(load-tool-modules)  ; Loads src/tools/*.lisp
```

**Each Module Registers Tools:**
```lisp
(define-tool "describe_symbol"
  "Get detailed information about a Lisp symbol"
  '((:name "symbol_name" :type "string" :required t)
    (:name "package" :type "string" :required nil))
  :safety-level :safe
  :handler #'describe-symbol-handler)
```

### Step 2: Registry Collects Tool Definitions

**Registry State:**
```lisp
*agent-q-registry* = {
  "describe_symbol" => #<TOOL describe_symbol :safe>,
  "eval_lisp" => #<TOOL eval_lisp :dangerous>,
  "read_file" => #<TOOL read_file :cautious>,
  ...  ; 18 total tools
}
```

### Step 3: System Generates LLM Tool Specification

**Tool Spec for LLM:**
```json
{
  "name": "describe_symbol",
  "description": "Get detailed information about a Lisp symbol",
  "input_schema": {
    "type": "object",
    "properties": {
      "symbol_name": {"type": "string", "description": "Symbol to describe"},
      "package": {"type": "string", "description": "Package name (optional)"}
    },
    "required": ["symbol_name"]
  }
}
```

---

## Postconditions

1. All tool modules loaded
2. 18 tools registered in registry
3. Tool specifications sent to LLM
4. LLM can call any registered tool

---

**Scenario Status:** ✅ Verified (18 tools successfully registered)
**Confidence:** 0.90
