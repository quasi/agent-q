# Agent-Q Quickstart

Get Agent-Q running in 10 minutes.

## What You'll Need

- ✅ Emacs with SLY installed
- ✅ Common Lisp (SBCL recommended)
- ✅ Quicklisp
- ✅ API key for Anthropic Claude or OpenAI

## Step 1: Set Your API Key (2 minutes)

Add to your shell configuration (`~/.bashrc` or `~/.zshrc`):

```bash
# For Anthropic Claude (recommended)
export ANTHROPIC_API_KEY="sk-ant-your-key-here"

# OR for OpenAI
export OPENAI_API_KEY="sk-your-key-here"
```

Then reload your shell:

```bash
source ~/.bashrc  # or ~/.zshrc
```

## Step 2: Install Agent-Q (3 minutes)

### Clone the repository

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/yourusername/agent-q.git
```

### Load in Common Lisp

Start your Lisp REPL and run:

```lisp
(ql:quickload "cl-llm-provider")  ; Install LLM provider
(ql:quickload :agent-q)            ; Load Agent-Q
```

You should see output showing tools being registered:

```
Registering tool: describe_symbol
Registering tool: eval_form
...
Loaded 18 Agent-Q tools
```

### Configure Emacs

Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(with-eval-after-load 'sly
  (add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")
  (require 'sly-agent-q)
  (sly-agent-q-setup))
```

Restart Emacs or evaluate with `M-x eval-buffer`.

## Step 3: Your First Chat (5 minutes)

### Start SLY

```
M-x sly
```

Agent-Q mode activates automatically when SLY connects.

### Ask a Question

Press `C-c q s` (or use menu: Agent-Q → Send Message)

Type:

```
How do I create a hash table in Common Lisp?
```

Press Enter. Agent-Q will respond with an explanation and examples.

### Try a Code Task

Write this function in a `.lisp` file:

```lisp
(defun greet (name)
  (format nil "Hello, ~A!" name))
```

1. **Add to context**: Place cursor on the function and press `C-c q c d`
2. **Ask for help**: Press `C-c q S` (capital S = with context)
3. **Type instruction**:
   ```
   Add a docstring and handle nil names gracefully
   ```
4. **Review response**: Agent-Q proposes improvements
5. **Insert code**: Press `C-c q i` to insert at point

## Step 4: Watch the Tools Work

Agent-Q has 18 tools to explore your Lisp environment. Try this:

```
Ask Agent-Q: "What does the MAPCAR function do and show me an example."
```

Watch as Agent-Q:
1. Uses `describe_symbol` to look up MAPCAR
2. Uses `eval_form` to run example code
3. Shows you real results from your REPL

## What's Next?

### Learn More

- **User Guide**: `user-guide.md` - Complete feature tour
- **Keyboard Shortcuts**: See the cheat sheet below
- **Advanced Features**: Diff review, sessions, context management

### Try These Workflows

**Debug a function:**
1. Add buggy function to context (`C-c q c d`)
2. Ask: "This crashes on empty lists. Fix it."
3. Review the proposed diff
4. Accept or reject the changes

**Explore a package:**
```
Ask: "What utility functions does Alexandria provide?"
```

**Document your code:**
1. Place cursor on function
2. Press `C-c q q d` (quick document)
3. Review and accept docstring

## Keyboard Shortcuts Cheat Sheet

| Key | Action |
|-----|--------|
| `C-c q s` | Send message (without context) |
| `C-c q S` | Send message WITH context |
| `C-c q c d` | Add current function to context |
| `C-c q c r` | Add selected region to context |
| `C-c q c c` | Clear all context |
| `C-c q i` | Insert last response at point |
| `C-c q v` | View conversation buffer |
| `C-c q n` | New conversation |
| `C-c q q d` | Quick: Document function |
| `C-c q q e` | Quick: Explain code |
| `C-c q q f` | Quick: Fix last error |

## Troubleshooting

### "No response" or authentication errors

**Check your API key:**

```bash
echo $ANTHROPIC_API_KEY  # Should show your key
```

If empty, set it and **restart your Lisp REPL** (environment variables aren't reloaded dynamically).

### "Tool not found" errors

**Verify tools loaded:**

```lisp
(length (agent-q.tools:get-agent-q-tools))
;; Should return 18
```

If wrong, reload:

```lisp
(ql:quickload :agent-q :force t)
```

### Emacs can't find sly-agent-q

**Check the load path:**

```elisp
;; In Emacs, evaluate:
(add-to-list 'load-path "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/")
(load "~/quicklisp/local-projects/agent-q/contrib/sly-agent-q/sly-agent-q.el")
```

## Success!

You now have Agent-Q running! The agent can:

- ✅ Answer questions about Common Lisp
- ✅ Inspect your running Lisp image
- ✅ Execute code and show real results
- ✅ Propose code improvements with reviewable diffs
- ✅ Iterate autonomously until tasks complete

**Ready for more?** Read the [User Guide](user-guide.md) for the complete feature tour.

---

**Need Help?**
- Check the [User Guide](user-guide.md) for detailed documentation
- See [Troubleshooting](user-guide.md#troubleshooting) section
- Review conversation buffer (`C-c q v`) for error messages
