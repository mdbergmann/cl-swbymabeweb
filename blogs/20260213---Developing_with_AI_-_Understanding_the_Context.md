### Introduction

AI-assisted development tools like Claude Code have become part of many developers' daily workflow. They can write code, run tests, search codebases, and orchestrate complex multi-step tasks. But to use them effectively -- and to avoid frustrating surprises mid-session -- you need to understand the single most important concept underlying these tools: **the context window**.

This post is about what the context actually is, how it works under the hood, why running out of it silently degrades your results, and what strategies you can use to stay in control.

### What Is the Context?

Here is the key insight: **the context is an array**. It is a client-side data structure -- a list of message objects -- that gets sent to the LLM with every single API call. The LLM itself is stateless. It has no memory between calls. Everything it "knows" about your conversation exists only because the client sends it along each time.

The array follows a strict alternating structure of `user` and `assistant` role messages:

```
messages = [
  { role: "user",      content: "Please refactor the auth module" },
  { role: "assistant", content: [text blocks, tool_use blocks] },
  { role: "user",      content: [tool_result blocks] },
  { role: "assistant", content: [text blocks, tool_use blocks] },
  ...
]
```

Each element's `content` field can be a plain string or an array of typed content blocks. These blocks include:

- **Text blocks**: The actual conversational text from you or the assistant.
- **Tool use blocks**: When the AI wants to read a file, run a command, or search your codebase, it emits a tool_use block with the tool name and parameters.
- **Tool result blocks**: After the tool executes, the output goes back into the array as a tool_result block in the next user message.
- **Thinking blocks**: When extended thinking is enabled, the AI's reasoning steps appear as thinking blocks. These are large but get automatically stripped from previous turns to save space.

There is also a **system prompt** that travels alongside the array (but is not part of it). This contains the AI's core instructions -- what tools it has, how it should behave, what safety rules to follow. In Claude Code, this system prompt is substantial.

The important thing to internalize: **this array is the AI's entire short-term memory**. If something is not in the array, the AI does not know about it. If the array gets too long, older content gets summarized or dropped. Every tool call, every file read, every command output -- it all goes into this array and consumes space.

### CLAUDE.md -- Persistent Instructions in the Context

AI coding tools support project-level instruction files that get loaded into the context at session start. In Claude Code, this file is called `CLAUDE.md` (other tools like Cursor use `AGENTS.md` or similar conventions, but the concept is the same).

When a Claude Code session starts, it reads `CLAUDE.md` files from multiple locations:

- **Project root**: `./CLAUDE.md` -- shared with your team via version control.
- **User-level**: `~/.claude/CLAUDE.md` -- your personal preferences across all projects.
- **Local overrides**: `./CLAUDE.local.md` -- personal, project-specific, not committed.
- **Auto memory**: `~/.claude/projects/<project>/memory/MEMORY.md` -- Claude's own notes from previous sessions.

These files are injected into the context as system reminders. They persist across the entire session and survive compaction (more on that below). This makes `CLAUDE.md` the right place for information that should never be forgotten: build commands, coding conventions, architectural decisions, test strategies.

But there is a trade-off. Everything in `CLAUDE.md` consumes context space on every API call. If you stuff it with 5,000 tokens of instructions, that is 5,000 tokens less for your actual conversation. Keep it concise. Put only universally relevant information there.

### Context Window Limits

Every LLM has a maximum context window size -- the upper bound on how large the array can be. Current Claude models offer:

| Model | Context Window | Max Output |
|-------|---------------|-----------|
| Claude Opus 4.6 | 200K tokens | 128K tokens |
| Claude Sonnet 4.5 | 200K tokens | 64K tokens |
| Claude Haiku 4.5 | 200K tokens | 64K tokens |

There is also a 1M token beta available for some models, but the default is 200K. That sounds like a lot, but it fills up faster than you might expect. Consider what goes into the array during a typical session:

- System prompt: ~10-15K tokens
- CLAUDE.md files: 1-5K tokens
- Each file you read: hundreds to thousands of tokens
- Each tool call and result: variable, but adds up quickly
- Each conversation turn: your message plus the AI's response
- Extended thinking: can be very large per turn (though stripped from previous turns)

A session where you read ten files, run a few commands, and have a back-and-forth discussion can easily consume 100K+ tokens. A complex refactoring session that touches many files can hit the limit within an hour.

### What Happens When You Run Out: Compaction

When the context array approaches the window limit, Claude Code triggers **auto-compaction**. This fires at roughly 83% of the context window (around 167K tokens for a 200K window). Here is what happens:

1. The system makes an additional API call asking the AI to summarize the entire conversation so far.
2. The summary replaces all previous messages in the array.
3. The conversation continues with just the summary as history.

This sounds reasonable in theory. In practice, compaction has significant downsides:

- **Information loss is inevitable.** A summary cannot preserve every detail. Specific variable names, exact error messages, nuanced decisions from early in the session -- these get compressed into approximations. The AI may "forget" constraints you established earlier.
- **It costs money.** The summarization step is an additional API call using the same model. You pay for it.
- **Timing is unpredictable.** Auto-compaction triggers based on token count, not logical session boundaries. It might fire right in the middle of a complex multi-file refactoring, losing context about what was already done and what remains.
- **Degradation can compound.** If important instructions are lost during compaction, the AI may start making mistakes. Those mistakes generate more context (error messages, corrections), which leads to more compaction, which loses more context. This is a downward spiral.

You can trigger compaction manually with `/compact` (and even guide it with `/compact focus on the API changes`), which gives you more control over what gets preserved. But the fundamental problem remains: once context is compacted, the original detail is gone.

### The Goal: Stay Within the Context Window

The single most effective strategy is simple: **do not let compaction happen**. If you can complete your task within the context window, you get the AI's full attention on everything that was said and done during the session. No summaries, no lost details, no degradation.

This means being deliberate about how you use context:

- **Do not dump entire files into the conversation if you only need a few functions.** Point the AI at specific line ranges.
- **Use `/context` to monitor usage.** Know where you stand before starting a large operation.
- **Be aware that MCP servers add tool definitions to every request.** A few MCP servers can consume significant context before you write a single line.
- **Break large tasks into phases** (see below).

A reasonable rule of thumb: if you estimate your task will consume more than 80% of the context window, restructure it into phases. If you are already at 95% and close to done, push through. Otherwise, plan for a clean context reset.

### Multi-Phase Development with State Files

For tasks too large for a single context window -- a major refactoring, a new feature spanning many files, a migration -- the most reliable approach is **multi-phase development with explicit state synchronization**.

The idea is straightforward:

1. **Break the task into phases** that each fit within a context window.
2. **Maintain a state file** that captures everything needed to continue from one phase to the next.
3. **Reset the context between phases** by starting a new session and having the AI read the state file.

The state file is the key. It serves as the synchronization mechanism between phases -- a handoff document that bridges the gap between one context and the next. A good state file contains:

```markdown
# Project State: Auth Module Migration

## Goal
Migrate from session-based auth to JWT tokens across the API.

## Completed (Phase 1)
- Created JWT utility module at src/auth/jwt.ts
- Updated User model with refresh token field
- Added token generation to login endpoint
- Tests passing for jwt.ts (14/14)

## In Progress (Phase 2)
- Replacing session checks in middleware (3 of 7 routes done)
- Routes completed: /api/users, /api/projects, /api/settings
- Routes remaining: /api/billing, /api/admin, /api/webhooks, /api/export

## Decisions Made
- Using RS256 algorithm (asymmetric) for token signing
- Access token TTL: 15 minutes
- Refresh token TTL: 7 days
- Storing refresh tokens in database, not Redis

## Known Issues
- /api/admin has custom middleware that needs special handling
- Rate limiter depends on session ID; needs new key strategy

## Next Steps
1. Continue middleware migration for remaining routes
2. Update rate limiter to use JWT subject claim
3. Add token refresh endpoint
```

When you start a new phase, the conversation is fresh. The AI reads the state file, understands where things stand, and picks up where the previous phase left off -- all without carrying the accumulated context of everything that happened before.

This approach has several advantages:

- **Each phase gets the full context window.** No compaction, no degradation.
- **The state file is human-readable.** You can review it, edit it, correct mistakes before the next phase.
- **It survives across sessions, machines, and even different AI tools.** It is just a markdown file.
- **It forces you to think about task decomposition.** This usually leads to better results regardless of the tooling.

You can ask the AI to create and update the state file as part of each phase: "Before we wrap up this phase, update the state file with what we accomplished and what comes next."

### Subagents: Separate Contexts for Parallel Work

Claude Code has another mechanism for managing context effectively: **subagents**. These are specialized AI instances that the main agent can delegate tasks to. The critical architectural detail is that each subagent runs in its **own, separate context window**.

When the main agent spawns a subagent, here is what happens:

1. A new AI instance is created with a fresh, empty context.
2. The subagent receives only a task description and its specialized system prompt -- not the main conversation history.
3. The subagent works independently: reading files, searching code, running commands, making multiple tool calls.
4. When done, the subagent returns a **concise summary** of its findings to the main agent.
5. Only that summary enters the main agent's context array.

This is important: the subagent's full work -- every file it read, every search it ran, every intermediate reasoning step -- stays in the subagent's own context. It does not "pollute" the main context. The main agent receives only the distilled result.

Claude Code includes several built-in subagent types:

- **Explore**: Fast codebase search and exploration (runs on a smaller, faster model).
- **Plan**: Research and design implementation approaches (read-only, no file modifications).
- **General-purpose**: Complex multi-step tasks with full tool access.
- **Bash**: Command execution in a separate context.

The main agent acts as an orchestrator. It decides when to delegate, what to delegate, and how to integrate the results. You can even run multiple subagents in parallel -- for example, having one search for all usages of a deprecated API while another reads the migration guide.

The practical benefit for context management is significant. Consider a task where you need to understand how authentication works across a large codebase. Without subagents, the main agent reads file after file, and each file's contents enter the main context. Twenty files later, you have consumed a huge chunk of your context window just on exploration.

With subagents, the main agent delegates: "Explore the codebase and explain how authentication works." The Explore subagent reads those twenty files in its own context, synthesizes the findings, and returns a two-paragraph summary. The main context gains those two paragraphs instead of twenty files' worth of content.

There are limitations. Subagents cannot spawn other subagents (no nesting). And if many subagents return detailed results, the summaries themselves still consume main context. But used strategically, subagents are one of the most effective tools for keeping the main context lean.

### Practical Tips

A few additional strategies worth mentioning:

- **Use CLAUDE.md for persistent context.** Anything that should survive across sessions -- build commands, conventions, architecture notes -- goes in CLAUDE.md. It is reloaded on every API call and survives compaction.
- **Manual compaction over auto-compaction.** If you must compact, do it manually at a logical boundary (`/compact`) rather than letting it trigger randomly. You can guide the summary: `/compact focus on the database migration progress`.
- **Git as a checkpoint mechanism.** Commit frequently during AI-assisted sessions. If context degrades after compaction, you can always start a fresh session and point the AI at the git log to understand what happened.
- **Monitor with `/context`.** This command shows you what is consuming space. Run it before starting a large operation.
- **Structured data survives compaction better than prose.** If you are tracking task lists or test results, use structured formats (markdown tables, JSON) rather than narrative descriptions.

### Conclusion

The context window is the fundamental constraint of AI-assisted development. Understanding it -- knowing that it is an array on the client, that the AI is stateless, that every interaction consumes space, that compaction loses information -- changes how you work with these tools.

The most effective developers I have seen treat context like a scarce resource. They plan their sessions, decompose large tasks into phases, use state files for synchronization, delegate exploration to subagents, and avoid hitting the compaction wall whenever possible.

The tools are powerful. But they are most powerful when you understand what is happening under the hood.
