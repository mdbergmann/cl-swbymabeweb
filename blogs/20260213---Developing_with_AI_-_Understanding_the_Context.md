### Intro

AI coding tools like Claude Code have become part of many developers' daily work. They can write code, run tests, search code, and handle complex tasks with many steps. But to use them well -- and to avoid surprises in the middle of a session -- you need to understand one key concept: **the context window**.

This post explains what the context is, how it works, why running out of it makes your results worse, and what you can do to stay in control.

### What Is the Context?

Here is the key idea: **the context is an array**. It is a list of message objects on the client side. This list gets sent to the LLM with every single API call. The LLM itself has no state. It has no memory between calls. Everything it "knows" about your conversation is only there because the client sends it each time.

The array follows a strict pattern where `user` and `assistant` messages take turns:

```
messages = [
  { role: "user",      content: "Please refactor the auth module" },
  { role: "assistant", content: [text blocks, tool_use blocks] },
  { role: "user",      content: [tool_result blocks] },
  { role: "assistant", content: [text blocks, tool_use blocks] },
  ...
]
```

The `content` field of each element can be a plain string or an array of typed content blocks. These blocks include:

- **Text blocks**: The actual text from you or the assistant.
- **Tool use blocks**: When the AI wants to read a file, run a command, or search your code, it creates a tool_use block with the tool name and its parameters.
- **Tool result blocks**: After the tool runs, its output goes back into the array as a tool_result block in the next user message.
- **Thinking blocks**: When extended thinking is turned on, the AI's reasoning steps show up as thinking blocks. These are large but get removed from older turns to save space.

There is also a **system prompt** that is sent along with the array but is not part of it. It holds the AI's main instructions -- what tools it has, how it should behave, what rules to follow. In Claude Code, this system prompt is quite large.

The key point to remember: **this array is the AI's entire short-term memory**. If something is not in the array, the AI does not know about it. If the array gets too long, older content gets shortened or removed. Every tool call, every file read, every command output -- it all goes into this array and takes up space.

### CLAUDE.md -- Instructions That Stay in the Context

AI coding tools support project-level instruction files that get loaded into the context when a session starts. In Claude Code, this file is called `CLAUDE.md`. Other tools like Cursor use `AGENTS.md` or similar names, but the idea is the same.

When a Claude Code session starts, it reads `CLAUDE.md` files from several places:

- **Project root**: `./CLAUDE.md` -- shared with your team through version control.
- **User-level**: `~/.claude/CLAUDE.md` -- your personal settings for all projects.
- **Local overrides**: `./CLAUDE.local.md` -- personal, project-specific, not committed.
- **Auto memory**: `~/.claude/projects/<project>/memory/MEMORY.md` -- notes that Claude saves from earlier sessions.

These files are added to the context as system reminders. They stay there for the whole session and survive compaction (more on that below). This makes `CLAUDE.md` the right place for things that should never be forgotten: build commands, coding rules, architecture decisions, test strategies.

But there is a trade-off. Everything in `CLAUDE.md` uses context space on every API call. If you put 5,000 tokens of instructions in it, that is 5,000 tokens less for your actual conversation. So, keep it short. Only put things there that are always needed.

### Context Window Limits

Every LLM has a maximum context window size -- the upper limit on how large the array can be. Current Claude models offer:

| Model | Context Window | Max Output |
|-------|---------------|-----------|
| Claude Opus 4.6 | 200K tokens | 128K tokens |
| Claude Sonnet 4.5 | 200K tokens | 64K tokens |
| Claude Haiku 4.5 | 200K tokens | 64K tokens |

There is also a 1M token beta for some models, but the default is 200K. That sounds like a lot, but it fills up faster than you might think. Let's look at what goes into the array during a typical session:

- System prompt: ~10-15K tokens
- CLAUDE.md files: 1-5K tokens
- Each file you read: hundreds to thousands of tokens
- Each tool call and result: different sizes, but it adds up fast
- Each conversation turn: your message plus the AI's answer
- Extended thinking: can be very large per turn (but gets removed from older turns)

A session where you read ten files, run a few commands, and have some back-and-forth can easily use 100K+ tokens. A complex session that touches many files can hit the limit within an hour.

### What Happens When You Run Out: Compaction

When the context array gets close to the window limit, Claude Code triggers **auto-compaction**. This happens at about 83% of the context window (around 167K tokens for a 200K window). Here is what happens:

1. The system makes an extra API call asking the AI to summarize the whole conversation so far.
2. The summary replaces all previous messages in the array.
3. The conversation continues with just the summary as history.

This sounds fine in theory. In practice, compaction has real problems:

**You will lose information.** A summary cannot keep every detail. Specific variable names, exact error messages, careful decisions from earlier in the session -- these get shortened into approximations. The AI may "forget" things you decided earlier.

**It costs money.** The summary step is an extra API call using the same model. You pay for it.

**The timing is hard to predict.** Auto-compaction triggers based on token count, not at a good moment in your work. It might happen right in the middle of a complex change across many files, losing track of what was already done and what still needs doing.

**Problems can get worse over time.** If important instructions get lost during compaction, the AI may start making mistakes. Those mistakes create more context (error messages, corrections), which leads to more compaction, which loses more context. This is a downward spiral.

You can trigger compaction manually with `/compact` (and even guide it with `/compact focus on the API changes`). This gives you more control over what gets kept. But the basic problem stays: once context is compacted, the original details are gone.

### The Goal: Stay Within the Context Window

The best strategy is simple: **do not let compaction happen**. If you can finish your task within the context window, the AI has full access to everything that was said and done during the session. No summaries, no lost details, no degradation over time.

This means being careful about how you use context:

- Do not load whole files into the conversation if you only need a few functions. Point the AI at specific line ranges.
- Use `/context` to check your usage. Know where you stand before starting a big task.
- Be aware that MCP servers add tool definitions to every request. A few MCP servers can use a lot of context before you even write a single line.
- Break large tasks into phases (see below).

A good rule of thumb: if you think your task will use more than 80% of the context window, split it into phases. If you are already at 95% and almost done, push through. Otherwise, plan for a clean context reset.

### Multi-Phase Development with State Files

For tasks too large for a single context window -- a big refactoring, a new feature across many files, a migration -- I find the best approach is **multi-phase development with state files**.

The idea is straightforward:

1. **Break the task into phases** that each fit within a context window.
2. **Keep a state file** that holds everything needed to continue from one phase to the next.
3. **Reset the context between phases** by starting a new session and having the AI read the state file.

The state file is the key. It works as a handoff document that connects one context to the next. A good state file looks something like this:

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

When you start a new phase, the conversation is fresh. The AI reads the state file, sees where things stand, and picks up where the last phase stopped -- without carrying the weight of everything that happened before.

This approach has several nice properties:

- **Each phase gets the full context window.** No compaction, no degradation.
- **The state file is easy to read.** You can check it, edit it, and fix mistakes before the next phase.
- **It works across sessions, machines, and even different AI tools.** It is just a markdown file.
- **It forces you to think about how to split tasks.** This usually leads to better results regardless of which tools you use.

You can ask the AI to create and update the state file as part of each phase: "Before we finish this phase, update the state file with what we did and what comes next."

### Subagents: Separate Contexts for Parallel Work

Claude Code has another way to manage context well: **subagents**. These are separate AI instances that the main agent can give tasks to. The important thing is that each subagent runs in its **own, separate context window**.

When the main agent starts a subagent, here is what happens:

1. A new AI instance is created with a fresh, empty context.
2. The subagent only gets a task description and its own system prompt -- not the main conversation history.
3. The subagent works on its own: reading files, searching code, running commands, making many tool calls.
4. When done, the subagent sends back a **short summary** of what it found to the main agent.
5. Only that summary goes into the main agent's context array.

This is important: all the work the subagent did -- every file it read, every search it ran, every step of reasoning -- stays in the subagent's own context. It does not fill up the main context. The main agent only gets the final result.

Claude Code has several built-in subagent types:

- **Explore**: Fast code search (runs on a smaller, faster model).
- **Plan**: Research and design approaches (read-only, no file changes).
- **General-purpose**: Complex tasks with many steps and full tool access.
- **Bash**: Command execution in a separate context.

The main agent works as a coordinator. It decides when to hand off work, what to hand off, and how to use the results. You can even run several subagents at the same time -- for example, one searches for all uses of an old API while another reads the migration guide.

The practical benefit for context management is quite significant. Think of a task where you need to understand how authentication works across a large codebase. Without subagents, the main agent reads file after file, and each file goes into the main context. Twenty files later, you have used a huge part of your context window just for exploration.

With subagents, the main agent just hands off the work: "Explore the codebase and explain how authentication works." The Explore subagent reads those twenty files in its own context, puts the findings together, and sends back a two-paragraph summary. The main context gets those two paragraphs instead of twenty files worth of content. Pretty cool.

There are limits. Subagents cannot start other subagents (no nesting). And if many subagents return detailed results, those summaries still use main context space. But when used wisely, subagents are one of the best tools for keeping the main context lean.

### Practical Tips

A few more strategies worth knowing:

**Use CLAUDE.md for lasting context.** Anything that should survive across sessions -- build commands, rules, architecture notes -- goes in CLAUDE.md. It is reloaded on every API call and survives compaction.

**Manual compaction is better than auto-compaction.** If you must compact, do it manually at a good stopping point (`/compact`) instead of letting it trigger at random. You can guide the summary: `/compact focus on the database migration progress`.

**Use git as a checkpoint.** Commit often during AI-assisted sessions. If context gets worse after compaction, you can always start a fresh session and point the AI at the git log to understand what happened.

**Check usage with `/context`.** This command shows you what is using space. Run it before starting a big task.

**Structured data survives compaction better than prose.** If you are tracking task lists or test results, use structured formats (markdown tables, JSON) instead of long descriptions.

### Conclusion

The context window is the basic constraint of AI-assisted development. Understanding it -- knowing that it is an array on the client, that the AI has no state, that every interaction uses space, that compaction loses information -- changes how you work with these tools.

The most effective developers I have seen treat context like a limited resource. They plan their sessions, split large tasks into phases, use state files to pass information between phases, hand off exploration to subagents, and try to avoid hitting the compaction wall.

The tools are powerful. But they work best when you understand what is happening behind the scenes.
