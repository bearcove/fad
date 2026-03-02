# AGENTS.md

## Test policy
- Always run tests after making code changes.
- Use `cargo nextest run` for Rust tests.
- Run at least the tests directly covering the changed code before reporting completion.
- If a user asks for broader validation, run the full requested test set.
- Report the exact test command(s) and pass/fail result in your handoff.
