# Agent Instructions

- Do not run verification commands unless the user explicitly asks for them.
- This includes compiler checks, tests, linting, formatting checks, build commands, and smoke tests.
- Exception: validation may be run with `./yy 豫言编译器/入口。豫 --type-check-only` and `./yy_bs_stable 豫言编译器/入口。豫 --type-check-only`.
- Run these validation commands only after manual review, or otherwise sparsely, because each command takes more than 2 minutes to complete.
- Ignore `*_v0` directories unless the user explicitly asks about them.
- Always provide answers in Chinese.
