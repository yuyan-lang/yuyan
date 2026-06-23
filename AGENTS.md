# Agent Instructions

- Do not run verification commands unless the user explicitly asks for them, except as described below.
- This includes compiler checks, tests, linting, formatting checks, build commands, smoke tests, and bootstrapping checks.
- Type-checking validation may be run for major refactors after manual review:
  - `./yy 豫言编译器/入口。豫 --type-check-only`
  - `./yy_bs_stable 豫言编译器/入口。豫 --type-check-only`
- Do not run type-checking validation for minor refactors unless the user asks for it.
- Full bootstrapping validation must only be run when the user explicitly asks for it. Use exactly this pipeline:
  - `./yy_bs_stable 豫言编译器/入口。豫 -o yy2_bs`
  - `./yy2_bs 豫言编译器/入口。豫 -o yy3_bs`
- The bootstrapping validation is considered successful if `yy3_bs` is produced, which means the compiler semantics did not change.
- To update cache identity for `yy_bs_stable` or `yy*_bs*`, touch the executable; the cache directory is based on the executable mtime.
- Ignore `*_v0` directories unless the user explicitly asks about them.
- Always provide answers in Chinese.
