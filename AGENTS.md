# Agent Instructions

- Run relevant validation by default after making code changes, unless the user explicitly asks not to run validation.
- Prefer the cheapest validation that gives useful confidence for the change.
- Type-checking validation may be run after manual review:
  - `./yy 豫言编译器/入口。豫 --type-check-only`
  - `./yy_bs_stable 豫言编译器/入口。豫 --type-check-only`
- Full bootstrapping validation should be run for compiler changes that may affect generated compiler semantics, unless the user explicitly asks not to run validation. Use exactly this pipeline:
  - `./yy_bs_stable 豫言编译器/入口。豫 -o yy2_bs --parallel >/dev/null 2>/dev/null`
  - `./yy2_bs 豫言编译器/入口。豫 -o yy3_bs --parallel >/dev/null 2>/dev/null`
- In Codex, run `--parallel` validation outside the sandbox / with escalated permissions. The parallel runner uses Python `multiprocessing.Manager`, which binds a local socket; the Codex sandbox may reject that bind with `PermissionError: [Errno 1] Operation not permitted`, even when the compiler itself is fine.
- The bootstrapping validation is considered successful if `yy3_bs` is produced, which means the compiler semantics did not change.
- If a bootstrapping validation command fails, rerun the failed command without `--parallel` and without redirecting stdout/stderr to extract the error message. The cache is shared, so the diagnostic rerun can reuse work from the parallel run.
- To update cache identity for `yy_bs_stable` or `yy*_bs*`, touch the executable; the cache directory is based on the executable mtime.
- Ignore `*_v0` directories unless the user explicitly asks about them.
- Always provide answers in Chinese.
