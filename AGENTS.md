# Agent Instructions

- Run relevant validation by default after making code changes, unless the user explicitly asks not to run validation.
- Prefer the cheapest validation that gives useful confidence for the change.
- Full bootstrapping validation should be run for compiler changes that may affect generated compiler semantics, unless the user explicitly asks not to run validation. Use exactly this pipeline:
  - `./yy_bs_stable 豫言编译器/入口。豫 -o yy2_bs --parallel >/dev/null 2>/dev/null`
  - `./yy2_bs 豫言编译器/入口。豫 -o yy3_bs --parallel >/dev/null 2>/dev/null`
  - `./yy3_bs 豫言编译器/入口。豫 -o yy4_bs --parallel >/dev/null 2>/dev/null`
  - `yy3_bs` and `yy4_bs` should be identical.
- Type-checking validation should also be run after manual review:
  - `./yy 豫言编译器/入口。豫 --type-check-only`
  - The fastest type-checking validation is `./yy_bs_stable 豫言编译器/入口。豫 --type-check-only --parallel >/dev/null 2>/dev/null`.
  - If it fails, rerun it without `--parallel` and without redirecting stdout/stderr to see the diagnostic.
- In Codex, run `--parallel` validation outside the sandbox / with escalated permissions. The parallel runner uses Python `multiprocessing.Manager`, which binds a local socket; the Codex sandbox may reject that bind with `PermissionError: [Errno 1] Operation not permitted`, even when the compiler itself is fine.
- The bootstrapping validation is considered successful if `yy3_bs` is produced, which means the compiler semantics did not change.
- If a bootstrapping validation command fails, rerun the failed command without `--parallel` and without redirecting stdout/stderr to extract the error message. The cache is shared, so the diagnostic rerun can reuse work from the parallel run.
- To update cache identity for `yy_bs_stable` or `yy*_bs*`, touch the executable; the cache directory is based on the executable mtime.
- Ignore `*_v0` directories unless the user explicitly asks about them.
- Always provide answers in Chinese.


# 使用中文

除了`yuyan-vscode`之外，所有的文档代码文件名均应使用中文。


## `sync [branch_name] with yybs`

当用户调用 `sync [branch_name] with yybs` 时，同步指定分支；若省略 `[branch_name]`，则使用当前分支，并先确认该分支及 `yybs` 的 worktree 都是干净状态。
先在目标分支的 worktree 执行 `git rebase yybs`，解决所有冲突并完成 rebase，再运行相关验证。
然后在 `yybs` 的 worktree 执行 `git merge --no-ff --no-edit <目标分支>`；除非用户明确要求，否则不要 fetch、push、删除分支或移除 worktree。
