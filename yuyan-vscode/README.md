# Yuyan VS Code Extension

## Repository

- GitHub: [https://github.com/yuyan-lang/yuyan-vscode](https://github.com/yuyan-lang/yuyan-vscode)
- Publisher: `yuyan-lang`

## Syntax Highlighting

The extension uses the TextMate grammar in `yuyan.tmGrammar.json` for strings,
numbers, builtins, identifiers, fixed operators, punctuation, and nested block
comments. Highlighting does not require compiler-generated semantic token files.

## Inspecting Build Artifacts

With a Yuyan source file active, run **Yuyan: Jump to Build Artifact 跳转到构建产物**
from the command palette. The picker finds matching JSON trees under `.yybuild`,
puts the newest cache first, and asks `yy_bs_stable debug showtrees` to decode and
pretty-print the selected compiler tree. The result opens as a read-only Yuyan
preview that can be closed without saving; the raw JSON is not shown.

## Icon Attribution

Icon design inspired by the Chinese character 豫 (yu): https://www.zdic.net/hans/豫

## License

See [LICENSE](./LICENSE) file for details.
