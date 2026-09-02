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

## Hover and Jump to Definition

The compiler writes `<source stem>.语言服务.json` alongside the other artifacts
under `.yybuild`. The extension reads the newest matching artifact when VS Code
requests hover help or a definition location. The metadata protocol uses Chinese
field names and Chinese kind values throughout; the removed `_build/lsp_tokens_info`
protocol is not used.

## Icon Attribution

Icon design inspired by the Chinese character 豫 (yu): https://www.zdic.net/hans/豫

## License

See [LICENSE](./LICENSE) file for details.
