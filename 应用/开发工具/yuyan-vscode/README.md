# Yuyan VS Code Extension

## Repository

- GitHub: [https://github.com/yuyan-lang/yuyan-vscode](https://github.com/yuyan-lang/yuyan-vscode)
- Publisher: `yuyan-lang`

## Syntax Highlighting

The extension uses the TextMate grammar in `yuyan.tmGrammar.json` for strings,
numbers, builtins, identifiers, fixed operators, punctuation, and nested block
comments. Highlighting does not require compiler-generated semantic token files.

The compiler ignores spaces and line breaks, so the grammar does not depend on
them either: statements are delimited by `。`, declarations are recognized only
at the start of a statement, and field lists (`「典」`, `「组类」`, `「组值」`),
parentheses, and brackets are tracked as nested scopes. `test/删空白核验.mjs`
checks that deleting spaces and newlines does not change the color of any other
character; `npm run test:grammar` runs it on a multi-line sample. Legacy bare
identifiers (names written without `「」`) are the exception: keywords inside a
bare identifier stay uncolored, so joining a bare identifier and a keyword
changes the keyword's color.

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

See [LICENSE](LICENSE) file for details.
