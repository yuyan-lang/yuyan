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
from the command palette. The extension runs:

```text
豫构 产物 <source file>
```

豫构 enables the package context of the source file, resolves every stage artifact
through the compiler core's cache-read path (`讯储读径`), and prints one line of
结绳 (JSON): the source file, the package-context file, and one entry per stage with
its name, actual path, and whether it exists. The picker lists the stages that exist,
in pipeline order, and calls the Yuyan Wasm tool `树码 显示` with the package-context
file that 豫构 supplied to decode the selected tree. The result opens as a read-only
Yuyan preview. Build or type-check the package first; the command only lists artifacts
that have already been written.

## Hover and Jump to Definition

The compiler writes language-service data as a tree artifact. The extension asks
the build tool for it instead of composing cache paths itself:

```text
豫构 语言服务 <source file>
```

The command uses the compiler core's cache-read path, reads the artifact bytes
from the shared cache (or from the local cache when the bytes stay in the tree),
decodes them with the editor tree reader, and prints the language-service
document as one line of 结绳 (JSON). The extension parses that document and
feeds it to VS Code's hover and definition APIs. The metadata protocol uses
Chinese field names and Chinese kind values throughout.

The setting `yuyan.buildToolCommand` names the 豫构 program. When it is empty the
extension looks for `豫构。<system>。豫行` at the workspace root and otherwise
runs `豫构` from `PATH`.

See [中文说明](说明.汉语.md) and [文言说明](说明.文言.md).

## Icon Attribution

Icon design inspired by the Chinese character 豫 (yu): https://www.zdic.net/hans/豫

## License

See [LICENSE](LICENSE) file for details.
