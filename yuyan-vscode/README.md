# Yuyan VS Code Extension

[![VS Code Marketplace](https://img.shields.io/visual-studio-marketplace/v/yuyan-lang.yuyan-vscode.svg)](https://marketplace.visualstudio.com/items?itemName=yuyan-lang.yuyan-vscode)
[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/yuyan-lang/yuyan-vscode)

豫言 (Yuyan) is a Chinese programming language. This VS Code extension provides comprehensive language support for Yuyan, including syntax highlighting, language server integration, and debugging tools.

## Features

- **Syntax Highlighting**: Full syntax highlighting for Yuyan code with semantic token support
- **Language Server**: Integrated language server for intelligent code analysis
- **File Extensions**: Supports `.yuyan`, `。豫`, and `.yyon` files
- **Chinese Brackets**: Automatic pairing and closing of Chinese brackets (`「」`, `（）`, `『』`)
- **Comments**: Support for Yuyan comment syntax using `「：` and `：」`
- **Custom Commands**: Built-in commands for language server management and debugging

## Installation

1. Install from VS Code Marketplace (search for "yuyan-vscode")
2. Or install the `.vsix` file directly in VS Code


## Commands

The extension provides the following commands:

- **Yuyan: Restart Yuyan Language Server** (`yuyan.restartyuyanlsp`)
  - 重启豫言的语言服务器
  - Restarts the Yuyan language server if it becomes unresponsive

- **Yuyan: Debug Show Tree** (`yuyan.debugshowtree`)
  - 调试显式语法树
  - Shows the syntax tree for debugging purposes

- **Yuyan: Debug Show Trees** (`yuyan.debugshowtrees`)
  - 调试显式语法树组
  - Shows multiple syntax trees for debugging purposes

## Supported File Extensions

- `.yuyan` - Standard Yuyan source files
- `。豫` - Chinese extension for Yuyan files
- `.yyon` - Alternative Yuyan file extension

## Language Features

### Bracket Pairs
The extension automatically handles Chinese bracket pairs:
- `（` and `）` - Chinese parentheses
- `「` and `」` - Chinese quotation marks
- `『` and `』` - Chinese double quotation marks

### Comments
Use `「：` to start and `：」` to end block comments in Yuyan code.

### Syntax Highlighting
The extension provides semantic highlighting for:
- String constants
- Numeric constants
- Structure keywords
- Expression keywords
- User-defined operator keywords
- Identifiers
- Comments

## Development

To contribute to this extension:

1. Clone the repository
2. Run `npm install` to install dependencies
3. Run `npm run compile` to compile TypeScript
4. Press `F5` in VS Code to run the extension in development mode

### Scripts

- `npm run compile` - Compile TypeScript
- `npm run watch` - Watch for changes and compile
- `npm run lint` - Run ESLint
- `npm run test` - Run tests

## Requirements

- VS Code version 1.73.1 or higher
- Yuyan language implementation for full language server functionality

## Repository

- GitHub: [https://github.com/yuyan-lang/yuyan-vscode](https://github.com/yuyan-lang/yuyan-vscode)
- Publisher: `yuyan-lang`

## Icon Attribution

Icon design inspired by the Chinese character 豫 (yu): https://www.zdic.net/hans/豫

## License

See [LICENSE](./LICENSE) file for details.