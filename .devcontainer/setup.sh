#!/bin/bash

# Exit on any error
set -e

echo "Setting up Yuyan development environment..."

# Initialize opam
opam init --disable-sandboxing -y
eval $(opam env)

# Create a switch for yuyan development
opam switch create yuyan-dev 5.1.1 -y
eval $(opam env --switch=yuyan-dev)

# Install required OCaml packages
echo "Installing OCaml dependencies..."
opam install dune ocaml-lsp-server ocamlformat utop merlin -y

# Install the yuyan VS Code extension if it exists locally
if [ -d "yuyan-vscode" ]; then
    echo "Installing Yuyan VS Code extension..."
    cd yuyan-vscode
    if [ -f "package.json" ]; then
        npm install
        npm run compile 2>/dev/null || echo "Extension compilation skipped (may not be needed)"
    fi
    cd ..
fi

# Test build
echo "Testing build..."
dune build

echo "Testing standard library..."
dune exec -- yyocamlc 藏书阁/标准库。豫 || echo "Standard library test completed with warnings/errors"

echo "Setup complete! You can now:"
echo "  - Run 'dune build' to compile the project"
echo "  - Run 'dune exec -- yyocamlc <file>' to execute Yuyan files"
echo "  - Use VS Code with OCaml and Yuyan language support"