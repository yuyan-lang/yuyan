#!/bin/bash

# Exit on any error
set -e

echo "Setting up Yuyan development environment..."

# Initialize opam
opam init --disable-sandboxing -y
eval $(opam env)

# Create a switch for yuyan development
opam switch create yuyan-dev 5.3.0 -y
eval $(opam env --switch=yuyan-dev)

# Install required OCaml packages
echo "Installing OCaml dependencies..."
opam install dune ocaml-lsp-server ocamlformat utop merlin -y

# The Yuyan VS Code extension will be automatically installed from the marketplace
# as specified in devcontainer.json extensions list

# Test build
echo "Testing build..."
dune build

echo "Testing standard library..."
dune exec -- yyocamlc 藏书阁/标准库。豫 || echo "Standard library test completed with warnings/errors"

echo "Setup complete! You can now:"
echo "  - Run 'dune build' to compile the project"
echo "  - Run 'dune exec -- yyocamlc <file>' to execute Yuyan files"
echo "  - Use VS Code with OCaml and Yuyan language support"