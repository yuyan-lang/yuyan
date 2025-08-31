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

echo "Running matrix multiplication demo..."
dune exec -- yyocamlc 矩阵乘法。豫 || echo "Matrix multiplication demo completed with warnings/errors"

echo "Setup complete! You can now:"
echo "  - Run 'dune build' to compile the project"
echo "  - Run 'dune exec -- yyocamlc 矩阵乘法。豫' to run the matrix multiplication demo"
echo "  - Run 'dune exec -- yyocamlc <file>' to execute other Yuyan files"
echo "  - Use VS Code with OCaml and Yuyan language support"