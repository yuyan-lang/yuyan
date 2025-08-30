import * as vscode from 'vscode';
import * as fs from 'fs';
import * as path from 'path';
import { exec } from 'child_process';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

function getTokensInfo(document: vscode.TextDocument): void {
  let path = document.uri.path;
  let token_file_path = "./_build/lsp_tokens_info/" + path + ".tokens.json";
  if (fs.existsSync(token_file_path)) {
    let tokens = JSON.parse(fs.readFileSync(token_file_path, 'utf8'));
    return tokens;
  }
  return undefined;

}

function startLSP(): void {
  let language_selector = "*";
  vscode.languages.registerDocumentSemanticTokensProvider(language_selector, {
    provideDocumentSemanticTokens(
      document: vscode.TextDocument,
      token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.SemanticTokens> {
      return undefined;
    }
  }, new vscode.SemanticTokensLegend(
    ["StringConstant",
      "NumericConstant",
      "StructureKeyword",
      "ExpressionKeyword",
      "UserDefinedOperatorKeyword",
      "Identifier"]
  ));
  vscode.languages.registerCompletionItemProvider(language_selector, {
    provideCompletionItems(
      document: vscode.TextDocument,
      position: vscode.Position,
      token: vscode.CancellationToken,
      context: vscode.CompletionContext
    ): vscode.ProviderResult<vscode.CompletionItem[] | vscode.CompletionList> {
      return undefined;
    }
  }, '/');
  vscode.languages.registerHoverProvider(language_selector, {
    provideHover(
      document: vscode.TextDocument,
      position: vscode.Position,
      token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Hover> {
      return undefined;
    }
  });
  vscode.languages.registerDocumentSymbolProvider(language_selector, {
    async provideDocumentSymbols(
      document: vscode.TextDocument,
      token: vscode.CancellationToken
    ): Promise<vscode.SymbolInformation[] | vscode.DocumentSymbol[] | undefined> {
      return undefined;
    }
  });
  vscode.languages.registerDefinitionProvider(language_selector, {
    provideDefinition(
      document: vscode.TextDocument,
      position: vscode.Position,
      token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Definition | vscode.LocationLink[]> {
      return undefined;
    }
  });
}

export function activate(context: vscode.ExtensionContext): void {
  startLSP();
}

export function deactivate(): void {

}