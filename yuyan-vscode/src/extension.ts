import * as vscode from 'vscode';
import * as path from 'path';
import { exec } from 'child_process';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

function getTokensInfo(document: vscode.TextDocument): void {
  
}

function startLSP(): void {
  vscode.languages.registerCompletionItemProvider('istari', {
    provideCompletionItems(
      document: vscode.TextDocument,
      position: vscode.Position,
      token: vscode.CancellationToken,
      context: vscode.CompletionContext
    ): vscode.ProviderResult<vscode.CompletionItem[] | vscode.CompletionList> {
      return undefined;
    }
  }, '/');

  vscode.languages.registerHoverProvider('istari', {
    provideHover(
      document: vscode.TextDocument,
      position: vscode.Position,
      token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Hover> {
      return undefined;
    }
  });

  vscode.languages.registerDocumentSymbolProvider('istari', {
    async provideDocumentSymbols(
      document: vscode.TextDocument,
      token: vscode.CancellationToken
    ): Promise<vscode.SymbolInformation[] | vscode.DocumentSymbol[] | undefined> {
      return undefined;
    }
  });

  vscode.languages.registerDefinitionProvider('istari', {
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