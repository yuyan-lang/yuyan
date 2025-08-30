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

interface TokenExtent {
  file: string;
  start_line: number;
  start_col: number;
  end_line: number;
  end_col: number;
}

interface TokenDetail {
  type: string;
  semantic_token_type: string;
}

interface TokenInfo {
  extent: TokenExtent;
  detail: TokenDetail;
}

function getTokensInfo(document: vscode.TextDocument): any[] | undefined {
  const docPath = document.uri.path;
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
  
  if (!workspaceFolder) {
    return undefined;
  }
  
  const tokenFilePath = path.join(workspaceFolder.uri.fsPath, '_build', 'lsp_tokens_info', `${docPath}.tokens.json`);
  
  if (fs.existsSync(tokenFilePath)) {
    try {
      const tokens = JSON.parse(fs.readFileSync(tokenFilePath, 'utf8'));
      return tokens;
    } catch (error) {
      console.error('Error parsing token file:', error);
      return undefined;
    }
  }
  return undefined;
}

function provideSemanticTokens(
  document: vscode.TextDocument,
  legend: vscode.SemanticTokensLegend
): vscode.SemanticTokens | undefined {
  const allTokens = getTokensInfo(document);
  
  if (!allTokens) {
    return undefined;
  }
  
  // Filter for semantic tokens only
  const semanticTokens = allTokens.filter(t => 
    t.detail && t.detail.type === "SemanticToken" && t.detail.semantic_token_type
  );
  
  // Sort tokens by position (line, then column)
  semanticTokens.sort((a, b) => {
    if (a.extent.start_line !== b.extent.start_line) {
      return a.extent.start_line - b.extent.start_line;
    }
    return a.extent.start_col - b.extent.start_col;
  });
  
  const builder = new vscode.SemanticTokensBuilder(legend);
  const tokenTypes = legend.tokenTypes;
  
  for (const tokenInfo of semanticTokens) {
    const tokenType = tokenInfo.detail.semantic_token_type;
    const tokenTypeIndex = tokenTypes.indexOf(tokenType);
    
    if (tokenTypeIndex === -1) {
      console.warn(`Unknown token type: ${tokenType}`);
      continue;
    }
    
    // VSCode uses 0-based indexing, token file uses 1-based
    const line = tokenInfo.extent.start_line - 1;
    const startChar = tokenInfo.extent.start_col - 1;
    const endChar = tokenInfo.extent.end_col - 1;
    const length = endChar - startChar;
    
    if (line >= 0 && startChar >= 0 && length > 0) {
      builder.push(line, startChar, length, tokenTypeIndex);
    }
  }
  
  return builder.build();
}

function startLSP(): void {
  let language_selector = "*";
  
  const tokenTypes = [
    "StringConstant",
    "NumericConstant",
    "StructureKeyword",
    "ExpressionKeyword",
    "UserDefinedOperatorKeyword",
    "Identifier"
  ];
  
  const legend = new vscode.SemanticTokensLegend(tokenTypes);
  
  vscode.languages.registerDocumentSemanticTokensProvider(language_selector, {
    provideDocumentSemanticTokens(
      document: vscode.TextDocument,
      token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.SemanticTokens> {
      return provideSemanticTokens(document, legend);
    }
  }, legend);
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