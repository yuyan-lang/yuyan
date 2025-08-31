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
  semantic_token_type?: string;
  content?: string; // For Hover
  extent?: TokenExtent; // For Definition
}

interface TokenInfo {
  text: string;
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
    
    // Both VSCode and token file use 0-based indexing
    const startLine = tokenInfo.extent.start_line;
    const startChar = tokenInfo.extent.start_col;
    const endLine = tokenInfo.extent.end_line;
    const endChar = tokenInfo.extent.end_col;
    
    // Handle multi-line tokens
    if (startLine === endLine) {
      // Single line token
      const length = endChar - startChar;
      if (startLine >= 0 && startChar >= 0 && length > 0) {
        builder.push(startLine, startChar, length, tokenTypeIndex);
      }
    } else {
      // Multi-line token - need to get actual line content to calculate lengths
      const lines = document.getText().split('\n');
      
      // First line: from startChar to end of line
      if (startLine < lines.length) {
        const firstLineLength = lines[startLine].length - startChar;
        if (firstLineLength > 0) {
          builder.push(startLine, startChar, firstLineLength, tokenTypeIndex);
        }
      }
      
      // Middle lines: entire lines
      for (let line = startLine + 1; line < endLine && line < lines.length; line++) {
        const lineLength = lines[line].length;
        if (lineLength > 0) {
          builder.push(line, 0, lineLength, tokenTypeIndex);
        }
      }
      
      // Last line: from start of line to endChar
      if (endLine < lines.length && endChar > 0) {
        builder.push(endLine, 0, endChar, tokenTypeIndex);
      }
    }
  }
  
  return builder.build();
}

function provideHover(
  document: vscode.TextDocument,
  position: vscode.Position
): vscode.Hover | undefined {
  const allTokens = getTokensInfo(document);
  
  if (!allTokens) {
    return undefined;
  }
  
  // Filter for hover tokens at the current position
  const hoverTokens = allTokens.filter(t => {
    if (t.detail.type !== "Hover") return false;
    
    const startLine = t.extent.start_line;
    const startCol = t.extent.start_col;
    const endLine = t.extent.end_line;
    const endCol = t.extent.end_col;
    
    // Check if position is within token range
    if (position.line < startLine || position.line > endLine) return false;
    
    if (position.line === startLine && position.line === endLine) {
      // Single line token
      return position.character >= startCol && position.character < endCol;
    } else if (position.line === startLine) {
      // First line of multi-line token
      return position.character >= startCol;
    } else if (position.line === endLine) {
      // Last line of multi-line token
      return position.character < endCol;
    } else {
      // Middle line of multi-line token
      return true;
    }
  });
  
  if (hoverTokens.length === 0) {
    return undefined;
  }
  
  // Use the first hover token found
  const hoverToken = hoverTokens[0];
  const content = hoverToken.detail.content || "";
  
  const range = new vscode.Range(
    new vscode.Position(hoverToken.extent.start_line, hoverToken.extent.start_col),
    new vscode.Position(hoverToken.extent.end_line, hoverToken.extent.end_col)
  );
  
  return new vscode.Hover(content, range);
}

function provideDefinition(
  document: vscode.TextDocument,
  position: vscode.Position
): vscode.Location | undefined {
  const allTokens = getTokensInfo(document);
  
  if (!allTokens) {
    return undefined;
  }
  
  // Filter for definition tokens at the current position
  const definitionTokens = allTokens.filter(t => {
    if (t.detail.type !== "Definition") return false;
    
    const startLine = t.extent.start_line;
    const startCol = t.extent.start_col;
    const endLine = t.extent.end_line;
    const endCol = t.extent.end_col;
    
    // Check if position is within token range
    if (position.line < startLine || position.line > endLine) return false;
    
    if (position.line === startLine && position.line === endLine) {
      // Single line token
      return position.character >= startCol && position.character < endCol;
    } else if (position.line === startLine) {
      // First line of multi-line token
      return position.character >= startCol;
    } else if (position.line === endLine) {
      // Last line of multi-line token
      return position.character < endCol;
    } else {
      // Middle line of multi-line token
      return true;
    }
  });
  
  if (definitionTokens.length === 0) {
    return undefined;
  }
  
  // Use the first definition token found
  const defToken = definitionTokens[0];
  const defExtent = defToken.detail.extent;
  
  if (!defExtent) {
    return undefined;
  }
  
  const targetUri = vscode.Uri.file(defExtent.file);
  const targetRange = new vscode.Range(
    new vscode.Position(defExtent.start_line, defExtent.start_col),
    new vscode.Position(defExtent.end_line, defExtent.end_col)
  );
  
  return new vscode.Location(targetUri, targetRange);
}

function startLSP(): void {
  let language_selector = "*";
  
  const tokenTypes = [
    "StringConstant",
    "NumericConstant",
    "StructureKeyword",
    "ExpressionKeyword",
    "UserDefinedOperatorKeyword",
    "Identifier",
    "Comment"
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
      return provideHover(document, position);
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
      return provideDefinition(document, position);
    }
  });
}

export function activate(context: vscode.ExtensionContext): void {
  startLSP();
}

export function deactivate(): void {

}