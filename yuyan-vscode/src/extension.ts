import * as vscode from 'vscode';
import * as child_process from 'child_process';
import * as path from 'path';
import {
  jsonArtifactStage,
  sortBuildCachesNewestFirst,
  sourceRelativePathToArtifactStem
} from './buildArtifacts';

const BUILD_ARTIFACT_SCHEME = 'yuyan-build-artifact';
const JUMP_TO_BUILD_ARTIFACT_COMMAND = 'yuyan.jumpToBuildArtifact';

// Create output channel for logging
const outputChannel = vscode.window.createOutputChannel('Yuyan Language Extension');

// Create diagnostic collection for compiler errors/warnings
const diagnosticCollection = vscode.languages.createDiagnosticCollection('yuyan');

interface BuildArtifactQuickPickItem extends vscode.QuickPickItem {
  artifactUri: vscode.Uri;
}

class BuildArtifactContentProvider implements vscode.TextDocumentContentProvider {
  private readonly contents = new Map<string, string>();
  private nextDocumentId = 0;

  provideTextDocumentContent(uri: vscode.Uri): string | undefined {
    return this.contents.get(uri.toString());
  }

  createDocumentUri(fileName: string, content: string): vscode.Uri {
    this.nextDocumentId += 1;
    const uri = vscode.Uri.from({
      scheme: BUILD_ARTIFACT_SCHEME,
      path: `/${fileName}`,
      query: `document=${this.nextDocumentId}`
    });
    this.contents.set(uri.toString(), content);
    return uri;
  }

  forgetDocument(uri: vscode.Uri): void {
    this.contents.delete(uri.toString());
  }
}

function log(message: string, ...args: any[]): void {
  const timestamp = new Date().toISOString();
  const formattedMessage = `[${timestamp}] ${message}`;

  if (args.length > 0) {
    outputChannel.appendLine(formattedMessage + ' ' + JSON.stringify(args));
  } else {
    outputChannel.appendLine(formattedMessage);
  }
}

interface TokenExtent {
  file: string;
  start_line: number;
  start_col: number;
  end_line: number;
  end_col: number;
}

interface TokenDetail {
  type: string;
  content?: string; // For Hover
  extent?: TokenExtent; // For Definition
}

interface TokenInfo {
  text: string;
  extent: TokenExtent;
  detail: TokenDetail;
}

type DiagnosticInfo = TokenInfo;  // Reuse the same interface for diagnostics

async function getTokensInfo(document: vscode.TextDocument): Promise<any[] | undefined> {
  const docPath = document.uri.path;
  const docUri = document.uri.toString();
  log(`getTokensInfo called for document path: ${docPath}`);
  log(`Document URI: ${docUri}`);
  log(`Document scheme: ${document.uri.scheme}`);

  // Log all workspace folders for debugging
  const allWorkspaceFolders = vscode.workspace.workspaceFolders;
  if (allWorkspaceFolders) {
    log(`Total workspace folders: ${allWorkspaceFolders.length}`);
    allWorkspaceFolders.forEach((folder, index) => {
      log(`Workspace folder ${index}: ${folder.uri.fsPath}`);
    });
  } else {
    log('No workspace folders open');
  }

  let workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);

  // Fallback: If no workspace folder found, try to find one that contains the file
  if (!workspaceFolder && allWorkspaceFolders) {
    log('Attempting fallback workspace folder detection');
    for (const folder of allWorkspaceFolders) {
      if (docPath.startsWith(folder.uri.fsPath)) {
        workspaceFolder = folder;
        log(`Fallback found workspace folder: ${folder.uri.fsPath}`);
        break;
      }
    }
  }

  // Second fallback: Use the first workspace folder if file is outside all workspaces
  if (!workspaceFolder && allWorkspaceFolders && allWorkspaceFolders.length > 0) {
    workspaceFolder = allWorkspaceFolders[0];
    log(`Using first workspace folder as fallback: ${workspaceFolder.uri.fsPath}`);
  }

  if (!workspaceFolder) {
    log('No workspace folder found for document after all attempts');
    return undefined;
  }

  log(`Using workspace folder: ${workspaceFolder.uri.fsPath}`);
  const tokenFileUri = vscode.Uri.joinPath(workspaceFolder.uri, '_build', 'lsp_tokens_info', `${docPath}.tokens.json`);
  log(`Looking for token file at: ${tokenFileUri.fsPath}`);

  try {
    const tokenFileData = await vscode.workspace.fs.readFile(tokenFileUri);
    const tokenFileContent = new TextDecoder().decode(tokenFileData);
    const tokens = JSON.parse(tokenFileContent);
    log(`Successfully loaded ${tokens.length} tokens from file`);
    return tokens;
  } catch (error: any) {
    log(`Failed to load token file: ${error.message || error}`);
    return undefined;
  }
}

async function provideHover(
  document: vscode.TextDocument,
  position: vscode.Position
): Promise<vscode.Hover | undefined> {
  // Skip non-file documents
  if (document.uri.scheme !== 'file') {
    return undefined;
  }

  const allTokens = await getTokensInfo(document);

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

async function provideDefinition(
  document: vscode.TextDocument,
  position: vscode.Position
): Promise<vscode.Location | undefined> {
  // Skip non-file documents
  if (document.uri.scheme !== 'file') {
    return undefined;
  }

  const allTokens = await getTokensInfo(document);

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

interface CompilerCommand {
  fileEnding: string;
  command: string;
}

async function loadAndApplyDiagnostics(): Promise<void> {
  log('Loading diagnostics...');
  
  // Clear existing diagnostics
  diagnosticCollection.clear();
  
  const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
  if (!workspaceFolder) {
    log('No workspace folder found for diagnostics');
    return;
  }
  
  const diagnosticsFileUri = vscode.Uri.joinPath(
    workspaceFolder.uri, 
    '_build', 
    'lsp_tokens_info', 
    'diagnostics.json'
  );
  
  try {
    const diagnosticsData = await vscode.workspace.fs.readFile(diagnosticsFileUri);
    const diagnosticsContent = new TextDecoder().decode(diagnosticsData);
    const diagnosticsInfo: DiagnosticInfo[] = JSON.parse(diagnosticsContent);
    
    log(`Loaded ${diagnosticsInfo.length} diagnostics`);
    
    // Group diagnostics by file
    const diagnosticsByFile = new Map<string, vscode.Diagnostic[]>();
    
    for (const diagInfo of diagnosticsInfo) {
      const fileUri = vscode.Uri.file(diagInfo.extent.file);
      const range = new vscode.Range(
        new vscode.Position(diagInfo.extent.start_line, diagInfo.extent.start_col),
        new vscode.Position(diagInfo.extent.end_line, diagInfo.extent.end_col)
      );
      
      // Map diagnostic type to VSCode severity
      let severity: vscode.DiagnosticSeverity;
      const diagType = diagInfo.detail.type;
      if (diagType === 'DiagnosticError') {
        severity = vscode.DiagnosticSeverity.Error;
      } else if (diagType === 'DiagnosticWarning') {
        severity = vscode.DiagnosticSeverity.Warning;
      } else if (diagType === 'DiagnosticInfo') {
        severity = vscode.DiagnosticSeverity.Information;
      } else if (diagType === 'DiagnosticHint') {
        severity = vscode.DiagnosticSeverity.Hint;
      } else {
        severity = vscode.DiagnosticSeverity.Error;
      }
      
      const diagnostic = new vscode.Diagnostic(
        range, 
        diagInfo.detail.content || diagInfo.text, 
        severity
      );
      
      const filePath = diagInfo.extent.file;
      if (!diagnosticsByFile.has(filePath)) {
        diagnosticsByFile.set(filePath, []);
      }
      diagnosticsByFile.get(filePath)!.push(diagnostic);
    }
    
    // Apply diagnostics to each file
    for (const [filePath, diagnostics] of diagnosticsByFile) {
      const fileUri = vscode.Uri.file(filePath);
      diagnosticCollection.set(fileUri, diagnostics);
    }
    
    log(`Applied diagnostics to ${diagnosticsByFile.size} files`);
    
  } catch (error: any) {
    log(`Failed to load diagnostics: ${error.message || error}`);
  }
}

function executeCompilerCommand(filePath: string): void {
  const config = vscode.workspace.getConfiguration('yuyan');
  const compilerCommands = config.get<CompilerCommand[]>('compilerCommand', []);

  // Find matching command based on file ending
  const matchingCommand = compilerCommands.find(cmd => filePath.endsWith(cmd.fileEnding));

  if (!matchingCommand) {
    log(`No compiler command configured for file: ${filePath}`);
    return;
  }

  // Replace <filepath> placeholder with actual file path
  const command = matchingCommand.command.replace('<filepath>', filePath);

  log(`Executing compiler command: ${command}`);
  outputChannel.appendLine(`\n--- Executing compiler command ---`);
  outputChannel.appendLine(`Command: ${command}`);

  // Get workspace folder for working directory
  const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
  const cwd = workspaceFolder ? workspaceFolder.uri.fsPath : path.dirname(filePath);

  // Execute command
  child_process.exec(command, { cwd }, async (error, stdout, stderr) => {
    if (error) {
      outputChannel.appendLine(`Error: ${error.message}`);
      if (stderr) {
        outputChannel.appendLine(`stderr: ${stderr}`);
      }
      // Don't show output channel automatically
      vscode.window.showErrorMessage(`Compiler error: ${error.message}`);
    } else {
      if (stdout) {
        outputChannel.appendLine(stdout);
      }
      if (stderr) {
        outputChannel.appendLine(`stderr: ${stderr}`);
      }
      log(`Compiler command executed successfully`);
      // Don't show output channel automatically
    }
    outputChannel.appendLine(`--- Compiler command finished ---`);
    
    // Load and apply diagnostics after compiler finishes
    await loadAndApplyDiagnostics();
  });
}

function registerLanguageProviders(context: vscode.ExtensionContext): void {
  log('Registering language providers');

  // Get language selector from configuration or default to all files
  const config = vscode.workspace.getConfiguration('yuyan');
  const languageSelector = config.get<string>('languageSelector', 'yuyan');
  log(`Using language selector: ${languageSelector}`);

  const completionProvider = vscode.languages.registerCompletionItemProvider(languageSelector, {
    provideCompletionItems(
      document: vscode.TextDocument,
      position: vscode.Position,
      token: vscode.CancellationToken,
      context: vscode.CompletionContext
    ): vscode.ProviderResult<vscode.CompletionItem[] | vscode.CompletionList> {
      return undefined;
    }
  }, '/');
  context.subscriptions.push(completionProvider);

  const hoverProvider = vscode.languages.registerHoverProvider(languageSelector, {
    async provideHover(
      document: vscode.TextDocument,
      position: vscode.Position,
      token: vscode.CancellationToken
    ): Promise<vscode.Hover | undefined> {
      return await provideHover(document, position);
    }
  });
  context.subscriptions.push(hoverProvider);
  log('Hover provider registered');

  const symbolProvider = vscode.languages.registerDocumentSymbolProvider(languageSelector, {
    async provideDocumentSymbols(
      document: vscode.TextDocument,
      token: vscode.CancellationToken
    ): Promise<vscode.SymbolInformation[] | vscode.DocumentSymbol[] | undefined> {
      return undefined;
    }
  });
  context.subscriptions.push(symbolProvider);

  const definitionProvider = vscode.languages.registerDefinitionProvider(languageSelector, {
    async provideDefinition(
      document: vscode.TextDocument,
      position: vscode.Position,
      token: vscode.CancellationToken
    ): Promise<vscode.Location | undefined> {
      return await provideDefinition(document, position);
    }
  });
  context.subscriptions.push(definitionProvider);

  log('Definition provider registered');

  log('All language providers registered successfully');
}

interface BuildCacheDirectory {
  name: string;
  mtime: number;
  uri: vscode.Uri;
}

async function findJsonBuildArtifacts(
  workspaceFolder: vscode.WorkspaceFolder,
  artifactStem: string
): Promise<BuildArtifactQuickPickItem[]> {
  const buildRootUri = vscode.Uri.joinPath(workspaceFolder.uri, '.yybuild');
  const buildRootEntries = await vscode.workspace.fs.readDirectory(buildRootUri);
  const cacheDirectories = await Promise.all(
    buildRootEntries
      .filter(([, fileType]) => (fileType & vscode.FileType.Directory) !== 0)
      .map(async ([name]) => {
        const uri = vscode.Uri.joinPath(buildRootUri, name);
        const stat = await vscode.workspace.fs.stat(uri);
        return { name, mtime: stat.mtime, uri };
      })
  );
  const sortedCaches = sortBuildCachesNewestFirst<BuildCacheDirectory>(cacheDirectories);
  const artifactDirectory = path.posix.dirname(artifactStem);
  const sourceBaseName = path.posix.basename(artifactStem);
  const quickPickItems: BuildArtifactQuickPickItem[] = [];

  for (const [cacheIndex, cache] of sortedCaches.entries()) {
    const artifactDirectoryUri = artifactDirectory === '.'
      ? cache.uri
      : vscode.Uri.joinPath(cache.uri, artifactDirectory);
    let entries: [string, vscode.FileType][];

    try {
      entries = await vscode.workspace.fs.readDirectory(artifactDirectoryUri);
    } catch {
      continue;
    }

    const artifacts = entries
      .filter(([, fileType]) => (fileType & vscode.FileType.File) !== 0)
      .map(([fileName]) => ({ fileName, stage: jsonArtifactStage(fileName, sourceBaseName) }))
      .filter((artifact): artifact is { fileName: string; stage: string } => artifact.stage !== undefined)
      .sort((left, right) => left.stage.localeCompare(right.stage, 'zh-CN'));

    for (const artifact of artifacts) {
      const artifactUri = vscode.Uri.joinPath(artifactDirectoryUri, artifact.fileName);
      quickPickItems.push({
        label: `$(json) ${artifact.stage}`,
        description: cacheIndex === 0 ? `最新缓存 · ${cache.name}` : cache.name,
        detail: artifactUri.fsPath,
        artifactUri
      });
    }
  }

  return quickPickItems;
}

function prettyPrintBuildArtifact(
  workspaceFolder: vscode.WorkspaceFolder,
  artifactUri: vscode.Uri
): Promise<string> {
  const compilerPath = vscode.Uri.joinPath(workspaceFolder.uri, 'yy_bs_stable').fsPath;
  return new Promise((resolve, reject) => {
    child_process.execFile(
      compilerPath,
      ['debug', 'showtrees', artifactUri.fsPath],
      {
        cwd: workspaceFolder.uri.fsPath,
        encoding: 'utf8',
        maxBuffer: 128 * 1024 * 1024
      },
      (error, stdout, stderr) => {
        if (error) {
          const diagnostic = stderr.trim() || stdout.trim() || error.message;
          reject(new Error(diagnostic));
          return;
        }
        resolve(stdout.endsWith('\n') ? stdout : `${stdout}\n`);
      }
    );
  });
}

async function jumpToBuildArtifact(
  contentProvider: BuildArtifactContentProvider,
  resource?: vscode.Uri
): Promise<void> {
  const sourceUri = resource?.scheme === 'file'
    ? resource
    : vscode.window.activeTextEditor?.document.uri;
  if (!sourceUri || sourceUri.scheme !== 'file') {
    void vscode.window.showErrorMessage(
      '请先打开豫言源文件。 Open a Yuyan source file before jumping to a build artifact.'
    );
    return;
  }

  const workspaceFolder = vscode.workspace.getWorkspaceFolder(sourceUri);
  if (!workspaceFolder) {
    void vscode.window.showErrorMessage(
      '当前源文件不在工作区中。 The current source file is not inside a workspace.'
    );
    return;
  }

  const relativeSourcePath = path.relative(workspaceFolder.uri.fsPath, sourceUri.fsPath);
  const artifactStem = sourceRelativePathToArtifactStem(relativeSourcePath);
  if (!artifactStem) {
    void vscode.window.showErrorMessage(
      '当前文件不是受支持的豫言源文件（。豫、.yuyan 或 .yyon）。'
    );
    return;
  }

  let artifacts: BuildArtifactQuickPickItem[];
  try {
    artifacts = await findJsonBuildArtifacts(workspaceFolder, artifactStem);
  } catch (error: any) {
    log(`Failed to inspect .yybuild: ${error.message || error}`);
    void vscode.window.showErrorMessage(
      `无法读取 ${vscode.Uri.joinPath(workspaceFolder.uri, '.yybuild').fsPath}。请先构建项目。`
    );
    return;
  }

  if (artifacts.length === 0) {
    void vscode.window.showInformationMessage(
      `没有找到 ${artifactStem}.<阶段>.json 构建产物。 No matching JSON build artifacts were found.`
    );
    return;
  }

  const selectedArtifact = await vscode.window.showQuickPick(artifacts, {
    title: 'Yuyan: Jump to Build Artifact 跳转到构建产物',
    placeHolder: '选择要查看的 JSON 阶段产物（最新缓存优先）',
    matchOnDescription: true,
    matchOnDetail: true
  });
  if (!selectedArtifact) {
    return;
  }

  try {
    const prettyArtifact = await vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Window,
        title: `Yuyan: 正在解码 ${path.basename(selectedArtifact.artifactUri.fsPath)}`
      },
      () => prettyPrintBuildArtifact(workspaceFolder, selectedArtifact.artifactUri)
    );
    const artifactFileName = path.basename(selectedArtifact.artifactUri.fsPath);
    const previewFileName = artifactFileName.endsWith('.json')
      ? `${artifactFileName.slice(0, -'.json'.length)}.pretty.yuyan`
      : `${artifactFileName}.pretty.yuyan`;
    const virtualUri = contentProvider.createDocumentUri(
      previewFileName,
      prettyArtifact
    );
    const document = await vscode.workspace.openTextDocument(virtualUri);
    await vscode.window.showTextDocument(document, { preview: true });
  } catch (error: any) {
    log(`Failed to open build artifact ${selectedArtifact.artifactUri.fsPath}: ${error.message || error}`);
    const diagnostic = String(error.message || error).replace(/\s+/g, ' ').slice(0, 300);
    void vscode.window.showErrorMessage(
      `编译器无法解码构建产物：${diagnostic}`
    );
  }
}

function registerBuildArtifactCommand(context: vscode.ExtensionContext): void {
  const contentProvider = new BuildArtifactContentProvider();
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider(BUILD_ARTIFACT_SCHEME, contentProvider),
    vscode.commands.registerCommand(
      JUMP_TO_BUILD_ARTIFACT_COMMAND,
      (resource?: vscode.Uri) => jumpToBuildArtifact(contentProvider, resource)
    ),
    vscode.workspace.onDidCloseTextDocument(document => {
      if (document.uri.scheme === BUILD_ARTIFACT_SCHEME) {
        contentProvider.forgetDocument(document.uri);
      }
    })
  );
  log('Build artifact command registered');
}

export function activate(context: vscode.ExtensionContext): void {
  log('Extension activation started');
  log(`Extension path: ${context.extensionPath}`);
  log(`Extension mode: ${context.extensionMode === vscode.ExtensionMode.Production ? 'Production' : 'Development'}`);
  log(`VS Code version: ${vscode.version}`);

  // Output channel available but not auto-shown
  // outputChannel.show(true);

  try {
    registerLanguageProviders(context);
    registerBuildArtifactCommand(context);

    // Register file save event handler for compiler commands
    const onSaveDisposable = vscode.workspace.onDidSaveTextDocument((document: vscode.TextDocument) => {
      // Only process file documents
      if (document.uri.scheme !== 'file') {
        return;
      }

      const filePath = document.uri.fsPath;
      log(`File saved: ${filePath}`);

      // Execute compiler command if configured
      executeCompilerCommand(filePath);
    });
    context.subscriptions.push(onSaveDisposable);
    log('File save handler registered for compiler commands');
    
    // Add diagnostic collection to subscriptions
    context.subscriptions.push(diagnosticCollection);
    
    log('Extension activated successfully');
  } catch (error: any) {
    log(`Extension activation failed: ${error.message || error}`);
    vscode.window.showErrorMessage(`Yuyan Extension failed to activate: ${error.message || error}`);
  }
}

export function deactivate(): void {
  log('Extension deactivation started');
  outputChannel.dispose();
}
