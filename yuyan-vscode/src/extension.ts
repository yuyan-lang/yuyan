import * as vscode from 'vscode';
import * as child_process from 'child_process';
import * as path from 'path';
import {
  jsonArtifactStage,
  sortBuildCachesNewestFirst,
  sourceRelativePathToArtifactStem
} from './buildArtifacts';
import {
  DefinitionInfo,
  HoverInfo,
  LanguageServiceDocument,
  SourceRangeInfo,
  languageServiceArtifactPath,
  parseLanguageServiceDocument,
  selectNarrowestInfo
} from './languageServiceArtifacts';

const BUILD_ARTIFACT_SCHEME = 'yuyan-build-artifact';
const JUMP_TO_BUILD_ARTIFACT_COMMAND = 'yuyan.jumpToBuildArtifact';

// Create output channel for logging
const outputChannel = vscode.window.createOutputChannel('Yuyan Language Extension');

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

interface CachedLanguageService {
  artifactUri: vscode.Uri;
  mtime: number;
  document: LanguageServiceDocument;
}

const languageServiceCache = new Map<string, CachedLanguageService>();

function sourceRangeToVscodeRange(range: SourceRangeInfo): vscode.Range {
  return new vscode.Range(
    new vscode.Position(range.开始行, range.开始列),
    new vscode.Position(range.结束行, range.结束列)
  );
}

async function findLanguageServiceArtifact(
  workspaceFolder: vscode.WorkspaceFolder,
  artifactRelativePath: string
): Promise<{ uri: vscode.Uri; mtime: number } | undefined> {
  const buildRootUri = vscode.Uri.joinPath(workspaceFolder.uri, '.yybuild');
  let buildRootEntries: [string, vscode.FileType][];
  try {
    buildRootEntries = await vscode.workspace.fs.readDirectory(buildRootUri);
  } catch {
    return undefined;
  }

  const cacheDirectories = await Promise.all(
    buildRootEntries
      .filter(([, fileType]) => (fileType & vscode.FileType.Directory) !== 0)
      .map(async ([name]) => {
        const uri = vscode.Uri.joinPath(buildRootUri, name);
        const stat = await vscode.workspace.fs.stat(uri);
        return { name, mtime: stat.mtime, uri };
      })
  );

  for (const cache of sortBuildCachesNewestFirst<BuildCacheDirectory>(cacheDirectories)) {
    const uri = vscode.Uri.joinPath(cache.uri, artifactRelativePath);
    try {
      const stat = await vscode.workspace.fs.stat(uri);
      if ((stat.type & vscode.FileType.File) !== 0) {
        return { uri, mtime: stat.mtime };
      }
    } catch {
      // This compiler cache does not contain metadata for the source file.
    }
  }

  return undefined;
}

async function getLanguageServiceDocument(
  sourceDocument: vscode.TextDocument
): Promise<LanguageServiceDocument | undefined> {
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(sourceDocument.uri);
  if (!workspaceFolder) {
    return undefined;
  }

  const relativeSourcePath = path.relative(
    workspaceFolder.uri.fsPath,
    sourceDocument.uri.fsPath
  );
  const artifactRelativePath = languageServiceArtifactPath(relativeSourcePath);
  if (!artifactRelativePath) {
    return undefined;
  }

  const artifact = await findLanguageServiceArtifact(workspaceFolder, artifactRelativePath);
  if (!artifact) {
    return undefined;
  }

  const cacheKey = sourceDocument.uri.toString();
  const cached = languageServiceCache.get(cacheKey);
  if (
    cached &&
    cached.artifactUri.toString() === artifact.uri.toString() &&
    cached.mtime === artifact.mtime
  ) {
    return cached.document;
  }

  try {
    const content = new TextDecoder().decode(await vscode.workspace.fs.readFile(artifact.uri));
    const metadata = parseLanguageServiceDocument(content);
    if (!metadata) {
      log(`Invalid language service artifact: ${artifact.uri.fsPath}`);
      return undefined;
    }
    languageServiceCache.set(cacheKey, {
      artifactUri: artifact.uri,
      mtime: artifact.mtime,
      document: metadata
    });
    return metadata;
  } catch (error: any) {
    log(`Failed to read language service artifact: ${error.message || error}`);
    return undefined;
  }
}

async function provideHover(
  document: vscode.TextDocument,
  position: vscode.Position
): Promise<vscode.Hover | undefined> {
  if (document.uri.scheme !== 'file') {
    return undefined;
  }

  const metadata = await getLanguageServiceDocument(document);
  if (!metadata) {
    return undefined;
  }

  const hover = selectNarrowestInfo(
    metadata.信息.filter((item): item is HoverInfo => item.种类 === '悬停'),
    position.line,
    position.character
  );
  if (!hover) {
    return undefined;
  }

  return new vscode.Hover(hover.内容, sourceRangeToVscodeRange(hover.范围));
}

async function provideDefinition(
  document: vscode.TextDocument,
  position: vscode.Position
): Promise<vscode.Location | undefined> {
  if (document.uri.scheme !== 'file') {
    return undefined;
  }

  const metadata = await getLanguageServiceDocument(document);
  if (!metadata) {
    return undefined;
  }

  const definition = selectNarrowestInfo(
    metadata.信息.filter((item): item is DefinitionInfo => item.种类 === '定义'),
    position.line,
    position.character
  );
  if (!definition) {
    return undefined;
  }

  return new vscode.Location(
    vscode.Uri.file(definition.目标.文件),
    sourceRangeToVscodeRange(definition.目标)
  );
}

interface CompilerCommand {
  fileEnding: string;
  command: string;
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
  child_process.exec(command, { cwd }, (error, stdout, stderr) => {
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
    languageServiceCache.clear();
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
