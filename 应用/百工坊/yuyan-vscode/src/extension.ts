import * as vscode from 'vscode';
import * as child_process from 'child_process';
import * as fs from 'fs';
import * as path from 'path';
import {
  BuildArtifactEntry,
  BuildArtifactReport,
  isSupportedYuyanSource,
  parseBuildArtifactReport
} from './buildArtifacts';
import {
  DefinitionInfo,
  HoverInfo,
  LanguageServiceDocument,
  SourceRangeInfo,
  parseLanguageServiceDocument,
  selectNarrowestInfo
} from './languageServiceArtifacts';

const BUILD_ARTIFACT_SCHEME = 'yuyan-build-artifact';
const JUMP_TO_BUILD_ARTIFACT_COMMAND = 'yuyan.jumpToBuildArtifact';

// Create output channel for logging
const outputChannel = vscode.window.createOutputChannel('Yuyan Language Extension');

interface BuildArtifactQuickPickItem extends vscode.QuickPickItem {
  stage: string;
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
  version: number;
  fetchedAt: number;
  document: LanguageServiceDocument;
}

// 同一版本的文档在这段时间内复用已取到的资料；过了就再问一次豫构，
// 这样别处跑一次构建、资料变了，很快就能看到。
const LANGUAGE_SERVICE_CACHE_MS = 2000;

const languageServiceCache = new Map<string, CachedLanguageService>();

function sourceRangeToVscodeRange(range: SourceRangeInfo): vscode.Range {
  return new vscode.Range(
    new vscode.Position(range.开始行, range.开始列),
    new vscode.Position(range.结束行, range.结束列)
  );
}

async function findProjectRoot(sourceUri: vscode.Uri): Promise<vscode.Uri | undefined> {
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(sourceUri);
  if (!workspaceFolder) {
    return undefined;
  }

  const workspaceRootPath = path.resolve(workspaceFolder.uri.fsPath);
  let candidatePath = path.resolve(path.dirname(sourceUri.fsPath));

  while (true) {
    for (const marker of ['.豫构缓存', '.git']) {
      try {
        await vscode.workspace.fs.stat(vscode.Uri.joinPath(vscode.Uri.file(candidatePath), marker));
        return vscode.Uri.file(candidatePath);
      } catch {
        // Keep walking toward the containing workspace folder.
      }
    }

    if (candidatePath === workspaceRootPath) {
      return workspaceFolder.uri;
    }

    const parentPath = path.dirname(candidatePath);
    if (
      parentPath === candidatePath ||
      path.relative(workspaceRootPath, parentPath).startsWith('..')
    ) {
      return workspaceFolder.uri;
    }
    candidatePath = parentPath;
  }
}

// 找豫构（构建工具）：先看设置里的路径，其次工作区根下按本机系统命名的原生程序，
// 最后交给 PATH 上的 `豫构`。扩展不自己拼缓存路径，语言服务资料一律问这个命令。
function findBuildToolCommand(projectRootUri: vscode.Uri): string {
  const config = vscode.workspace.getConfiguration('yuyan', projectRootUri);
  const configured = (config.get<string>('buildToolCommand', '') || '').trim();
  if (configured.length > 0) {
    return configured;
  }

  for (const name of ['豫构。林檎。豫行', '豫构。林纳。豫行', '豫构。轩窗。豫行']) {
    const candidate = vscode.Uri.joinPath(projectRootUri, name).fsPath;
    try {
      if (fs.statSync(candidate).isFile()) {
        return candidate;
      }
    } catch {
      // 不在这一份，试下一个名字。
    }
  }

  return '豫构';
}

// 跑一条豫构子命令并把标准输出交回。缓存布局、包上下文与阶段路径都由豫构解析，
// 扩展只认子命令与它印出的结绳。
function runBuildToolCommand(
  projectRootUri: vscode.Uri,
  args: string[]
): Promise<string> {
  const command = findBuildToolCommand(projectRootUri);
  return new Promise((resolve, reject) => {
    child_process.execFile(
      command,
      args,
      {
        cwd: projectRootUri.fsPath,
        encoding: 'utf8',
        maxBuffer: 128 * 1024 * 1024,
        timeout: 120000,
        windowsHide: true
      },
      (error, stdout, stderr) => {
        if (error) {
          const diagnostic = stderr.trim() || stdout.trim() || error.message;
          reject(new Error(diagnostic));
          return;
        }
        resolve(stdout);
      }
    );
  });
}

// 调 `豫构 语言服务 <源文件>`：由译场按「讯储读径」解析资料位置、读出并解成结绳，
// 命令把结绳文字印到标准输出；缓存布局的变化都由豫构跟着，扩展不必知道。
function runLanguageServiceCommand(
  projectRootUri: vscode.Uri,
  sourceUri: vscode.Uri
): Promise<string> {
  return runBuildToolCommand(projectRootUri, ['语言服务', sourceUri.fsPath]);
}

// 调 `豫构 产物 <源文件>`：由豫构按所属包的包上下文，经译场「讯储读径」逐阶取实际路径，
// 印一行结绳文字（源文件、包上下文、阶段产物）。扩展只解析这一行，缓存布局的变化都由豫构跟着。
function runBuildArtifactsCommand(
  projectRootUri: vscode.Uri,
  sourceUri: vscode.Uri
): Promise<string> {
  return runBuildToolCommand(projectRootUri, ['产物', sourceUri.fsPath]);
}

async function getLanguageServiceDocument(
  sourceDocument: vscode.TextDocument
): Promise<LanguageServiceDocument | undefined> {
  const projectRootUri = await findProjectRoot(sourceDocument.uri);
  if (!projectRootUri) {
    return undefined;
  }

  const cacheKey = sourceDocument.uri.toString();
  const cached = languageServiceCache.get(cacheKey);
  if (
    cached &&
    cached.version === sourceDocument.version &&
    Date.now() - cached.fetchedAt < LANGUAGE_SERVICE_CACHE_MS
  ) {
    return cached.document;
  }

  try {
    const content = await runLanguageServiceCommand(projectRootUri, sourceDocument.uri);
    const metadata = parseLanguageServiceDocument(JSON.parse(content));
    if (!metadata) {
      log(`Invalid language service document: ${sourceDocument.uri.fsPath}`);
      return undefined;
    }
    if (path.resolve(metadata.源文件) !== path.resolve(sourceDocument.uri.fsPath)) {
      log(`Language service source mismatch: ${sourceDocument.uri.fsPath}`);
      return undefined;
    }
    languageServiceCache.set(cacheKey, {
      version: sourceDocument.version,
      fetchedAt: Date.now(),
      document: metadata
    });
    return metadata;
  } catch (error: any) {
    log(`Failed to read language service document: ${error.message || error}`);
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

async function executeCompilerCommand(filePath: string): Promise<void> {
  const sourceUri = vscode.Uri.file(filePath);
  const config = vscode.workspace.getConfiguration('yuyan', sourceUri);
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

  const projectRootUri = await findProjectRoot(sourceUri);
  const cwd = projectRootUri ? projectRootUri.fsPath : path.dirname(filePath);

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

// 读树码：包上下文由 `豫构 产物` 给出，扩展不再按缓存路径里的记号自己还原。
function runTreeTool(
  projectRootUri: vscode.Uri,
  command: string,
  artifactUri: vscode.Uri,
  packageContextPath: string
): Promise<string> {
  const hostPath = vscode.Uri.joinPath(projectRootUri, '豫言操作系统', '宿主', '节点', '宿主.cjs').fsPath;
  const toolPath = vscode.Uri.joinPath(projectRootUri, '树码.wasm').fsPath;
  const args = [hostPath, toolPath, command, artifactUri.fsPath];
  if (command === '显示' || command === '读取编辑器资料') {
    args.push('--包上下文', packageContextPath);
  }
  return new Promise((resolve, reject) => {
    child_process.execFile(
      'node',
      args,
      {
        cwd: projectRootUri.fsPath,
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

// 把豫构印出的阶段产物变成选择项：只列实际存在的阶段，按译场「阶段名们」的流水次序。
function buildArtifactQuickPickItems(report: BuildArtifactReport): BuildArtifactQuickPickItem[] {
  return report.artifacts
    .filter((artifact: BuildArtifactEntry) => artifact.present)
    .map(artifact => ({
      label: `$(json) ${artifact.stage}`,
      detail: artifact.path,
      stage: artifact.stage,
      artifactUri: vscode.Uri.file(artifact.path)
    }));
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

  const projectRootUri = await findProjectRoot(sourceUri);
  if (!projectRootUri) {
    void vscode.window.showErrorMessage(
      '当前源文件不在工作区中。 The current source file is not inside a workspace.'
    );
    return;
  }

  if (!isSupportedYuyanSource(sourceUri.fsPath)) {
    void vscode.window.showErrorMessage(
      '当前文件不是受支持的豫言源文件（。豫、.yuyan 或 .yyon）。'
    );
    return;
  }

  let report: BuildArtifactReport | undefined;
  try {
    const output = await runBuildArtifactsCommand(projectRootUri, sourceUri);
    report = parseBuildArtifactReport(output.trim());
  } catch (error: any) {
    log(`Failed to run 豫构 产物: ${error.message || error}`);
    void vscode.window.showErrorMessage(
      `无法从豫构取得构建产物：${String(error.message || error).replace(/\s+/g, ' ').slice(0, 300)}`
    );
    return;
  }

  if (!report || path.resolve(report.sourceFile) !== path.resolve(sourceUri.fsPath)) {
    log(`Invalid build artifact report for: ${sourceUri.fsPath}`);
    void vscode.window.showErrorMessage(
      '豫构没有给出这个源文件的构建产物清单。请先构建或类型检查所属包。'
    );
    return;
  }

  const packageContextPath = report.packageContext;
  const artifacts = buildArtifactQuickPickItems(report);
  if (artifacts.length === 0) {
    void vscode.window.showInformationMessage(
      '没有找到已写出的阶段产物。请先构建或类型检查所属包。'
    );
    return;
  }

  const selectedArtifact = await vscode.window.showQuickPick(artifacts, {
    title: 'Yuyan: Jump to Build Artifact 跳转到构建产物',
    placeHolder: '选择要查看的树码阶段产物（按流水次序）',
    matchOnDetail: true
  });
  if (!selectedArtifact) {
    return;
  }

  try {
    const prettyArtifact = await vscode.window.withProgress(
      {
        location: vscode.ProgressLocation.Window,
        title: `Yuyan: 正在解码 ${selectedArtifact.stage}`
      },
      () => runTreeTool(projectRootUri, '显示', selectedArtifact.artifactUri, packageContextPath)
    );
    const previewFileName = `${path.basename(sourceUri.fsPath)}.${selectedArtifact.stage}.pretty.yuyan`;
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
      languageServiceCache.clear();

      // Execute compiler command if configured
      void executeCompilerCommand(filePath);
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
