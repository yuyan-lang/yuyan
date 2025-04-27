const { workspace, ExtensionContext, languages, commands} =require('vscode');
const vscode = require('vscode')
var path = require('path');
const { exec } = require('child_process');

const {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} =require('vscode-languageclient/node');

let client
const executeDebugCommandOnCurrentFile = ((debugDirective) => {
  // Get the active text editor
  const editor = vscode.window.activeTextEditor;
  
  if (editor) {
    // Get the workspace root
    const workspaceRoot = vscode.workspace.rootPath;

    if (!workspaceRoot) {
        vscode.window.showErrorMessage('No workspace is opened');
        return;
    }

    // Get the file path of the active document
    const filePath = editor.document.fileName;

    // Construct the full file path relative to the workspace root
    const relativeFilePath = path.relative(workspaceRoot, filePath);

    // Run the command from the workspace root: `./yy_bs debug showtree <relative-file-path>`
    const command = `./yy_bs debug ${debugDirective} "${relativeFilePath}"`;

    exec(command, { cwd: workspaceRoot, maxBuffer: 1024 * 1024 * 20 /* 20MB */ }, (error, stdout, stderr) => {
        if (error) {
            vscode.window.showErrorMessage(`Error running command: ${error.message}`);
            return;
        }


        const newFilePath = filePath + ".txt";
        // // Create a new untitled document
        // vscode.workspace.fs.writeFile(vscode.Uri.file(newFilePath), Buffer.from(stdout)).then(() => 
        // {
        //   vscode.workspace.openTextDocument(newFilePath)
        //   .then(document => {
        //       // Show the document in a new editor
        //       vscode.window.showTextDocument(document, { viewColumn: vscode.ViewColumn.Beside });
        //   })
        //   .catch(error => {
        //       vscode.window.showErrorMessage(`Error opening document: ${error.message}`);
        //   });
        // }).catch(error => {
        //       vscode.window.showErrorMessage(`Error saving document: ${error.message}`);
        // });
          vscode.workspace.openTextDocument({content: stdout, language: 'plaintext'})
          .then(document => {
              // Show the document in a new editor
              // document.fileName = newFilePath;
              vscode.window.showTextDocument(document, { viewColumn: vscode.ViewColumn.Active, preview: true });
          })
          .catch(error => {
              vscode.window.showErrorMessage(`Error opening document: ${error.message}`);
          });
    });
} else {
    vscode.window.showErrorMessage('No active text editor');
}
});
const showTreeDisposable = commands.registerCommand('yuyan.debugshowtree', () => executeDebugCommandOnCurrentFile("showtree"));

const showTreesDisposable = commands.registerCommand('yuyan.debugshowtrees', () => executeDebugCommandOnCurrentFile("showtrees"));

function activate(context ) {
  

  // The server is implemented in node
//   let serverModule = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
  // The debug options for the server
  // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
  let debugOptions = {}
  let yyPath = workspace.getConfiguration("yuyan").get("executablePath")
  if (yyPath=="") {
    yyPath = "yy"
  } else if (!path.isAbsolute(yyPath)){
    yyPath = path.join(workspace.workspaceFolders[0].uri.fsPath, yyPath)
  }
  console.log(yyPath)
  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  let serverOptions = {
    run: { command: yyPath, args:["lsp"], transport: TransportKind.stdio },
    debug: {
      command: yyPath, args:["lsp"],
      transport: TransportKind.stdio,
      options: debugOptions
    }
  };

  // Options to control the language client
  let clientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'yuyan' }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
    //   fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  // Create the language client and start the client.
   client = new LanguageClient(
    'yuyan',
    'Yuyan',
    serverOptions,
    clientOptions
  );

  const restartDisposable = commands.registerCommand('yuyan.restartyuyanlsp', () => {
		// The code you place here will be executed every time your command is executed

		// Display a message box to the user
    client.restart();
	});

	context.subscriptions.push(restartDisposable);
  context.subscriptions.push(showTreeDisposable);
  context.subscriptions.push(showTreesDisposable);

  // Start the client. This will also launch the server
  client.start();
}

function deactivate() {
  if (!client) {
    return undefined;
  }
  return client.stop();
}


module.exports = {
  activate,deactivate
}