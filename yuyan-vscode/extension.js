const { workspace, ExtensionContext, languages, commands } = require('vscode');
const vscode = require('vscode')
var path = require('path');
const { exec } = require('child_process');

const {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} = require('vscode-languageclient/node');

function getTokensInfo(document) {

}


function startLSP() {
  // let languages = vscode.languages.getLanguages();
  // console.log(languages);
  // Signature help
  // vscode.languages.registerSignatureHelpProvider('*', {
  //   provideSignatureHelp(document, position, token, context) {
  //     let lastWord = getTokensInfo(document);
  //     if (lastWord) {
  //       let word: string = lastWord;
  //       let istari = getIstariForDocument(document);
  //       return new Promise((resolve, reject) => {
  //         istari.getTypeForConstant(word,
  //           (data) => {
  //             let signatureHelp = new vscode.SignatureHelp();
  //             let signature = new vscode.SignatureInformation(
  //               word,
  //               new vscode.MarkdownString().appendCodeblock(data, "istari")
  //             );
  //             signatureHelp.signatures = [signature];
  //             signatureHelp.activeSignature = 0;
  //             signatureHelp.activeParameter = 0;
  //             resolve(signatureHelp);
  //             return true;
  //           }
  //         );
  //       });
  //     } else {
  //       return undefined;
  //     }
  //   }
  // }, ' ');
  // Completion
  vscode.languages.registerCompletionItemProvider('istari', {
    provideCompletionItems(document, position, token, context) {
      return undefined;
    }
  }, '/'); // only triggers on the / character
  // Hover
  vscode.languages.registerHoverProvider('istari', {
    provideHover(document, position, token) {
      return undefined;
    }
  });
  // Document Outline
  vscode.languages.registerDocumentSymbolProvider('istari', {
    async provideDocumentSymbols(document, token) {
      return undefined;
    }
  });
  // goto definition
  vscode.languages.registerDefinitionProvider('istari', {
    provideDefinition(document, position, token) {
      return undefined;
    }
  });
}

function activate(context) {

  startLSP();
}

function deactivate() {

}


module.exports = {
  activate, deactivate
}