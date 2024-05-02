import * as vscode from "vscode";
import * as path from "path";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from "vscode-languageclient/node";

export async function activate(context: vscode.ExtensionContext) {
  const outputChannel = vscode.window.createOutputChannel("ScalaRC", "scalarc");

  const debugOptions = { execArgv: ["--nolazy", "--inspect=6009"] };
  const serverPath = context.asAbsolutePath(
    path.join("..", "..", "target", "debug", "scalarc-lsp"),
  );

  const run: Executable = { command: serverPath };
  const serverOptions: ServerOptions = { run, debug: run };
  const clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [
      {
        scheme: "file",
        language: "scala",
      },
    ],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: vscode.workspace.createFileSystemWatcher("**/.clientrc"),
    },
    outputChannel,
    revealOutputChannelOn: RevealOutputChannelOn.Info,
  };

  const client = new LanguageClient(
    "scala",
    "ScalaRC",
    serverOptions,
    clientOptions,
  );

  // Start the client. This will also launch the server
  await client.start();
}

export function deactivate() {}
