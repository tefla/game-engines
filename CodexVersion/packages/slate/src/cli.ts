import { readFile } from "node:fs/promises";
import process from "node:process";

import { parse } from "./parser";
import { Interpreter } from "./interpreter";
import { createRuntime, installStdlib } from "./stdlib";

async function main(): Promise<void> {
  const target = process.argv[2];
  if (!target) {
    console.error("Usage: bun run packages/slate/src/cli.ts <file-path|/vfs/path.sl>");
    process.exitCode = 1;
    return;
  }

  const runtime = createRuntime();
  const interpreter = new Interpreter({
    signals: runtime.signals,
    loadScript: async (path) => parse(runtime.vfs.exec(path)),
  });
  installStdlib(interpreter, runtime);

  const source = target.startsWith("/") ? runtime.vfs.read(target) : await readFile(target, "utf8");
  const program = parse(source);
  await interpreter.runWithSource(program, interpreter.globals, target);
}

main().catch((err) => {
  console.error(err);
  process.exitCode = 1;
});
