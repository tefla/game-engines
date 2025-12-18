import { SlateThreeScene } from "@oort/renderer";
import { parse, Interpreter, createRuntime, installStdlib, type BuiltinFnValue, type RecordValue } from "@oort/slate";

function $(id: string): HTMLElement {
  const el = document.getElementById(id);
  if (!el) throw new Error(`Missing element: ${id}`);
  return el;
}

function appendLog(line: string): void {
  const log = $("log") as HTMLPreElement;
  log.textContent = (log.textContent ? log.textContent + "\n" : "") + line;
  log.scrollTop = log.scrollHeight;
}

async function main(): Promise<void> {
  const canvas = $("canvas") as HTMLCanvasElement;
  const editor = $("editor") as HTMLTextAreaElement;

  const runtime = createRuntime();
  const interpreter = new Interpreter({
    signals: runtime.signals,
    loadScript: async (path) => parse(runtime.vfs.exec(path)),
  });
  installStdlib(interpreter, runtime);

  interpreter.globals.forceSet("say", {
    kind: "builtin",
    name: "say",
    call: (args) => {
      appendLog(args.map(String).join(" "));
      return null;
    },
  } satisfies BuiltinFnValue);
  interpreter.globals.forceSet("notify", interpreter.globals.get("say"));
  interpreter.globals.forceSet("play_sound", {
    kind: "builtin",
    name: "play_sound",
    call: (args) => {
      appendLog(`[sound] ${args.map(String).join(" ")}`);
      return null;
    },
  } satisfies BuiltinFnValue);

  const doorPath = "/puzzles/door.sl";
  editor.value = runtime.vfs.read(doorPath);
  await interpreter.runWithSource(parse(editor.value), interpreter.globals, doorPath);

  const scene = new SlateThreeScene({ canvas });

  const getDoor = (): RecordValue => interpreter.globals.get("Door") as RecordValue;

  $("btn-interact").addEventListener("click", async () => {
    const door = getDoor();
    await runtime.signals.emit("player.interact", door);
  });

  $("btn-key").addEventListener("click", async () => {
    const player = interpreter.globals.get("player") as RecordValue;
    await (player.entries.give as BuiltinFnValue).call(["gold_key"], interpreter);
    appendLog("player.give \"gold_key\"");
  });

  $("btn-reset").addEventListener("click", () => {
    const door = getDoor();
    door.entries.locked = true;
    door.entries.color = "#8B4513";
    appendLog("Door reset");
  });

  $("btn-save").addEventListener("click", async () => {
    const write = interpreter.globals.get("write") as BuiltinFnValue;
    try {
      await write.call([doorPath, editor.value], interpreter);
      appendLog("Saved + hot-reloaded.");
    } catch (err) {
      appendLog(String(err));
      throw err;
    }
  });

  function frame(): void {
    scene.resizeToDisplay();
    scene.sync(runtime.templates.values());
    scene.render();
    requestAnimationFrame(frame);
  }
  frame();
}

main().catch((err) => {
  appendLog(String(err));
  console.error(err);
});

