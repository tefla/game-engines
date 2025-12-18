import { parse, Interpreter, createRuntime, installStdlib, type BuiltinFnValue, type RecordValue } from "@oort/slate";

async function run(): Promise<void> {
  const runtime = createRuntime();
  runtime.signals.on("door.opened", () => {
    console.log("[signal] door.opened");
  });

  const interpreter = new Interpreter({
    signals: runtime.signals,
    loadScript: async (path) => parse(runtime.vfs.exec(path)),
  });
  installStdlib(interpreter, runtime);

  const doorPath = "/puzzles/door.sl";
  const doorSource = runtime.vfs.read(doorPath);
  await interpreter.runWithSource(parse(doorSource), interpreter.globals, doorPath);

  const door = interpreter.globals.get("Door") as RecordValue;
  const player = interpreter.globals.get("player") as RecordValue;

  console.log("Door (initial):", door.entries);
  await runtime.signals.emit("player.interact", door);
  console.log("Door (after interact, no key):", door.entries);

  await (player.entries.give as BuiltinFnValue).call(["gold_key"], interpreter);
  door.entries.locked = true;
  door.entries.color = "#8B4513";
  await runtime.signals.emit("player.interact", door);
  console.log("Door (after interact, with key, buggy):", door.entries);

  const fixedDoorSource = doorSource.replaceAll("if not player.has", "if player.has");
  await (interpreter.globals.get("write") as BuiltinFnValue).call([doorPath, fixedDoorSource], interpreter);

  door.entries.locked = true;
  door.entries.color = "#8B4513";
  await runtime.signals.emit("player.interact", door);
  console.log("Door (after hot-reload fix):", door.entries);
}

run().catch((err) => {
  console.error(err);
  process.exitCode = 1;
});

