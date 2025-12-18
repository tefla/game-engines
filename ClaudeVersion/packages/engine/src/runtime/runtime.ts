// Slate Runtime - Integrates Interpreter with SignalBus and VFS

import {
  type SlateValue,
  type SlateNativeFunction,
  Null,
  stringify,
  type Program,
} from "@oort/core";
import { Interpreter, parseSlate, stdlib } from "@oort/slate";
import { SignalBus, type SignalPath, createSignalStdlib } from "../signals";
import { VirtualFileSystem, createVfsStdlib } from "../vfs";

// Extended interpreter that bridges internal signals to SignalBus
class RuntimeInterpreter extends Interpreter {
  private signalBus: SignalBus;

  constructor(globals: Map<string, SlateValue>, signalBus: SignalBus) {
    super(globals);
    this.signalBus = signalBus;
  }

  // Override emit to also trigger SignalBus handlers
  emit(signalPath: string[], data: SlateValue = Null()): void {
    // First trigger internal handlers (for on statements in same program)
    super.emit(signalPath, data);
    // Then trigger SignalBus handlers (for external listeners)
    this.signalBus.emit(signalPath, data);
  }
}

export interface RuntimeOptions {
  vfs?: VirtualFileSystem;
  signalBus?: SignalBus;
  globals?: Map<string, SlateValue>;
  onOutput?: (message: string) => void;
}

export class Runtime {
  private interpreter!: Interpreter;
  private signalBus: SignalBus;
  private vfs: VirtualFileSystem;
  private outputHandler: (message: string) => void;
  private globals: Map<string, SlateValue>;

  constructor(options: RuntimeOptions = {}) {
    this.signalBus = options.signalBus ?? new SignalBus();
    this.vfs = options.vfs ?? VirtualFileSystem.createGameFileSystem();
    this.outputHandler = options.onOutput ?? console.log;

    // Build globals map with all stdlib functions
    this.globals = new Map<string, SlateValue>();

    // Add base stdlib first
    for (const [name, fn] of stdlib) {
      this.globals.set(name, fn);
    }

    // Add user-provided globals (can override stdlib)
    if (options.globals) {
      for (const [name, value] of options.globals) {
        this.globals.set(name, value);
      }
    }

    // Add VFS functions
    const vfsStdlib = createVfsStdlib(this.vfs);
    for (const [name, fn] of vfsStdlib) {
      this.globals.set(name, fn);
    }

    // Add signal functions
    const signalStdlib = createSignalStdlib(this.signalBus);
    for (const [name, fn] of signalStdlib) {
      this.globals.set(name, fn);
    }

    // Override say/print to use output handler
    this.globals.set("say", {
      type: "native",
      name: "say",
      arity: "variadic",
      fn: (args: SlateValue[]) => {
        const message = args.map((a) => stringify(a)).join(" ");
        this.outputHandler(message);
        return Null();
      },
    });

    this.globals.set("print", {
      type: "native",
      name: "print",
      arity: "variadic",
      fn: (args: SlateValue[]) => {
        const message = args.map((a) => stringify(a)).join(" ");
        this.outputHandler(message);
        return Null();
      },
    });

    this.createInterpreter();
  }

  private createInterpreter(): void {
    this.interpreter = new RuntimeInterpreter(this.globals, this.signalBus);
  }

  // Run Slate source code
  run(source: string): SlateValue {
    const program = parseSlate(source);
    return this.runProgram(program);
  }

  // Run a parsed program
  runProgram(program: Program): SlateValue {
    // Run the program
    const result = this.interpreter.run(program);

    // Register any "on" handlers with the SignalBus
    // This allows external emit() calls to trigger Slate handlers
    for (const handler of this.interpreter.getSignalHandlers()) {
      this.signalBus.on(handler.signal, (data) => handler.handler(data), {
        filter: handler.filter,
      });
    }

    return result;
  }

  // Emit a signal
  emit(signal: SignalPath | string, data: SlateValue = Null()): void {
    const path = typeof signal === "string" ? signal.split(".") : signal;
    this.signalBus.emit(path, data);
  }

  // Register a handler from JS
  on(
    signal: SignalPath | string,
    handler: (data: SlateValue, signal: SignalPath) => void,
    options?: { filter?: SlateValue; once?: boolean }
  ): number {
    const path = typeof signal === "string" ? signal.split(".") : signal;
    return this.signalBus.on(path, handler, options);
  }

  // Unregister a handler
  off(handlerId: number): boolean {
    return this.signalBus.off(handlerId);
  }

  // Get the VFS instance
  getVfs(): VirtualFileSystem {
    return this.vfs;
  }

  // Get the SignalBus instance
  getSignalBus(): SignalBus {
    return this.signalBus;
  }

  // Get the interpreter instance
  getInterpreter(): Interpreter {
    return this.interpreter;
  }

  // Get output history from interpreter
  getOutput(): string[] {
    return this.interpreter.getOutput();
  }

  // Load and run a file from VFS
  runFile(path: string): SlateValue {
    const source = this.vfs.read(path);
    return this.run(source);
  }

  // Import a module from VFS
  import(path: string): SlateValue {
    // Normalize path
    let normalizedPath = path;
    if (!normalizedPath.endsWith(".sl")) {
      normalizedPath += ".sl";
    }
    if (!normalizedPath.startsWith("/")) {
      normalizedPath = "/" + normalizedPath;
    }

    return this.runFile(normalizedPath);
  }

  // Tick - emit game tick signal and process queued events
  tick(deltaTime: number = 0): void {
    const data = new Map<string, SlateValue>();
    data.set("delta", { type: "number", value: deltaTime });
    this.emit("game.tick", { type: "record", fields: data });
  }
}
