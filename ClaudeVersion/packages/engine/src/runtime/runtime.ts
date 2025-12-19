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
import { sceneStdlib } from "../renderer3d/scene-stdlib";
import { InputManager, createInputStdlib } from "../input";

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
  inputManager?: InputManager;
  globals?: Map<string, SlateValue>;
  onOutput?: (message: string) => void;
}

export type GameState = "stopped" | "running" | "paused";

export class Runtime {
  private interpreter!: Interpreter;
  private signalBus: SignalBus;
  private vfs: VirtualFileSystem;
  private inputManager: InputManager;
  private outputHandler: (message: string) => void;
  private globals: Map<string, SlateValue>;

  // Game loop state
  private animationFrameId: number | null = null;
  private lastTime: number = 0;
  private gameState: GameState = "stopped";
  private accumulatedTime: number = 0;

  constructor(options: RuntimeOptions = {}) {
    this.signalBus = options.signalBus ?? new SignalBus();
    this.vfs = options.vfs ?? VirtualFileSystem.createGameFileSystem();
    this.inputManager = options.inputManager ?? new InputManager();
    this.outputHandler = options.onOutput ?? console.log;

    // Connect input manager to signal bus for input signals
    this.inputManager.setSignalBus(this.signalBus);

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

    // Add input functions
    const inputStdlib = createInputStdlib(this.inputManager);
    for (const [name, fn] of inputStdlib) {
      this.globals.set(name, fn);
    }

    // Add 3D scene functions
    for (const [name, fn] of sceneStdlib) {
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

  // Get the InputManager instance
  getInputManager(): InputManager {
    return this.inputManager;
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
    data.set("time", { type: "number", value: this.accumulatedTime });
    this.emit("game.tick", { type: "record", fields: data });
  }

  // Start the game loop
  startGameLoop(): void {
    if (this.gameState === "running") return;

    // If we're resuming from pause, don't emit game.start
    const waspaused = this.gameState === "paused";

    this.gameState = "running";
    this.lastTime = performance.now();

    if (!waspaused) {
      // Fresh start
      this.accumulatedTime = 0;
      this.emit("game.start");
    } else {
      // Resume from pause
      this.emit("game.resume");
    }

    const loop = (currentTime: number) => {
      if (this.gameState !== "running") return;

      const deltaMs = currentTime - this.lastTime;
      const deltaSec = deltaMs / 1000;
      this.lastTime = currentTime;
      this.accumulatedTime += deltaSec;

      // Update input state (before game logic)
      this.inputManager.update(deltaSec);

      // Emit tick signal (game logic runs here)
      this.tick(deltaSec);

      // End input frame (after game logic)
      this.inputManager.endFrame();

      // Schedule next frame
      this.animationFrameId = requestAnimationFrame(loop);
    };

    this.animationFrameId = requestAnimationFrame(loop);
  }

  // Stop the game loop completely
  stopGameLoop(): void {
    if (this.gameState === "stopped") return;

    if (this.animationFrameId !== null) {
      cancelAnimationFrame(this.animationFrameId);
      this.animationFrameId = null;
    }

    // Clear input state when game stops
    this.inputManager.clearState();

    this.gameState = "stopped";
    this.emit("game.stop");
  }

  // Pause the game loop (can be resumed)
  pauseGameLoop(): void {
    if (this.gameState !== "running") return;

    if (this.animationFrameId !== null) {
      cancelAnimationFrame(this.animationFrameId);
      this.animationFrameId = null;
    }

    this.gameState = "paused";
    this.emit("game.pause");
  }

  // Resume from pause
  resumeGameLoop(): void {
    if (this.gameState !== "paused") return;
    this.startGameLoop();
  }

  // Get current game state
  getGameState(): GameState {
    return this.gameState;
  }

  // Check if game is running
  isRunning(): boolean {
    return this.gameState === "running";
  }

  // Check if game is paused
  isPaused(): boolean {
    return this.gameState === "paused";
  }

  // Get accumulated game time in seconds
  getGameTime(): number {
    return this.accumulatedTime;
  }
}
