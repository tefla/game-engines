/**
 * Input Service - Handles keyboard and mouse input for the game runtime
 *
 * Captures input events and:
 * 1. Emits them via messageBus for UI components
 * 2. Forwards them to the game runtime as signals when playing
 */

import { messageBus } from "./message-bus";
import { runtimeService } from "./runtime-service";

export interface KeyEvent {
  key: string;
  code: string;
  shift: boolean;
  ctrl: boolean;
  alt: boolean;
  meta: boolean;
  repeat: boolean;
}

export interface MouseEvent {
  x: number;
  y: number;
  button: number;
  buttons: number;
}

export interface MouseMoveEvent extends MouseEvent {
  deltaX: number;
  deltaY: number;
}

export interface WheelEvent extends MouseEvent {
  deltaX: number;
  deltaY: number;
  deltaZ: number;
}

// Input channels for messageBus
export const InputChannels = {
  KEY_DOWN: "input:key:down",
  KEY_UP: "input:key:up",
  MOUSE_DOWN: "input:mouse:down",
  MOUSE_UP: "input:mouse:up",
  MOUSE_MOVE: "input:mouse:move",
  MOUSE_WHEEL: "input:mouse:wheel",
} as const;

class InputService {
  private isCapturing = false;
  private targetElement: HTMLElement | null = null;
  private keysPressed: Set<string> = new Set();
  private mousePosition = { x: 0, y: 0 };
  private mouseButtons: Set<number> = new Set();

  // Event handler references for cleanup
  private boundHandlers: {
    keydown?: (e: globalThis.KeyboardEvent) => void;
    keyup?: (e: globalThis.KeyboardEvent) => void;
    mousedown?: (e: globalThis.MouseEvent) => void;
    mouseup?: (e: globalThis.MouseEvent) => void;
    mousemove?: (e: globalThis.MouseEvent) => void;
    wheel?: (e: globalThis.WheelEvent) => void;
    blur?: () => void;
  } = {};

  /**
   * Start capturing input events on a target element
   * Usually the 3D viewport canvas
   */
  startCapture(target: HTMLElement): void {
    if (this.isCapturing) {
      this.stopCapture();
    }

    this.targetElement = target;
    this.isCapturing = true;

    // Create bound handlers
    this.boundHandlers = {
      keydown: this.handleKeyDown.bind(this),
      keyup: this.handleKeyUp.bind(this),
      mousedown: this.handleMouseDown.bind(this),
      mouseup: this.handleMouseUp.bind(this),
      mousemove: this.handleMouseMove.bind(this),
      wheel: this.handleWheel.bind(this),
      blur: this.handleBlur.bind(this),
    };

    // Attach key listeners to window (keys work globally when focused)
    window.addEventListener("keydown", this.boundHandlers.keydown!);
    window.addEventListener("keyup", this.boundHandlers.keyup!);
    window.addEventListener("blur", this.boundHandlers.blur!);

    // Attach mouse listeners to target element
    target.addEventListener("mousedown", this.boundHandlers.mousedown!);
    target.addEventListener("mouseup", this.boundHandlers.mouseup!);
    target.addEventListener("mousemove", this.boundHandlers.mousemove!);
    target.addEventListener("wheel", this.boundHandlers.wheel!);

    messageBus.emit("input:capture-started", { target });
  }

  /**
   * Stop capturing input events
   */
  stopCapture(): void {
    if (!this.isCapturing) return;

    // Remove key listeners
    if (this.boundHandlers.keydown) {
      window.removeEventListener("keydown", this.boundHandlers.keydown);
    }
    if (this.boundHandlers.keyup) {
      window.removeEventListener("keyup", this.boundHandlers.keyup);
    }
    if (this.boundHandlers.blur) {
      window.removeEventListener("blur", this.boundHandlers.blur);
    }

    // Remove mouse listeners
    if (this.targetElement) {
      if (this.boundHandlers.mousedown) {
        this.targetElement.removeEventListener("mousedown", this.boundHandlers.mousedown);
      }
      if (this.boundHandlers.mouseup) {
        this.targetElement.removeEventListener("mouseup", this.boundHandlers.mouseup);
      }
      if (this.boundHandlers.mousemove) {
        this.targetElement.removeEventListener("mousemove", this.boundHandlers.mousemove);
      }
      if (this.boundHandlers.wheel) {
        this.targetElement.removeEventListener("wheel", this.boundHandlers.wheel);
      }
    }

    this.isCapturing = false;
    this.targetElement = null;
    this.keysPressed.clear();
    this.mouseButtons.clear();
    this.boundHandlers = {};

    messageBus.emit("input:capture-stopped", {});
  }

  /**
   * Handle keydown events
   */
  private handleKeyDown(e: globalThis.KeyboardEvent): void {
    // Only capture when game is running
    const runtime = runtimeService.getRuntime();
    if (!runtime?.isRunning()) return;

    // Ignore repeat events for key tracking
    if (!e.repeat) {
      this.keysPressed.add(e.code);
    }

    const event: KeyEvent = {
      key: e.key,
      code: e.code,
      shift: e.shiftKey,
      ctrl: e.ctrlKey,
      alt: e.altKey,
      meta: e.metaKey,
      repeat: e.repeat,
    };

    // Emit on messageBus
    messageBus.emit(InputChannels.KEY_DOWN, event);

    // Emit to game runtime
    this.emitToRuntime("input.key.down", event);

    // Prevent default for game keys (WASD, space, arrows)
    if (this.shouldPreventDefault(e.code)) {
      e.preventDefault();
    }
  }

  /**
   * Handle keyup events
   */
  private handleKeyUp(e: globalThis.KeyboardEvent): void {
    const runtime = runtimeService.getRuntime();
    if (!runtime?.isRunning()) return;

    this.keysPressed.delete(e.code);

    const event: KeyEvent = {
      key: e.key,
      code: e.code,
      shift: e.shiftKey,
      ctrl: e.ctrlKey,
      alt: e.altKey,
      meta: e.metaKey,
      repeat: false,
    };

    messageBus.emit(InputChannels.KEY_UP, event);
    this.emitToRuntime("input.key.up", event);
  }

  /**
   * Handle mousedown events
   */
  private handleMouseDown(e: globalThis.MouseEvent): void {
    const runtime = runtimeService.getRuntime();
    if (!runtime?.isRunning()) return;

    this.mouseButtons.add(e.button);
    this.mousePosition = { x: e.offsetX, y: e.offsetY };

    const event: MouseEvent = {
      x: e.offsetX,
      y: e.offsetY,
      button: e.button,
      buttons: e.buttons,
    };

    messageBus.emit(InputChannels.MOUSE_DOWN, event);
    this.emitToRuntime("input.mouse.down", event);
  }

  /**
   * Handle mouseup events
   */
  private handleMouseUp(e: globalThis.MouseEvent): void {
    const runtime = runtimeService.getRuntime();
    if (!runtime?.isRunning()) return;

    this.mouseButtons.delete(e.button);
    this.mousePosition = { x: e.offsetX, y: e.offsetY };

    const event: MouseEvent = {
      x: e.offsetX,
      y: e.offsetY,
      button: e.button,
      buttons: e.buttons,
    };

    messageBus.emit(InputChannels.MOUSE_UP, event);
    this.emitToRuntime("input.mouse.up", event);
  }

  /**
   * Handle mousemove events
   */
  private handleMouseMove(e: globalThis.MouseEvent): void {
    const runtime = runtimeService.getRuntime();
    if (!runtime?.isRunning()) return;

    const deltaX = e.offsetX - this.mousePosition.x;
    const deltaY = e.offsetY - this.mousePosition.y;
    this.mousePosition = { x: e.offsetX, y: e.offsetY };

    const event: MouseMoveEvent = {
      x: e.offsetX,
      y: e.offsetY,
      deltaX,
      deltaY,
      button: e.button,
      buttons: e.buttons,
    };

    messageBus.emit(InputChannels.MOUSE_MOVE, event);
    this.emitToRuntime("input.mouse.move", event);
  }

  /**
   * Handle wheel events
   */
  private handleWheel(e: globalThis.WheelEvent): void {
    const runtime = runtimeService.getRuntime();
    if (!runtime?.isRunning()) return;

    const event: WheelEvent = {
      x: e.offsetX,
      y: e.offsetY,
      deltaX: e.deltaX,
      deltaY: e.deltaY,
      deltaZ: e.deltaZ,
      button: e.button,
      buttons: e.buttons,
    };

    messageBus.emit(InputChannels.MOUSE_WHEEL, event);
    this.emitToRuntime("input.mouse.wheel", event);
  }

  /**
   * Handle window blur - release all keys
   */
  private handleBlur(): void {
    this.keysPressed.clear();
    this.mouseButtons.clear();
  }

  /**
   * Emit an input event to the game runtime
   */
  private emitToRuntime(signal: string, data: Record<string, any>): void {
    const runtime = runtimeService.getRuntime();
    if (!runtime) return;

    // Convert JS object to Slate record
    const fields = new Map<string, any>();
    for (const [key, value] of Object.entries(data)) {
      if (typeof value === "string") {
        fields.set(key, { type: "string", value });
      } else if (typeof value === "number") {
        fields.set(key, { type: "number", value });
      } else if (typeof value === "boolean") {
        fields.set(key, { type: "bool", value });
      }
    }

    runtime.emit(signal, { type: "record", fields });
  }

  /**
   * Check if a key is currently pressed
   */
  isKeyPressed(code: string): boolean {
    return this.keysPressed.has(code);
  }

  /**
   * Check if a mouse button is currently pressed
   */
  isMouseButtonPressed(button: number): boolean {
    return this.mouseButtons.has(button);
  }

  /**
   * Get current mouse position
   */
  getMousePosition(): { x: number; y: number } {
    return { ...this.mousePosition };
  }

  /**
   * Get all currently pressed keys
   */
  getPressedKeys(): string[] {
    return Array.from(this.keysPressed);
  }

  /**
   * Check if input capture is active
   */
  isActive(): boolean {
    return this.isCapturing;
  }

  /**
   * Determine if we should prevent default for certain keys
   */
  private shouldPreventDefault(code: string): boolean {
    const gameKeys = [
      "KeyW",
      "KeyA",
      "KeyS",
      "KeyD",
      "Space",
      "ArrowUp",
      "ArrowDown",
      "ArrowLeft",
      "ArrowRight",
      "Tab",
    ];
    return gameKeys.includes(code);
  }
}

// Singleton instance
export const inputService = new InputService();
