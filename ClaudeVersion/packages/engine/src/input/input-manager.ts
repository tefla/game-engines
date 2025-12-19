// Input Manager - Handles action bindings and input state

import type { SignalBus } from "../signals";

export interface InputBinding {
  keys: string[]; // Key codes like "KeyW", "Space", "ArrowUp", "GamepadA"
  mouse?: number[]; // Mouse buttons (0=left, 1=middle, 2=right)
}

export type InputState = {
  pressed: boolean;
  justPressed: boolean;
  justReleased: boolean;
  duration: number; // How long the input has been held (seconds)
};

export class InputManager {
  private bindings: Map<string, InputBinding> = new Map();
  private keyStates: Map<string, boolean> = new Map();
  private previousKeyStates: Map<string, boolean> = new Map();
  private mouseStates: Map<number, boolean> = new Map();
  private previousMouseStates: Map<number, boolean> = new Map();
  private actionHeldTime: Map<string, number> = new Map();
  private signalBus: SignalBus | null = null;
  private deltaTime: number = 0;

  constructor() {
    // Initialize with common default bindings
    this.bind("move_forward", { keys: ["KeyW", "ArrowUp"] });
    this.bind("move_backward", { keys: ["KeyS", "ArrowDown"] });
    this.bind("move_left", { keys: ["KeyA", "ArrowLeft"] });
    this.bind("move_right", { keys: ["KeyD", "ArrowRight"] });
    this.bind("jump", { keys: ["Space"] });
    this.bind("sprint", { keys: ["ShiftLeft", "ShiftRight"] });
    this.bind("interact", { keys: ["KeyE", "KeyF"] });
    this.bind("primary_action", { keys: [], mouse: [0] });
    this.bind("secondary_action", { keys: [], mouse: [2] });
  }

  /**
   * Connect to signal bus for emitting input signals
   */
  setSignalBus(signalBus: SignalBus): void {
    this.signalBus = signalBus;
  }

  /**
   * Bind an action to one or more keys/buttons
   */
  bind(action: string, binding: InputBinding): void {
    this.bindings.set(action, binding);
    this.actionHeldTime.set(action, 0);
  }

  /**
   * Unbind an action
   */
  unbind(action: string): void {
    this.bindings.delete(action);
    this.actionHeldTime.delete(action);
  }

  /**
   * Get all bindings for debugging
   */
  getBindings(): Map<string, InputBinding> {
    return new Map(this.bindings);
  }

  /**
   * Handle key down event
   */
  handleKeyDown(code: string): void {
    const wasPressed = this.keyStates.get(code) ?? false;
    this.keyStates.set(code, true);

    // Only emit signals on the initial press
    if (!wasPressed) {
      this.emitActionSignals(code, "pressed");
    }
  }

  /**
   * Handle key up event
   */
  handleKeyUp(code: string): void {
    this.keyStates.set(code, false);
    this.emitActionSignals(code, "released");
  }

  /**
   * Handle mouse down event
   */
  handleMouseDown(button: number): void {
    const wasPressed = this.mouseStates.get(button) ?? false;
    this.mouseStates.set(button, true);

    if (!wasPressed) {
      this.emitMouseActionSignals(button, "pressed");
    }
  }

  /**
   * Handle mouse up event
   */
  handleMouseUp(button: number): void {
    this.mouseStates.set(button, false);
    this.emitMouseActionSignals(button, "released");
  }

  /**
   * Emit signals for actions bound to a key
   */
  private emitActionSignals(
    code: string,
    type: "pressed" | "released"
  ): void {
    if (!this.signalBus) return;

    for (const [action, binding] of this.bindings) {
      if (binding.keys.includes(code)) {
        const signalPath = ["input", action, type];
        this.signalBus.emit(signalPath, {
          type: "record",
          fields: new Map([
            ["action", { type: "string", value: action }],
            ["key", { type: "string", value: code }],
            ["type", { type: "string", value: type }],
          ]),
        });

        // Also emit generic input.<action> signal on press
        if (type === "pressed") {
          this.signalBus.emit(["input", action], {
            type: "record",
            fields: new Map([
              ["action", { type: "string", value: action }],
              ["key", { type: "string", value: code }],
            ]),
          });
        }
      }
    }
  }

  /**
   * Emit signals for actions bound to a mouse button
   */
  private emitMouseActionSignals(
    button: number,
    type: "pressed" | "released"
  ): void {
    if (!this.signalBus) return;

    for (const [action, binding] of this.bindings) {
      if (binding.mouse?.includes(button)) {
        const signalPath = ["input", action, type];
        this.signalBus.emit(signalPath, {
          type: "record",
          fields: new Map([
            ["action", { type: "string", value: action }],
            ["button", { type: "number", value: button }],
            ["type", { type: "string", value: type }],
          ]),
        });

        if (type === "pressed") {
          this.signalBus.emit(["input", action], {
            type: "record",
            fields: new Map([
              ["action", { type: "string", value: action }],
              ["button", { type: "number", value: button }],
            ]),
          });
        }
      }
    }
  }

  /**
   * Check if an action is currently pressed
   */
  isPressed(action: string): boolean {
    const binding = this.bindings.get(action);
    if (!binding) return false;

    // Check keys
    for (const key of binding.keys) {
      if (this.keyStates.get(key)) return true;
    }

    // Check mouse buttons
    if (binding.mouse) {
      for (const button of binding.mouse) {
        if (this.mouseStates.get(button)) return true;
      }
    }

    return false;
  }

  /**
   * Check if an action was just pressed this frame
   */
  wasJustPressed(action: string): boolean {
    const binding = this.bindings.get(action);
    if (!binding) return false;

    // Check keys
    for (const key of binding.keys) {
      const current = this.keyStates.get(key) ?? false;
      const previous = this.previousKeyStates.get(key) ?? false;
      if (current && !previous) return true;
    }

    // Check mouse
    if (binding.mouse) {
      for (const button of binding.mouse) {
        const current = this.mouseStates.get(button) ?? false;
        const previous = this.previousMouseStates.get(button) ?? false;
        if (current && !previous) return true;
      }
    }

    return false;
  }

  /**
   * Check if an action was just released this frame
   */
  wasJustReleased(action: string): boolean {
    const binding = this.bindings.get(action);
    if (!binding) return false;

    // Check keys
    for (const key of binding.keys) {
      const current = this.keyStates.get(key) ?? false;
      const previous = this.previousKeyStates.get(key) ?? false;
      if (!current && previous) return true;
    }

    // Check mouse
    if (binding.mouse) {
      for (const button of binding.mouse) {
        const current = this.mouseStates.get(button) ?? false;
        const previous = this.previousMouseStates.get(button) ?? false;
        if (!current && previous) return true;
      }
    }

    return false;
  }

  /**
   * Get full input state for an action
   */
  getInputState(action: string): InputState {
    return {
      pressed: this.isPressed(action),
      justPressed: this.wasJustPressed(action),
      justReleased: this.wasJustReleased(action),
      duration: this.actionHeldTime.get(action) ?? 0,
    };
  }

  /**
   * Get horizontal axis value (-1 to 1)
   */
  getHorizontalAxis(): number {
    let axis = 0;
    if (this.isPressed("move_left")) axis -= 1;
    if (this.isPressed("move_right")) axis += 1;
    return axis;
  }

  /**
   * Get vertical axis value (-1 to 1)
   */
  getVerticalAxis(): number {
    let axis = 0;
    if (this.isPressed("move_backward")) axis -= 1;
    if (this.isPressed("move_forward")) axis += 1;
    return axis;
  }

  /**
   * Called each frame to update input state
   * Should be called before game logic
   */
  update(deltaTime: number): void {
    this.deltaTime = deltaTime;

    // Update held time for pressed actions
    for (const [action] of this.bindings) {
      if (this.isPressed(action)) {
        const current = this.actionHeldTime.get(action) ?? 0;
        this.actionHeldTime.set(action, current + deltaTime);
      } else {
        this.actionHeldTime.set(action, 0);
      }
    }
  }

  /**
   * Called at the end of each frame to store previous state
   * Should be called after game logic
   */
  endFrame(): void {
    // Copy current states to previous
    this.previousKeyStates = new Map(this.keyStates);
    this.previousMouseStates = new Map(this.mouseStates);
  }

  /**
   * Check if a specific key is pressed (by code)
   */
  isKeyPressed(code: string): boolean {
    return this.keyStates.get(code) ?? false;
  }

  /**
   * Check if a specific mouse button is pressed
   */
  isMousePressed(button: number): boolean {
    return this.mouseStates.get(button) ?? false;
  }

  /**
   * Clear all input state (useful when losing focus)
   */
  clearState(): void {
    this.keyStates.clear();
    this.mouseStates.clear();
    this.previousKeyStates.clear();
    this.previousMouseStates.clear();
    for (const [action] of this.bindings) {
      this.actionHeldTime.set(action, 0);
    }
  }
}
