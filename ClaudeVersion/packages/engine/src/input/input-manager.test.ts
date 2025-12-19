import { describe, it, expect, beforeEach } from "bun:test";
import { InputManager } from "./input-manager";
import { SignalBus } from "../signals";

describe("InputManager", () => {
  let inputManager: InputManager;

  beforeEach(() => {
    inputManager = new InputManager();
  });

  describe("default bindings", () => {
    it("has default movement bindings", () => {
      const bindings = inputManager.getBindings();
      expect(bindings.has("move_forward")).toBe(true);
      expect(bindings.has("move_backward")).toBe(true);
      expect(bindings.has("move_left")).toBe(true);
      expect(bindings.has("move_right")).toBe(true);
    });

    it("has default action bindings", () => {
      const bindings = inputManager.getBindings();
      expect(bindings.has("jump")).toBe(true);
      expect(bindings.has("sprint")).toBe(true);
      expect(bindings.has("interact")).toBe(true);
    });
  });

  describe("key handling", () => {
    it("tracks key press state", () => {
      inputManager.handleKeyDown("KeyW");
      expect(inputManager.isKeyPressed("KeyW")).toBe(true);
      expect(inputManager.isKeyPressed("KeyS")).toBe(false);
    });

    it("tracks key release state", () => {
      inputManager.handleKeyDown("KeyW");
      inputManager.handleKeyUp("KeyW");
      expect(inputManager.isKeyPressed("KeyW")).toBe(false);
    });

    it("action is pressed when bound key is pressed", () => {
      inputManager.handleKeyDown("KeyW");
      expect(inputManager.isPressed("move_forward")).toBe(true);
    });

    it("action is pressed when any bound key is pressed", () => {
      inputManager.handleKeyDown("ArrowUp");
      expect(inputManager.isPressed("move_forward")).toBe(true);
    });

    it("action is not pressed when key is released", () => {
      inputManager.handleKeyDown("KeyW");
      inputManager.handleKeyUp("KeyW");
      expect(inputManager.isPressed("move_forward")).toBe(false);
    });
  });

  describe("mouse handling", () => {
    it("tracks mouse button press state", () => {
      inputManager.handleMouseDown(0);
      expect(inputManager.isMousePressed(0)).toBe(true);
      expect(inputManager.isMousePressed(2)).toBe(false);
    });

    it("tracks mouse button release state", () => {
      inputManager.handleMouseDown(0);
      inputManager.handleMouseUp(0);
      expect(inputManager.isMousePressed(0)).toBe(false);
    });

    it("action is pressed when bound mouse button is pressed", () => {
      inputManager.handleMouseDown(0);
      expect(inputManager.isPressed("primary_action")).toBe(true);
    });
  });

  describe("just pressed/released detection", () => {
    it("detects just pressed on first frame", () => {
      inputManager.handleKeyDown("Space");
      expect(inputManager.wasJustPressed("jump")).toBe(true);
    });

    it("just pressed is false on subsequent frames", () => {
      inputManager.handleKeyDown("Space");
      inputManager.endFrame();
      expect(inputManager.wasJustPressed("jump")).toBe(false);
    });

    it("detects just released", () => {
      inputManager.handleKeyDown("Space");
      inputManager.endFrame();
      inputManager.handleKeyUp("Space");
      expect(inputManager.wasJustReleased("jump")).toBe(true);
    });

    it("just released is false on subsequent frames", () => {
      inputManager.handleKeyDown("Space");
      inputManager.endFrame();
      inputManager.handleKeyUp("Space");
      inputManager.endFrame();
      expect(inputManager.wasJustReleased("jump")).toBe(false);
    });
  });

  describe("custom bindings", () => {
    it("can add custom bindings", () => {
      inputManager.bind("crouch", { keys: ["ControlLeft", "KeyC"] });
      inputManager.handleKeyDown("KeyC");
      expect(inputManager.isPressed("crouch")).toBe(true);
    });

    it("can remove bindings", () => {
      inputManager.unbind("jump");
      inputManager.handleKeyDown("Space");
      expect(inputManager.isPressed("jump")).toBe(false);
    });

    it("can override default bindings", () => {
      inputManager.bind("jump", { keys: ["KeyJ"] });
      inputManager.handleKeyDown("Space");
      expect(inputManager.isPressed("jump")).toBe(false);
      inputManager.handleKeyDown("KeyJ");
      expect(inputManager.isPressed("jump")).toBe(true);
    });
  });

  describe("axis input", () => {
    it("horizontal axis is 0 when no keys pressed", () => {
      expect(inputManager.getHorizontalAxis()).toBe(0);
    });

    it("horizontal axis is -1 when left pressed", () => {
      inputManager.handleKeyDown("KeyA");
      expect(inputManager.getHorizontalAxis()).toBe(-1);
    });

    it("horizontal axis is 1 when right pressed", () => {
      inputManager.handleKeyDown("KeyD");
      expect(inputManager.getHorizontalAxis()).toBe(1);
    });

    it("horizontal axis is 0 when both left and right pressed", () => {
      inputManager.handleKeyDown("KeyA");
      inputManager.handleKeyDown("KeyD");
      expect(inputManager.getHorizontalAxis()).toBe(0);
    });

    it("vertical axis is -1 when backward pressed", () => {
      inputManager.handleKeyDown("KeyS");
      expect(inputManager.getVerticalAxis()).toBe(-1);
    });

    it("vertical axis is 1 when forward pressed", () => {
      inputManager.handleKeyDown("KeyW");
      expect(inputManager.getVerticalAxis()).toBe(1);
    });
  });

  describe("input state", () => {
    it("returns full input state for action", () => {
      inputManager.handleKeyDown("Space");
      const state = inputManager.getInputState("jump");
      expect(state.pressed).toBe(true);
      expect(state.justPressed).toBe(true);
      expect(state.justReleased).toBe(false);
      expect(state.duration).toBe(0);
    });

    it("tracks duration of held input", () => {
      inputManager.handleKeyDown("Space");
      inputManager.update(0.1);
      let state = inputManager.getInputState("jump");
      expect(state.duration).toBeCloseTo(0.1, 5);

      inputManager.update(0.2);
      state = inputManager.getInputState("jump");
      expect(state.duration).toBeCloseTo(0.3, 5);
    });

    it("resets duration when released", () => {
      inputManager.handleKeyDown("Space");
      inputManager.update(0.5);
      inputManager.handleKeyUp("Space");
      inputManager.update(0.1);
      const state = inputManager.getInputState("jump");
      expect(state.duration).toBe(0);
    });
  });

  describe("signal emission", () => {
    it("emits signal when action is pressed", () => {
      const signalBus = new SignalBus();
      inputManager.setSignalBus(signalBus);

      let receivedSignal = false;
      signalBus.on(["input", "jump"], () => {
        receivedSignal = true;
      });

      inputManager.handleKeyDown("Space");
      expect(receivedSignal).toBe(true);
    });

    it("emits pressed signal with action details", () => {
      const signalBus = new SignalBus();
      inputManager.setSignalBus(signalBus);

      let receivedData: any = null;
      signalBus.on(["input", "jump", "pressed"], (data) => {
        receivedData = data;
      });

      inputManager.handleKeyDown("Space");
      expect(receivedData).not.toBeNull();
      expect(receivedData.fields.get("action")?.value).toBe("jump");
      expect(receivedData.fields.get("type")?.value).toBe("pressed");
    });

    it("emits released signal when key released", () => {
      const signalBus = new SignalBus();
      inputManager.setSignalBus(signalBus);

      let receivedType = "";
      signalBus.on(["input", "jump", "released"], (data: any) => {
        receivedType = data.fields.get("type")?.value;
      });

      inputManager.handleKeyDown("Space");
      inputManager.handleKeyUp("Space");
      expect(receivedType).toBe("released");
    });
  });

  describe("clear state", () => {
    it("clears all input state", () => {
      inputManager.handleKeyDown("KeyW");
      inputManager.handleMouseDown(0);
      inputManager.update(1.0);

      inputManager.clearState();

      expect(inputManager.isPressed("move_forward")).toBe(false);
      expect(inputManager.isPressed("primary_action")).toBe(false);
      expect(inputManager.getInputState("move_forward").duration).toBe(0);
    });
  });
});
