// Input System Stdlib - Slate functions for input handling

import type { SlateValue, SlateNativeFunction, SlateList } from "@oort/core";
import { Num, Bool, Str, Null, Record, List, isList, isString, isNumber } from "@oort/core";
import type { InputManager } from "./input-manager";

/**
 * Create input stdlib functions bound to an InputManager instance
 */
export function createInputStdlib(
  inputManager: InputManager
): Map<string, SlateNativeFunction> {
  return new Map([
    // bindInput(action, keys) - Bind an action to keys
    [
      "bindInput",
      {
        type: "native",
        name: "bindInput",
        arity: 2,
        fn: (args: SlateValue[]) => {
          const action = args[0];
          const keys = args[1];

          if (!isString(action)) {
            throw new Error("bindInput: action must be a string");
          }

          if (!isList(keys)) {
            throw new Error("bindInput: keys must be a list");
          }

          const keyStrings = keys.elements
            .filter(isString)
            .map((k) => k.value);

          inputManager.bind(action.value, { keys: keyStrings });
          return Null();
        },
      },
    ],

    // unbindInput(action) - Remove an action binding
    [
      "unbindInput",
      {
        type: "native",
        name: "unbindInput",
        arity: 1,
        fn: (args: SlateValue[]) => {
          const action = args[0];
          if (!isString(action)) {
            throw new Error("unbindInput: action must be a string");
          }
          inputManager.unbind(action.value);
          return Null();
        },
      },
    ],

    // isPressed(action) - Check if an action is currently pressed
    [
      "isPressed",
      {
        type: "native",
        name: "isPressed",
        arity: 1,
        fn: (args: SlateValue[]) => {
          const action = args[0];
          if (!isString(action)) {
            throw new Error("isPressed: action must be a string");
          }
          return Bool(inputManager.isPressed(action.value));
        },
      },
    ],

    // wasJustPressed(action) - Check if an action was just pressed this frame
    [
      "wasJustPressed",
      {
        type: "native",
        name: "wasJustPressed",
        arity: 1,
        fn: (args: SlateValue[]) => {
          const action = args[0];
          if (!isString(action)) {
            throw new Error("wasJustPressed: action must be a string");
          }
          return Bool(inputManager.wasJustPressed(action.value));
        },
      },
    ],

    // wasJustReleased(action) - Check if an action was just released this frame
    [
      "wasJustReleased",
      {
        type: "native",
        name: "wasJustReleased",
        arity: 1,
        fn: (args: SlateValue[]) => {
          const action = args[0];
          if (!isString(action)) {
            throw new Error("wasJustReleased: action must be a string");
          }
          return Bool(inputManager.wasJustReleased(action.value));
        },
      },
    ],

    // getInputState(action) - Get full input state for an action
    [
      "getInputState",
      {
        type: "native",
        name: "getInputState",
        arity: 1,
        fn: (args: SlateValue[]) => {
          const action = args[0];
          if (!isString(action)) {
            throw new Error("getInputState: action must be a string");
          }
          const state = inputManager.getInputState(action.value);
          return Record(
            new Map([
              ["pressed", Bool(state.pressed)],
              ["justPressed", Bool(state.justPressed)],
              ["justReleased", Bool(state.justReleased)],
              ["duration", Num(state.duration)],
            ])
          );
        },
      },
    ],

    // getHorizontalAxis() - Get horizontal movement axis (-1 to 1)
    [
      "getHorizontalAxis",
      {
        type: "native",
        name: "getHorizontalAxis",
        arity: 0,
        fn: () => Num(inputManager.getHorizontalAxis()),
      },
    ],

    // getVerticalAxis() - Get vertical movement axis (-1 to 1)
    [
      "getVerticalAxis",
      {
        type: "native",
        name: "getVerticalAxis",
        arity: 0,
        fn: () => Num(inputManager.getVerticalAxis()),
      },
    ],

    // getMoveVector() - Get movement as vec3 (horizontal, 0, vertical)
    [
      "getMoveVector",
      {
        type: "native",
        name: "getMoveVector",
        arity: 0,
        fn: () => {
          const h = inputManager.getHorizontalAxis();
          const v = inputManager.getVerticalAxis();
          return Record(
            new Map([
              ["x", Num(h)],
              ["y", Num(0)],
              ["z", Num(v)],
            ])
          );
        },
      },
    ],

    // isKeyPressed(keyCode) - Check if a specific key is pressed
    [
      "isKeyPressed",
      {
        type: "native",
        name: "isKeyPressed",
        arity: 1,
        fn: (args: SlateValue[]) => {
          const keyCode = args[0];
          if (!isString(keyCode)) {
            throw new Error("isKeyPressed: keyCode must be a string");
          }
          return Bool(inputManager.isKeyPressed(keyCode.value));
        },
      },
    ],

    // isMousePressed(button) - Check if a mouse button is pressed
    [
      "isMousePressed",
      {
        type: "native",
        name: "isMousePressed",
        arity: 1,
        fn: (args: SlateValue[]) => {
          const button = args[0];
          if (!isNumber(button)) {
            throw new Error("isMousePressed: button must be a number");
          }
          return Bool(inputManager.isMousePressed(Math.floor(button.value)));
        },
      },
    ],
  ]);
}
