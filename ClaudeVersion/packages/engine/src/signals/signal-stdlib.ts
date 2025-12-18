// Signal Standard Library Functions
// These functions are bound to a SignalBus instance

import {
  type SlateValue,
  type SlateNativeFunction,
  Str,
  Bool,
  Null,
  List,
  Record,
  Num,
  isString,
  isList,
  isCallable,
  RuntimeError,
} from "@oort/core";

import { type SignalBus, type SignalPath } from "./signal-bus";

// Helper to create a native function
function native(
  name: string,
  arity: number | "variadic",
  fn: (args: SlateValue[]) => SlateValue
): SlateNativeFunction {
  return { type: "native", name, arity, fn };
}

// Helper to assert argument types
function assertString(v: SlateValue, name: string): string {
  if (!isString(v)) {
    throw new RuntimeError(`${name} expects a string, got ${v.type}`);
  }
  return v.value;
}

// Parse signal path from string or list
function parseSignalPath(v: SlateValue, name: string): SignalPath {
  if (isString(v)) {
    return v.value.split(".");
  }
  if (isList(v)) {
    return v.elements.map((e) => {
      if (!isString(e)) {
        throw new RuntimeError(`${name}: signal path elements must be strings`);
      }
      return e.value;
    });
  }
  throw new RuntimeError(`${name} expects a string or list for signal path`);
}

// Convert SignalEvent to Slate record
function eventToRecord(event: {
  signal: SignalPath;
  data: SlateValue;
  timestamp: number;
}): SlateValue {
  const fields = new Map<string, SlateValue>();
  fields.set("signal", Str(event.signal.join(".")));
  fields.set("data", event.data);
  fields.set("timestamp", Num(event.timestamp));
  return Record(fields);
}

// Create signal stdlib functions bound to a specific SignalBus instance
// Note: Function names avoid Slate keywords (emit, on are keywords)
export function createSignalStdlib(
  bus: SignalBus
): Map<string, SlateNativeFunction> {
  // Note: "emit" is a keyword in Slate, so we use "signal" as the function name
  // Usage: signal("path.to.signal", data)
  const signalFn = native("signal", "variadic", (args) => {
    if (args.length < 1) {
      throw new RuntimeError("signal requires a signal path");
    }
    const signal = parseSignalPath(args[0], "signal");
    const data = args.length > 1 ? args[1] : Null();
    bus.emit(signal, data);
    return Null();
  });

  const hasListeners = native("hasListeners", 1, ([signal]) => {
    const path = parseSignalPath(signal, "hasListeners");
    return Bool(bus.hasHandlers(path));
  });

  const getSignalHistory = native("getSignalHistory", "variadic", (args) => {
    const limit = args.length > 0 ? (args[0] as any).value : undefined;
    const history = bus.getHistory(limit);
    return List(history.map(eventToRecord));
  });

  const clearSignalHistory = native("clearSignalHistory", 0, () => {
    bus.clearHistory();
    return Null();
  });

  const pauseSignals = native("pauseSignals", 0, () => {
    bus.pause();
    return Null();
  });

  const resumeSignals = native("resumeSignals", 0, () => {
    bus.resume();
    return Null();
  });

  const isSignalsPaused = native("isSignalsPaused", 0, () => {
    return Bool(bus.isPaused());
  });

  const getHandlerCount = native("getHandlerCount", 0, () => {
    return Num(bus.getHandlerCount());
  });

  const removeAllHandlers = native("removeAllHandlers", "variadic", (args) => {
    if (args.length > 0) {
      const signal = parseSignalPath(args[0], "removeAllHandlers");
      bus.offAll(signal);
    } else {
      bus.offAll();
    }
    return Null();
  });

  return new Map<string, SlateNativeFunction>([
    ["signal", signalFn],
    ["hasListeners", hasListeners],
    ["getSignalHistory", getSignalHistory],
    ["clearSignalHistory", clearSignalHistory],
    ["pauseSignals", pauseSignals],
    ["resumeSignals", resumeSignals],
    ["isSignalsPaused", isSignalsPaused],
    ["getHandlerCount", getHandlerCount],
    ["removeAllHandlers", removeAllHandlers],
  ]);
}
