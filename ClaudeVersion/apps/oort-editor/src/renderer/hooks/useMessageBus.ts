import { useEffect, useCallback, useRef } from "react";
import { messageBus, Channel } from "@/core/message-bus";

/**
 * Hook to subscribe to a message bus channel
 * Automatically unsubscribes on unmount
 */
export function useMessageBus(
  channel: string | Channel,
  handler: (data: any) => void
): void {
  // Use ref to avoid re-subscribing when handler changes
  const handlerRef = useRef(handler);
  handlerRef.current = handler;

  useEffect(() => {
    const unsubscribe = messageBus.on(channel, (data) => {
      handlerRef.current(data);
    });

    return unsubscribe;
  }, [channel]);
}

/**
 * Hook to subscribe to multiple channels
 */
export function useMessageBusMulti(
  subscriptions: Array<{ channel: string; handler: (data: any) => void }>
): void {
  useEffect(() => {
    const unsubscribers = subscriptions.map(({ channel, handler }) =>
      messageBus.on(channel, handler)
    );

    return () => {
      unsubscribers.forEach((unsub) => unsub());
    };
  }, [subscriptions.map((s) => s.channel).join(",")]);
}

/**
 * Hook to get an emit function
 * Useful for components that only emit, don't subscribe
 */
export function useEmit(): (channel: string, data?: any) => void {
  return useCallback((channel: string, data?: any) => {
    messageBus.emit(channel, data);
  }, []);
}

/**
 * Hook to get both subscribe and emit
 */
export function useChannel(channel: string) {
  const emit = useCallback(
    (data?: any) => {
      messageBus.emit(channel, data);
    },
    [channel]
  );

  const subscribe = useCallback(
    (handler: (data: any) => void) => {
      return messageBus.on(channel, handler);
    },
    [channel]
  );

  return { emit, subscribe };
}

/**
 * Hook for console logging via message bus
 */
export function useConsole() {
  const emit = useEmit();

  return {
    log: (text: string, source?: string) =>
      emit("console:log", { text, source }),
    error: (text: string, source?: string) =>
      emit("console:error", { text, source }),
    warn: (text: string, source?: string) =>
      emit("console:warn", { text, source }),
    info: (text: string, source?: string) =>
      emit("console:info", { text, source }),
    clear: () => emit("console:clear"),
  };
}

/**
 * Hook for selection management
 */
export function useSelection() {
  const emit = useEmit();

  return {
    select: (target: any, type?: string) =>
      emit("selection:changed", { target, type }),
    deselect: () => emit("selection:changed", { target: null }),
  };
}

export { messageBus };
