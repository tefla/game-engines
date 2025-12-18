export type SignalHandler = (data: unknown) => void | Promise<void>;

export class Signals {
  private readonly handlersBySignal = new Map<string, SignalHandler[]>();

  on(signal: string, handler: SignalHandler): () => void {
    const handlers = this.handlersBySignal.get(signal) ?? [];
    handlers.push(handler);
    this.handlersBySignal.set(signal, handlers);

    return () => {
      const currentHandlers = this.handlersBySignal.get(signal);
      if (!currentHandlers) return;
      const nextHandlers = currentHandlers.filter((h) => h !== handler);
      if (nextHandlers.length === 0) this.handlersBySignal.delete(signal);
      else this.handlersBySignal.set(signal, nextHandlers);
    };
  }

  async emit(signal: string, data?: unknown): Promise<void> {
    const handlers = this.handlersBySignal.get(signal);
    if (!handlers) return;
    for (const handler of handlers) {
      await handler(data);
    }
  }
}

