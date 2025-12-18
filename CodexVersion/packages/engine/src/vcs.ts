export type Snapshot<T> = {
  readonly label?: string;
  readonly state: T;
};

export class History<T> {
  private readonly past: Snapshot<T>[] = [];
  private readonly future: Snapshot<T>[] = [];

  commit(snapshot: Snapshot<T>): void {
    this.past.push(snapshot);
    this.future.length = 0;
  }

  undo(): Snapshot<T> | undefined {
    if (this.past.length <= 1) return this.past.at(-1);
    const current = this.past.pop();
    if (!current) return undefined;
    this.future.push(current);
    return this.past.at(-1);
  }

  redo(): Snapshot<T> | undefined {
    const next = this.future.pop();
    if (!next) return undefined;
    this.past.push(next);
    return next;
  }

  current(): Snapshot<T> | undefined {
    return this.past.at(-1);
  }
}
