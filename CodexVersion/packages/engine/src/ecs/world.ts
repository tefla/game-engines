export type EntityId = string;

export class World<T> {
  private nextId = 1;
  private readonly entities = new Map<EntityId, T>();

  spawn(value: T): EntityId {
    const id = `e${this.nextId++}`;
    this.entities.set(id, value);
    return id;
  }

  destroy(id: EntityId): boolean {
    return this.entities.delete(id);
  }

  get(id: EntityId): T | undefined {
    return this.entities.get(id);
  }

  has(id: EntityId): boolean {
    return this.entities.has(id);
  }

  size(): number {
    return this.entities.size;
  }

  clear(): void {
    this.entities.clear();
  }

  entries(): IterableIterator<[EntityId, T]> {
    return this.entities.entries();
  }

  values(): IterableIterator<T> {
    return this.entities.values();
  }

  find(predicate: (value: T) => boolean): T[] {
    const out: T[] = [];
    for (const value of this.entities.values()) {
      if (predicate(value)) out.push(value);
    }
    return out;
  }
}

