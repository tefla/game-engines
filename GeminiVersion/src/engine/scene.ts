import { Vector3 } from "./vector";

export class Entity {
    constructor(
        public pos: Vector3,
        public visual: string // Single char for ASCII
    ) { }
}

export class Scene {
    private entities: Entity[] = [];

    add(entity: Entity) {
        this.entities.push(entity);
    }

    clear() {
        this.entities = [];
    }

    getEntities() {
        return this.entities;
    }
}
