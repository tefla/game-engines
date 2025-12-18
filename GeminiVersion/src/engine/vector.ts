
export class Vector3 {
    constructor(
        public x: number,
        public y: number,
        public z: number
    ) { }

    // Flux Operator Overloads
    _add(other: Vector3): Vector3 {
        return new Vector3(this.x + other.x, this.y + other.y, this.z + other.z);
    }

    toString(): string {
        return `Vec3(${this.x}, ${this.y}, ${this.z})`;
    }
}
