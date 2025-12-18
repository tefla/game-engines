import { Scene } from "./scene";

export class Renderer {
    constructor(private width: number = 40, private height: number = 20) { }

    render(scene: Scene) {
        // Create empty buffer
        const buffer: string[][] = [];
        for (let y = 0; y < this.height; y++) {
            buffer[y] = [];
            for (let x = 0; x < this.width; x++) {
                buffer[y][x] = ' ';
            }
        }

        // Draw Border
        for (let x = 0; x < this.width; x++) {
            buffer[0][x] = '#';
            buffer[this.height - 1][x] = '#';
        }
        for (let y = 0; y < this.height; y++) {
            buffer[y][0] = '#';
            buffer[y][this.width - 1] = '#';
        }

        // Draw Entities
        const centerX = Math.floor(this.width / 2);
        const centerY = Math.floor(this.height / 2);

        for (const entity of scene.getEntities()) {
            // Map world (0,0) to center
            const x = Math.floor(entity.pos.x) + centerX;
            const y = Math.floor(entity.pos.y) + centerY;

            if (x >= 0 && x < this.width && y >= 0 && y < this.height) {
                buffer[y][x] = entity.visual;
            }
        }

        // Output
        console.clear();
        console.log(buffer.map(row => row.join('')).join('\n'));
    }
}
