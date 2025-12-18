export class Input {
    private static keys: Set<string> = new Set();

    static setKey(key: string, pressed: boolean) {
        if (pressed) this.keys.add(key);
        else this.keys.delete(key);
    }

    static isKeyDown(key: string): boolean {
        return this.keys.has(key);
    }
}
