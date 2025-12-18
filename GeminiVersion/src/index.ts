import { Lexer } from "./lang/lexer";
import { Parser } from "./lang/parser";
import { TypeChecker } from "./lang/typechecker";
import { Interpreter } from "./lang/interpreter";
import { TokenType } from "./lang/token";
import { Vector3 } from "./engine/vector";
import { Scene } from "./engine/scene";
import { Renderer } from "./engine/renderer";
import * as fs from "fs";

const SCRIPT_PATH = "./scripts/main.flux";

// Setup Engine Natives
const makeVec3 = {
    call: (_: any, args: any[]) => new Vector3(args[0], args[1], args[2]),
    toString: () => "<native class Vector3>"
};

// Persistent Game State (Lives across reloads)
const GLOBAL_PLAYER = { pos: new Vector3(0, 0, 0) };
const SCENE = new Scene();
const RENDERER = new Renderer();

// 4. Interpret (Persistent Interpreter)
const INTERPRETER = new Interpreter(SCENE);
INTERPRETER.globals.define("Vec3", makeVec3);
INTERPRETER.globals.define("Player", GLOBAL_PLAYER);

function runScript(source: string) {
    console.log("--- Reloading Script ---");
    try {
        // 1. Lex
        const lexer = new Lexer(source);
        const tokens = lexer.scanTokens();

        // 2. Parse
        const parser = new Parser(tokens);
        const statements = parser.parse();

        // 3. Update Definitions (Re-run script to update function defs and global vars)
        // Note: Re-running global var decls might reset them. 
        // Ideal: Only update functions. But for now, user accepts reload resets if vars are in script.
        // GLOBAL_PLAYER key persists.
        INTERPRETER.interpret(statements);

        console.log("--- Script Updated ---");
    } catch (e) {
        console.error("Script Error:", e);
    }
}

// Ensure scripts dir exists
if (!fs.existsSync("./scripts")) {
    fs.mkdirSync("./scripts");
}

// Initial Run
if (fs.existsSync(SCRIPT_PATH)) {
    runScript(fs.readFileSync(SCRIPT_PATH, "utf-8"));
} else {
    console.log(`Waiting for ${SCRIPT_PATH} to be created...`);
}

// Watch
console.log(`Watching ${SCRIPT_PATH}...`);
fs.watch("./scripts", (eventType, filename) => {
    if (filename === "main.flux" && eventType === "change") {
        setTimeout(() => {
            try {
                const content = fs.readFileSync(SCRIPT_PATH, "utf-8");
                runScript(content);
            } catch (e) {
                // ignore read errors
            }
        }, 100);
    }
});

// Game Loop
setInterval(() => {
    // 1. Clear Scene
    SCENE.clear();

    // 2. Mock Input (TODO: Real Input)

    // 3. Call "Tick"
    try {
        const tick = INTERPRETER.globals.get({ lexeme: "Tick", type: TokenType.IDENTIFIER, line: 0 } as any);
        if (tick && tick.call) {
            tick.call(INTERPRETER, []);
        }
    } catch (e) {
        // Tick might not exist yet
    }

    // 4. Render
    RENDERER.render(SCENE);

}, 100); // 10 FPS
