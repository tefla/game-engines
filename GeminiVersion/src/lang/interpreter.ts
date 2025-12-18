import * as Stmt from "./ast";
import * as Expr from "./ast";
import { Token, TokenType } from "./token";
import { Input } from "../engine/input";
import { Entity, Scene } from "../engine/scene";
import { Vector3 } from "../engine/vector";

export class Environment {
    public values: Record<string, any> = {};
    public parent: Environment | null = null;

    constructor(parent: Environment | null = null) {
        this.parent = parent;
    }

    define(name: string, value: any) {
        this.values[name] = value;
    }

    get(name: Token): any {
        if (name.lexeme in this.values) return this.values[name.lexeme];
        if (this.parent) return this.parent.get(name);
        throw new Error(`Undefined variable '${name.lexeme}'.`);
    }

    assign(name: Token, value: any) {
        if (name.lexeme in this.values) {
            this.values[name.lexeme] = value;
            return;
        }
        if (this.parent) {
            this.parent.assign(name, value);
            return;
        }
        throw new Error(`Undefined variable '${name.lexeme}'.`);
    }
}

export class Interpreter implements Stmt.Visitor<void>, Expr.Visitor<any> {
    public globals = new Environment();
    private environment = this.globals;

    constructor(private scene?: Scene) {
        // Native Print
        this.globals.define("print", {
            call: (_: any, args: any[]) => console.log(...args),
            toString: () => "<native fn>"
        });

        // Native Input
        this.globals.define("Input", {
            get: (name: Token) => {
                // If accessing property? No, Input is likely a Class or Object.
                // We want Input.isKeyDown("w")
                // In Flux: Input.isKeyDown("w")
                return Input;
            },
            // Hack: Expose static methods via an object
            isKeyDown: (_: any, args: any[]) => Input.isKeyDown(args[0]),
            call: (_: any, args: any[]) => { throw new Error("Input is not a function"); }
        });
        // Better way: Define an object with methods
        this.globals.define("Input", {
            call: (_interpreter: any, _args: any[]) => null, // Not callable
            // We need to implement Property Access on Native Objects better?
            // Or just expose functions Input_isKeyDown.
            // Let's rely on `visitGetExpr` handling objects.
            // If I return a JS object, `visitGetExpr` does `object[name]`.
            // So:
            isKeyDown: (key: string) => Input.isKeyDown(key)
        });

        // Native Entity
        this.globals.define("Entity", {
            call: (_: any, args: any[]) => new Entity(args[0], args[1])
        });

        // Native Scene (Global Game Object)
        this.globals.define("Game", {
            add: (entity: Entity) => this.scene?.add(entity),
            clear: () => this.scene?.clear()
        });
    }

    interpret(statements: Stmt.Stmt[]) {
        try {
            for (const stmt of statements) {
                if (stmt) stmt.accept(this);
            }
        } catch (e) {
            console.error("Runtime Error:", e);
        }
    }

    visitBlockStmt(stmt: Stmt.Block): void {
        this.executeBlock(stmt.statements, new Environment(this.environment));
    }

    executeBlock(statements: Stmt.Stmt[], env: Environment) {
        const previous = this.environment;
        try {
            this.environment = env;
            for (const statement of statements) {
                if (statement) statement.accept(this);
            }
        } finally {
            this.environment = previous;
        }
    }

    visitVarStmt(stmt: Stmt.Var): void {
        let value = null;
        if (stmt.initializer) {
            value = stmt.initializer.accept(this);
        }
        this.environment.define(stmt.name.lexeme, value);
    }

    visitFunctionStmt(stmt: Stmt.Function): void {
        // Create a callable object that captures the current env (closure)
        const func = {
            call: (interpreter: Interpreter, args: any[]) => {
                const env = new Environment(interpreter.environment);
                for (let i = 0; i < stmt.params.length; i++) {
                    env.define(stmt.params[i].name.lexeme, args[i]);
                }
                interpreter.executeBlock(stmt.body, env);
            }
        };
        this.environment.define(stmt.name.lexeme, func);
    }

    visitExpressionStmt(stmt: Stmt.Expression): void {
        stmt.expression.accept(this);
    }

    visitIfStmt(stmt: Stmt.If): void {
        if (this.isTruthy(stmt.condition.accept(this))) {
            stmt.thenBranch.accept(this);
        } else if (stmt.elseBranch) {
            stmt.elseBranch.accept(this);
        }
    }

    visitWhileStmt(stmt: Stmt.While): void {
        while (this.isTruthy(stmt.condition.accept(this))) {
            stmt.body.accept(this);
        }
    }

    visitReturnStmt(stmt: Stmt.Return): void {
        // Todo: Exceptions for return unwinding
        // For now just eval
        if (stmt.value) stmt.value.accept(this);
    }

    visitAssignExpr(expr: Expr.Assign): any {
        const value = expr.value.accept(this);
        this.environment.assign(expr.name, value);
        return value;
    }

    visitBinaryExpr(expr: Expr.Binary): any {
        const left = expr.left.accept(this);
        const right = expr.right.accept(this);

        // Operator Overloading Dispatch
        // If left has _add/_sub etc.
        if (left && typeof left === 'object') {
            switch (expr.operator.type) {
                case TokenType.PLUS: if (left._add) return left._add(right); break;
            }
        }

        switch (expr.operator.type) {
            case TokenType.MINUS: return left - right;
            case TokenType.SLASH: return left / right;
            case TokenType.STAR: return left * right;
            case TokenType.PLUS:
                if (typeof left === 'string' && typeof right === 'string') return left + right;
                return left + right;
            case TokenType.GREATER: return left > right;
            case TokenType.GREATER_EQUAL: return left >= right;
            case TokenType.LESS: return left < right;
            case TokenType.LESS_EQUAL: return left <= right;
            case TokenType.BANG_EQUAL: return !this.isEqual(left, right);
            case TokenType.EQUAL_EQUAL: return this.isEqual(left, right);
        }
        return null;
    }

    visitCallExpr(expr: Expr.Call): any {
        const callee = expr.callee.accept(this);
        const args = expr.args.map(arg => arg.accept(this));

        if (!callee || !callee.call) {
            throw new Error("Can only call functions and classes.");
        }

        return callee.call(this, args);
    }

    visitGetExpr(expr: Expr.Get): any {
        const object = expr.object.accept(this);
        if (object && typeof object === 'object') {
            return object[expr.name.lexeme];
        }
        throw new Error("Only objects have properties.");
    }

    visitGroupingExpr(expr: Expr.Grouping): any {
        return expr.expression.accept(this);
    }

    visitLiteralExpr(expr: Expr.Literal): any {
        return expr.value;
    }

    visitLogicalExpr(expr: Expr.Logical): any {
        const left = expr.left.accept(this);
        if (expr.operator.type === TokenType.OR) {
            if (this.isTruthy(left)) return left;
        } else {
            if (!this.isTruthy(left)) return left;
        }
        return expr.right.accept(this);
    }

    visitUnaryExpr(expr: Expr.Unary): any {
        const right = expr.right.accept(this);
        switch (expr.operator.type) {
            case TokenType.BANG: return !this.isTruthy(right);
            case TokenType.MINUS: return -right;
        }
        return null;
    }

    visitVariableExpr(expr: Expr.Variable): any {
        return this.environment.get(expr.name);
    }

    visitLambdaExpr(expr: Expr.Lambda): any {
        // Return a callable for the block
        return {
            call: (interpreter: Interpreter, args: any[]) => {
                const env = new Environment(interpreter.environment);
                // TODO: Lambda params
                interpreter.executeBlock(expr.body, env);
            }
        };
    }

    private isTruthy(object: any): boolean {
        if (object === null) return false;
        if (typeof object === 'boolean') return object;
        return true;
    }

    private isEqual(a: any, b: any): boolean {
        return a === b;
    }
}
