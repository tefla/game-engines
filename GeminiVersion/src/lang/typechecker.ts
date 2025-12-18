import * as Stmt from "./ast";
import * as Expr from "./ast";
import { Token } from "./token";

export type TypeName = string;

export class TypeChecker implements Stmt.Visitor<void>, Expr.Visitor<TypeName> {
    private scopes: Record<string, TypeName>[] = [{}];
    private functions: Record<string, { params: string[], returnType: string }> = {};

    constructor() {
        // Built-ins
        this.define("print", "Void"); // print(...)
    }

    check(statements: Stmt.Stmt[]) {
        for (const stmt of statements) {
            if (stmt) stmt.accept(this);
        }
    }

    private define(name: string, type: string) {
        this.scopes[this.scopes.length - 1][name] = type;
    }

    private lookup(name: string): TypeName {
        for (let i = this.scopes.length - 1; i >= 0; i--) {
            if (this.scopes[i][name]) return this.scopes[i][name];
        }
        throw new Error(`Undefined variable '${name}'.`);
    }

    private enterScope() {
        this.scopes.push({});
    }

    private exitScope() {
        this.scopes.pop();
    }

    visitBlockStmt(stmt: Stmt.Block): void {
        this.enterScope();
        this.check(stmt.statements);
        this.exitScope();
    }

    visitVarStmt(stmt: Stmt.Var): void {
        let valueType = "Any";
        if (stmt.initializer) {
            valueType = stmt.initializer.accept(this);
        }

        if (stmt.typeName) {
            if (valueType !== "Any" && valueType !== stmt.typeName) {
                console.error(`Type Error: Expected '${stmt.typeName}' but got '${valueType}' at ${stmt.name.line}`);
            }
            this.define(stmt.name.lexeme, stmt.typeName);
        } else {
            // Inference
            this.define(stmt.name.lexeme, valueType);
        }
    }

    visitFunctionStmt(stmt: Stmt.Function): void {
        // Define function in current scope (or global)
        // TODO: Handle function signatures properly
        this.functions[stmt.name.lexeme] = {
            params: stmt.params.map(p => p.typeName || "Any"),
            returnType: stmt.returnType || "Any"
        };

        this.enterScope();
        for (const param of stmt.params) {
            this.define(param.name.lexeme, param.typeName || "Any");
        }
        this.check(stmt.body);
        this.exitScope();
    }

    visitExpressionStmt(stmt: Stmt.Expression): void {
        stmt.expression.accept(this);
    }

    visitIfStmt(stmt: Stmt.If): void {
        stmt.condition.accept(this); // Check it's boolean?
        stmt.thenBranch.accept(this);
        if (stmt.elseBranch) stmt.elseBranch.accept(this);
    }

    visitWhileStmt(stmt: Stmt.While): void {
        stmt.condition.accept(this);
        stmt.body.accept(this);
    }

    visitReturnStmt(stmt: Stmt.Return): void {
        if (stmt.value) stmt.value.accept(this);
    }

    // Expressions
    visitAssignExpr(expr: Expr.Assign): TypeName {
        const valueType = expr.value.accept(this);
        const varType = this.lookup(expr.name.lexeme);
        if (varType !== "Any" && valueType !== "Any" && varType !== valueType) {
            console.error(`Type Error: Assigning '${valueType}' to '${varType}'`);
        }
        return varType;
    }

    visitBinaryExpr(expr: Expr.Binary): TypeName {
        const left = expr.left.accept(this);
        const right = expr.right.accept(this);

        // Operator Overloading Check
        // If left is Object, look for _add, _sub etc.
        // For now, simple primitive check
        if (left === "Int" && right === "Int") return "Int";
        if (left === "Float" && right === "Float") return "Float";

        // Assume valid for Any
        return "Any";
    }

    visitCallExpr(expr: Expr.Call): TypeName {
        const calleeType = expr.callee.accept(this);
        // If callee is a function name, we can check params.
        // If it's a Variable, we might know its Function Type.

        // Check args
        for (const arg of expr.args) {
            arg.accept(this);
        }

        return "Any"; // Todo return type lookup
    }

    visitGroupingExpr(expr: Expr.Grouping): TypeName {
        return expr.expression.accept(this);
    }

    visitLiteralExpr(expr: Expr.Literal): TypeName {
        if (typeof expr.value === 'number') return Number.isInteger(expr.value) ? "Int" : "Float";
        if (typeof expr.value === 'string') return "String";
        if (typeof expr.value === 'boolean') return "Bool";
        return "Nil";
    }

    visitVariableExpr(expr: Expr.Variable): TypeName {
        return this.lookup(expr.name.lexeme);
    }

    visitLambdaExpr(expr: Expr.Lambda): TypeName {
        // Type check body?
        return "Function";
    }

    visitLogicalExpr(expr: Expr.Logical): TypeName {
        return "Bool";
    }

    visitUnaryExpr(expr: Expr.Unary): TypeName {
        return expr.right.accept(this); // -Int -> Int
    }

    visitGetExpr(expr: Expr.Get): TypeName {
        return "Any";
    }
}
