import { Token } from "./token";

export interface Visitor<R> {
    visitAssignExpr(expr: Assign): R;
    visitBinaryExpr(expr: Binary): R;
    visitCallExpr(expr: Call): R;
    visitGetExpr(expr: Get): R;
    visitGroupingExpr(expr: Grouping): R;
    visitLiteralExpr(expr: Literal): R;
    visitLogicalExpr(expr: Logical): R;
    visitUnaryExpr(expr: Unary): R;
    visitVariableExpr(expr: Variable): R;
    visitLambdaExpr(expr: Lambda): R;
}

export abstract class Expr {
    abstract accept<R>(visitor: Visitor<R>): R;
}

export class Assign extends Expr {
    constructor(
        public name: Token,
        public value: Expr
    ) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitAssignExpr(this); }
}

export class Binary extends Expr {
    constructor(
        public left: Expr,
        public operator: Token,
        public right: Expr
    ) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitBinaryExpr(this); }
}

export class Call extends Expr {
    constructor(
        public callee: Expr,
        public paren: Token,
        public args: Expr[]
    ) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitCallExpr(this); }
}

export class Get extends Expr {
    constructor(
        public object: Expr,
        public name: Token
    ) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitGetExpr(this); }
}

export class Grouping extends Expr {
    constructor(public expression: Expr) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitGroupingExpr(this); }
}

export class Literal extends Expr {
    constructor(public value: any) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitLiteralExpr(this); }
}

export class Logical extends Expr {
    constructor(
        public left: Expr,
        public operator: Token,
        public right: Expr
    ) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitLogicalExpr(this); }
}

export class Unary extends Expr {
    constructor(
        public operator: Token,
        public right: Expr
    ) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitUnaryExpr(this); }
}

export class Variable extends Expr {
    constructor(public name: Token) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitVariableExpr(this); }
}

export class Lambda extends Expr {
    constructor(
        public params: { name: Token, typeName?: string }[], // Explicit types in lambda args?
        public body: Stmt[]
    ) { super(); }
    accept<R>(visitor: Visitor<R>): R { return visitor.visitLambdaExpr(this); }
}

// Statements
export interface StmtVisitor<R> {
    visitBlockStmt(stmt: Block): R;
    visitExpressionStmt(stmt: Expression): R;
    visitFunctionStmt(stmt: Function): R;
    visitIfStmt(stmt: If): R;
    visitReturnStmt(stmt: Return): R;
    visitVarStmt(stmt: Var): R;
    visitWhileStmt(stmt: While): R;
}

export abstract class Stmt {
    abstract accept<R>(visitor: StmtVisitor<R>): R;
}

export class Block extends Stmt {
    constructor(public statements: Stmt[]) { super(); }
    accept<R>(visitor: StmtVisitor<R>): R { return visitor.visitBlockStmt(this); }
}

export class Expression extends Stmt {
    constructor(public expression: Expr) { super(); }
    accept<R>(visitor: StmtVisitor<R>): R { return visitor.visitExpressionStmt(this); }
}

export class Function extends Stmt {
    constructor(
        public name: Token,
        public params: { name: Token, typeName?: string }[],
        public returnType: string | null,
        public body: Stmt[]
    ) { super(); }
    accept<R>(visitor: StmtVisitor<R>): R { return visitor.visitFunctionStmt(this); }
}

export class If extends Stmt {
    constructor(
        public condition: Expr,
        public thenBranch: Stmt,
        public elseBranch: Stmt | null
    ) { super(); }
    accept<R>(visitor: StmtVisitor<R>): R { return visitor.visitIfStmt(this); }
}

export class Return extends Stmt {
    constructor(
        public keyword: Token,
        public value: Expr | null
    ) { super(); }
    accept<R>(visitor: StmtVisitor<R>): R { return visitor.visitReturnStmt(this); }
}

export class Var extends Stmt {
    constructor(
        public name: Token,
        public typeName: string | null, // The Type Annotation
        public initializer: Expr | null
    ) { super(); }
    accept<R>(visitor: StmtVisitor<R>): R { return visitor.visitVarStmt(this); }
}

export class While extends Stmt {
    constructor(
        public condition: Expr,
        public body: Stmt
    ) { super(); }
    accept<R>(visitor: StmtVisitor<R>): R { return visitor.visitWhileStmt(this); }
}
