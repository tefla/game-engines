export type Loc = {
  readonly line: number;
  readonly col: number;
};

export type Program = {
  readonly type: "Program";
  readonly body: Stmt[];
};

export type Stmt =
  | LetStmt
  | VarStmt
  | FnStmt
  | OnStmt
  | ExtendStmt
  | ImportStmt
  | EmitStmt
  | ExprStmt;

export type LetStmt = {
  readonly type: "Let";
  readonly loc: Loc;
  readonly name: string;
  readonly expr: Expr;
};

export type VarStmt = {
  readonly type: "Var";
  readonly loc: Loc;
  readonly name: string;
  readonly expr: Expr;
};

export type FnStmt = {
  readonly type: "Fn";
  readonly loc: Loc;
  readonly name: string;
  readonly params: string[];
  readonly body: Stmt[];
};

export type OnStmt = {
  readonly type: "On";
  readonly loc: Loc;
  readonly signal: string;
  readonly filters: Expr[];
  readonly body: Stmt[];
};

export type ExtendStmt = {
  readonly type: "Extend";
  readonly loc: Loc;
  readonly name: string;
  readonly params: string[];
  readonly body: Stmt[];
};

export type ImportStmt = {
  readonly type: "Import";
  readonly loc: Loc;
  readonly kind: "module" | "path";
  readonly name: string;
};

export type EmitStmt = {
  readonly type: "Emit";
  readonly loc: Loc;
  readonly signal: string;
  readonly data?: Expr;
};

export type ExprStmt = {
  readonly type: "Expr";
  readonly loc: Loc;
  readonly expr: Expr;
};

export type Expr =
  | LiteralExpr
  | IdentExpr
  | ListExpr
  | RecordExpr
  | UnaryExpr
  | BinaryExpr
  | IfExpr
  | MatchExpr
  | CallExpr
  | MemberExpr
  | AssignExpr;

export type LiteralExpr = {
  readonly type: "Literal";
  readonly loc: Loc;
  readonly value: null | boolean | number | string;
};

export type IdentExpr = {
  readonly type: "Ident";
  readonly loc: Loc;
  readonly name: string;
};

export type ListExpr = {
  readonly type: "List";
  readonly loc: Loc;
  readonly items: Expr[];
};

export type RecordExpr = {
  readonly type: "Record";
  readonly loc: Loc;
  readonly entries: { readonly key: string; readonly value: Expr }[];
};

export type UnaryExpr = {
  readonly type: "Unary";
  readonly loc: Loc;
  readonly op: "not" | "-";
  readonly expr: Expr;
};

export type BinaryExpr = {
  readonly type: "Binary";
  readonly loc: Loc;
  readonly op: "+" | "-" | "*" | "/" | "==" | "!=" | ">" | ">=" | "<" | "<=" | "and" | "or";
  readonly left: Expr;
  readonly right: Expr;
};

export type IfExpr = {
  readonly type: "If";
  readonly loc: Loc;
  readonly condition: Expr;
  readonly thenBlock: Stmt[];
  readonly elseBlock?: Stmt[];
};

export type MatchExpr = {
  readonly type: "Match";
  readonly loc: Loc;
  readonly expr: Expr;
  readonly cases: { readonly pattern: Pattern; readonly expr: Expr }[];
};

export type CallExpr = {
  readonly type: "Call";
  readonly loc: Loc;
  readonly callee: Expr;
  readonly args: Expr[];
  readonly block?: Expr;
};

export type MemberExpr = {
  readonly type: "Member";
  readonly loc: Loc;
  readonly object: Expr;
  readonly property: string;
};

export type AssignExpr = {
  readonly type: "Assign";
  readonly loc: Loc;
  readonly target: IdentExpr | MemberExpr;
  readonly value: Expr;
};

export type Pattern = WildcardPattern | LiteralPattern | IdentPattern | RecordPattern;

export type WildcardPattern = { readonly type: "Wildcard"; readonly loc: Loc };
export type LiteralPattern = {
  readonly type: "PLiteral";
  readonly loc: Loc;
  readonly value: null | boolean | number | string;
};
export type IdentPattern = { readonly type: "PIdent"; readonly loc: Loc; readonly name: string };
export type RecordPattern = {
  readonly type: "PRecord";
  readonly loc: Loc;
  readonly fields: { readonly key: string; readonly pattern: Pattern }[];
};
