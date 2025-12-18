// AST node types for Slate

import type { Token } from "./tokens";

// Base AST node interface
export interface ASTNode {
  type: string;
  line: number;
  column: number;
}

// ============ Expressions ============

export interface LiteralExpr extends ASTNode {
  type: "Literal";
  value: number | string | boolean | null;
}

export interface IdentifierExpr extends ASTNode {
  type: "Identifier";
  name: string;
}

export interface BinaryExpr extends ASTNode {
  type: "Binary";
  left: Expr;
  operator: Token;
  right: Expr;
}

export interface UnaryExpr extends ASTNode {
  type: "Unary";
  operator: Token;
  operand: Expr;
}

export interface LogicalExpr extends ASTNode {
  type: "Logical";
  left: Expr;
  operator: Token;
  right: Expr;
}

export interface CallExpr extends ASTNode {
  type: "Call";
  callee: Expr;
  args: Expr[];
}

export interface MemberExpr extends ASTNode {
  type: "Member";
  object: Expr;
  property: string;
}

export interface IndexExpr extends ASTNode {
  type: "Index";
  object: Expr;
  index: Expr;
}

export interface RecordExpr extends ASTNode {
  type: "Record";
  fields: Array<{ key: string; value: Expr }>;
}

export interface ListExpr extends ASTNode {
  type: "List";
  elements: Expr[];
}

export interface IfExpr extends ASTNode {
  type: "If";
  condition: Expr;
  thenBranch: Block;
  elseBranch?: Block | IfExpr;
}

export interface MatchExpr extends ASTNode {
  type: "Match";
  subject: Expr;
  arms: MatchArm[];
}

export interface MatchArm extends ASTNode {
  type: "MatchArm";
  pattern: Pattern;
  body: Expr;
}

export interface RangeExpr extends ASTNode {
  type: "Range";
  start: Expr;
  end: Expr;
  step?: Expr;
}

export interface WithExpr extends ASTNode {
  type: "With";
  base: Expr;
  updates: RecordExpr;
}

export interface AssignExpr extends ASTNode {
  type: "Assign";
  target: IdentifierExpr | MemberExpr | IndexExpr;
  value: Expr;
}

export interface ColorExpr extends ASTNode {
  type: "Color";
  hex: string;
}

// Union of all expression types
export type Expr =
  | LiteralExpr
  | IdentifierExpr
  | BinaryExpr
  | UnaryExpr
  | LogicalExpr
  | CallExpr
  | MemberExpr
  | IndexExpr
  | RecordExpr
  | ListExpr
  | IfExpr
  | MatchExpr
  | RangeExpr
  | WithExpr
  | AssignExpr
  | ColorExpr;

// ============ Patterns ============

export interface LiteralPattern extends ASTNode {
  type: "LiteralPattern";
  value: number | string | boolean | null;
}

export interface IdentifierPattern extends ASTNode {
  type: "IdentifierPattern";
  name: string;
}

export interface WildcardPattern extends ASTNode {
  type: "WildcardPattern";
}

export interface RecordPattern extends ASTNode {
  type: "RecordPattern";
  fields: Array<{ key: string; pattern?: Pattern }>;
}

export interface ListPattern extends ASTNode {
  type: "ListPattern";
  elements: Pattern[];
  rest?: string; // identifier for rest elements
}

export type Pattern =
  | LiteralPattern
  | IdentifierPattern
  | WildcardPattern
  | RecordPattern
  | ListPattern;

// ============ Statements ============

export interface LetStmt extends ASTNode {
  type: "Let";
  name: string;
  typeAnnotation?: TypeExpr;
  value: Expr;
}

export interface VarStmt extends ASTNode {
  type: "Var";
  name: string;
  typeAnnotation?: TypeExpr;
  value: Expr;
}

export interface FnStmt extends ASTNode {
  type: "Fn";
  name: string;
  params: Array<{ name: string; typeAnnotation?: TypeExpr }>;
  returnType?: TypeExpr;
  body: Block;
}

export interface OnStmt extends ASTNode {
  type: "On";
  signal: SignalPath;
  filter?: Expr; // Optional second argument like "find_key"
  body: Block;
}

export interface SignalPath extends ASTNode {
  type: "SignalPath";
  parts: string[];
}

export interface EmitStmt extends ASTNode {
  type: "Emit";
  signal: SignalPath;
  data?: Expr;
}

export interface ExtendStmt extends ASTNode {
  type: "Extend";
  keyword: string;
  params: string[];
  body: Block;
}

export interface ExprStmt extends ASTNode {
  type: "ExprStmt";
  expression: Expr;
}

export interface ImportStmt extends ASTNode {
  type: "Import";
  path: string;
}

export interface LoopStmt extends ASTNode {
  type: "Loop";
  body: Block;
}

export interface ForStmt extends ASTNode {
  type: "For";
  variable: string;
  iterable: Expr;
  body: Block;
}

export interface YieldStmt extends ASTNode {
  type: "Yield";
}

export interface SpawnStmt extends ASTNode {
  type: "Spawn";
  expression: Expr;
}

export interface WaitStmt extends ASTNode {
  type: "Wait";
  duration: Expr;
}

export interface TypeDefStmt extends ASTNode {
  type: "TypeDef";
  name: string;
  typeParams?: string[];
  definition: TypeExpr;
}

export interface Block extends ASTNode {
  type: "Block";
  statements: Stmt[];
}

// Union of all statement types
export type Stmt =
  | LetStmt
  | VarStmt
  | FnStmt
  | OnStmt
  | EmitStmt
  | ExtendStmt
  | ExprStmt
  | ImportStmt
  | LoopStmt
  | ForStmt
  | YieldStmt
  | SpawnStmt
  | WaitStmt
  | TypeDefStmt
  | Block;

// ============ Type Expressions ============

export interface NamedType extends ASTNode {
  type: "NamedType";
  name: string;
  typeArgs?: TypeExpr[];
}

export interface RecordType extends ASTNode {
  type: "RecordType";
  fields: Array<{ key: string; typeExpr: TypeExpr }>;
}

export interface SumType extends ASTNode {
  type: "SumType";
  variants: Array<{ name: string; fields?: RecordType }>;
}

export interface FunctionType extends ASTNode {
  type: "FunctionType";
  params: TypeExpr[];
  returnType: TypeExpr;
}

export type TypeExpr = NamedType | RecordType | SumType | FunctionType;

// ============ Program ============

export interface Program extends ASTNode {
  type: "Program";
  statements: Stmt[];
}
