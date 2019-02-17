package plpc.internal

sealed abstract class AST(private val _type: Type = NoType) {
  def getType: Type = _type
}

case class Root(asts: List[AST]) extends AST
case class ValDef(t: Type, name: String, expr: AST) extends AST(t)
case class VarDef(t: Type, name: String, expr: AST) extends AST(t)
case class FunDef(t: Type, name: String, expr: Fun) extends AST(t)

abstract class Expr(t: Type) extends AST(t)
case class Var(t: Type, name: String) extends Expr(t)
case class Assign(t: Type, left: AST, right: AST) extends Expr(t)
case class IfExpr(t: Type, cond: AST, ifExpr: AST, elseExpr: AST) extends Expr(t)
case class BlockExpr(t: Type, exprs: List[AST]) extends Expr(t)
case class FunCall(t: Type, fun: AST, args: List[AST]) extends Expr(t)
case class BinOp(t: Type, op: String, left: AST, right: AST) extends Expr(t)
case class UnaryOp(t: Type, op: String, operand: AST) extends Expr(t)
case class Number(t: Type, value: Value) extends Expr(t)
case class Str(str: Value) extends Expr(StringType)
case class Bool(bool: Value) extends Expr(BoolType)

case object Separate extends AST
case object NewLine extends AST