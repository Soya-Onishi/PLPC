package plpc.internal

sealed abstract class AST

case class ValDef(name: String, expr: AST) extends AST
case class VarDef(name: String, expr: AST) extends AST
case class FunDef(name: String, expr: Fun) extends AST
case class Var(name: String) extends AST
case class Assign(left: AST, right: AST) extends AST
case class IfExpr(cond: AST, ifExpr: AST, elseExpr: AST) extends AST
case class BlockExpr(exprs: List[AST]) extends AST
case class FunCall(fun: AST, args: List[AST]) extends AST
case class BinOp(op: String, left: AST, right: AST) extends AST
case class UnaryOp(op: String, operand: AST) extends AST
case class Number(value: Value) extends AST
case class Str(str: Value) extends AST
case class Bool(bool: Value) extends AST

case object Separate extends AST
case object NewLine extends AST