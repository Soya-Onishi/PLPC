package plpc.internal

import scala.util.parsing.combinator._
import java.io.Reader

class Parser extends JavaTokenParsers {
  def apply(in: String) = parseAll(top, in)

  def top: Parser[List[AST]] =
    defineOrExpr ~ rep(rep(separate) ~> defineOrExpr) ^^ {
      case head ~ tail => head :: tail
    }

  def defineOrExpr: Parser[AST] =
    define | expr

  def separate: Parser[AST] =
    (";" | newLine) ^^ { _ => Separate }

  def newLine: Parser[AST] =
    "\n" ^^ { _ => NewLine }

  def define: Parser[AST] = varDef | funDef

  def varDef: Parser[AST] =
    ("val" | "var") ~ ident ~ "=" ~ expr ^^ {
      case modifier ~ id ~ _ ~ e => modifier match {
        case "val" => ValDef(id.toString, e)
        case "var" => VarDef(id.toString, e)
      }
    }

  def funDef: Parser[AST] =
    "def" ~ ident ~ "(" ~ opt(repsep(ident, ",")) ~ ")" ~ "=" ~ expr ^^ {
      case _ ~ id ~ _ ~ params ~ _ ~ _ ~ body =>
        FunDef(
          id.toString,
          Fun(
            params.getOrElse(Nil).map { _.toString },
            body
          )
        )
    }

  def expr: Parser[AST] =
    ifExpr | assignExpr | condExpr

  def ifExpr: Parser[AST] =
    "if" ~ "(" ~ condExpr ~ ")" ~ expr ~ "else" ~ expr ^^ {
      case _ ~ _ ~ cond ~ _ ~ e1 ~ _ ~ e2 => IfExpr(cond, e1, e2)
    }

  def assignExpr: Parser[AST] =
    ident ~ "=" ~ expr ^^ {
      case id ~ _ ~ e => Assign(Var(id.toString), e)
    }

  def condExpr: Parser[AST] =
    additiveExpr ~ rep(("==" | "!=" | "<=" | "<" | ">=" | ">") ~ additiveExpr) ^^ {
      case left ~ right =>
        right.foldLeft(left) { (x, y) =>
          y match {
            case op ~ e => BinOp(op, x, e)
          }
        }
    }

  def additiveExpr: Parser[AST] =
    term ~ rep(("+" | "-") ~ term) ^^ {
      case left ~ right =>
        right.foldLeft(left) { (x, y) =>
          y match {
            case op ~ t => BinOp(op, x, t)
          }
        }
    }

  def term: Parser[AST] =
    prefixExpr ~ rep(("*" | "/" | "%") ~ prefixExpr) ^^ {
      case left ~ right =>
        right.foldLeft(left) { (x, y) =>
          y match {
            case op ~ p => BinOp(op, x, p)
          }
        }
    }

  def prefixExpr: Parser[AST] =
    opt("-" | "!" | "+") ~ simpleExpr ^^ {
      case op ~ s => op match {
        case None => s
        case Some(o) => o match {
          case "-" | "!" => UnaryOp(o, s)
          case "+"       => s
        }
      }
    }

  def simpleExpr: Parser[AST] = funCallOrFactor | blockExpr

  def funCallOrFactor: Parser[AST] =
    factor ~ opt("(" ~> opt(repsep(expr, ",")) <~ ")") ^^ {
      case f ~ args => args match {
        case Some(as) => FunCall(f, as.getOrElse(Nil))
        case None => f
      }
    }

  def factor: Parser[AST] =
    literal | ident ^^ {
      n => Var(n.toString)
    } | "(" ~> expr <~ ")"

  def blockExpr: Parser[AST] =
    "{" ~> block <~ "}" ^^ { BlockExpr(_) }

  def block: Parser[List[AST]] =
    opt(repsep(defineOrExpr, separate)) ^^ {
      _.getOrElse(Nil)
    }

  def literal: Parser[AST] =
    boolLit | wholeNumber ^^ {
      n => Number(IntConst(n.toInt))
    } | stringLiteral ^^ {
      s => Str(StrConst(s.substring(1, s.length - 1)))
    }

  def boolLit: Parser[AST] =
    ("true" | "false") ^^ {
      case "true" => Bool(BoolConst(true))
      case "false" => Bool(BoolConst(false))
    }

  val reserved: Parser[String] = ( "val" | "var" | "if" | "for" | "while" )

  override def ident: Parser[String] =
    not(reserved) ~> super.ident
}
