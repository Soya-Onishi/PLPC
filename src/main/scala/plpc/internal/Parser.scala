package plpc.internal

import scala.util.parsing.combinator._
import java.io.Reader

class Parser extends JavaTokenParsers {
  def apply(in: String) = parseAll(top, in)

  def top: Parser[AST] =
    repsep(defineOrExpr, separate) ~ opt(separate) ^^ {
      case list ~ _ => Root(list)
    }

  def defineOrExpr: Parser[AST] =
    define | expr

  def separate: Parser[AST] =
    (";" | newLine) ^^ { _ => Separate }

  def newLine: Parser[AST] =
    '\n' ^^ { _ => NewLine }

  def define: Parser[AST] = varDef | funDef

  def varDef: Parser[AST] =
    ("val" | "var") ~ ident ~ opt(":" ~> defType) ~ "=" ~ expr ^^ {
      case modifier ~ id ~ t ~ _ ~ e =>
        val varType = t match {
          case Some(vt) => vt
          case None => NoType
        }

        modifier match {
          case "val" => ValDef(varType, id.toString, e)
          case "var" => VarDef(varType, id.toString, e)
        }
    }

  def funDef: Parser[AST] =
    "def" ~ ident ~ "(" ~ repsep(param, ",") ~ ")" ~ opt(":" ~> defType) ~ "=" ~ expr ^^ {
      case _ ~ id ~ _ ~ params ~ _ ~ t ~ _ ~ body =>
        val returnType = t.getOrElse(NoType)

        FunDef(
          returnType,
          id.toString,
          Fun(
            returnType, params, body
          )
        )
    }

  def param: Parser[(String, Type)] =
    ident ~ ":" ~ defType ^^ {
      case id ~ _ ~ t => (id, t)
    }

  def defType: Parser[Type] =
    ("Unit" | "Int" | "String" | "Bool") ^^ {
      case "Unit" => UnitType
      case "Int" => IntType
      case "String" => StringType
      case "Bool" => BoolType
    }

  def expr: Parser[AST] =
    ifExpr | assignExpr | condExpr | blockExpr

  def ifExpr: Parser[AST] =
    "if" ~ "(" ~ condExpr ~ ")" ~ expr ~ "else" ~ expr ^^ {
      case _ ~ _ ~ cond ~ _ ~ e1 ~ _ ~ e2 => IfExpr(NoType, cond, e1, e2)
    }

  def assignExpr: Parser[AST] =
    ident ~ "=" ~ expr ^^ {
      case id ~ _ ~ e => Assign(UnitType, Var(NoType, id.toString), e)
    }

  def condExpr: Parser[AST] = logicalOrExpr
  def logicalOrExpr: Parser[AST] =
    logicalAndExpr ~ rep("||" ~ logicalAndExpr) ^^ {
      case left ~ right =>
        right.foldLeft(left){ (x, y) =>
          y match {
            case op ~ e => BinOp(op, x, e)
          }
        }
    }

  def logicalAndExpr: Parser[AST] =
    compExpr ~ rep("&&" ~ compExpr) ^^ {
      case left ~ right =>
        right.foldLeft(left) { (x, y) =>
          y match {
            case op ~ e => BinOp(op, x, e)
          }
        }
    }

  def compExpr: Parser[AST] =
    additiveExpr ~ rep(("==" | "!=" | "<=" | "<" | ">=" | ">") ~ additiveExpr) ^^ {
      case left ~ right =>
        right.foldLeft(left) { (x, y) =>
          y match {
            case op ~ e => BinOp(NoType, op, x, e)
          }
        }
    }

  def additiveExpr: Parser[AST] =
    term ~ rep(("+" | "-") ~ term) ^^ {
      case left ~ right =>
        right.foldLeft(left) { (x, y) =>
          y match {
            case op ~ t => BinOp(NoType, op, x, t)
          }
        }
    }

  def term: Parser[AST] =
    prefixExpr ~ rep(("*" | "/" | "%") ~ prefixExpr) ^^ {
      case left ~ right =>
        right.foldLeft(left) { (x, y) =>
          y match {
            case op ~ p => BinOp(NoType, op, x, p)
          }
        }
    }

  def prefixExpr: Parser[AST] =
    opt("-" | "!" | "+") ~ funCallOrFactor ^^ {
      case op ~ s => op match {
        case None => s
        case Some(o) => o match {
          case "-" | "!" => UnaryOp(NoType, o, s)
          case "+" => s
        }
      }
    }

  def funCallOrFactor: Parser[AST] =
    factor ~ opt("(" ~> repsep(expr, ",") <~ ")") ^^ {
      case f ~ args => args match {
        case Some(as) => FunCall(NoType, f, as)
        case None => f
      }
    }

  def factor: Parser[AST] =
    literal | ident ^^ {
      n => Var(NoType, n.toString)
    } | "(" ~> expr <~ ")"

  def blockExpr: Parser[AST] =
    "{" ~> block <~ "}" ^^ { BlockExpr(NoType, _) }

  def block: Parser[List[AST]] =
    opt(repsep(defineOrExpr, separate)) ~ opt(separate) ^^ {
      case x ~ _ => x.getOrElse(Nil)
    }

  def literal: Parser[AST] =
    boolLit | wholeNumber ^^ {
      n => Number(IntType, IntConst(n.toInt))
    } | stringLiteral ^^ {
      s => Str(StrConst(s.substring(1, s.length - 1)))
    }

  def boolLit: Parser[AST] =
    ("true" | "false") ^^ {
      case "true" => Bool(BoolConst(true))
      case "false" => Bool(BoolConst(false))
    }

  val reserved: Parser[String] = ("val" | "var" | "if" | "for" | "while")

  override def ident: Parser[String] =
    not(reserved) ~> super.ident
}
