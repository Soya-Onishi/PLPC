package plpc.internal

import sun.awt.im.CompositionArea

class Interpreter {
  private var _env = new Environment(None)
  private var _traceEval = false
  private var _nCall = 0

  def traceEval: Boolean = _traceEval
  def traceEval_=(flag: Boolean): Unit = _traceEval = flag

  def nCall: Int = _nCall
  private def nCall_=(n: Int): Unit = nCall = n

  def env: Environment = _env
  private def env_=(newEnv: Environment): Unit = _env = newEnv

  private def add(a: Value, b: Value): Value = (a, b) match {
    case (IntConst(x), IntConst(y)) => IntConst(x + y)
    case (StrConst(x), StrConst(y)) => StrConst(x + y)
    case (StrConst(x), IntConst(y)) => StrConst(x + y.toString)
    case (IntConst(x), StrConst(y)) => StrConst(x.toString + y)
    case (StrConst(x), BoolConst(y)) => StrConst(x + y.toString)
    case (BoolConst(x), StrConst(y)) => StrConst(x.toString + y)
    case _ => Wrong
  }

  private def sub(a: Value, b: Value): Value = (a, b) match {
    case (IntConst(x), IntConst(y)) => IntConst(x - y)
    case _ => Wrong
  }

  private def mul(a: Value, b: Value): Value = (a, b) match {
    case (IntConst(x), IntConst(y)) => IntConst(x * y)
    case (StrConst(x), IntConst(y)) => StrConst(x * y)
    case _ => Wrong
  }

  private def div(a: Value, b: Value): Value = (a, b) match {
    case (IntConst(x), IntConst(y)) => IntConst(x / y)
    case _ => Wrong
  }

  private def mod(a: Value, b: Value): Value = (a, b) match {
    case (IntConst(x), IntConst(y)) => IntConst(x % y)
    case _ => Wrong
  }

  private def negate(a: Value): Value = a match {
    case IntConst(x) => IntConst(-x)
    case _ => Wrong
  }

  private def compare(a: Value, b: Value)(f: (Int, Int) => Boolean): Value = (a, b) match {
    case (IntConst(x), IntConst(y)) => BoolConst(f(x, y))
    case (StrConst(x), StrConst(y)) => BoolConst(f(x.compareTo(y), 0))
    case (BoolConst(x), BoolConst(y)) => BoolConst(f(x.compareTo(y), 0))
    case _ => Wrong
  }

  private def eq(a: Value, b: Value): Value = compare(a, b)(_ == _)
  private def ne(a: Value, b: Value): Value = compare(a, b)(_ != _)
  private def lt(a: Value, b: Value): Value = compare(a, b)(_ < _)
  private def lte(a: Value, b: Value): Value = compare(a, b)(_ <= _)
  private def gt(a: Value, b: Value): Value = compare(a, b)(_ > _)
  private def gte(a: Value, b: Value): Value = compare(a, b)(_ >= _)

  /*
  private def ne(a: Value, b: Value): Value = (a, b) match {
    case (IntConst(x), IntConst(y)) => BoolConst(x != y)
    case (StrConst(x), StrConst(y)) => BoolConst(x != y)
    case (BoolConst(x), BoolConst(y)) => BoolConst(x != y)
    case _ => Wrong
  }

  private def lt(a: Value, b: Value): Value = (a, b) match {
    case (IntConst(x), IntConst(y)) => BoolConst(x < y)
    case (StrConst(x), StrConst(y)) => BoolConst(x < y)
    case ()
  }
  */

  private def logicalNot(a: Value): Value = a match {
    case BoolConst(v) => BoolConst(!v)
    case _ => Wrong
  }

  private def logicalAnd(a: Value, b: Value): Value = (a, b) match {
    case (BoolConst(x), BoolConst(y)) => BoolConst(x && y)
    case _ => Wrong
  }

  private def logicalOr(a: Value, b: Value): Value = (a, b) match {
    case (BoolConst(x), BoolConst(y)) => BoolConst(x || y)
    case _ => Wrong
  }

  private val binOpMap = Map[String, (Value, Value) => Value](
    "+" -> add,
    "-" -> sub,
    "*" -> mul,
    "/" -> div,
    "%" -> mod,
    "==" -> eq,
    "!=" -> ne,
    "<" -> lt,
    "<=" -> lte,
    ">" -> gt,
    ">=" -> gte,
    "&&" -> logicalAnd,
    "||" -> logicalOr
  )

  private val unaryOpMap = Map[String, Value => Value](
    "-" -> negate,
    "!" -> logicalNot
  )

  private def spaces(n: Int): String = " " * n
  private var _traceLevel = 0

  private def traceLevel: Int = _traceLevel
  private def traceLevel_=(n: Int): Unit =
    if(n < 0) _traceLevel = 0
    else      _traceLevel = n

  def eval(ast: AST): Value = eval(ast, env)
  private def eval(ast: AST, env: Environment): Value = evaluation(ast, env)

  private def evaluation(ast: AST, env: Environment): Value = ast match {
    case Root(asts) => asts.foldLeft(NoValue: Value){
      (_, ast) => eval(ast, env)
    }
    case BinOp(op, left, right) =>
      binOpMap(op)(eval(left, env), eval(right, env))
    case UnaryOp(op, operand) =>
      unaryOpMap(op)(eval(operand, env))
    case Number(n) => n
    case Str(s) => s
    case Bool(b) => b
    case Var(n) => env.lookup(n) match {
      case Some(x) => x
      case None => throw new NotFoundException(s"$n is not found")
    }
    case Assign(Var(n), right) =>
      env.setenv(n, eval(right))
      NoValue
    case IfExpr(cond, ifExpr, elseExpr) => eval(cond, env) match {
      case BoolConst(true) => eval(ifExpr, env)
      case BoolConst(false) => eval(elseExpr, env)
      case _ => throw new InvalidConditionValueException("conditional expression value must be boolean")
    }
    case BlockExpr(asts) => {
      val localEnv = new Environment(Some(env))
      asts.foldLeft(NoValue: Value)((_, ast) => eval(ast, localEnv))
    }
    case ValDef(name, right) =>
      env.regVal(name, eval(right, env))
      NoValue
    case VarDef(name, right) =>
      env.regVar(name, eval(right, env))
      NoValue
    case FunDef(name, function) =>
      env.regFun(name, Closure(function, env))
      NoValue
    case FunCall(name, args) =>
      eval(name, env) match {
        case Closure(Fun(params, proc), e) =>
          if(params.length != args.length)
            throw new InvalidFunctionCallException(s"function needs ${params.length} parameters but send ${args.length}")

          val localEnv = new Environment(Some(e))
          params.zip(args).foreach{ case (p, a) => localEnv.regVal(p, eval(a, env))}
          eval(proc, localEnv)
        case _ => throw new InvalidFunctionCallException("this object is not function")
      }
    case _ => throw new InvalidASTException("match error AST node")
  }
}
