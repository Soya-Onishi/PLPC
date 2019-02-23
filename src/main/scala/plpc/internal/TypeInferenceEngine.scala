package plpc.internal

class TypeInferenceEngine {
  val typeEnv = new TypeEnvironment(None)

  def apply(ast: AST): AST = infer(ast, typeEnv)

  private def infer(ast: AST, env: TypeEnvironment): AST = {
    ast match {
      case Root(asts) => Root(asts.map(infer(_, env)))
      case BinOp(_, op, left, right) =>
        val newLeft = infer(left, env)
        val newRight = infer(right, env)

        binOpMap(op)(op, newLeft.getType, newRight.getType) match {
          case Some(t) => BinOp(t, op, newLeft, newRight)
          case None => throw NotTypeInferrableException(s"${newLeft.getType} $op ${newRight.getType} can not infer type")
        }
      case UnaryOp(t, op, operand) =>
        val newOperand = infer(operand, env)
        val operandType = newOperand.getType

        op match {
          case "+" | "-" => UnaryOp(operandType, op, newOperand)
          case "!" => operandType match {
            case BoolType => UnaryOp(operandType, op, newOperand)
            case _ => throw NotTypeInferrableException(s"$op $operandType can not infer type")
          }
        }
      case n@Number(_, _) => n
      case s@Str(_) => s
      case b@Bool(_) => b
      case Var(_, name) =>
        env.get(name) match {
          case Some(t) => Var(t, name)
          case None => throw NotFoundException(s"$name is not found")
        }
      case Assign(t, left, right) =>
        val newRight = infer(right, env)

        val newLeft = left match {
          case Var(leftType, id) =>
            if(leftType != newRight.getType)
              throw TypeMissMatchException(s"expected $leftType, but actual ${newRight.getType}")

            Var(leftType, id)
          case _ => ???
        }

        Assign(t, newLeft, newRight)
      case IfExpr(t, cond, e1, e2) =>
        val newCond = infer(cond, env)

        if (newCond.getType != BoolType)
          throw InvalidConditionValueException("conditional expression value must be boolean")

        val newE1 = infer(e1, env)
        val newE2 = infer(e2, env)

        if (newE1.getType != newE2.getType)
          throw TypeMissMatchException(s"if expression type is ${newE1.getType} but else expression type is ${newE2.getType}")

        IfExpr(newE1.getType, newCond, newE1, newE2)
      case BlockExpr(t, exprs) =>
        val (newType, newExprs) = exprs match {
          case Nil => (UnitType, Nil)
          case _ =>
            val e = exprs.map(infer(_, new TypeEnvironment(Some(env))))
            (e.last.getType, e)
        }

        BlockExpr(newType, newExprs)
      case ValDef(t, name, e) =>
        val expr = infer(e, env)


        if (t != expr.getType && t != NoType)
          throw TypeMissMatchException(s"expected type is $t, but actual ${expr.getType}")

        env.reg(name, expr.getType)
        ValDef(expr.getType, name, expr)
      case VarDef(t, name, e) =>
        val expr = infer(e, env)

        if (t != expr.getType && t != NoType)
          throw TypeMissMatchException(s"expected type is $t, but actual ${expr.getType}")

        env.reg(name, expr.getType)
        VarDef(expr.getType, name, expr)
      case FunDef(t, name, f) =>
        def getReturnType(t: Type, fun: Fun): Type = {
          val returnType = fun.getType match {
            case FuncType(rt, _) => rt
            case _ => ???
          }

          if (t == NoType) returnType
          else if (t == returnType) returnType
          else throw TypeMissMatchException(s"expected type[$t] and actual type[$returnType] is not match")
        }

        val funcType = FuncType(t, f.params.map { _._2 })

        env.reg(name, funcType)
        val fun = infer(f, env)

        val returnType = getReturnType(t, fun)
        env.reg(name, FuncType(returnType, f.params.map{ _._2 }))

        FunDef(returnType, name, fun)
      case FunCall(t, f, args) =>
        val fun = infer(f, env)

        val funName = fun match {
          case Var(_, id) => id
          case _ => ???
        }

        val (returnType, paramTypes) = env.get(funName) match {
          case None => throw NotFoundException(s"$funName not found as function")
          case Some(funcType) => funcType match {
            case FuncType(rt, pts) => (rt, pts)
            case _ => throw InvalidFunctionCallException(s"$funName is not function")
          }
        }

        val inferredArgs = args.map(infer(_, env))
        val argTypes = inferredArgs.map { _.getType }

        if (paramTypes.length != argTypes.length)
          throw InvalidFunctionCallException(s"function expected ${paramTypes.length} parameters, but actual ${inferredArgs.length}")
        else {
          argTypes.zip(paramTypes).zipWithIndex.foreach {
            case (tuple, id) => tuple match {
              case (a, p) if a != p => throw TypeMissMatchException(s"function ${id}rd arguments type expected $p, but actual $a")
              case _ =>
            }
          }
        }

        FunCall(returnType, fun, inferredArgs)
      case _ => ???
    }
  }

  private def infer(f: Fun, env: TypeEnvironment): Fun = {
    val newEnv = new TypeEnvironment(Some(env))

    f.params.foreach {
      case (name, t) => newEnv.reg(name, t)
    }

    val paramTypes = f.params.map(_._2)
    val body = infer(f.proc, newEnv)

    val funcType = FuncType(body.getType, paramTypes)

    Fun(funcType, f.params, body)
  }

  private val binOpMap = Map[String, (String, Type, Type) => Option[Type]](
    "+" -> arith,
    "-" -> arith,
    "*" -> arith,
    "/" -> arith,
    "%" -> arith,
    "==" -> comp,
    "!=" -> comp,
    "<" -> comp,
    "<=" -> comp,
    ">" -> comp,
    ">=" -> comp
  )

  private def comp(op: String, t1: Type, t2: Type): Option[Type] = {
    if (t1 == t2) Some(BoolType)
    else None
  }

  private def arith(op: String, t1: Type, t2: Type): Option[Type] = {
    (t1, t2) match {
      case (IntType, IntType) => Some(IntType)
      case (IntType, StringType) => Some(StringType)
      case (StringType, StringType) => Some(StringType)
      case (StringType, IntType) => Some(StringType)
      case (StringType, BoolType) => Some(StringType)
      case (BoolType, StringType) => Some(StringType)
      case _ => None
    }
  }

  private def logical(op: String, t1: Type, t2: Type): Option[Type] = {
    (t1, t2) match {
      case (BoolType, BoolType) => Some(BoolType)
      case _ => None
    }
  }
}
