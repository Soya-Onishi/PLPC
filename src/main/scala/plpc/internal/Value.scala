package plpc.internal

sealed abstract class Value

case object Wrong extends Value {
  override def toString: String = "Wrong"
}

case object NoValue extends Value {
  override def toString: String = "NoValue"
}

case class IntConst(n: Int) extends Value {
  override def toString: String = n.toString
}

case class StrConst(s: String) extends Value {
  override def toString: String = s
}

case class BoolConst(v: Boolean) extends Value {
  override def toString: String = v.toString
}

case class Fun(params: List[String], proc: AST) extends Value {
  override def toString: String = {
    val p = params.tail.foldLeft(params.head){
      case (l, r) => s"$l, $r"
    }

    s"Fun($p)"
  }
}
case class Closure(fun: Fun, env: Environment) extends Value {
  override def toString: String = fun.toString
}