package plpc.internal

sealed abstract class Value {
  def getType: Type
}

/*
case object Wrong extends Value {
  override def toString: String = "Wrong"
  override def gettype
}
*/

case object NoValue extends Value {
  override def toString: String = "NoValue"
  override def getType: Type = UnitType
}

case class IntConst(n: Int) extends Value {
  override def toString: String = n.toString
  override def getType: Type = IntType
}

case class StrConst(s: String) extends Value {
  override def toString: String = s
  override def getType: Type = StringType
}

case class BoolConst(v: Boolean) extends Value {
  override def toString: String = v.toString
  override def getType: Type = BoolType
}

case class Fun(t: Type, params: List[(String, Type)], proc: AST) extends Value {
  override def toString: String = {
    val p = params.tail.foldLeft(params.head._1){
      case (l, r) => s"$l, ${r._1}"
    }

    s"Fun($p)"
  }

  override def getType: Type = t
}
case class Closure(fun: Fun, env: Environment) extends Value {
  override def toString: String = fun.toString
  override def getType: Type = fun.getType
}