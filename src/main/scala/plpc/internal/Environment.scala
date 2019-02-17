package plpc.internal

import scala.collection.mutable.Map

sealed abstract class EnvValue
case class VarValue(v: Value) extends EnvValue
case class ValValue(v: Value) extends EnvValue
case class FunValue(v: Value) extends EnvValue

class Environment(parent: Option[Environment]) {
  val table = Map[String, EnvValue]()

  def regVar(name: String, v: Value): Unit = {
    table(name) = VarValue(v)
  }

  def regVal(name: String, v: Value): Unit = {
    table(name) = ValValue(v)
  }

  def regFun(name: String, v: Value): Unit = {
    table(name) = FunValue(v)
  }

  def lookup(name: String): Option[Value] = {
    table.get(name) match {
      case Some(x) => x match {
        case VarValue(v) => Some(v)
        case ValValue(v) => Some(v)
        case FunValue(v) => Some(v)
      }
      case None => parent match {
        case Some(p) => p.lookup(name)
        case None => None
      }
    }
  }

  def setenv(name: String, value: Value): Unit = {
    table.get(name) match {
      case Some(v) => v match {
        case VarValue(_) => table(name) = VarValue(value)
        case _ => throw new NotAssignableException(s"$name is not assignable")
      }
      case None => parent match {
        case Some(p) => p.setenv(name, value)
        case None => throw new NotFoundException(s"$name is not found")
      }
    }
  }

  def dump(): String = {
    table.keysIterator.foldLeft("") { (s, k) =>
      val newLine = table.get(k) match {
        case Some(ev) => ev match {
          case VarValue(v) => s"(var) $v\n"
          case ValValue(v) => s"(val) $v\n"
          case FunValue(v) => s"(fun) $v\n"
        }
        case None => ""
      }

      s + newLine
    }
  }
}
