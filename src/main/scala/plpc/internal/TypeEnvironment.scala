package plpc.internal

import scala.collection.mutable.Map

class TypeEnvironment(parent: Option[TypeEnvironment]){
  val table = Map[String, Type]()

  def reg(name: String, t: Type): Unit = {
    table(name) = t
  }

  def get(name: String): Option[Type] = {
    table.get(name) match {
      case t @ Some(_) => t
      case None => parent match {
        case Some(p) => p.get(name)
        case None => None
      }
    }
  }
}
