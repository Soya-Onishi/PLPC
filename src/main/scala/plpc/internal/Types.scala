package plpc.internal

sealed abstract class Type {
  val getSignature: String
}

case object NoType extends Type {
  val getSignature = "-"
}

sealed abstract class PrimitiveType extends Type

case object UnitType extends PrimitiveType {
  override val getSignature = "U"
  override def toString: String = "Unit"
}
case object IntType extends PrimitiveType {
  override val getSignature = "I"
  override def toString: String = "Int"
}
case object StringType extends PrimitiveType {
  override val getSignature = "S"
  override def toString: String = "String"
}

case object BoolType extends PrimitiveType {
  override val getSignature = "B"
  override def toString: String = "Bool"
}

case class FuncType(t: Type, paramTypes: List[Type]) extends Type {
  override lazy val getSignature: String = {
    val params = paramTypes.foldLeft(""){ _ + _.getSignature}

    s"($params)${t.getSignature}"
  }

  override def toString: String = {
    val params = paramTypes.foldLeft(""){ _ + _.toString}

    s"($params)${t.toString}"
  }

}