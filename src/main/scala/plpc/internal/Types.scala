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
}
case object IntType extends PrimitiveType {
  override val getSignature = "I"
}
case object StringType extends PrimitiveType {
  override val getSignature = "S"
}

case object BoolType extends PrimitiveType {
  override val getSignature = "B"
}

case class FuncType(t: Type, paramTypes: List[Type]) extends Type {
  override lazy val getSignature: String = {
    val params = paramTypes.foldLeft(""){ _ + _.getSignature}

    s"($params)${t.getSignature}"
  }
}