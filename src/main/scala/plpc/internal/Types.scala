package plpc.internal

sealed abstract class Type

sealed abstract class PrimitiveType extends Type

case object Unit extends PrimitiveType
case object Int extends PrimitiveType
case object String extends PrimitiveType
case object Double extends PrimitiveType
case object Boolean extends PrimitiveType