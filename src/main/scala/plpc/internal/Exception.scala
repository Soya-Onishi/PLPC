package plpc.internal

final case class NotFoundException(message: String, cause: Throwable = None.orNull) extends Exception(message, cause)
final case class NotAssignableException(message: String, cause: Throwable = None.orNull) extends Exception(message, cause)
final case class InvalidConditionValueException(message: String, cause: Throwable = None.orNull) extends Exception(message, cause)
final case class InvalidFunctionCallException(message: String, cause: Throwable = None.orNull) extends Exception(message, cause)
final case class InvalidASTException(message: String, cause: Throwable = None.orNull) extends Exception(message, cause)