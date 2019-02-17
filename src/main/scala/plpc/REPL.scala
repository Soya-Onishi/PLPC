package plpc

import plpc.internal._

object REPL extends App {
  private def read(prompt: String): Option[String] = {
    val text = scala.io.StdIn.readLine(prompt)

    if (text == null) None
    else if (text.length == 0) None
    else Some(text)
  }

  val prompt = "PLPC > "
  val interpreter = new Interpreter
  val parser = new Parser
  var continue = true

  while (continue) {
    read(prompt) match {
      case None =>
      case Some(t) =>
        if (t == ":q")
          continue = false
        else {
          val ast = parser(t)
          if (ast.successful) {
            try {
              val value = interpreter.eval(ast.get)

              println(s" ===> $value")
            } catch {
              case NotFoundException(m, _) => println(m)
              case NotAssignableException(m, _) => println(m)
              case InvalidConditionValueException(m, _) => println(m)
              case InvalidFunctionCallException(m, _) => println(m)
              case InvalidASTException(m, _) => println(m)
            }
          }
        }
    }
  }
}
