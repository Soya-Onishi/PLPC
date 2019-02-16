import org.scalatest._
import plpc.internal.Parser
class ParserTester extends FlatSpec with Matchers {
  val parser = new Parser

  "Parser" should "success" in {
    assert(parser("val a = 20").successful == true)
  }

  it should "fail" in {
    assert(parser("val = 20").successful == false)
  }
}
