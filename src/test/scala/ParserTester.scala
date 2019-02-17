import org.scalatest._
import plpc.internal.Parser

class ParserTester extends FlatSpec with Matchers {
  val parser = new Parser

  "Declare variable test" should "success" in {
    assert(parser("val a = 20").successful == true)
    assert(parser("val a = 20; val b = 10").successful == true)
    assert(parser("val a = 20; val b = 10;").successful == true)
    assert(parser("var a = 20").successful == true)
    assert(parser("var a = 20; var b = 10").successful == true)
    assert(parser("var a = 20; var b = 10;").successful == true)
    assert(parser("var a = 20; val b = 10").successful == true)
    assert(parser("val a = 20; var b = 10").successful == true)
  }

  it should "fail" in {
    assert(parser("val = 20").successful == false)
    assert(parser("var = 20").successful == false)
    assert(parser("val a, b = 20").successful == false)
    assert(parser("var a, b = 20").successful == false)
    assert(parser("val = 20;").successful == false)
    assert(parser("var = 20;").successful == false)
  }
}
