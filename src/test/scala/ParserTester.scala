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
    assert(parser(
      """
        |val a = 20
        |val b = 10
      """.stripMargin).successful == true)
    assert(parser("val a = b").successful == true)
  }

  it should "fail" in {
    assert(parser("val = 20").successful == false)
    assert(parser("var = 20").successful == false)
    assert(parser("val a, b = 20").successful == false)
    assert(parser("var a, b = 20").successful == false)
    assert(parser("val = 20;").successful == false)
    assert(parser("var = 20;").successful == false)
    assert(parser("val a = ").successful == false)
    assert(parser("var a = ").successful == false)
    assert(parser("val a = ;").successful == false)
    assert(parser("var a = ;").successful == false)
  }

  "Declare function test" should "success" in {
    assert(parser(
      """
        |def function() = 1
      """.stripMargin).successful == true)

    assert(parser(
      """
        |def function() = {
        |  val a = 20
        |  a + 20
        |}
      """.stripMargin
    ).successful == true)

    assert(parser(
      """
        |def function(n: Int) = n
      """.stripMargin
    ).successful == true)

    assert(parser(
      """
        |def function(m: Int, n: Int) = n + m
      """.stripMargin
    ).successful == true)
  }

  it should "fail" in {
    assert(parser(
      """
        |def function = 1
      """.stripMargin
    ).successful == false)

    assert(parser(
      """
        |def function() =
      """.stripMargin
    ).successful == false)

    assert(parser(
      """def () = 1"""
    ).successful == false)

    assert(parser(
      """def function() 1"""
    ).successful == false)

    assert(parser(
      """def function()"""
    ).successful == false)

    assert(parser(
      """def function(n) = n"""
    ).successful == false)

  }

  "If expression" should "success" in {
    assert(parser("if(a == 10) b = 20 else b = 10").successful == true)
    assert(parser(
      """
        |if(a == 10) {
        |  b = 20
        |} else {
        |  b = 10
        |}
      """.stripMargin
    ).successful == true)
    assert(parser(
      """
        |if(a == 10) {
        |  b = 20
        |} else
        |  b = 10
      """.stripMargin
    ).successful == true)
    assert(parser(
      """
        |if(a == 10) {} else {}
      """.stripMargin
    ).successful == true)
  }

  it should "fail" in {
    assert(parser("if(a == 10) b = 20").successful == false)
  }
}
