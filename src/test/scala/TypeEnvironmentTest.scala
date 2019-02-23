import org.scalatest._
import plpc.internal._

class TypeEnvironmentTest extends FlatSpec with Matchers {
  val parser = new Parser

  "Val Declare Test" should "appropriate Type inferred" in {
    val inferior = new TypeInferenceEngine

    val ast = parser(
      """
        |val a = 20
        |val b: Int = 20
        |val c = "String"
        |val d: String = "String"
        |val e = true
        |val f: Bool = true
        |val g = false
        |val h: Bool = false
      """.stripMargin
    )

    assert(ast.successful == true)
    inferior(ast.get)

    val env = inferior.typeEnv

    val expectedTypes = List(IntType, IntType, StringType, StringType, BoolType, BoolType, BoolType, BoolType)
    val variableNames = List("a", "b", "c", "d", "e", "f", "g", "h")

    expectedTypes.zip(variableNames).foreach {
      case (t, n) =>
        val v = env.get(n)
        assert(v.isDefined == true, s"$n must be defined ")
        assert(v.get == t, s"$n must be $t")
    }
  }

  "Var Declare ReAssign Test" should "raise TypeMissMatchException" in {
    assertThrows[TypeMissMatchException] {
      val inferior = new TypeInferenceEngine
      val ast = parser("""var a = 20; a = true""")
      assert(ast.successful == true)
      inferior(ast.get)
    }

    assertThrows[TypeMissMatchException] {
      val inferior = new TypeInferenceEngine
      val ast = parser("""var a: Int = 1; a = true""")
      assert(ast.successful == true)
      inferior(ast.get)
    }

    assertThrows[TypeMissMatchException] {
      val inferior = new TypeInferenceEngine
      val ast = parser("""var a = "abc"; a = true""")
      assert(ast.successful == true)
      inferior(ast.get)
    }
  }

  it should "raise NotFoundException" in {
    assertThrows[NotFoundException] {
      val inferior = new TypeInferenceEngine
      val ast = parser("""var a = b""")
      assert(ast.successful == true)
      inferior(ast.get)
    }
  }

  "Val Assign from Variable" should "a must be type of b" in {
    val inferior = new TypeInferenceEngine
    val ast = parser("val b = 2; val a = b")

    assert(ast.successful == true)
    noException should be thrownBy inferior(ast.get)

    assert(inferior.typeEnv.get("a").get == IntType)
    assert(inferior.typeEnv.get("b").get == IntType)
  }

  "Declare function" should "raise TypeMissMatchException" in {
    val inferior = new TypeInferenceEngine
    val ast = parser("def func(n: Int): String = n")

    assert(ast.successful == true)
    an[TypeMissMatchException] should be thrownBy inferior(ast.get)
  }

  it should "infer appropriate return type" in {
    {
      val inferior = new TypeInferenceEngine
      val ast = parser("def func(n: Int) = n")

      assert(ast.successful == true)
      noException should be thrownBy inferior(ast.get)

      val returnType = inferior.typeEnv.get("func").get.asInstanceOf[FuncType].t
      assert(returnType == IntType)
    }

    {
      val inferior = new TypeInferenceEngine
      val ast = parser("""def func(n: Int) = "String"""")

      assert(ast.successful == true)
      noException should be thrownBy inferior(ast.get)

      val returnType = inferior.typeEnv.get("func").get.asInstanceOf[FuncType].t
      assert(returnType == StringType)
    }

    {
      val inferior = new TypeInferenceEngine
      val ast = parser("""def func(n: Bool) = n""")

      assert(ast.successful == true)
      noException should be thrownBy inferior(ast.get)

      val returnType = inferior.typeEnv.get("func").get.asInstanceOf[FuncType].t
      assert(returnType == BoolType)
    }
  }

  it should "appropriate param type" in {
    val inferior = new TypeInferenceEngine
    val ast = parser("def func(n: Int, s: String, b: Bool): String = s")

    assert(ast.successful == true)
    noException should be thrownBy inferior(ast.get)

    val paramTypes = inferior.typeEnv.get("func").get.asInstanceOf[FuncType].paramTypes
    val expectedTypes = List(IntType, StringType, BoolType)

    paramTypes.zip(expectedTypes).foreach { case (a, e) => assert(e == a) }
  }

  "FuncCall Test" should "raise TypeMissMatchException" in {
    {
      val inferior = new TypeInferenceEngine
      val ast = parser(
        """
          |def func(n: Int) = n
          |func(true)
        """.stripMargin)

      assert(ast.successful == true)
      an[TypeMissMatchException] should be thrownBy inferior(ast.get)
    }
  }

  it should "raise InvalidFunctionCallException" in {
    {
      val inferior = new TypeInferenceEngine
      val ast = parser(
        """
          |def func(s: String) = s
          |func("abc", 1)
        """.stripMargin)

      assert(ast.successful == true)
      an[InvalidFunctionCallException] should be thrownBy inferior(ast.get)
    }

    {
      val inferior = new TypeInferenceEngine
      val ast = parser(
        """
          |def func(s: String) = s
          |func()
        """.stripMargin)

      assert(ast.successful == true)
      an[InvalidFunctionCallException] should be thrownBy inferior(ast.get)
    }
  }
}

