import org.scalatest.FunSuite

class TKY2JGDTest extends FunSuite {

  test("testMain") {
    val a = TKY2JGD.main(36.10377,140.0878)
    println(a)

  }

}
