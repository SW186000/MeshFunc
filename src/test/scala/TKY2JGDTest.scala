import org.scalatest.FunSuite

class TKY2JGDTest extends FunSuite {

  test("testMain") {
    val a = TKY2JGD.main(36.10377,140.0878)
    assert(a._1.get == 36.106961492)
    assert(a._2.get == 140.084521831)

  }

}
