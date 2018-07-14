import org.scalatest.FunSuite

class ParFileTest extends FunSuite {

  test("testGetMap") {
    val pmFile = new ParFile("src/main/resource/test.par")
    pmFile.getMap("46303595")

  }

}
