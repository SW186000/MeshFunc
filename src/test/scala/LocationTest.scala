import org.scalatest.FunSuite

class LocationTest extends FunSuite {

  test("testLtLn2mesh") {
      val loc = new Location(36.20,138.4375)
    /*
      assert(loc.printMesh("1st") == "5339")
      assert(loc.printMesh("2nd") == "533930")
      assert(loc.printMesh("1km") == "53393073")
      assert(loc.printMesh("500m") == "533930731")
      assert(loc.printMesh("250m") == "5339307312")
      assert(loc.printMesh("125m") == "53393073124")
    */
    print(loc.printMesh("1km"))

  }


}
