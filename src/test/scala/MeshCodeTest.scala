import org.scalatest.FunSuite

class MeshCodeTest extends FunSuite {

  test("testMesh2SWltln") {
    val meshcd = new MeshCode("54394151","1km")
    print(meshcd.mesh2SWltln("lat"))
    print(meshcd.mesh2SWltln("lon"))
    /*assert(meshcd.mesh2SWltln("lat") == 36.38832751)
    assert(meshcd.mesh2SWltln("lon") == 139.15760268)
    */

  }

}
