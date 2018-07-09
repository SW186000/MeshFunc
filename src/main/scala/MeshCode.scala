import io.circe.parser._
import io.circe.generic.auto._



class MeshCode(mesh:String,meshsize:String) extends CalcMesh {

  override def toString():String = {
    mesh
  }

  def getCentLocation:Location = {
    new Location(0,0)
  }

  def getOtherMesh(eastnum:Int,northnum:Int):MeshCode = {

    new MeshCode("555","1km")
  }


  def FourMesh2Lat():(String,Double) = {
    ("",333)
  }

  def FourMesh2Lon():Double = {
    333
  }

}

