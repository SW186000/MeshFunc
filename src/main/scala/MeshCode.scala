import io.circe.parser._
import io.circe.generic.auto._



class MeshCode(mesh:String,meshsize:String) extends CalcMesh {

  override def toString():String = {
    mesh
  }

  /* メッシュコードから中心位置を取得する関数*/
  def getCentLocation:Location = {
    new Location(0,0)
  }

  /* 右左に移動して、異なるメッシュを取得する関数 */
  def getOtherMesh(eastnum:Int,northnum:Int):MeshCode = {
    new MeshCode("555","1km")
  }

  /* メッシュから南西端の緯度経度を取得する関数 */
  def mesh2SWltln(mesh:String,meshsize:String,meshOrder:List[List[String]],meshMap:Map[String,(Double,Double,Int)],flg:String):Double = {

      val attr:Double = 0
      /* 処理順序を定義 */
      val meshProcOrder = meshOrder.filter(x => x.contains(meshsize)).head
        .foldLeft(List[String]())((x, y) => if (x.contains(meshsize)) x else x :+ y)

      /* 処理順序に沿って、メッシュを左から右へ*/
      mesh2SWltln_main(attr,meshOrder,meshMap,mesh,flg)

  }

  /* メッシュ番号から緯度経度を出力する(loop用) */
  private def mesh2SWltln_main(attr:Double,meshOrder:List[List[String]],meshMap:Map[String,(Double,Double,Int)],mesh:String,flg:String):Double = {
    if (mesh.length() < 1) 0
    else if (meshMap(meshsize)._3 == 0 || flg == "lat") FstMesh2Lt(meshsize)._2 + mesh2SWltln_main(attr, meshOrder.tail,meshMap,mesh.substring(2,mesh.length()),flg)
    else if (meshMap(meshsize)._3 == 0 || flg == "lon") FstMesh2Ln(meshsize)._2  + mesh2SWltln_main(attr, meshOrder.tail,meshMap,mesh.substring(2,mesh.length()),flg)
    else if (meshMap(meshsize)._3 == 1 ) GeneralMesh2Attr(mesh,meshMap(meshsize)._2,flg)._2 + mesh2SWltln_main(attr, meshOrder.tail,meshMap,mesh.substring(2,mesh.length()),flg)
    else if (meshMap(meshsize)._3 == 2 ) MeshNumByLtLn(mesh,(meshMap(meshsize)._1,meshMap(meshsize)._2),flg)._2 + mesh2SWltln_main(attr, meshOrder.tail,meshMap,mesh.substring(1,mesh.length()),flg)
    else 0.0
  }

  def FourMesh2Lat():(String,Double) = {
    ("",333)
  }

  def FourMesh2Lon():Double = {
    333
  }

}

