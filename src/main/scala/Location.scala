case class Location(lat:Double,lon:Double) extends CalcMesh {

  /* 緯度経度の繰り返し演算用　*/
  private var tmp_lat = lat
  private var tmp_lon = lon

  def printMesh(meshSize:String):String = LtLn2mesh(meshSize, meshOrder, meshMap)

  def createMesh(meshSize:String):MeshCode = new MeshCode(printMesh(meshSize),meshSize)

  /* 緯度経度から該当するメッシュを出力する関数 */
  def LtLn2mesh(meshsize: String, meshOrder: List[List[String]], meshMap: Map[String, MeshInfo]):String = {
    if (tmp_lat != lat || tmp_lon != lon){
      tmp_lat = lat
      tmp_lon = lon
    }

    if (validLtLn(lat, lon)){
        /* 処理順序を定義 */
        val meshProcOrder = meshOrder.filter(x => x.contains(meshsize)).head
          .foldLeft(List[String]())((x, y) => if (x.contains(meshsize)) x else x :+ y)

        /* 処理順序に沿って、メッシュを左から右へ*/
        meshProcOrder.foldLeft("")(LtLn2mesh_main)
    }
    else{
      ""
    }
  }

  private def LtLn2mesh_main(aft:String,src:String) :String = {
      var ans:String = ""
      val meshinfo = meshMap(src)
      meshinfo.calcType match {
      /* 最初の１次メッシュの場合 */
      case 0 =>
        val LtTup = Lt21stMesh(tmp_lat)
        val LnTup = Ln21stMesh(tmp_lon)
        tmp_lat = LtTup.residual
        tmp_lon = LnTup.residual
        ans = LtTup.mesh + LnTup.mesh
      /* 商を一桁ずつで決まる場合 */
      case 1 =>
        val LtTup = GeneralAttr2Mesh(tmp_lat, meshinfo.lat)
        val LnTup = GeneralAttr2Mesh(tmp_lon, meshinfo.lon)
        tmp_lat = LtTup.residual
        tmp_lon = LnTup.residual
        ans = LtTup.mesh + LnTup.mesh
      /* 両方の商で決まる場合 */
      case 2 =>
        val LtTup = GeneralAttr2Mesh(tmp_lat, meshinfo.lat)
        val LnTup = GeneralAttr2Mesh(tmp_lon, meshinfo.lon)
        tmp_lat = LtTup.residual
        tmp_lon = LnTup.residual
        ans = MeshNumByLtLn(LtTup.mesh, LnTup.mesh)
      case _ => ""
    }
    aft.concat(ans)

  }

}
