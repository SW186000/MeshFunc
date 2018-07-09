class Location(lat:Double,lon:Double) extends CalcMesh {

  /* 緯度経度の繰り返し演算用　*/
  private var tmp_lat = lat
  private var tmp_lon = lon

  def printMesh(meshSize:String):String = LtLn2mesh(lat,lon,meshSize,meshOrder,meshMap)

  def createMesh(meshSize:String):MeshCode = new MeshCode(printMesh(meshSize),meshSize)

  /* 緯度経度から該当するメッシュを出力する関数 */
  def LtLn2mesh(lat:Double,lon:Double,meshsize:String,meshOrder:List[List[String]],meshMap:Map[String,(Double,Double,Int)]):String = {
    if (tmp_lat != lat || tmp_lon != lon){
      tmp_lat = lat
      tmp_lon = lon
    }

    if (validLtLn(lat, lon)){
        /* 処理順序を定義 */
        val meshProcOrder = meshOrder.filter(x => x.contains(meshsize)).head
          .foldLeft(List[String]())((x, y) => if (x.contains(meshsize)) x else x :+ y)

        /* 処理順序に沿って、メッシュを左から右へ*/
        meshProcOrder.foldLeft("")(Ltln2mesh)
    }
    else{
      ""
    }
  }

  def Ltln2mesh(aft:String,src:String) :String = {
      var ans:String = ""
      meshMap(src)._3 match {
      /* 最初の１次メッシュの場合 */
      case 0 => {
        val LtTup = Lt21stMesh(tmp_lat)
        val LnTup = Ln21stMesh(tmp_lon)
        tmp_lat = LtTup._2
        tmp_lon = LnTup._2
        ans = LtTup._1 + LnTup._1
      }
      /* 商を一桁ずつで決まる場合 */
      case 1 => {
        val LtTup = GeneralAttr2Mesh(tmp_lat, meshMap(src)._1)
        val LnTup = GeneralAttr2Mesh(tmp_lon, meshMap(src)._2)
        tmp_lat = LtTup._2
        tmp_lon = LnTup._2
        ans = LtTup._1 + LnTup._1
      }
      /* 両方の商で決まる場合 */
      case 2 => {
        val LtTup = GeneralAttr2Mesh(tmp_lat, meshMap(src)._1)
        val LnTup = GeneralAttr2Mesh(tmp_lon, meshMap(src)._2)
        tmp_lat = LtTup._2
        tmp_lon = LnTup._2
        ans = MeshNumByLtLn(LtTup._1, LnTup._1)
      }
      case _ => ""
    }
    aft.concat(ans)



  }

}
