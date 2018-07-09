class Location(lat:Double,lon:Double) extends CalcMesh {
  /* メッシュ毎のパラメータ */
  override val meshMap:Map[String,(Double,Double,Int)] = Map(
    "1st" -> (0.6666666,100,0),
    "2nd" -> (0.0833333,0.125,1),
    "1km" -> (0.0083333,0.0125,1),
    "500m" -> (0.00416665,0.00625,2),
    "250m" -> (0.002083325,0.003125,2),
    "125m" -> (0.0010416625,0.0015625,2),
    "100m" -> (0.00083333,0.00125,1),
    "50m" -> (0.000416665,0.0000625,2),
    "10m" -> (0.000083333,0.000125,1)
  )

  /* メッシュの計算順序 */
  override val meshOrder = List(
    List("1st","2nd","1km","500m","250m","125m"),
    List("1st","2nd","1km","100m","50m"),
    List("1st","2nd","1km","100m","10m")
  )

  var tmp_lat = lat
  var tmp_lon = lon

  def printMesh(meshSize:String):MeshCode = {
    val meshcd = LtLn2mesh(lat,lon,meshSize,meshOrder,meshMap)
    new MeshCode(meshcd,meshSize)
  }

  def LtLn2mesh(lat:Double,lon:Double,meshsize:String,meshOrder:List[List[String]],meshMap:Map[String,(Double,Double,Int)]):String = {
    if (validLtLn(lat, lon)){
      if (tmp_lat != lat || tmp_lon != lon){
        tmp_lat = lat
        tmp_lon = lon
      }

      /* 処理順序を定義 */
      val meshProcOrder = meshOrder.filter(x => x.contains(meshsize)).head
        .foldLeft(List[String]())((x,y) => if (x.contains(meshsize)) x else x :+ y)

      /* 処理順序に沿って、メッシュを左から右へ*/
      val ans: String = meshProcOrder.foldLeft("")(Ltlntomesh)

      ans
    }
    else{
      ""
    }
  }

  def Ltlntomesh(src:String,aft:String) :String = {
    if (meshMap.get(src).isEmpty) ""
    else meshMap(src)._3 match {
      /* 最初の１次メッシュの場合 */
      case 0 => {
        val LtTup = Lt21stMesh(tmp_lat)
        val LnTup = Ln21stMesh(tmp_lon)
        LtTup._1 + LnTup._1
      }
      /* 商を一桁ずつで決まる場合 */
      case 1 => {
        val LtTup = GeneralAttr2Mesh(lat, meshMap(src)._1)
        val LnTup = GeneralAttr2Mesh(lon, meshMap(src)._2)
        LtTup._1 + LnTup._1
      }
      /* 両方の商で決まる場合 */
      case 2 => {
        val LtTup = GeneralAttr2Mesh(lat, meshMap(src)._1)
        val LnTup = GeneralAttr2Mesh(lon, meshMap(src)._2)
        MeshNumByLtLn(LtTup._1, LnTup._1)
      }
      case _ => ""
    }

  }

}
