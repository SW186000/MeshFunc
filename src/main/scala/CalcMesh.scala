trait CalcMesh {
  /* メッシュ毎のパラメータ */
  val meshMap:Map[String,(Double,Double,Int)]

  /* メッシュの計算順序 */
  val meshOrder:List[List[String]]


  /*
  メッシュ範囲対象外の場合Falseを返し、対象の場合Trueを返す
  */
  def validLtLn(lat:Double,lon:Double):Boolean = {
    !(lat < 20 || lat > 46 || lon < 120 || lon > 154)
  }



  /* 緯度経度が指定されたメッシュコードになるまで追加を繰り返す。
  緯度経度の剰余は引き継いで、メッシュ情報リストを徐々に除外していく。

  def LtLnCalc(midData:(String,(Double,Double)),meshsize:String,ML:List[MeshInfo]):(String,(Double,Double)) = {
    if (ML.isEmpty) {
      (midData)
    }
    else if (ML.head.Name == meshsize) {
      val addmesh = LtLn2AddMesh(midData._2._1, midData._2._2, ML.head)
      (midData._1 + addmesh._1, addmesh._2)
    }
    else {
      val addmesh = LtLn2AddMesh(midData._2._1, midData._2._2, ML.head)
      LtLnCalc((midData._1 + addmesh._1, addmesh._2) , meshsize, ML.tail)
    }
  }
     */

  def LtLn2AddMesh(lat:Double,lon:Double,ProcOrder:List[String],meshMap:Map[String,(Double,Double,Int)]): String = {
    println(ProcOrder)
    if (ProcOrder.isEmpty) ""
    else meshMap(ProcOrder.head)._3 match {
      /* 最初の１次メッシュの場合 */
      case 0 => {
        val LtTup = Lt21stMesh(lat)
        val LnTup = Ln21stMesh(lon)
        LtTup._1 + LnTup._1 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail,meshMap)
      }
      /* 商を一桁ずつで決まる場合 */
      case 1 => {
        val LtTup = GeneralAttr2Mesh(lat, meshMap(ProcOrder.head)._1)
        val LnTup = GeneralAttr2Mesh(lon, meshMap(ProcOrder.head)._2)
        LtTup._1 + LnTup._1 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail,meshMap)
      }
      /* 両方の商で決まる場合 */
      case 2 => {
        val LtTup = GeneralAttr2Mesh(lat, meshMap(ProcOrder.head)._1)
        val LnTup = GeneralAttr2Mesh(lon, meshMap(ProcOrder.head)._2)
        MeshNumByLtLn(LtTup._1, LnTup._1) + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail,meshMap)
      }
      case _ => ""
    }
  }
  /* 緯度の１次メッシュコードと剰余を返す*/
  def Lt21stMesh(lat:Double):(String,Double) = {
    val d1 = (lat * 1.5)
    (d1.toInt.toString(),(d1 - d1.toInt))
  }

  /* 経度の１次メッシュコードと剰余を返す*/
  def Ln21stMesh(lon:Double):(String,Double) = {
    ((lon - 100.0).toString(),(lon - lon.toInt))
  }

  /* 緯度経度について、メッシュ番号と一般的な剰余を返す*/
  def GeneralAttr2Mesh(Attr:Double,DivAttr:Double):(String,Double) = {
    ((Attr / DivAttr).toInt.toString(), (Attr % DivAttr))
  }

  /* メッシュ番号を緯度経度両方で判定する場合 */
  def MeshNumByLtLn(Attr1:String,Attr2:String):String = {
    (Attr1.toInt + Attr2.toInt * 2 + 1).toString()
  }
}
