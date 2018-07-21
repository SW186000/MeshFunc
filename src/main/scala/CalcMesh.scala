trait CalcMesh {
  /* メッシュ毎のパラメータ
  * １次メッシュの緯度経度は不要なのでダミー */
  val meshMap:Map[String,MeshInfo] = Map(
    "1st" ->  MeshInfo(0,0,0),
    "2nd" ->  MeshInfo(0.083333333,0.125,1),
    "1km" ->  MeshInfo(0.008333333,0.0125,1),
    "500m" ->  MeshInfo(0.004166665,0.00625,2),
    "250m" -> MeshInfo(0.002083325,0.003125,2),
    "125m" -> MeshInfo(0.001041662,0.0015625,2),
    "100m" -> MeshInfo(0.000833333,0.00125,1),
    "50m" -> MeshInfo(0.000416665,0.0000625,2),
    "10m" -> MeshInfo(0.000083333,0.000125,1)
  )

  /* メッシュの計算順序
   * 複数の計算手法があるため、リストを複数用意 */
  val meshOrder = List(
    List("1st","2nd","1km","500m","250m","125m"),
    List("1st","2nd","1km","100m","50m"),
    List("1st","2nd","1km","100m","10m")
  )
  /*
  メッシュ範囲対象外の場合Falseを返し、対象の場合Trueを返す
  */
  def validLtLn(lat:Double,lon:Double):Boolean = {
    !(lat < 20 || lat > 46 || lon < 120 || lon > 154)
  }

  /* 緯度の１次メッシュコードと剰余を返す
  * 40 / 60 は40分の剰余の度分秒計算 */
  def Lt21stMesh(lat:Double):CalcResidual = {
    val d1 = lat * 1.5
    CalcResidual(d1.toInt.toString,(d1 - d1.toInt) * 40 / 60)
  }

  /* 経度の１次メッシュコードと剰余を返す*/
  def Ln21stMesh(lon:Double):CalcResidual = CalcResidual((lon - 100.0).toInt.toString, lon - lon.toInt)

  /* 緯度経度について、メッシュ番号と一般的な剰余を返す
  * 境界点の場合は北もしくは東に割り当てられるようにする。
  * 上2行が微少量を加えるのは，lon=138.45のとき3次が5とならずに6となるように
  * */
  def GeneralAttr2Mesh(Attr:Double,DivAttr:Double):CalcResidual = CalcResidual(((Attr + 0.00000000001) / DivAttr).toInt.toString, Attr % DivAttr)


  /* メッシュ番号を緯度経度両方で判定する場合 */
  def MeshNumByLtLn(Attr1:String,Attr2:String):String = (Attr1.toInt * 2 + Attr2.toInt + 1).toString


  /* 1次メッシュの南西端の取得(緯度） */
  def FstMesh2Lt(mesh:String):Double = mesh.slice(0,2).toDouble / 1.5

  /* 1次メッシュの南西端の取得(経度） */
  def FstMesh2Ln(mesh:String):Double = mesh.slice(2,4).toInt + 100

  /* メッシュ番号について緯度経度とメッシュの余りを返す
  * 境界点の場合は北もしくは東に割り当てられるようにする。
  * そのため、微小量を追加するようにする。
  * */
  def GeneralMesh2Attr(Attr:String,MultiplyAttr:Double,ltLnFlg:String):Double = {
    if (Attr.length() < 2){
      0
    }
    else {
      if (ltLnFlg == "lat") Attr.substring(0,1).toInt * MultiplyAttr
      else if (ltLnFlg == "lon") Attr.substring(1,2).toInt * MultiplyAttr
      else 0
    }
  }

  /* 緯度経度のプラスを商で判断する。*/
  def MeshNumByLtLn(Attr1:String,meshInfo:MeshInfo,ltLnFlg:String):Double = {
    if (Attr1.length() < 1){
      0
    }
    else {
      if ((Attr1.toInt == 3 || Attr1.toInt == 4) && ltLnFlg == "lat")  meshInfo.lat
      else if ((Attr1.toInt == 2 || Attr1.toInt == 4) && ltLnFlg == "lon") meshInfo.lon
      else 0
    }
  }

}
