trait CalcMesh {
  /* メッシュ毎のパラメータ
  * １次メッシュの緯度経度は不要なのでダミー */
  val meshMap:Map[String,(Double,Double,Int)] = Map(
    "1st" -> (0,0,0),
    "2nd" -> (0.0833333,0.125,1),
    "1km" -> (0.0083333,0.0125,1),
    "500m" -> (0.00416665,0.00625,2),
    "250m" -> (0.002083325,0.003125,2),
    "125m" -> (0.0010416625,0.0015625,2),
    "100m" -> (0.00083333,0.00125,1),
    "50m" -> (0.000416665,0.0000625,2),
    "10m" -> (0.000083333,0.000125,1)
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
  def Lt21stMesh(lat:Double):(String,Double) = {
    val d1 = (lat * 1.5)
    (d1.toInt.toString(),(d1 - d1.toInt) * 40 / 60)
  }

  /* 経度の１次メッシュコードと剰余を返す*/
  def Ln21stMesh(lon:Double):(String,Double) = {
    ((lon - 100.0).toInt.toString(),(lon - lon.toInt))
  }

  /* 緯度経度について、メッシュ番号と一般的な剰余を返す*/
  def GeneralAttr2Mesh(Attr:Double,DivAttr:Double):(String,Double) = {
    ((Attr / DivAttr).toInt.toString(), (Attr % DivAttr))
  }

  /* メッシュ番号を緯度経度両方で判定する場合 */
  def MeshNumByLtLn(Attr1:String,Attr2:String):String = {
    (Attr1.toInt + Attr2.toInt * 2 + 1).toString()
  }

  /* 1次メッシュの南西端の取得(緯度） */
  def FstMesh2Lt(mesh:String,meshsize:String):(String,Double) = {
    (mesh.substring(4,mesh.length()),mesh.substring(0,1).toDouble / 1.5)
  }

  /* 1次メッシュの南西端の取得(経度） */
  def FstMesh2Ln(mesh:String,meshsize:String):(String,Double) = {
    (mesh.substring(4,mesh.length()),mesh.substring(2,3).toInt + 100)
  }

  /* メッシュ番号について緯度経度とメッシュの余りを返す*/
  def GeneralMesh2Attr(Attr:String,DivAttr:Double,flg:String):(String,Double,String) = {
    if (flg == "lat") ("0",0.0,"lat")
    else if (flg == "lon") ("0",0.0,"lon")
    else ("-",0.0,"-")
  }

}
