trait CalcMesh {

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
}
