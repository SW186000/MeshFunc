object MeshUtil {
  /* １点の中心とした半径Xメートルのメッシュコードを取得する
  * メッシュコードの取得方法としては中央点と当該点の距離を測って
  * 指定メートルの範囲を取得する　
  * */
  def PointAroundMesh(lat:Double,lon:Double,meshsize:String,meter:Double):List[MeshCode] = {
    val tgtPoint = Location(lat,lon)

    val meter2lat = Meter2Lat(tgtPoint,meter)
    val meter2lon = Meter2Lon(tgtPoint,meter)

    val divMeshinfo = tgtPoint.meshMap(meshsize)

    /* input:divMeshinfo, meter2lat,meter2lon,tgtPoint
    * output:List[MeshCode]
    * １点から端点の２点を取り、端点の２点間のメッシュコードを取得する。
    * メッシュコードサイズあh指定されているもの
    * */

    /*
    input:List[MeshCode],tgtPoint
    output:List[MeshCode]
    メッシュコードの中央点をとり、先程指定した端点と中央点の距離を一個一個確認する。
     */

    List(Location(lat,lon).createMesh(meshsize))

  }

  def Meter2Lat(location: Location, d: Double):Double = 0

  def Meter2Lon(location: Location, d: Double):Double = 0

}
