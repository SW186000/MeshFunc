object TKY2JGD {

  /*
     TYO2JGD（旧日本測地系の緯度経度を世界測地系に変換)。
  入出力は緯度経度
  */

  def main(LatTKY:Double,LonTKY:Double):(Option[Double],Option[Double]) = {
    /* パラメータ参照 */
    val pardata = new ParFile("src/main/resource/TKY2JGD.par")

    /* bilinear補間 */
    val dBdL = Bilinear(LatTKY,LonTKY,pardata)

    /* 補間の追加
    * ただし、補間がNULLの場合、NULLを返す。
    * */
    if (dBdL._1.isEmpty || dBdL._2.isEmpty){
      (None,None)
    }
    else {
      (Some(LatTKY + dBdL._1.get / 3600), Some(LonTKY + dBdL._2.get / 3600))
    }
  }

  /*
  Ver.1.3  1999/3/11  (C) Mikio TOBITA 飛田幹男，国土地理院
  Bilinear interpolationをする準備とBilinear interpolationのcall
  入力座標 lat:y, lon:x   単位は度
  # from Ver.1.3.78 2001/11/22
  */
  def Bilinear(LatTKY: Double, LonTKY: Double, Parfile: ParFile):(Option[Double],Option[Double]) = {
  /*
  # マイナスの緯度（南緯），経度（西経）等日本の領土外は標準地域メッシュコードが定義されていない場所では，
  # 地域毎の変換パラメータがないので，直ちにOutsideにとぶ。
  # この判定がなかったVer.1.3.77では，メッシュコードを検索に行ってしまい。見つかってしまうバグがあった。
  # なお，このバグは日本領土内では結果にまったく影響しない。
  */

    /* メッシュコード作成 */
    val Mesh1km = Location(LatTKY,LonTKY).createMesh("1km")

    if (Mesh1km.toString() == ""){
      (None,None)
    }
    else{

      /* 東、北、北東のメッシュを取得　*/
      val meshE = Mesh1km.getOtherMesh(1,0)
      val meshN = Mesh1km.getOtherMesh(0,1)
      val meshNE = Mesh1km.getOtherMesh(1,1)

      /* 補正データを取得 */
      val d0 = Parfile.getMap(Mesh1km)
      val d1 = Parfile.getMap(meshE)
      val d2 = Parfile.getMap(meshN)
      val d3 = Parfile.getMap(meshNE)

      /* 剰余などを使ってInterpolする*/
      val dB = interpol(d0.lat,d1.lat,d2.lat,d3.lat,0.0,0.0)
      val dL = interpol(d0.lon,d1.lon,d2.lon,d3.lon,0.0,0.0)
      (Some(dB),Some(dL))

    }

  }

  def interpol(u1:Double, u2:Double, u3:Double, u4:Double, X:Double,Y:Double): Double ={
    /*' Ver.2.1
    1999 / 2 / 4 (C) Mikio TOBITA 飛田幹男
    ， 国土地理院
    ' Bilinear interpolation
    ' X0to1
    , Y0to1は0以上1未満
    '
    ' ^
    ' Y|
      u3   u4
    ' u1   u2  -> X
    '
    */
    val a = u1
    val B = u2 - u1
    val C = u3 - u1
    val D = u4 - u2 - u3 + u1
    a + B * X + C * Y + D * X * Y

  }

}
