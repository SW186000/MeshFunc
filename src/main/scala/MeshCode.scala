import io.circe.parser._
import io.circe.generic.auto._



class MeshCode(_mesh:String,meshsize:String)  {


  override def toString():String = {
    _mesh
  }

  /*
  緯度経度からメッシュコードを返す関数
  メッシュコードの範囲内にある場合、緯度経度を基にして、パラメータ順に
  部分まで繰り返し緯度経度を基にして値をつなげていく。
  */
  /* def Ltln2mesh(lat:Double,lon:Double,meshsize:String):String = {
    if (validLtLn(lat, lon)){

      /* 処理順序を定義 */
      val meshProcOrder = meshOrder.filter(x => x.contains(meshsize)).head
       .foldLeft(List[String]())(
         (y,keyword) =>
           if (y.contains(keyword)) y
           else y :+ keyword
       )

     /* 処理順序に沿って、メッシュをコンカチ */
      LtLn2AddMesh(lat,lon,meshProcOrder)._1
    }
    else{
      ""
    }
  }

  /*
  メッシュ範囲対象外の場合Falseを返し、対象の場合Trueを返す
  */
  def validLtLn(lat:Double,lon:Double):Boolean = {
    !(lat < 20 || lat > 46 || lon < 120 || lon > 154)
  }}
  */


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
/*
  def LtLn2AddMesh(lat:Double,lon:Double,ProcOrder:List[String]): (String,Double,Double) = {
    meshMap(ProcOrder.head)._3 match {
      /* 最初の１次メッシュの場合 */
      case 0 => {
        val LtTup = Lt21stMesh(lat)
        val LnTup = Ln21stMesh(lon)
        (LtTup._1 + LnTup._1 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail)._1,
          LtTup._2 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail)._2,
          LnTup._2 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail)._3
        )
      }
      /* 商を一桁ずつで決まる場合 */
      case 1 => {
        val LtTup = GeneralAttr2Mesh(lat, meshMap(ProcOrder.head)._1)
        val LnTup = GeneralAttr2Mesh(lat, meshMap(ProcOrder.head)._2)

        (LtTup._1 + LnTup._1 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail)._1,
          LtTup._2 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail)._2,
          LnTup._2 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail)._3
        )
      }
      /* 両方の商で決まる場合 */
      case 2 => {
        val LtTup = GeneralAttr2Mesh(lat, meshMap(ProcOrder.head)._1)
        val LnTup = GeneralAttr2Mesh(lat, meshMap(ProcOrder.head)._2)

        (LtTup._1 + LnTup._1 + MeshNumByLtLn(LtTup._1, LnTup._1),
          LtTup._2 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail)._2,
          LnTup._2 + LtLn2AddMesh(LtTup._2, LnTup._2, ProcOrder.tail)._3
        )
      }
      case _ => ("", 0, 0)
    }
  }
  */
  /* 緯度経度のmesh作成方法を判定
  * (メッシュコード、（緯度の剰余、経度の剰余））
  *
  def LtLn2AddMesh(lat:Double,lon:Double,MI:MeshInfo):(String,(Double,Double)) = {

    /* １次メッシュの緯度経度の場合*/
    if (MI.funcType == 0) {
      val LtTup = Lt21stMesh(lat)
      val LnTup = Ln21stMesh(lon)
      (LtTup._1 + LnTup._1, (LtTup._2,LnTup._2))
    }
    /* 個別に商で判定した後、データをマージする場合 */
    else if (MI.funcType == 1){
      val LtTup = GeneralAttr2Mesh(lat,MI.lat)
      val LnTup = GeneralAttr2Mesh(lon,MI.lon)
      (LtTup._1 + LnTup._1, (LtTup._2,LnTup._2))
    }
    /* 両方の商が１もしくは０で判定した後、データをマージする場合 */
    else if (MI.funcType == 2){
      val LtTup = GeneralAttr2Mesh(lat,MI.lat)
      val LnTup = GeneralAttr2Mesh(lon,MI.lon)
      (MeshNumByLtLn(LtTup._1,LnTup._1), (LtTup._2,LnTup._2))
    }
    else{
      ("0",(0,0))
    }
  }
  */
/*
  /* 緯度の１次メッシュコードと剰余を返す*/
  def Lt21stMesh(lat:Double):(String,Double) = {
    val d1 = (lat * 1.5)
    (d1.toInt.toString(),(d1 - d1.toInt))
  }

  /* 経度の１次メッシュコードと剰余を返す*/
  def Ln21stMesh(lon:Double):(String,Double) = {
    ((lon - 100.0).toString(),(lon - 100.0))
  }

  /* 緯度経度について、メッシュ番号と一般的な剰余を返す*/
  def GeneralAttr2Mesh(Attr:Double,DivAttr:Double):(String,Double) = {
    ((Attr / DivAttr).toString(), (Attr % DivAttr))
  }

  /* メッシュ番号を緯度経度両方で判定する場合 */
  def MeshNumByLtLn(Attr1:String,Attr2:String):String = {
    (Attr1.toInt + Attr2.toInt * 2 + 1).toString()
  }
*/

  def FirstMesh2Lt():Double = {
    23.0
  }

  def FirstMesh2Ln():Double = {
    24.0
  }

  def mesh2Ltln(flg:String):Double = {
    666
  }

  def getOtherMesh(eastnum:Int,northnum:Int):MeshCode = {
    new MeshCode("555","1km")
  }


  def GeneralMesh2Attr(MNum:Int,Attr:Double): Double = {
    333
  }

  def FourMesh2Lat():(String,Double) = {
    ("",333)
  }

  def FourMesh2Lon():Double = {
    333
  }

}

