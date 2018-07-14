import io.circe.parser._
import io.circe.generic.auto._



class MeshCode(mesh:String,meshsize:String) extends CalcMesh {

  override def toString():String = {
    mesh
  }

  /* メッシュコードから中心位置を取得する関数*/
  def getCentLocation:Location = {
    val swlat = mesh2SWltln("lat")
    val swlon = mesh2SWltln("lon")
    new Location(swlat + 0.5 * meshMap(meshsize)._1,swlon + 0.5 * meshMap(meshsize)._2)
  }

  /* 右左に移動して、異なるメッシュを取得する関数 */
  def getOtherMesh(eastnum:Int,northnum:Int):MeshCode = {
    val swlat = mesh2SWltln("lat")
    val swlon = mesh2SWltln("lon")
    val tgtlat = swlat + meshMap(meshsize)._1 * northnum
    val tgtlon = swlon + meshMap(meshsize)._2 * eastnum
    new Location(tgtlat,tgtlon).createMesh(meshsize)
  }

  /* メッシュから南西端の緯度経度を取得する関数
  * 誤差により端点がうまく表示されない可能性あり
  * 要改修
  * */
  def mesh2SWltln(flg:String):Double = {

      /* 処理順序を定義 */
      val meshProcOrder = meshOrder.filter(x => x.contains(meshsize)).head
        .foldLeft(List[String]())((x, y) => if (x.contains(meshsize)) x else x :+ y)

      /* 処理順序に沿って、メッシュを左から右へ*/
      mesh2SWltln_main(meshProcOrder,meshMap,mesh,flg)

  }

  /* メッシュ番号から緯度経度を出力する(loop用)
   * GetOtherMesh用に作成したもので、多少の誤差はある。
   * */
  private def mesh2SWltln_main(meshOrder:List[String],meshMap:Map[String,(Double,Double,Int)],mesh:String,flg:String):Double = {

    if (mesh.length() < 1 || meshOrder.isEmpty) 0
    else {
      val meshsize_tmp = meshOrder.head
      val calcType = meshMap(meshsize_tmp)_3
      val ans = calcType match {
      case 0 =>
        flg match {
          case "lat" => FstMesh2Lt (mesh) + mesh2SWltln_main (meshOrder.tail, meshMap, mesh.slice (4, mesh.length () ), flg)
          case "lon" => FstMesh2Ln (mesh) + mesh2SWltln_main (meshOrder.tail, meshMap, mesh.slice (4, mesh.length () ), flg)
        }
      case 1 =>
          flg match {
            case "lat" => GeneralMesh2Attr(mesh, meshMap(meshsize_tmp)._1, flg) + mesh2SWltln_main(meshOrder.tail, meshMap, mesh.slice(2, mesh.length()), flg)
            case "lon" => GeneralMesh2Attr(mesh, meshMap(meshsize_tmp)._2, flg) + mesh2SWltln_main(meshOrder.tail, meshMap, mesh.slice(2, mesh.length()), flg)
          }
      case 2 => MeshNumByLtLn (mesh, (meshMap (meshsize_tmp)._1, meshMap (meshsize_tmp)._2), flg) + mesh2SWltln_main (meshOrder.tail, meshMap, mesh.slice (1, mesh.length () ), flg)
      }
      ans
    }
  }

}

