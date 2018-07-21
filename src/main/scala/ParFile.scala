import java.io.File
import scala.collection.JavaConversions._
import org.apache.commons.io._

class ParFile(datafile:String) {
  def getMap(mesh: MeshCode):Location = {

    val ite = FileUtils.lineIterator( new File(datafile) )

    /* 対象メッシュだけ拾う */
    val tgtrow = ite.map(x => x.toString.split(" +").toList).find(x => x.head == mesh.toString)
    val ans = Location(tgtrow.get(1).toDouble,tgtrow.get(2).toDouble)

    ite.close()

    ans

  }
}
