import java.io.File
import scala.collection.JavaConversions._
import org.apache.commons.io._

class ParFile(datafile:String) {
  def getMap(mesh:String):(Double,Double) = {

    val ite = FileUtils.lineIterator( new File(datafile) )
    val tgtrow = ite.map(x => x.toString().split(" +").toList).find(x => x.head == mesh)
    val ans = (tgtrow.get(1).toDouble,tgtrow.get(2).toDouble)
    ite.close()

    ans

  }
}
