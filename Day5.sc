object Day5 {

  def main(args: Array[String]): Unit = {
    println(numStraigthOverlappingVents("input_big.txt"))

  }

  def loadData(filePath: String): List[List[String]] =
  {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.split(" -> ").toList).toList
    bufferedSource.close

    lines
  }

  def numStraigthOverlappingVents(filePath: String): Int = {
    val data = loadData(filePath)

    val out = data.filter(e => ParsedCoordinates(e).notDiagonal).map(e => ParsedCoordinates(e).get).flatten
      .groupBy(identity).filter((t) => t._2.size>1).size

    out
  }

}
// so you want to load each line to this trait
trait Coordinates {
  def x1: Int
  def x2: Int
  def y1: Int
  def y2: Int

  def notDiagonal: Boolean = x1 == x2 | y1 == y2
  def get: List[(Int,Int)] = (x1,x2,y1,y2) match {
    case x if x1 == x2 =>{
      val order = Seq(y1,y2).sorted
      Range(order.head,order.last).inclusive.map(e => (x2,e)).toList
    }
    case y if y1 == y2 => {
      val order = List(x1,x2).sorted
      Range(order.head,order.last).inclusive.map(e => (e,y2)).toList
    }}
  }

case class ParsedCoordinates(inputLine: List[String]) extends Coordinates {
  val x1 = inputLine.head.split(",").head.toInt
  val x2 = inputLine.last.split(",").head.toInt
  val y1 = inputLine.head.split(",").last.toInt
  val y2 = inputLine.last.split(",").last.toInt
}
