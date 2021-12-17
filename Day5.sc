object Day5 {

  def main(args: Array[String]): Unit = {
    val data = loadData("input_smol.txt")

    println(ParsedCoordinates(data(2)).get)

  }

  def loadData(filePath: String): List[List[String]] =
  {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.split(" -> ").toList).toList
    bufferedSource.close

    lines
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
      Range(order.head,order.last).inclusive.map(e => (e,x2)).toList
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
