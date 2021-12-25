
object Day7 {

  def main(args: Array[String]): Unit = {
    val data = loadData("input_big.txt")
    val bestPosition = getMedian(data)
    println(getFuelEmissions(data, bestPosition))
  }

  def loadData(filePath: String): List[Int] = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.split(",")).toList.flatten
    bufferedSource.close

    lines.map(e => e.toInt)
  }

  def getMedian(positions: List[Int]): Int = {

    val SortedPositions = positions.sorted

    val medianPosition = SortedPositions.length/2
    val median = SortedPositions(medianPosition)

    median
  }

  def getFuelEmissions(positions: List[Int], bestMeetingPoint: Int): Int = {
    val fuelEmissions = positions.map(e => (e-bestMeetingPoint).abs).foldLeft(0)(_+_)
    fuelEmissions
  }
}


