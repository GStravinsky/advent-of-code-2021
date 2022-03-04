
object Day7 {

  def main(args: Array[String]): Unit = {
    val data = loadData("input_big.txt")
    val bestPositions = getMean(data)//getMedian(data)
    println(getFuelEmissionsLinear(data, bestPositions))
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

  def getMean(position: List[Int]): BestMeetingPoints = {
    val mean = position.foldLeft(0)(_+_).toFloat/position.length
    BestMeetingPointsOptions(mean = mean)
  }

  def getFuelEmissions(positions: List[Int], bestMeetingPoint: Int): Int = {
    val fuelEmissions = positions.map(e => (e-bestMeetingPoint).abs).foldLeft(0)(_+_)
    fuelEmissions
  }

  def getFuelEmissionsLinear(positions: List[Int], bestMeetingPointOptions: BestMeetingPoints): Int = {
    val fuelEmissionsFloor = positions.map(e => (e-bestMeetingPointOptions.floor).abs).map(e => (e.toFloat/2*(2+e-1)).toInt).foldLeft(0)(_+_)
    val fuelEmissionsCeiling = positions.map(e => (e-bestMeetingPointOptions.ceiling).abs).map(e => (e.toFloat/2*(2+e-1)).toInt).foldLeft(0)(_+_)

    fuelEmissionsFloor < fuelEmissionsCeiling match {
      case true => return fuelEmissionsFloor
      case false => return fuelEmissionsCeiling
    }
  }
}


trait BestMeetingPoints {
  def mean: Double
  def floor: Int = mean.floor.toInt
  def ceiling: Int = mean.ceil.toInt
}

case class BestMeetingPointsOptions(mean: Double) extends BestMeetingPoints