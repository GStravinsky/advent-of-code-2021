
object Day9 {

  def main(args: Array[String]): Unit = {
    val data = LavaCave(loadData("input_big.txt"))
    //println(data.numColumns)


    println(findAllLowPoints(data))
  }

  def loadData(filePath: String): List[List[Int]] = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.map(e => e.asDigit).toList).toList
    bufferedSource.close

    lines
  }

  def findAllLowPoints(cave: Cave, currentIndex: Int = 0, lowPoints: List[Int] = List()): Int = {

    if (cave.allCoordinates.size-1 == currentIndex) {
       return lowPoints.map(e => e+1).foldLeft(0)(_+_)
    }

    val currentCoordinates = cave.allCoordinates(currentIndex)
    val currentPoint = CavePoint(cave, currentCoordinates._1, currentCoordinates._2)

    val addedLowPoints = currentPoint.isLowPoint match {
      case true => List(currentPoint.pointValue)
      case false => List()
    }

    val mostRecentLowPointList = lowPoints ++ addedLowPoints
    findAllLowPoints(cave, currentIndex+1, mostRecentLowPointList)


  }


}
trait Cave {
  def apply: List[List[Int]]
  def numRows: Int
  def numColumns: Int
  def allCoordinates = (0 to numRows-1)
    .flatMap(e => List(e).zipAll((0 to numColumns-1).toList, e, e))
  def isValidCoordinate(row: Int, column: Int): Boolean = row match {
    case c if 0 <= row & row < numRows => column match {
      case b if 0 <= column & column < numColumns => true
      case _ => false
    }
    case _ => false
  }
}
case class LavaCave(caveData: List[List[Int]]) extends Cave {
  val numRows = caveData.length
  val numColumns = caveData(0).length
  def apply = caveData
}

trait Point {
  def row: Int
  def column: Int
  def pointValue: Int
  def getNeighbours: Map[(Int,Int), Int]

  def isLowPoint: Boolean = {
    val neighbours = getNeighbours.values
    val lowerNeighbours = neighbours.filter(e => (e < pointValue | e == pointValue))

    lowerNeighbours.size match {
      case 0 => true
      case _ => false
    }

  }
}

case class CavePoint(caveData: Cave, row: Int, column: Int) extends Point {
  def pointValue = caveData.apply(row)(column)

  def getNeighbours: Map[(Int,Int), Int] = {
    val potentialNeighbours = List((row, column-1), (row, column+1), (row+1, column), (row-1, column))
    val actualNeighbours = potentialNeighbours.filter(e => caveData.isValidCoordinate(e._1, e._2)).map(
      e =>  e -> caveData.apply(e._1)(e._2)
    ).toMap
    actualNeighbours
  }
}