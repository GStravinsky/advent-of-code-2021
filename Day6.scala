import scala.language.postfixOps
object Day6 {

  def main(args: Array[String]): Unit = {
    val initialPopulation = loadData("input_big.txt")
    println(simulateFishGrowth(initialPopulation))
  }

  def loadData(filePath: String): List[Int] =
  {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.split(",")).toList.flatten
    bufferedSource.close

    lines.map(e => e.toInt)
  }

  def simulateFishGrowth(population: List[Int], daysPassed: Int = 0, targetDay: Int = 80): Any = {

    println(daysPassed)
    if (daysPassed == targetDay) {
      return population.size
    }
    val newPopulation = population.flatMap(e => FishTime(e).nextDay)

    simulateFishGrowth(newPopulation, daysPassed+1)

  }

}
trait Fish {
  def daysToReproduction: Int
  def reproduction: List[Int] = List(8,6)

  def nextDay: List[Int] =
    daysToReproduction > 0 match {
      case true => List(daysToReproduction-1)
      case false => reproduction
    }
}
case class FishTime(daysToReproduction: Int) extends Fish

