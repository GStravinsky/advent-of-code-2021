object Day6 {

  def main(args: Array[String]): Unit = {
    val initialPopulation = loadData("input_big.txt")
    val populationMap = transformRowFishPopToMap(initialPopulation)
   println(simulateFishGrowthOptimized(populationMap))
  }

  def loadData(filePath: String): List[Int] =
  {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.split(",")).toList.flatten
    bufferedSource.close

    lines.map(e => e.toInt)
  }

  def simulateFishGrowth(population: List[Int], daysPassed: Long = 0, targetDay: Long = 80): Int = {

    if (daysPassed == targetDay) {
      return population.size
    }
    val newPopulation = population.flatMap(e => FishTime(e).nextDay)

    simulateFishGrowth(newPopulation, daysPassed+1, targetDay = targetDay)

  }


  def simulateFishGrowthOptimized(population: Map[Int, Long], daysPassed: Int = 0, targetDay: Int = 256): Long = {

    if (daysPassed == targetDay) {
      println(population)
      return population.values.foldLeft(0L)(_+_)
    }
    // this is mutable
    val newPop = updateFishPopulation(population)

    simulateFishGrowthOptimized(newPop, daysPassed+1, targetDay = targetDay)

  }

  def transformRowFishPopToMap(rawPop: List[Int]): Map[Int,Long] = {
    val populationMap = rawPop.groupBy(identity).map {
      case (k, v) => (k, v.size.toLong)
    }
    populationMap
  }

  def updateFishPopulation(oldPopulation: Map[Int, Long]): Map[Int,Long] = {
    val oldPopulationActors = oldPopulation.keys.toList
    val newPopulationActors = oldPopulationActors.flatMap(e => FishTime(e).nextDay)
    val newPopulation = newPopulationActors.groupBy(identity).map {
      case (k, v) => (k, v.size)
    }


   var newPopulationWeightAdjusted: Map[Int,Long] = Map(0->0L,1->0L,2->0L,3->0L,4->0L, 5->0L,6->0L,7->0L,8->0L)
    newPopulation map {
      case (8,v) => {
        newPopulationWeightAdjusted = newPopulationWeightAdjusted + (8 -> (v + oldPopulation.getOrElse(0,1L) - 1L))
      }
      case (6,v) => {
        newPopulationWeightAdjusted = newPopulationWeightAdjusted + (6 -> (v + oldPopulation.getOrElse(7,1L) - 1L + oldPopulation.getOrElse(0,1L) - 1L))
      }
      case (k,v) => {
        newPopulationWeightAdjusted = newPopulationWeightAdjusted + (k -> (v + oldPopulation.getOrElse(k+1,1L) - 1L))

      }
    }

    newPopulationWeightAdjusted.filter(e => e._2!=0)
  }

}


trait Fish {
  def daysToReproduction: Int
  def reproduction: List[Int] = List(8,6)
}

case class FishTime(daysToReproduction: Int) extends Fish {
  def nextDay: List[Int] =
    daysToReproduction > 0 match {
      case true => List(daysToReproduction-1)
      case false => reproduction
    }
}


/*

HashMap(0 -> 3, 1 -> 3, 2 -> 2, 3 -> 2, 4 -> 2, 5 -> 1, 6 -> 5, 7 -> 1, 8 -> 4)
HashMap(0 -> 3, 5 -> 1, 1 -> 5, 6 -> 5, 2 -> 3, 7 -> 1, 3 -> 2, 8 -> 4, 4 -> 2)
*/

