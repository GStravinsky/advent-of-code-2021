import jdk.internal.util.xml.impl.Input

object Day3 {

  def main(args: Array[String]): Unit = {
    val power_consumption = get_power_consumption("input_big.txt")

    val oxygen = get_oxygen_generator_rating("input_big.txt")
    println(oxygen)

    val co2 = get_co2_rating("input_big.txt")
    println(co2)

    println(oxygen*co2)

  }

  def loadData(filePath: String): List[List[String]] = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.toList.map(e => e.toString)).toList
    bufferedSource.close

    lines
  }

  def get_power_consumption(filePath: String): Int = {

    val input = loadData(filePath).transpose

    val gamma_binary = input.map{
      x => x.groupBy(identity).maxBy(_._2.size)._1.toString
    }
    val epsilon_binary = gamma_binary.map{
        case "1" => "0"
        case "0" => "1"
    }
    Integer.parseInt(gamma_binary.mkString(""), 2) * Integer.parseInt(epsilon_binary.mkString(""), 2)
  }

/// other metrics
  // need to get the first numbers of each list, got the most common, discard those with no most common number

  def get_oxygen_generator_rating(filePath: String): Int = {
    var input = loadData(filePath)
    var input_trans = input.transpose

    println(input.filter(x => x(i) == "1"))
    // try to get an implicit iterator with the collect stuff
    for (i <- 0 to (input(0).length-1)) {
      val frequence_map = input_trans(i).groupBy(identity)


      var most_common_number = "1"
      if (frequence_map.contains("0") & frequence_map.contains("1")) {
        val zeros = frequence_map("0").size
        val ones = frequence_map("1").size

        if (zeros > ones) {
          most_common_number = "0"
        }
      } else {
        most_common_number = frequence_map.maxBy(_._2.size)._1.toString()
      }

      input = input.filter(x => x(i) == most_common_number)
      input_trans = input.transpose
    }
    Integer.parseInt(input(0).mkString(""), 2)
  }

  def get_co2_rating(filePath: String): Int = {
    var input = loadData(filePath)
    var input_trans = input.transpose


    // try to get an implicit iterator with the collect stuff
    for (i <- 0 to (input(0).length-1)) {
      val frequence_map = input_trans(i).groupBy(identity)

      var least_common_number = "0"
      if (frequence_map.contains("0") & frequence_map.contains("1")) {
        val zeros = frequence_map("0").size
        val ones = frequence_map("1").size

        if (zeros > ones) {
          least_common_number = "1"
        }
      } else {
        least_common_number = frequence_map.minBy(_._2.size)._1.toString()
      }


      input = input.filter(x => x(i) == least_common_number)
      input_trans = input.transpose
    }

    Integer.parseInt(input(0).mkString(""), 2)
  }



}
