import jdk.internal.util.xml.impl.Input

object Day3 {

  def main(args: Array[String]): Unit = {
    println(get_power_consumption("input_big.txt"))
    println(get_life_support_rating("input_big.txt"))


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

  /// PART 2 Recursive ///

  def most_common_or_one(input: List[List[String]], column: Int): String = {
    val input_trans = input.transpose
    val frequence_map = input_trans(column).groupBy(identity).map{ case (k,v) => (k,v.size) }

    val zeros = frequence_map.getOrElse("0",0)
    val ones = frequence_map.getOrElse("1",0)

    if (zeros > ones) return "0" else return "1"
  }

  def least_common_or_zero(input: List[List[String]], column: Int): String = {
    val max_num_digits = input.length
    val input_trans = input.transpose
    val frequence_map = input_trans(column).groupBy(identity).map{ case (k,v) => (k,v.size) }

    val zeros = frequence_map.getOrElse("0", max_num_digits)
    val ones = frequence_map.getOrElse("1", max_num_digits)

    if (ones < zeros) return "1" else return "0"
  }

  def filter_by_most_common_digit(input: List[List[String]], most_common: String, column: Int): List[List[String]] = {
  val filt_vals = input.filter(x => x(column) == most_common)
  filt_vals
}

  def get_oxygen_rating_recursion(input: List[List[String]], column: Int = 0): Int = {
    if (input.length == 1) {
      return Integer.parseInt(input(0).mkString(""), 2)
    } else {
      val most_common = most_common_or_one(input, column)
      val filt_list = filter_by_most_common_digit(input, most_common, column)
      get_oxygen_rating_recursion(filt_list, column+1)
    }
  }

  def get_c02_rating_recursion(input: List[List[String]], column: Int = 0): Int = {
    if (input.length == 1) {
      return Integer.parseInt(input(0).mkString(""), 2)
    } else {
      val most_common = least_common_or_zero(input, column)
      val filt_list = filter_by_most_common_digit(input, most_common, column)
      get_c02_rating_recursion(filt_list, column+1)
    }
  }

  def get_life_support_rating(inputPath: String): Int = {
    val input = loadData(inputPath)

    val oxygen_rating = get_oxygen_rating_recursion(input)
    val co2_rating = get_c02_rating_recursion(input)

    oxygen_rating * co2_rating

  }

  /// THE BELLOW IS A VERY SHITTY SOLUTION AND YOU BETTER AVOID LOOKING AT IT!!!!

  def get_oxygen_generator_rating(filePath: String): Int = {
    var input = loadData(filePath)
    var input_trans = input.transpose

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
