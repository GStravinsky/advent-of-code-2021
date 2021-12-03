import jdk.internal.util.xml.impl.Input

object Day3 {
  def main(args: Array[String]): Unit = {
    val input = loadData("input_big.txt")
    val gamma = gamma_binnary(input)
    val power_consumption = get_power_consumption(gamma)
    println(power_consumption)
  }


  def loadData(filePath: String): List[List[Char]] = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.toList).toList
    bufferedSource.close

    lines.transpose
  }

  def gamma_binnary(input: List[List[Char]]): List[String] = {
    // identity == x => x -- i.e. group by every character
    // maxby return only a tuple with the max value and its instance
    val gamma = input.map{
      x => x.groupBy(identity).maxBy(_._2.size)._1.toString
    }
    gamma
  }

  def get_power_consumption(binary_num: List[String]): Int = {
    val epsilon_binary = binary_num.map{
        case "1" => "0"
        case "0" => "1"
    }
    Integer.parseInt(binary_num.mkString(""), 2) * Integer.parseInt(epsilon_binary.mkString(""), 2)
  }



}
