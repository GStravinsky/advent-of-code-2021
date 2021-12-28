
object Day8 {

  def main(args: Array[String]): Unit = {
    val data = loadData("input_big.txt")
    println(countSimpleNumbers(data))

  }

  def loadData(filePath: String): DigitDisplay = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.split(" \\| ").toList).toList
    bufferedSource.close

    DisplayDigitParser(lines)
  }

  def countSimpleNumbers(output: DigitDisplay): Any = {
    val count = output.output.flatten.map(e => e.size).count(x => {x == 7 | x == 2 | x == 3 | x == 4})
    count
  }

}

trait DigitDisplay {
  def signalInput: List[List[String]]
  def output: List[List[String]]
}

case class DisplayDigitParser(input: List[List[String]]) extends DigitDisplay {
  def signalInput = input.map(e=>e.head.trim.split(" ").toList).toList
  def output = input.map(e=>e.last.trim.split(" ").toList).toList

}