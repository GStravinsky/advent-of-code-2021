
object Day8 {

  def main(args: Array[String]): Unit = {
    val data = loadData("input_smol.txt")
    println(calculateSumOfOutput(data))


  }

  def loadData(filePath: String): List[DigitDisplay] = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = (for (line <- bufferedSource.getLines()) yield line.split(" \\| ").toList).toList
    bufferedSource.close

    lines.map(e => DisplayDigitParser(e))
  }

  def countSimpleNumbers(in: List[DigitDisplay]): Int = {
    val count = in.flatMap(e => e.output).map(e => e.size).count(x => {x == 7 | x == 2 | x == 3 | x == 4})
    count
  }

  def constructValueMap(digits: DigitDisplay): Map[String, Int] = {

    val easyMap = digits.identifyOneFourSeven

    val twoThreeFiveMap = digits.joined.filter(e => e.size == 5)
      .map(e => e match {
        case x if easyMap(1).toSet.subsetOf(e.toSet) => e -> 3
        case y if easyMap(4).toSet.intersect(e.toSet).size == 3 => e -> 5
        case _ => e -> 2
    }).toMap


    val zeroNineSixMap = digits.joined.filter(e => e.size == 6)
      .map(e => e match {
        case x if easyMap(4).toSet.subsetOf(e.toSet) => e -> 9
        case y if easyMap(1).toSet.intersect(e.toSet).size == 1 => e -> 6
        case _ => e -> 0
      }).toMap


    twoThreeFiveMap ++ zeroNineSixMap ++ easyMap.map(_.swap)
  }

  def calculateSumOfOutput(allDigits: List[DigitDisplay], currentSlice: Int = 0, sum: Int = 0): Int = {

    val digits = allDigits(currentSlice)
    if (currentSlice == allDigits.size-1) {
      return sum
    }

    val valueMap = constructValueMap(digits)

    val realOutput = digits.output.map(e => valueMap(e)).mkString("").toInt

    calculateSumOfOutput(allDigits = allDigits, currentSlice = currentSlice+1, sum = sum + 1)


  }
}

trait DigitDisplay {
  def signalInput: List[String]
  def output: List[String]
  def joined: List[String] = signalInput ++ output

  private def easyNumberIdentifier(numberLength: Int): Int = numberLength match {
    case 2 => 1
    case 4 => 4
    case 3 => 7
    case 7 => 8
  }

  def identifyOneFourSeven = joined.filter(
    e => {e.size == 2 | e.size == 4 | e.size == 7 | e.size == 3} )
    .map(e => easyNumberIdentifier(e.size) -> e).toMap

}

case class DisplayDigitParser(input: List[String]) extends DigitDisplay {
  def signalInput = input.head.trim.split(" ").toList
  def output = input.last.trim.split(" ").toList



}