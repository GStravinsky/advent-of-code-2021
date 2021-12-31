
object Day8 {

  def main(args: Array[String]): Unit = {
    val data = loadData("input_big.txt")
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

  def constructValueMap(digits: DigitDisplay): Map[Set[Char], Int] = {

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


    val joinedMap = twoThreeFiveMap ++ zeroNineSixMap ++ easyMap.map(_.swap)

    joinedMap.map{case (k,v) => (k.toSet, v)}
  }

  def calculateSumOfOutput(allDigits: List[DigitDisplay], currentSlice: Int = 0, sum: Int = 0): Int = {

    if (currentSlice == allDigits.size) {
      return sum
    }

    val digits = allDigits(currentSlice)

    val valueMap = constructValueMap(digits)

    val realOutput = digits.output.map(e => valueMap(e.toSet)).mkString("").toInt

    calculateSumOfOutput(allDigits = allDigits, currentSlice = currentSlice+1, sum = sum + realOutput)


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


  // perhaps make it store other premutations of the same number?
  def identifyOneFourSeven = joined.filter(
    e => {e.size == 2 | e.size == 4 | e.size == 7 | e.size == 3} )
    .map(e => easyNumberIdentifier(e.size) -> e).toMap

}

case class DisplayDigitParser(input: List[String]) extends DigitDisplay {
  def signalInput = input.head.trim.split(" ").toList
  def output = input.last.trim.split(" ").toList



}