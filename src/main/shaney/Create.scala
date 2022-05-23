package shaney

import shaney.Training.getAsArray
import scala.annotation.tailrec

case class Distribution(value: String, pdf: Double)

object Create {

  def main(args: Array[String]): Unit = {

    val nText = "/Users/stravinskaiteg/Documents/Personal_stuff/advent-of-code-2021/src/main/resources/texts/Nietsche.txt"
    val cText = "/Users/stravinskaiteg/Documents/Personal_stuff/advent-of-code-2021/src/main/resources/texts/Corpo.txt"

    val data = getAsArray(nText, cText)
    val text = createMagicText("RELX", dataset = data, textLength = 100)

    println(text)
  }

  @tailrec
  def createMagicText(initialWord: String, dataset: Array[Triple], textLength: Int)(implicit text: Seq[String] = Seq(initialWord)): String = {

    textLength match {
      case 0 => text.mkString(" ")
      case _ => {
        val newWord = chooseNextWord(initialWord, dataset)
        val newText = text :+ newWord
        createMagicText(newWord, dataset, textLength - 1)(newText)
      }
    }
  }

  def chooseNextWord(wordOne: String, dataset: Array[Triple]): String = {
    val possibleWords = getAllPossibleNextWordsWithProbabilities(wordOne, dataset)

    val p = scala.util.Random.nextDouble

    val cdf = getCDF(possibleWords)

    cdf.filter(e => e.pdf >= p).head.value

    }

  def getCDF(words: Seq[Distribution]) ={

    val cdf = words.map(e => e.pdf).scanLeft(0.0)(_+_).tail

    (words zip cdf)
      .map{case (a,b) => Distribution(a.value, b)}
  }

  def getAllPossibleNextWordsWithProbabilities(wordOne: String, dataset: Array[Triple]) : Seq[Distribution] = {
    // We store duplicate pairs, so getting probabilities will be counting on the number of duplicates if any
    val relevantData = dataset.filter(e => e.wordOne == wordOne)

    val denominator = relevantData.size

    relevantData
      .map(e => e.wordTwo)
      .groupBy(identity)
      .map(e =>Distribution(e._1, e._2.size.toDouble/denominator))
      .toList
  }

}
