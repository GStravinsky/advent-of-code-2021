package shaney

import org.apache.spark.sql.functions.col
import org.apache.spark.sql.Dataset
import shaney.Training.getAsArray
import shaney.Triple
import sun.font.TextLabel

import scala.annotation.tailrec
import scala.util.Random


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
        val newWord = chooseNextWordBasedOnOneWord(initialWord, dataset)
        val newText = text :+ newWord
        createMagicText(newWord, dataset, textLength - 1)(newText)
      }
    }
  }

  def chooseNextWordBasedOnOneWord(wordOne: String, dataset: Array[Triple]): String = {
    val possibleWords = getAllPossibleNextWordsWithProbabilities(wordOne, dataset)

    val p = scala.util.Random.nextDouble

    //val cdf = possibleWords.values.head +: possibleWords.values.sliding(2).map(_.sum).toList

   //val t = possibleWords.sliding(2).map(e => e.values.sum -> e.keys.tail.head).toList

    val it = possibleWords.iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        return item  // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen")  // needed so it will compile
  }
//    val highestProbability= possibleWords.values.max
//
//    val highestProbabilityWords = po
    //   ssibleWords.filter(e => e._2 == highestProbability).keys
//
//    highestProbabilityWords.size match {
//      case 1 => highestProbabilityWords.head
//      case _ => chooseFromEqualProbability(highestProbabilityWords.toSeq, Random.nextInt(highestProbabilityWords.size))
//    }



  def getAllPossibleNextWordsWithProbabilities(wordOne: String, dataset: Array[Triple]) : Map[String, Double] = {
    // We store duplicate pairs, so getting probabilities will be counting on the number of duplicates if any
    val relevantData = dataset.filter(e => e.wordOne == wordOne)

    val denominator = relevantData.size

    relevantData
      .map(e => e.wordTwo)
      .groupBy(identity)
      .map(e => e._1 -> e._2.size.toDouble/denominator)
  }

}
