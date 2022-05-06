package shaney

import org.apache.spark.sql.{Dataset, SparkSession}

import scala.io.Source

object Training {

  def main(args: Array[String]): Unit = {
    val nText = "/Users/stravinskaiteg/Documents/Personal_stuff/advent-of-code-2021/src/main/resources/texts/Nietsche.txt"
    val cText = "/Users/stravinskaiteg/Documents/Personal_stuff/advent-of-code-2021/src/main/resources/texts/Corpo.txt"


    val nParsed = read(nText)
    val cParsed = read(cText)

    val nTrained = train(nParsed)
    val cTrained = train(cParsed)

    val finalData = nTrained.union(cTrained)

    finalData.show()
    finalData.write.mode("OVERWRITE")
      .parquet("/Users/stravinskaiteg/Documents/Personal_stuff/advent-of-code-2021/src/main/resources/texts/training.parquet")
  }

  def getAsArray(pathN: String, pathC: String) = {
    val nParsed = read(pathN)
    val cParsed = read(pathC)

    val nTrained = train(nParsed)
    val cTrained = train(cParsed)

    nTrained.union(cTrained).collect()

    }

  def read(path: String): Seq[String] = {
    val bufferedSource = Source.fromFile(path)
    val lines =  bufferedSource.getLines()
    val array = lines.toList.flatMap(e=> e.split(" ").toSeq)
    bufferedSource.close
    array
  }
  
  def train(fullText: Seq[String]): Dataset[Triple] = {

    val spark = SparkSession.builder()
      .master("local[2]")
      .appName(getClass.getSimpleName)
      .config("spark.driver.host", "127.0.0.1")
      .config("spark.ui.enabled", "false")
      .getOrCreate()

    val splitText =  fullText.sliding(3,1).map(e => Triple(e(0), e(1), e(2))).toSeq

    import spark.sqlContext.implicits._
    spark.createDataset(splitText)
      .na.drop(how="any")
      .as[Triple]

  }
}
case class Triple(
                   wordOne: String,
                   wordTwo: String,
                   wordThree:String
                 )