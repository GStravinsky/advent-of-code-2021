package onehotencode

import org.apache.spark.sql.SparkSession
import org.apache.spark.ml.feature.{OneHotEncoder, StringIndexer}

object OneHotEncode {

  def main(args: Array[String]): Unit = {
//    val sampleData = Seq(
//      ("India",1000),
//      ("Us", 2000),
//      ("China", 30000),
//      ("Japan", 400),
//      ("Korea", 500),
//      ("canada", 600)
//
//    )

    val sampleData = Seq(
      ("India",1000),
      ("Us", 2000),
      ("China", 30000),
      ("India", 400),
      ("Korea", 500),
      ("canada", 600)

    );

    val spark = SparkSession
      .builder()
      .appName("test")
      .config("spark.master", "local")
      .getOrCreate();

    val sampleDataDf = spark.createDataFrame(sampleData).toDF("country", "visitors");
    val sampleIndexedDf = new StringIndexer().setInputCol("country").setOutputCol("country_index").fit(sampleDataDf).transform(sampleDataDf);
    val oneHotEncoder = new OneHotEncoder().setInputCol("country_index").setOutputCol("country_vec");
    val encoded = oneHotEncoder.transform(sampleIndexedDf);
    encoded.show();
  }
}