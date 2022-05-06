name := "scala-hackery"
scalaVersion := "2.11.12"

val sparkVersion = "2.4.4"
libraryDependencies ++= Seq(

  "org.scalatest" %% "scalatest" % "3.1.0" % Test,
  // Spark
  "org.apache.spark" %% "spark-core" % sparkVersion,
  "org.apache.spark" %% "spark-sql" % sparkVersion,
  "org.apache.spark" %% "spark-mllib" % sparkVersion

)

