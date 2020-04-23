package utils

import org.apache.spark.sql.SparkSession

object SparkUtils {

  def createSparkSession(appName: String) =
    SparkSession.builder
      .master("local[*]")
      .appName(appName)
      .getOrCreate()

}
