package com.github.giamo.wikihistory.utils

import com.github.giamo.wikihistory.models.wikipedia.PageTitle
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.expressions.UserDefinedFunction
import org.apache.spark.sql.functions.udf

object SparkUtils {

  def createSparkSession(appName: String): SparkSession =
    SparkSession.builder
      .master("local[*]")
      .appName(appName)
      .getOrCreate()

  val parsePageTitleUdf: UserDefinedFunction = udf { (text: String) =>
    PageTitle.fromLink(text).map(_.value).getOrElse(text)
  }

}
