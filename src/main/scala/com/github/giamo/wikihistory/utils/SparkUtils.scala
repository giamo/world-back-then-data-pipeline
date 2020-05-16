package com.github.giamo.wikihistory.utils

import com.github.giamo.wikihistory.models.wikipedia.{Capital, PageTitle, WikiPage}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.expressions.UserDefinedFunction
import org.apache.spark.sql.functions.udf

object SparkUtils {

  def createSparkSession(appName: String): SparkSession =
    SparkSession.builder
      .master("local[*]")
      .appName(appName)
      .getOrCreate()

  val convertToHtmlUdf: UserDefinedFunction = udf { (s: String) =>
    Option(s).map(WikiPage.convertToHtml)
  }

  val parsePageTitleUdf: UserDefinedFunction = udf { (text: String) =>
    PageTitle.fromLink(text).map(_.value).getOrElse(text)
  }

  val parseCapitalsUdf: UserDefinedFunction = udf { (capitals: Seq[String]) =>
    capitals.map(Capital.fromString).map(c => (c.name, c.dates.map(_.toYear)))
  }

}
