package com.github.giamo.wikihistory.utils

import com.github.giamo.wikihistory.models.wikipedia.infoboxes.Infobox
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

  val convertToHtmlOneLineUdf: UserDefinedFunction = udf { (s: String) =>
    Option(s).map(WikiPage.getCleanHtml(_, keepAllOneLine = true))
  }

  val parsePageTitleUdf: UserDefinedFunction = udf { (text: String) =>
    PageTitle.fromLink(text).map(_.value).getOrElse(text)
  }

  val parseCapitalsUdf: UserDefinedFunction = udf { (capitalString: String) =>
    Option(capitalString)
      .map(Infobox.extractList(_).map { c =>
        PageTitle.fromLink(Capital.fromString(c).name)
          .map(_.value)
          .getOrElse(c)
      })
      .getOrElse(List.empty[String])
  }

}
