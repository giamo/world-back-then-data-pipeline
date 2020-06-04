package com.github.giamo.wikihistory.utils

import com.github.giamo.wikihistory.models.Date
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

  val prettyDatesUdf: UserDefinedFunction = udf { (yearStart: String, yearEnd: String) =>
    def prettyPrintDate(s: String) =
      Option(s).flatMap(Date.fromString(_).toOption).fold("?")(_.toPrettyString())
    s"${prettyPrintDate(yearStart)} - ${prettyPrintDate(yearEnd)}"
  }


  val parsePageTitleUdf: UserDefinedFunction = udf { (text: String) =>
    PageTitle.fromLink(text).map(_.value).getOrElse(text)
  }

  // TODO: support multiple capitals in the form of "region: city"
  val parseCapitalsUdf: UserDefinedFunction = udf { (capitalString: String) =>
    Option(capitalString)
      .map(Infobox.extractList(_).map { c =>
        PageTitle.fromLink(Capital.fromString(c).name)
          .map(_.value)
          .map(c => c.replaceFirst("\\s*\\(ancient city\\)", ""))
          .getOrElse(c)
      })
      .getOrElse(List.empty[String])
  }

  val parseDateUdf: UserDefinedFunction = udf { (s: String) =>
    Option(s).flatMap(Date.fromString(_).toOption).map(_.toString)
  }

}
