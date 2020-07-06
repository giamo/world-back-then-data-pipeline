package com.github.giamo.wikihistory.parsers

import java.io.ByteArrayInputStream

import com.databricks.spark.xml.XmlInputFormat
import info.bliki.wiki.dump.{IArticleFilter, Siteinfo, WikiArticle, WikiXMLParser}
import com.github.giamo.wikihistory.models.wikipedia.infoboxes.{ArchaeologicalCulture, ArtMovement, Country, MilitaryConflict, Philosopher, Site}
import com.github.giamo.wikihistory.models.wikipedia.{ParsedPage, WikiPage}
import org.apache.hadoop.io.{LongWritable, Text}
import org.apache.spark.sql.functions.udf
import org.apache.spark.sql.{DataFrame, Dataset, SparkSession}

final class WikiParser(spark: SparkSession) extends Serializable {

  import spark.implicits._

  def parsePages(rawDf: DataFrame): Dataset[WikiPage] = {
    rawDf
      .mapPartitions { rows =>
        case class WrappedPage(var page: WikiArticle = new WikiArticle)

        class SetterArticleFilter(val wrappedPage: WrappedPage)
          extends IArticleFilter {
          def process(page: WikiArticle, siteinfo: Siteinfo) {
            wrappedPage.page = page
          }
        }

        rows.flatMap { row =>
          val rawXML = row.getAs[String](0)
          val wrappedPage: WrappedPage = new WrappedPage
          //The parser occasionally exceptions out, we ignore these
          try {
            val parser = new WikiXMLParser(
              new ByteArrayInputStream(rawXML.getBytes),
              new SetterArticleFilter(wrappedPage)
            )
            parser.parse()
          } catch {
            case e: Exception =>
          }

          val wikiArticle: WikiArticle = wrappedPage.page
          if (wikiArticle.getText != null && wikiArticle.getTitle != null
            && wikiArticle.getId != null && wikiArticle.getRevisionId != null
            && wikiArticle.getTimeStamp != null) {
            Some(
              WikiPage(
                wikiArticle.getId.toLong,
                wikiArticle.getTitle,
                wikiArticle.getText,
                wikiArticle.isCategory,
                wikiArticle.isFile,
                wikiArticle.isTemplate
              )
            )
          } else {
            None
          }
        }
      }
    //      .toDF("title", "text", "isCategory", "isFile", "isTemplate")
  }

  def readXML(inputPath: String): DataFrame = {
    spark.sparkContext.hadoopConfiguration
      .set(XmlInputFormat.START_TAG_KEY, "<page>")
    spark.sparkContext.hadoopConfiguration
      .set(XmlInputFormat.END_TAG_KEY, "</page>")

    val records = spark.sparkContext.newAPIHadoopFile(
      inputPath,
      classOf[XmlInputFormat],
      classOf[LongWritable],
      classOf[Text]
    )

    records
      .map { case (k, v) => new String(v.copyBytes()) }
      .toDF("raw_xml")
  }

  def extractCategories(pagesDf: DataFrame): DataFrame = {
    val pattern = ".*\\[\\[Category:([a-zA-Z0-9\\s]+)(?:\\| )?\\]\\].*".r
    val extractCategories = udf { (text: String) =>
      pattern.findAllIn(text).matchData.toList.map(_.subgroups(0))
    }

    val withCategoriesDf =
      pagesDf.withColumn("categories", extractCategories($"text"))
    //    withCategoriesDf
    //      .filter(array_contains($"categories", "History") || array_contains($"categories", "Wikipedia level-5 vital articles"))
    //      .filter($"title" === "RomanRepublic" || $"title" === "Roman Republic")
    withCategoriesDf
  }

  def extractCountries(pagesDf: Dataset[WikiPage]): Dataset[Country] =
    pagesDf.flatMap(Country.fromInfobox)

  def extractArtMovements(pagesDf: Dataset[WikiPage]): Dataset[ArtMovement] =
    pagesDf.flatMap(ArtMovement.fromInfobox)

  def extractArchaeologicalCultures(pagesDf: Dataset[WikiPage]): Dataset[ArchaeologicalCulture] =
    pagesDf.flatMap(ArchaeologicalCulture.fromInfobox)

  def extractSites(pagesDf: Dataset[WikiPage]): Dataset[Site] =
    pagesDf.flatMap(Site.fromInfobox)

  def extractPhilosophers(pagesDf: Dataset[WikiPage]): Dataset[Philosopher] =
    pagesDf.flatMap(Philosopher.fromInfobox)

  def extractMilitaryConflicts(pagesDf: Dataset[WikiPage]): Dataset[MilitaryConflict] =
    pagesDf.flatMap(MilitaryConflict.fromInfobox)

  def extractGenericPage(pagesDf: Dataset[WikiPage]): Dataset[ParsedPage] =
    pagesDf.map(ParsedPage.fromRawPage)
}
