import models.wikipedia.{ArchaeologicalCulture, ArtMovement}
import org.apache.spark.sql.SparkSession
import parsers.WikiParser

object WikiExtractor extends App {
  import utils.SparkUtils._

  val inputPath = "data/split/decompressed/enwiki-20200401-pages-articles-multistream1.xml-p1p30303"
//  val inputPath = "data/split/decompressed/*"
  val spark = createSparkSession("WikiExtractor")
  val parser = new WikiParser(spark)
  val rawDf = parser.readXML(inputPath)
  val pagesDs = parser.parsePages(rawDf)

  val ds = parser.extractSettlements(pagesDs)
  ds
    .show(200, false)
//    .write.json("data/archaeological_cultures")
}
