import org.apache.spark.sql.{SparkSession}
import parsers.WikiParser

object WikiExtractor extends App {

  val inputPath =
    "data/split/decompressed/enwiki-20200401-pages-articles-multistream1.xml-p1p30303"
  val spark = createSparkSession()
  val parser = new WikiParser(spark)
  val rawDf = parser.readXML(inputPath)
  val pagesDs = parser.parsePages(rawDf)

  val artMovementsDs = parser.extractArtMovements(pagesDs)
  artMovementsDs.show(50, false)

  private def createSparkSession() =
    SparkSession.builder
      .master("local[*]")
      .appName("WikiExtractor")
      .getOrCreate()
}
