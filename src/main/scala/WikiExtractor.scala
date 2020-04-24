import models.wikipedia.{ArchaeologicalCulture, ArtMovement, Coordinates}
import org.apache.spark.sql.SparkSession
import parsers.WikiParser

object WikiExtractor extends App {

  import utils.SparkUtils._

  //  val inputPath = "data/split/decompressed/enwiki-20200401-pages-articles-multistream1.xml-p1p30303"
  val inputPath = "data/split/decompressed/*"
  val spark = createSparkSession("WikiExtractor")

  import spark.implicits._

  val parser = new WikiParser(spark)
  val rawDf = parser.readXML(inputPath)
  val pagesDs = parser.parsePages(rawDf)

  val ds = parser.extractSettlements(pagesDs)
  ds
    .map(s => (s.name, s.coordinates, s.parsedLatitude, s.parsedLongitude))
    .select($"_1" as "name", $"_2" as "coordinates", $"_3" as "latitude", $"_4" as "longitude")
    .write.json("data/settlements")
}
