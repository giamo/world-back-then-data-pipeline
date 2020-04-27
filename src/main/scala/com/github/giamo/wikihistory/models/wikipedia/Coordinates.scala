package com.github.giamo.wikihistory.models.wikipedia

import scala.util.Try

final case class Coordinates(latitude: Double, longitude: Double)

object Coordinates {
  private val degreesRegex = ".*\\{\\{(?:coord|coords)\\s*\\|\\s*([0-9]+)(?:\\s*\\|\\s*([0-9]+))?(?:\\s*\\|\\s*([0-9]+))?\\s*\\|\\s*(n|s)\\s*\\|\\s*([0-9]+)(?:\\s*\\|\\s*([0-9]+))?(?:\\s*\\|\\s*([0-9]+))?\\s*\\|\\s*(e|w).*\\}\\}.*".r
  private val decimalRegex = ".*\\{\\{(?:coord|coords)\\s*\\|\\s*([\\-]?[0-9\\.]+)(?:\\s*\\|\\s*([n|s]))?\\s*\\|\\s*([\\-]?[0-9\\.]+)(?:\\s*\\|\\s*([e|w]))?\\s*\\|\\s*.*\\}\\}.*".r

  def fromTemplate(text: String): Option[Coordinates] = text.toLowerCase match {
    case degreesRegex(degreeLat, minuteLat, secondLat, directionLat, degreeLong, minuteLong, secondLong, directionLong) =>
      for {
        lat <- convert(degreeLat, Option(minuteLat), Option(secondLat), directionLat)
        long <- convert(degreeLong, Option(minuteLong), Option(secondLong), directionLong)
      } yield Coordinates(lat, long)
    case decimalRegex(latitude, directionLat, longitude, directionLong)                                                 =>
      for {
        latVal <- toDouble(latitude).map(truncate(_))
        longVal <- toDouble(longitude).map(truncate(_))
        lat = evaluateDirection(latVal, directionLat)
        long = evaluateDirection(longVal, directionLong)
      } yield Coordinates(lat, long)
    case _                                                                                                              => None
  }

  private def convert(degrees: String, minutes: Option[String], seconds: Option[String], direction: String): Option[Double] = {
    for {
      d <- toDouble(degrees)
      m <- toDouble(minutes)
      s <- toDouble(seconds)
      dd = truncate(d + m / 60 + s / (60 * 60))
    } yield evaluateDirection(dd, direction)
  }

  private def evaluateDirection(value: Double, direction: String): Double =
    if ("s" == direction || "w" == direction) -1 * value
    else value

  private def toDouble(s: String): Option[Double] = Try(s.toDouble).toOption

  private def toDouble(s: Option[String]): Option[Double] =
    s.fold(Option(0d))(toDouble)

  private def truncate(d: Double, decimals: Int = 3) =
    BigDecimal(d).setScale(decimals, BigDecimal.RoundingMode.HALF_UP).toDouble
}
