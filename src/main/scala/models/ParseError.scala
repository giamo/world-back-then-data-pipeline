package models

trait ParseError {
  val message: String
}

final case class DateParseError(override val message: String) extends ParseError

final case class DateRangeParseError(override val message: String) extends ParseError
