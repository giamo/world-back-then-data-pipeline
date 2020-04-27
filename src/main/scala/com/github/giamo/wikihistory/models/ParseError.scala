package com.github.giamo.wikihistory.models

trait ParseError {
  val message: String
}

final case class DateParseError(override val message: String) extends ParseError
