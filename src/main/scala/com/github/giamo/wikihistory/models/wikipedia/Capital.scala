package com.github.giamo.wikihistory.models.wikipedia

import cats.implicits._
import com.github.giamo.wikihistory.models.Date

final case class Capital(name: String, dates: Option[String] = None)

object Capital {
  private val complexCapitalRegex = "([^{]+)\\s*\\{\\{small\\|([^}]+)\\}\\}".r

  def fromString(s: String): Capital = s.trim match {
    case complexCapitalRegex(c, d) => Capital(c, d.some)
    case _ => Capital(s)
  }
}
