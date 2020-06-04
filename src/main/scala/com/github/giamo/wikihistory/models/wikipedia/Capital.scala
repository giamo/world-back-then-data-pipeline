package com.github.giamo.wikihistory.models.wikipedia

import cats.implicits._
import com.github.giamo.wikihistory.models.Date
import com.github.giamo.wikihistory.utils.WikiCleanUtils

final case class Capital(name: String, dates: Option[Date] = None)

object Capital {
  private val simpleCapitalRegex = "\\s*([^(]+?)\\s*\\(([^)]+)\\)\\s*".r
  private val linkCapitalRegex = "\\s*(\\[\\[[^]]+?\\]\\])\\s*\\(?([^)]+)\\)?\\s*".r
  private val complexCapitalRegex = "\\s*([^{]+?)\\s*\\{\\{small\\|\\(?([^}]+)\\)?\\}\\}\\s*".r

  def fromString(s: String): Capital = s match {
    case complexCapitalRegex(c, d) if Date.fromString(d).isRight =>
      Capital(WikiCleanUtils.removeReferences(c), Date.fromString(d).toOption)
    case linkCapitalRegex(c, d) if Date.fromString(d).isRight =>
      Capital(WikiCleanUtils.removeReferences(c), Date.fromString(d).toOption)
    case simpleCapitalRegex(c, d) if Date.fromString(d).isRight =>
      Capital(WikiCleanUtils.removeReferences(c), Date.fromString(d).toOption)
    case _ => Capital(WikiCleanUtils.removeReferences(s))
  }
}
