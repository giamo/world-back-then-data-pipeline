package com.github.giamo.wikihistory.models.wikipedia

import cats.implicits._
import com.github.giamo.wikihistory.models.Date
import com.github.giamo.wikihistory.utils.WikiCleanUtils

final case class Capital(name: String, fromYear: Option[Int] = None, toYear: Option[Int] = None)

object Capital {
  private val simpleCapitalRegex = "\\s*([^(]+?)\\s*\\(([^)]+)\\)\\s*".r
  private val linkCapitalRegex = "\\s*(\\[\\[[^]]+?\\]\\])\\s*\\(?([^)]+)\\)?\\s*".r
  private val complexCapitalRegex = "\\s*([^{]+?)\\s*\\{\\{small\\|\\(?([^}]+)\\)?\\}\\}\\s*".r

  def fromString(s: String): Capital = s match {
    case complexCapitalRegex(c, d) if Date.fromString(d).isRight =>
      val dates = Date.fromString(d).toOption
      Capital(WikiCleanUtils.removeReferences(c), dates.map(_.fromYear), dates.map(_.toYear))
    case linkCapitalRegex(c, d) if Date.fromString(d).isRight =>
      val dates = Date.fromString(d).toOption
      Capital(WikiCleanUtils.removeReferences(c), dates.map(_.fromYear), dates.map(_.toYear))
    case simpleCapitalRegex(c, d) if Date.fromString(d).isRight =>
      val dates = Date.fromString(d).toOption
      Capital(WikiCleanUtils.removeReferences(c), dates.map(_.fromYear), dates.map(_.toYear))
    case _ => Capital(WikiCleanUtils.removeReferences(s))
  }
}
