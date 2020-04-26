package models.wikipedia

import cats.implicits._

final case class PageTitle(value: String) {
  override def toString: String = value
}

object PageTitle {
  private val simpleLinkRegex = "[^\\[]*\\[\\[([^\\[\\]\\|]+)\\]\\].*?".r
  private val complexLinkRegex = "[^\\[]*\\[\\[([^\\|]+)\\|(?:[^\\[\\]]+)\\]\\].*?".r

  def fromLink(text: String): Option[PageTitle] = text match {
    case simpleLinkRegex(value)  => PageTitle(value.trim).some
    case complexLinkRegex(value) => PageTitle(value.trim).some
    case _                       => None
  }
}
