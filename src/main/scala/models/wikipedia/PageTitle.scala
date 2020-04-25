package models.wikipedia

final case class PageTitle(value: String) {
  override def toString: String = value
}

object PageTitle {
  private val simpleLinkRegex = "\\[\\[([^\\[\\]\\|]+)\\]\\]".r
  private val complexLinkRegex = "\\[\\[(?:[^\\|]+)\\|([^\\[\\]]+)\\]\\]".r

  def fromLink(text: String): PageTitle = text.trim match {
    case simpleLinkRegex(value)  => PageTitle(value)
    case complexLinkRegex(value) => PageTitle(value)
    case _                       => PageTitle(text)
  }
}
