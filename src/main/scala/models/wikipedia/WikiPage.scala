package models.wikipedia

case class WikiPage(
  id: Long,
  title: String,
  text: String,
  isCategory: Boolean,
  isFile: Boolean,
  isTemplate: Boolean
)
