package models

case class WikiPage(
  title: String,
  text: String,
  isCategory: Boolean,
  isFile: Boolean,
  isTemplate: Boolean
)
