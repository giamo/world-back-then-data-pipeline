package com.github.giamo.wikihistory.models

final case class DbfRecord(
  year: Int,
  name: String,
  wikiPage: String,
  shortName: Option[String],
  area: Option[Double]
)
