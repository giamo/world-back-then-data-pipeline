package com.github.giamo.wikihistory.models

final case class DbfRecord(
  name: String,
  wikiPage: String,
  shortName: Option[String],
  area: Option[Double]
)
