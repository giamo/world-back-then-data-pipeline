package models

import cats.implicits._

object DatingLabel {
  final val BCVariants = List("bc", "b.c.", "bce", "b.c.e.", "bc.")
  final val ADVariants = List("ad", "a.d.", "ce", "c.e.", "ce.")
  final val DatingLabelVariantsStr =
    (DatingLabel.BCVariants ++ DatingLabel.ADVariants).mkString("|")

  sealed trait DatingLabel {
    val label: String
  }

  case object BC extends DatingLabel {
    override val label = "BC"
  }

  case object AD extends DatingLabel {
    override val label = "BC"
  }

  def fromString(labelStr: String): Either[DateParseError, DatingLabel] =
    labelStr match {
      case s if s == null || ADVariants.contains(s.trim) => AD.asRight
      case s if BCVariants.contains(s.trim) => BC.asRight
      case _ =>
        DateParseError(s"invalid dating label string: '$labelStr'").asLeft
    }
}
