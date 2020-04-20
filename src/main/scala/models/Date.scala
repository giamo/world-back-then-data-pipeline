package models

import models.DateApproximation.{DateApproximation, GENERIC, NONE}
import models.DatingLabel.{AD, DatingLabel}

sealed trait Date {
  val label: DatingLabel
  val approximation: DateApproximation
}

final case class Year(
  yearNumber: Int,
  label: DatingLabel = AD,
  approximation: DateApproximation = NONE
) extends Date

final case class Decade(
  decadeNumber: Int,
  label: DatingLabel = AD,
  approximation: DateApproximation = NONE
) extends Date
