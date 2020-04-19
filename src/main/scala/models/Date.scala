package models

import models.DatingLabel.{AD, DatingLabel}

trait Date

trait Year {
  val value: Int
  val label: DatingLabel
}

final case class ExactYear(override val value: Int, override val label: DatingLabel = AD) extends Year

final case class ApproximateYear(override val value: Int, override val label: DatingLabel = AD) extends Year
