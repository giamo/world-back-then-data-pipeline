package models

import models.DatingLabel.{AD, DatingLabel}

trait Date {
  val label: DatingLabel
}

trait Year extends Date {
  val yearNumber: Int
}

trait Decade extends Date {
  val decadeNumber: Int
}

final case class ExactYear(override val yearNumber: Int, override val label: DatingLabel = AD) extends Year

final case class ApproximateYear(override val yearNumber: Int, override val label: DatingLabel = AD) extends Year

final case class ExactDecade(override val decadeNumber: Int, override val label: DatingLabel = AD) extends Decade

final case class ApproximateDecade(override val decadeNumber: Int, override val label: DatingLabel = AD) extends Decade