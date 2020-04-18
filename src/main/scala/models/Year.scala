package models

import models.DatingLabel.{AD, DatingLabel}

final case class Year(value: Int, label: DatingLabel = AD)
