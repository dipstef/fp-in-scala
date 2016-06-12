package chapter08


object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

import chapter08.Prop.{FailedCase, SuccessCount}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}