package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Subtype, Validation }
import Assertion.*

import scala.annotation.targetName

object Price extends Subtype[Money]:
  override inline def assertion: Assertion[Money]         = between(Money(0), Money(1000))
  def make(amount: BigDecimal): Validation[String, Price] = Money.make(amount).flatMap(make)
  inline def apply(inline amount: BigDecimal): Price      = wrap(Money(amount))

  extension (self: Price)
    @targetName("*")
    def *(times: Long): Validation[String, Price] = make(unwrap(self) * times)
