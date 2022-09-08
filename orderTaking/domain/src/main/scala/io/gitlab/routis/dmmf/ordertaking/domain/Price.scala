package io.gitlab.routis.dmmf.ordertaking.domain

import MoneySupport.Money
import zio.prelude.{ Assertion, Subtype, Validation }
import Assertion.*

import scala.annotation.targetName

object Price extends Subtype[Money]:

  val zero: Price = Price.makeUnsafe(0)

  override inline def assertion: Assertion[Money] =
    import MoneySupport.Money.MoneyHasOrdering
    greaterThanOrEqualTo(Money.zero) && lessThanOrEqualTo(Money(1000))

  def make(amount: BigDecimal): Validation[String, Price.Type] =
    Money.make(amount).flatMap(make)
  def makeUnsafe(amount: BigDecimal): Price =
    Money
      .make(amount)
      .flatMap(make)
      .toEitherWith(es => new IllegalArgumentException(es.head))
      .fold(throw _, identity)

  extension (self: Price)
    @targetName("*")
    def *(times: Long): Validation[String, Price.Type] =
      make(unwrap(self) * times)
