package io.gitlab.routis.dmmf.ordertaking.domain

import MoneySupport.Money
import zio.prelude.{ Assertion, Newtype, Validation }

import scala.annotation.targetName

object Price extends Newtype[Money]:

  val zero: Price = Price.makeUnsafe(0)

  override inline def assertion: Assertion[Money] =
    import MoneySupport.Money.MoneyHasOrdering
    Assertion.greaterThanOrEqualTo(Money.zero) && Assertion.lessThanOrEqualTo(Money(1000))

  def make(amount: BigDecimal): Validation[String, Price.Type] =
    Money.make(amount).flatMap(make)
  def makeUnsafe(amount: BigDecimal): Price =
    Money
      .make(amount)
      .flatMap(make)
      .toEitherWith(es => new IllegalArgumentException(es.head))
      .fold(throw _, identity)

  extension (price: Price)
    @targetName("*")
    def *(times: Long): Validation[String, Price.Type] =
      val money: Money = Price.unwrap(price)
      make(money * times)

    def toMoney: Money = Price.unwrap(price)
