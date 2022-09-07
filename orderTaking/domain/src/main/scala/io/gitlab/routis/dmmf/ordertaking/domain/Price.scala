package io.gitlab.routis.dmmf.ordertaking.domain

import org.joda.money.Money as JodaMoney
import zio.prelude.{ Assertion, Newtype, Validation }

object Price extends Newtype[JodaMoney]:

  private val min: JodaMoney = MoneyUtils.zero
  private val max: JodaMoney = MoneyUtils.makeUnsafe(1000)
  val zero: Price.Type       = Price.makeUnsafe(0)

  override inline def assertion: Assertion[JodaMoney] =
    import MoneyUtils.JodaMoneyHasOrdering
    Assertion.greaterThanOrEqualTo(min) && Assertion.lessThanOrEqualTo(max)

  def make(amount: BigDecimal): Validation[String, Price.Type] =
    MoneyUtils.make(amount).flatMap(make)
  def makeUnsafe(money: BigDecimal): Price =
    MoneyUtils
      .make(money)
      .flatMap(make)
      .toEitherWith(es => new IllegalArgumentException(es.head))
      .fold(throw _, identity)

  extension (price: Price.Type)
    def multipliedBy(times: Double): Validation[String, Price.Type] =
      val multiplied: JodaMoney = Price.unwrap(price).multipliedBy(times.toLong)
      make(multiplied)
