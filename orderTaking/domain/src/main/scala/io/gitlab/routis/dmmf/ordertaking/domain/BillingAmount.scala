package io.gitlab.routis.dmmf.ordertaking.domain

import zio.NonEmptyChunk
import zio.prelude.{ Assertion, Newtype, Validation }
import Assertion.*
import MoneySupport.Money

object BillingAmount extends Newtype[Money]:

  override inline def assertion: Assertion[Money] =
    import MoneySupport.Money.MoneyHasOrdering
    greaterThan(Money.zero) && lessThanOrEqualTo(Money(10000))

  def total(prices: NonEmptyChunk[Price]): Validation[String, BillingAmount] =
    import zio.prelude.*
    import Price.toMoney
    import MoneySupport.Money.MoneyAdditionIsIdentity
    BillingAmount.make(prices.foldMap(_.toMoney))

  extension (billingAmount: BillingAmount) def toMoney: Money = BillingAmount.unwrap(billingAmount)
