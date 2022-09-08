package io.gitlab.routis.dmmf.ordertaking.domain

import zio.NonEmptyChunk
import zio.prelude.{ Assertion, Subtype, Validation }
import Assertion.*

object BillingAmount extends Subtype[Money]:

  override inline def assertion: Assertion[Money] =
    greaterThanOrEqualTo(Money(0)) && lessThanOrEqualTo(Money(10000))

  def total(prices: NonEmptyChunk[Price]): Validation[String, BillingAmount] =
    import zio.prelude.*
    import Money.MoneyAdditionIsIdentity
    make(prices.foldMap[Money](identity))
