package io.gitlab.routis.dmmf.ordertaking.domain

import zio.NonEmptyChunk
import zio.prelude.{ Assertion, Subtype, Validation }
import Assertion.*

object BillingAmount extends Subtype[Money]:

  private val min = Money.zero
  override inline def assertion: Assertion[Money] =
    import Money.MoneyHasOrdering
    greaterThanOrEqualTo(min) && lessThanOrEqualTo(Money(10000))

  def total(prices: NonEmptyChunk[Price]): Validation[String, BillingAmount] =
    import zio.prelude.*
    import Money.MoneyAdditionIsIdentity
    make(prices.foldMap[Money](identity))
