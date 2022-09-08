package io.gitlab.routis.dmmf.ordertaking.domain

import zio.NonEmptyChunk
import zio.prelude.{ Assertion, Identity, Subtype, Validation }
import Assertion.*
import zio.prelude.newtypes.Sum

object BillingAmount extends Subtype[Money]:

  override inline def assertion: Assertion[Money] = between(Money(0), Money(10000))

  def total(prices: NonEmptyChunk[Price]): Validation[String, BillingAmount] =
    import zio.prelude.*
    def sum(ms: NonEmptyChunk[Money]): Money =
      import Money.MoneyAdditionIsIdentity
      ms.reduceIdentity
    // Price type is not an Identity (aka Monoid) on addition because of it
    // could be the case that when adding two prices you may exceed the limit
    // Thus, to sum Prices (which is a sub-class of Money) you have to sum Money
    // which has an Identity on addition
    make(sum(prices))
