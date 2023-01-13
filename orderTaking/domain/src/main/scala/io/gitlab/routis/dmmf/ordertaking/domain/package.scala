package io.gitlab.routis.dmmf.ordertaking

import org.joda.money.Money as JodaMoney
import zio.NonEmptyChunk
import zio.prelude.Assertion.*
import zio.prelude.{ Assertion, Newtype, Subtype, Validation }

package object domain:

  //
  // New types (Value objects in DDD terms)
  //
  type OrderId       = OrderId.Type
  type OrderLineId   = OrderLineId.Type
  type Price         = Price.Type
  type BillingAmount = BillingAmount.Type
  type ZipCode       = ZipCode.Type
  type Country       = Iso3166.Part1Alpha2.Type
  type EmailAddress  = EmailAddress.Type
  type PromotionCode = PromotionCode.Type
  type String50      = String50.Type

  //
  // Constraint definitions
  //
  object OrderId     extends Newtype[String]
  object OrderLineId extends Newtype[String]
  object ZipCode extends Newtype[String]:
    override inline def assertion: Assertion[String] = matches("^\\d{5}$".r)
  object EmailAddress extends Newtype[String]:
    override inline def assertion: Assertion[String] = matches("^(.+)@(.+)$".r)
  object String50 extends Subtype[String]:
    override inline def assertion: Assertion[String] = hasLength(between(1, 50))
  object PromotionCode extends Newtype[String]
