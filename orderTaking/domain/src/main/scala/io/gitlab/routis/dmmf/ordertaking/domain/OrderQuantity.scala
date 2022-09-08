package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Newtype, Subtype, Validation }
import zio.prelude.Assertion.{ greaterThanOrEqualTo, lessThanOrEqualTo }
import io.gitlab.routis.dmmf.ordertaking.domain.OrderQuantity.{ KilogramsQuantity, UnitsQuantity }

enum OrderQuantity:
  def value: Double =
    this match
      case Kilograms(kilograms) => kilograms
      case Units(units)         => units.toDouble

  case Kilograms(quantity: KilogramsQuantity) extends OrderQuantity

  case Units(quantity: UnitsQuantity) extends OrderQuantity
object OrderQuantity:

  type KilogramsQuantity = OrderQuantity.KilogramsQuantity.Type
  type UnitsQuantity     = OrderQuantity.UnitsQuantity.Type

  object KilogramsQuantity extends Subtype[Double]:
    override inline def assertion: Assertion[Double] =
      greaterThanOrEqualTo(0d) && lessThanOrEqualTo(100d)

  object UnitsQuantity extends Subtype[Int]:
    override inline def assertion: Assertion[Int] =
      greaterThanOrEqualTo(0) && lessThanOrEqualTo(1000)

  def forProduct(productCode: ProductCode)(value: Double): Validation[String, OrderQuantity] =
    productCode match
      case _: ProductCode.Gizmo =>
        KilogramsQuantity.make(value).map(Kilograms.apply)
      case _: ProductCode.Widget =>
        UnitsQuantity.make(value.intValue()).map(Units.apply)
