package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Newtype, Subtype, Validation }
import zio.prelude.Assertion.{ greaterThanOrEqualTo, lessThanOrEqualTo }

sealed trait OrderQuantity
object OrderQuantity:

  type KilogramsQuantity = OrderQuantity.KilogramsQuantity.Type
  type UnitsQuantity     = OrderQuantity.UnitsQuantity.Type

  final case class Kilograms(quantity: KilogramsQuantity) extends OrderQuantity
  final case class Units(quantity: UnitsQuantity)         extends OrderQuantity

  object KilogramsQuantity extends Subtype[Double]:
    override inline def assertion: Assertion[Double] =
      greaterThanOrEqualTo(0d).&&(lessThanOrEqualTo(100d))

  object UnitsQuantity extends Subtype[Int]:
    override inline def assertion: Assertion[Int] =
      greaterThanOrEqualTo(0).&&(lessThanOrEqualTo(1000))

  def forProduct(productCode: ProductCode)(value: Double): Validation[String, OrderQuantity] =
    productCode match
      case _: ProductCode.GizmoCode =>
        KilogramsQuantity.make(value).map(Kilograms.apply)
      case _: ProductCode.WidgetCode =>
        UnitsQuantity.make(value.intValue()).map(Units.apply)

  def value(orderQuantity: OrderQuantity): Double =
    orderQuantity match
      case Kilograms(kilograms) => KilogramsQuantity.unwrap(kilograms)
      case Units(units)         => UnitsQuantity.unwrap(units).toDouble
