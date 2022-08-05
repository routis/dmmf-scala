package io.gitlab.routis.dmmf.ordertaking.cmn

import zio.prelude.{ Assertion, Newtype, Subtype, Validation }
import zio.prelude.Assertion.{ greaterThanOrEqualTo, lessThanOrEqualTo }

object OrderQuantity:

  type Kilograms = OrderQuantity.Kilograms.Type
  type Units     = OrderQuantity.Units.Type

  object Kilograms extends Subtype[Double]:
    override inline def assertion: Assertion[Double] =
      greaterThanOrEqualTo(0d).&&(lessThanOrEqualTo(100d))

  object Units extends Subtype[Int]:
    override inline def assertion: Assertion[Int] =
      greaterThanOrEqualTo(0).&&(lessThanOrEqualTo(1000))

  def forProduct(productCode: ProductCode)(value: Double): Validation[String, OrderQuantity] =
    productCode match
      case _: ProductCode.GizmoCode =>
        Kilograms.make(value)
      case _: ProductCode.WidgetCode =>
        Units.make(value.intValue())

  inline def value(orderQuantity: OrderQuantity): Double =
    orderQuantity match
      case Kilograms(k) => k
      case Units(u)     => u.toDouble
