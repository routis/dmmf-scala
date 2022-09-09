package io.gitlab.routis.dmmf.ordertaking.domain

import zio.prelude.{ Assertion, Newtype, Subtype, Validation }
import zio.prelude.Assertion.{ between, greaterThanOrEqualTo, lessThanOrEqualTo }
import io.gitlab.routis.dmmf.ordertaking.domain.OrderQuantity.{ KilogramsQuantity, UnitsQuantity }

enum OrderQuantity:
  self =>
  def value: Double = self match
    case Kilograms(kilograms) => kilograms
    case Units(units)         => units.toDouble

  case Kilograms(quantity: KilogramsQuantity)

  case Units(quantity: UnitsQuantity)
object OrderQuantity:

  type KilogramsQuantity = OrderQuantity.KilogramsQuantity.Type
  type UnitsQuantity     = OrderQuantity.UnitsQuantity.Type

  object KilogramsQuantity extends Subtype[Double]:
    override inline def assertion: Assertion[Double] = between(0d, 100d)

  object UnitsQuantity extends Subtype[Int]:
    override inline def assertion: Assertion[Int] = between(0, 1000)

  def forProduct(productCode: ProductCode)(value: Double): Validation[String, OrderQuantity] =
    productCode match
      case _: ProductCode.Gizmo =>
        KilogramsQuantity.make(value).map(Kilograms.apply)
      case _: ProductCode.Widget =>
        UnitsQuantity.make(value.intValue()).map(Units.apply)
