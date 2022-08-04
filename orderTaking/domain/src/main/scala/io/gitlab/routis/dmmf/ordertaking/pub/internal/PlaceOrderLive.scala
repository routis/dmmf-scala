package io.gitlab.routis.dmmf.ordertaking.pub.internal

import io.gitlab.routis.dmmf.ordertaking.cmn
import io.gitlab.routis.dmmf.ordertaking.cmn.Common.*
import io.gitlab.routis.dmmf.ordertaking.pub.{ CheckProductCodeExists, PlaceOrder }
import io.gitlab.routis.dmmf.ordertaking.pub.PlaceOrder.*
import io.gitlab.routis.dmmf.ordertaking.pub.internal.PlaceOrderLive.*
import io.gitlab.routis.dmmf.ordertaking.pub.CheckAddressExists
import zio.{ IO, NonEmptyChunk, UIO, URLayer, ZIO }

/**
 * The implementation of [PlaceOrder] Delegates validation to [Validate] which is implemented by
 * [ValidatePlacedOrder]
 */
private case class PlaceOrderLive(validator: Validate) extends PlaceOrder:

  override def placeOrder(
    unvalidatedOrder: UnvalidatedOrder
  ): IO[PlaceOrderError, PlaceOrderEvents] =
    ???

object PlaceOrderLive:

  private[internal] final case class ValidatedOrderLine(
    orderLineId: OrderLineId,
    productCode: ProductCode,
    quantity: OrderQuantity
  )

  private[internal] final case class ValidatedOrder(
    orderId: OrderId,
    customerInfo: CustomerInfo,
    shippingAddress: Address,
    billingAddress: Address,
    lines: NonEmptyChunk[ValidatedOrderLine]
  )

  private[internal] trait Validate:
    def validateOrder(
      unvalidatedOrder: UnvalidatedOrder
    ): IO[PlaceOrderError.ValidationFailure, ValidatedOrder]

  //
  // Dependency Injection
  //
  val layer: URLayer[CheckAddressExists & CheckProductCodeExists, PlaceOrderLive] =
    zio.ZLayer {
      for
        checkAddressExists     <- ZIO.service[CheckAddressExists]
        checkProductCodeExists <- ZIO.service[CheckProductCodeExists]
      yield PlaceOrderLive(ValidatePlacedOrder(checkAddressExists, checkProductCodeExists))
    }
