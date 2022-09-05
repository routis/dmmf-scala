package io.gitlab.routis.dmmf.ordertaking.application.port.in

import zio.{ IO, NonEmptyChunk, ZIO }

/**
 * A service for placing an order
 */
trait PlaceOrderUseCase:
  import PlaceOrderUseCase.{ PlaceOrderError, PlaceOrderEvents, UnvalidatedOrder }
  def placeOrder(unvalidatedOrder: UnvalidatedOrder): IO[PlaceOrderError, PlaceOrderEvents]

object PlaceOrderUseCase:

  // Types representing input of the place order (unvalidated order)

  case class UnvalidatedCustomerInfo(firstName: String, lastName: String, emailAddress: String, vipStatus: String)

  case class UnvalidatedAddress(
    addressLine1: String,
    addressLine2: String,
    addressLine3: String,
    addressLine4: String,
    city: String,
    zipCode: String
  )

  case class UnvalidatedOrderLine(orderLineId: String, productCode: String, quantity: Double)

  case class UnvalidatedOrder(
    orderId: String,
    customerInfo: UnvalidatedCustomerInfo,
    shippingAddress: UnvalidatedAddress,
    billingAddress: UnvalidatedAddress,
    lines: List[UnvalidatedOrderLine],
    promotionCode: String
  )

  // Types representing the outcome of placing an order

  import io.gitlab.routis.dmmf.ordertaking.domain.*

  case class ShippableOrderLine(productCode: ProductCode, quantity: OrderQuantity)

  enum PlaceOrderEvents:

    case ShippableOrderSent(
      orderId: OrderId,
      shippingAddress: Address,
      billingAddress: Address,
      lines: NonEmptyChunk[ShippableOrderLine],
      promotionCode: PromotionCode
    ) extends PlaceOrderEvents

    case BillableOrderPlaced(orderId: OrderId, billingAddress: Address, billingAmount: BillingAmount)
        extends PlaceOrderEvents

    case AcknowledgementSent(orderId: OrderId, emailAddress: EmailAddress) extends PlaceOrderEvents

  end PlaceOrderEvents

  // Errors
  enum PlaceOrderError:
    case ValidationFailure(errors: NonEmptyChunk[ValidationError]) extends PlaceOrderError
    case PricingError(cause: String)                               extends PlaceOrderError
