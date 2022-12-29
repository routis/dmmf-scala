package io.gitlab.routis.dmmf.ordertaking.application.port.in

import zio.{ IO, NonEmptyChunk, ZIO }

import PlaceOrderUseCase.{ PlaceOrderError, PlaceOrderEvent, UnvalidatedOrder }

/**
 * A service for placing an order
 */
trait PlaceOrderUseCase extends (UnvalidatedOrder => IO[PlaceOrderError, List[PlaceOrderEvent]])

object PlaceOrderUseCase:

  // Types representing input of the place order (unvalidated order)

  case class UnvalidatedCustomerInfo(firstName: String, lastName: String, emailAddress: String, vipStatus: String)

  case class UnvalidatedAddress(
    addressLine1: String,
    addressLine2: String,
    addressLine3: String,
    addressLine4: String,
    city: String,
    country: String,
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

  /**
   * The possible events resulting from the PlaceOrder workflow
   * Not all events will occur, depending on the logic of the workflow
   */
  enum PlaceOrderEvent:

    case ShippableOrderSent(orderId: OrderId, shippingAddress: Address, lines: NonEmptyChunk[ShippableOrderLine])
        extends PlaceOrderEvent

    case BillableOrderPlaced(orderId: OrderId, billingAddress: Address, billingAmount: BillingAmount)
        extends PlaceOrderEvent

    case AcknowledgementSent(orderId: OrderId, emailAddress: EmailAddress) extends PlaceOrderEvent

  end PlaceOrderEvent

  // Errors
  enum PlaceOrderError:
    case ValidationFailure(errors: NonEmptyChunk[ValidationError])
    case PricingError(cause: String)
