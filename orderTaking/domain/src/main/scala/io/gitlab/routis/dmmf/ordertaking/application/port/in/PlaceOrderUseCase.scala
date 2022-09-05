package io.gitlab.routis.dmmf.ordertaking.application.port.in

import zio.{ IO, NonEmptyChunk, ZIO }

/**
 * A service for placing an order
 */
trait PlaceOrderUseCase:
  import PlaceOrderUseCase.{ PlaceOrderError, PlaceOrderEvent, UnvalidatedOrder }
  def placeOrder(unvalidatedOrder: UnvalidatedOrder): IO[PlaceOrderError, List[PlaceOrderEvent]]

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

  enum PlaceOrderEvent:

    case ShippableOrderSent(
      orderId: OrderId,
      shippingAddress: Address,
      billingAddress: Address,
      lines: NonEmptyChunk[ShippableOrderLine],
      promotionCode: PromotionCode
    ) extends PlaceOrderEvent

    case BillableOrderPlaced(orderId: OrderId, billingAddress: Address, billingAmount: BillingAmount)
        extends PlaceOrderEvent

    case AcknowledgementSent(orderId: OrderId, emailAddress: EmailAddress) extends PlaceOrderEvent

  end PlaceOrderEvent

  // Errors
  enum PlaceOrderError:
    case ValidationFailure(errors: NonEmptyChunk[ValidationError]) extends PlaceOrderError
    case PricingError(cause: String)                               extends PlaceOrderError
