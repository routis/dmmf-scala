package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.*
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError.PricingError
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{
  CheckAddressExists,
  CheckProductCodeExists,
  GetPromotionProductPrice,
  GetStandardProductPrice
}
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService.*
import io.gitlab.routis.dmmf.ordertaking.domain.*
import zio.{ Chunk, IO, NonEmptyChunk, UIO, URLayer, ZIO }

/**
 * The implementation of [PlaceOrder] Delegates validation to [Validate] which is implemented by [ValidatePlacedOrder]
 */
private[service] case class PlaceOrderService(
  validateOrder: ValidateOrder,
  priceOrder: PriceOrder,
  calculateShippingCost: CalculateShippingCost
) extends PlaceOrderUseCase:

  override def execute(unvalidatedOrder: UnvalidatedOrder): IO[PlaceOrderError, List[PlaceOrderEvent]] =
    for
      validatedOrder               <- validateOrder(unvalidatedOrder)
      pricedOrder                  <- priceOrder(validatedOrder)
      pricedOrderWithShippingMethod = addShippingInfoToOrder(pricedOrder)
      shipping                      = PlaceOrderService.createShippingEvent(pricedOrder)
    yield List(shipping)

  private def addShippingInfoToOrder(pricedOrder: PricedOrder): PricedOrderWithShippingMethod =
    val shippingCost = calculateShippingCost(pricedOrder)
    val shippingInfo = ShippingInfo(ShippingMethod.Fedex48, shippingCost)
    PricedOrderWithShippingMethod(shippingInfo, pricedOrder)
object PlaceOrderService:

  private[service] type ValidateOrder         = UnvalidatedOrder => IO[PlaceOrderError.ValidationFailure, ValidatedOrder]
  private[service] type PriceOrder            = ValidatedOrder => IO[PricingError, PricedOrder]
  private[service] type CalculateShippingCost = PricedOrder => Price

  //
  // Validation types and services
  //
  private[service] final case class ValidatedOrderLine(
    orderLineId: OrderLineId,
    productCode: ProductCode,
    quantity: OrderQuantity
  )

  private[service] final case class ValidatedOrder(
    orderId: OrderId,
    customerInfo: CustomerInfo,
    shippingAddress: Address,
    billingAddress: Address,
    lines: NonEmptyChunk[ValidatedOrderLine],
    pricingMethod: PricingMethod
  )

  //
  // Pricing types and services
  //

  enum PricedOrderLine:

    def price: Price =
      this match
        case PricedOrderProductLine(_, _, _, linePrice) => linePrice
        case Comment(_)                                 => Price(0)

    case PricedOrderProductLine(
      orderLineId: OrderLineId,
      productCode: ProductCode,
      quantity: OrderQuantity,
      linePrice: Price
    )
    case Comment(comment: String)
  end PricedOrderLine

  enum PricingMethod:
    case Standard()                     extends PricingMethod
    case Promotion(code: PromotionCode) extends PricingMethod
  object PricingMethod:
    def create(promotionCode: String): PricingMethod =
      Option(promotionCode)
        .filter(_.nonEmpty)
        .flatMap(it => PromotionCode.make(it).toOption.map(Promotion.apply))
        .getOrElse(Standard())

  case class PricedOrder(
    orderId: OrderId,
    customerInfo: CustomerInfo,
    shippingAddress: Address,
    billingAddress: Address,
    amountToBill: BillingAmount,
    lines: NonEmptyChunk[PricedOrderLine],
    pricingMethod: PricingMethod
  )

  //
  // Shipping types and services
  //
  private[service] enum ShippingMethod:
    case PostalService, Fedex24, Fedex48, Ups48

  private[service] final case class ShippingInfo(shippingMethod: ShippingMethod, shippingCost: Price)
  private[service] final case class PricedOrderWithShippingMethod(shippingInfo: ShippingInfo, pricedOrder: PricedOrder)

  //
  // Event creators
  //
  import PlaceOrderUseCase.PlaceOrderEvent.*
  private def createShippingEvent(placedOrder: PricedOrder): ShippableOrderSent =
    ShippableOrderSent(
      placedOrder.orderId,
      placedOrder.shippingAddress,
      NonEmptyChunk
        .fromChunk(placedOrder.lines.toChunk.flatMap { line =>
          line match
            case PricedOrderLine.PricedOrderProductLine(_, productCode, quantity, _) =>
              Chunk(ShippableOrderLine(productCode, quantity))
            case _ => Chunk.empty

        })
        .get
    )

  //
  // Dependency Injection
  //
  val layer: URLayer[
    CheckAddressExists & CheckProductCodeExists & GetStandardProductPrice & GetPromotionProductPrice,
    PlaceOrderUseCase
  ] =
    zio.ZLayer {
      for
        checkAddressExists       <- ZIO.service[CheckAddressExists]
        checkProductCodeExists   <- ZIO.service[CheckProductCodeExists]
        getStandardProductPrice  <- ZIO.service[GetStandardProductPrice]
        getPromotionProductPrice <- ZIO.service[GetPromotionProductPrice]
        validateOrder             = ValidateOrder(checkAddressExists, checkProductCodeExists)
        priceOrder                = PriceOrder(getStandardProductPrice, getPromotionProductPrice)
        shippingCostCalculator    = CalculateShippingCost()
      yield PlaceOrderService(validateOrder, priceOrder, shippingCostCalculator)
    }
