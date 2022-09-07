package io.gitlab.routis.dmmf.ordertaking.application.service
import io.gitlab.routis.dmmf.ordertaking.domain.{ OrderQuantity, Price, ProductCode }
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError.PricingError
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{ GetPromotionProductPrice, GetStandardProductPrice }
import io.gitlab.routis.dmmf.ordertaking.application.service.PricingService.addCommentLine
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService.{
  PriceOrder,
  PricedOrder,
  PricedOrderLine,
  PricingMethod,
  ValidatedOrder,
  ValidatedOrderLine
}
import zio.{ IO, NonEmptyChunk, UIO, ZIO }

private[service] case class PricingService(
  standardPrices: GetStandardProductPrice,
  promoPrices: GetPromotionProductPrice
) extends PriceOrder:

  override def priceOrder(validatedOrder: ValidatedOrder): IO[PricingError, PricedOrder] =
    import zio.prelude.*
    import io.gitlab.routis.dmmf.ordertaking.domain.MoneyUtils.JodaMoneyAdditionIsIdentity
    for
      _                <- ZIO.unit
      pricingMethod     = validatedOrder.pricingMethod
      pricingFunction   = toPricedOrderLine(pricingMethod)
      lines            <- ZIO.collectAllPar(validatedOrder.lines.map(pricingFunction))
      linesWithComments = addCommentLine(pricingMethod)(lines)
      amountToBill      = lines.foldMap(line => Price.unwrap(line.price))
    yield PricedOrder(
      validatedOrder.orderId,
      validatedOrder.customerInfo,
      validatedOrder.shippingAddress,
      validatedOrder.billingAddress,
      amountToBill,
      linesWithComments,
      pricingMethod
    )

  def toPricedOrderLine(pricingMethod: PricingMethod)(line: ValidatedOrderLine): IO[PricingError, PricedOrderLine] =
    import Validations.toIO
    import Price.multipliedBy
    for
      productPrice <- getProductPrice(line.productCode, pricingMethod)
      qty           = line.quantity.value
      linePrice    <- productPrice.multipliedBy(qty).toIO.mapError[PricingError](es => PricingError(es.head))
    yield PricedOrderLine.PricedOrderProductLine(line.orderLineId, line.productCode, line.quantity, linePrice)

  def getProductPrice(productCode: ProductCode, pricingMethod: PricingMethod): UIO[Price] =
    pricingMethod match
      case PricingMethod.Standard() => standardPrices.getStandardProductPrice(productCode)
      case PricingMethod.Promotion(promotionCode) =>
        promoPrices
          .getPromotionProductPrice(promotionCode, productCode)
          .someOrElseZIO(standardPrices.getStandardProductPrice(productCode))

private[service] object PricingService:
  def addCommentLine(pricingMethod: PricingMethod)(
    lines: NonEmptyChunk[PricedOrderLine]
  ): NonEmptyChunk[PricedOrderLine] =
    pricingMethod match
      case PricingMethod.Standard() => lines
      case PricingMethod.Promotion(promotionCode) =>
        lines :+ PricedOrderLine.Comment(s"Applied promotion $promotionCode")
