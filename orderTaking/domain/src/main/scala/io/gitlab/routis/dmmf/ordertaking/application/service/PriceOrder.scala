package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.domain.{ BillingAmount, OrderQuantity, Price, ProductCode }
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError.PricingError
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{ GetPromotionProductPrice, GetStandardProductPrice }
import io.gitlab.routis.dmmf.ordertaking.application.service.PriceOrder.addCommentLine
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService.{
  PricedOrder,
  PricedOrderLine,
  PricingMethod,
  ValidatedOrder,
  ValidatedOrderLine
}
import zio.prelude.Validation
import zio.{ IO, NonEmptyChunk, UIO, ZIO }

private[service] case class PriceOrder(
  getStandardProductPrice: GetStandardProductPrice,
  getPromotionProductPrice: GetPromotionProductPrice
) extends (ValidatedOrder => IO[PricingError, PricedOrder]):

  import PriceOrder.toIO
  override def apply(validatedOrder: ValidatedOrder): IO[PricingError, PricedOrder] =
    val pricingMethod = validatedOrder.pricingMethod
    val toPriced      = toPricedOrderLine(pricingMethod)
    for
      lines            <- ZIO.collectAllPar(validatedOrder.lines.map(toPriced))
      linesWithComments = addCommentLine(pricingMethod)(lines)
      amountToBill     <- BillingAmount.total(lines.map(_.price)).toIO
    yield PricedOrder(
      validatedOrder.orderId,
      validatedOrder.customerInfo,
      validatedOrder.shippingAddress,
      validatedOrder.billingAddress,
      amountToBill,
      linesWithComments,
      pricingMethod
    )

  private def toPricedOrderLine(
    pricingMethod: PricingMethod
  )(line: ValidatedOrderLine): IO[PricingError, PricedOrderLine] =
    for
      productPrice <- getProductPrice(line.productCode, pricingMethod)
      qty           = line.quantity.value.toLong
      linePrice    <- (productPrice * qty).toIO
    yield PricedOrderLine.PricedOrderProductLine(line.orderLineId, line.productCode, line.quantity, linePrice)

  private def getProductPrice(productCode: ProductCode, pricingMethod: PricingMethod): UIO[Price] =
    import PricingMethod.{ Standard, Promotion }
    pricingMethod match
      case Standard() => getStandardProductPrice(productCode)
      case Promotion(promotionCode) =>
        getPromotionProductPrice(promotionCode, productCode)
          .someOrElseZIO(getStandardProductPrice(productCode))

private[service] object PriceOrder:

  def addCommentLine(pricingMethod: PricingMethod)(
    lines: NonEmptyChunk[PricedOrderLine]
  ): NonEmptyChunk[PricedOrderLine] =
    pricingMethod match
      case PricingMethod.Standard() => lines
      case PricingMethod.Promotion(promotionCode) =>
        lines :+ PricedOrderLine.Comment(s"Applied promotion $promotionCode")

  extension [A](v: Validation[String, A])
    def toIO: zio.IO[PricingError, A] =
      import Validations.toIO as valToIo
      v.valToIo.mapError[PricingError](es => PricingError(es.head))
