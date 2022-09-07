package io.gitlab.routis.dmmf.ordertaking.application.service
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.PlaceOrderError.PricingError
import io.gitlab.routis.dmmf.ordertaking.application.port.out.{ GetPromotionProductPrice, GetStandardProductPrice }
import io.gitlab.routis.dmmf.ordertaking.application.service.PlaceOrderService.{
  PricedOrderLine,
  PricingMethod,
  ValidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.domain.{ OrderQuantity, Price, ProductCode }
import zio.{ IO, UIO }

case class GetPriceLive(standardPrices: GetStandardProductPrice, promoPrices: GetPromotionProductPrice)
    extends PlaceOrderService.GetPrice:

  override def priceOrder(
    validatedOrder: PlaceOrderService.ValidatedOrder
  ): IO[PlaceOrderError.PricingError, PlaceOrderService.PricedOrder] = ???

  inline def getLinePrice(pricingMethod: PricingMethod)(line: ValidatedOrderLine): IO[PricingError, PricedOrderLine] =
    for
      productPrice <- getProductPrice(line.productCode, pricingMethod)
      qty           = OrderQuantity.value(line.quantity)
      linePrice     = productPrice * qty
    yield PricedOrderLine.PricedOrderProductLine(line.orderLineId, line.productCode, line.quantity, linePrice)

  def getProductPrice(productCode: ProductCode, pricingMethod: PricingMethod): UIO[Price] =
    pricingMethod match
      case PricingMethod.Standard() => standardPrices.getStandardProductPrice(productCode)
      case PricingMethod.Promotion(promotionCode) =>
        promoPrices
          .getPromotionProductPrice(promotionCode, productCode)
          .someOrElseZIO(standardPrices.getStandardProductPrice(productCode))

object GetPriceLive
