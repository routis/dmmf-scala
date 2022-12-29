package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.domain.{ Address, Price }

import scala.annotation.unused

private[service] case class ShippingCostCalculator() extends PlaceOrderService.CalculateShippingCost:

  override def calculate(pricedOrder: PlaceOrderService.PricedOrder): Price =
    import ShippingCostCalculator.{ area, Area }
    import Area.*
    area(pricedOrder.shippingAddress) match
      case Local         => Price(5)
      case EU            => Price(10)
      case International => Price(20)

private[service] object ShippingCostCalculator:

  private enum Area:
    case Local, EU, International

  private def area(@unused address: Address): Area = Area.EU
