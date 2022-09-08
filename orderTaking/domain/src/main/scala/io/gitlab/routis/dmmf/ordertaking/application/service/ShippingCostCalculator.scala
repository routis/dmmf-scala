package io.gitlab.routis.dmmf.ordertaking.application.service

import io.gitlab.routis.dmmf.ordertaking.domain.{ Address, Price }

private[service] case class ShippingCostCalculator() extends PlaceOrderService.CalculateShippingCost:

  override def calculate(pricedOrder: PlaceOrderService.PricedOrder): Price =
    import ShippingCostCalculator.{ area, Area }
    import Area.*
    area(pricedOrder.shippingAddress) match
      case Local         => Price.makeUnsafe(5)
      case EU            => Price.makeUnsafe(10)
      case International => Price.makeUnsafe(20)

private[service] object ShippingCostCalculator:

  enum Area:
    case Local, EU, International

  def area(address: Address): Area = Area.EU
