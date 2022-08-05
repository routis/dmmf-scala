package io.gitlab.routis.dmmf.ordertaking

import zio.prelude.Validation

package object cmn:
  type OrderId       = OrderId.Type
  type OrderLineId   = OrderLineId.Type
  type OrderQuantity = OrderQuantity.Kilograms | OrderQuantity.Units
  type Price         = BigDecimal
  type BillingAmount = zio.prelude.newtypes.Sum[BigDecimal]

  type ZipCode       = ZipCode.Type
  type EmailAddress  = EmailAddress.Type
  type PromotionCode = PromotionCode.Type

  def makeEmailAddress(s: String): Validation[String, EmailAddress] = EmailAddress.make(s)

  def makeString50(s: String): Validation[String, String50] = String50.make(s)

  def makeZipCode(s: String): Validation[String, ZipCode] = ZipCode.make(s)

  def makeVipStatus(s: String): Validation[String, VipStatus] = VipStatus.make(s)

  def makeProductCode(s: String): Validation[String, ProductCode] = ProductCode.make(s)

  def makeOrderQuantity(productCode: ProductCode)(quantity: Double): Validation[String, OrderQuantity] =
    OrderQuantity.forProduct(productCode)(quantity)

  def makeOrderId(s: String): Validation[String, OrderId] = OrderId.make(s)

  def makeOrderLineId(s: String): Validation[String, OrderLineId] = OrderLineId.make(s)
