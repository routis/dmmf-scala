package io.gitlab.routis.dmmf.ordertaking

import zio.prelude.Validation

package object cmn:
  //
  // New types (Value objects in DDD terms)
  //
  type OrderId       = OrderId.Type
  type OrderLineId   = OrderLineId.Type
  type OrderQuantity = OrderQuantity.Kilograms | OrderQuantity.Units
  type Price         = BigDecimal
  type BillingAmount = zio.prelude.newtypes.Sum[BigDecimal]

  type ZipCode       = ZipCode.Type
  type EmailAddress  = EmailAddress.Type
  type PromotionCode = PromotionCode.Type

  //
  // Constructors for simple types
  //
  val makeOrderId: SmartConstructor[String, String, OrderId]         = OrderId.make
  val makeOrderLineId: SmartConstructor[String, String, OrderLineId] = OrderLineId.make
  val makeProductCode: SmartConstructor[String, String, ProductCode] = ProductCode.make
  def makeOrderQuantity(productCode: ProductCode): SmartConstructor[Double, String, OrderQuantity] =
    OrderQuantity.forProduct(productCode)

  val makeZipCode: SmartConstructor[String, String, ZipCode]           = ZipCode.make
  val makeEmailAddress: SmartConstructor[String, String, EmailAddress] = EmailAddress.make
  val makeString50: SmartConstructor[String, String, String50]         = String50.make
  val makeVipStatus: SmartConstructor[String, String, VipStatus]       = VipStatus.make

  //
  // Constructors for compound types (Value objects in DDD terms)
  //
  def makePersonalName[E](
    firstName: Validation[E, String50],
    lastName: Validation[E, String50]
  ): Validation[E, PersonalName] =
    Validation.validateWith(firstName, lastName)(PersonalName.apply)

  def makeAddress[E](
    addressLine1: Validation[E, String50],
    addressLine2: Validation[E, Option[String50]],
    addressLine3: Validation[E, Option[String50]],
    addressLine4: Validation[E, Option[String50]],
    city: Validation[E, String50],
    zipCode: Validation[E, ZipCode]
  ): Validation[E, Address] =
    Validation
      .validateWith(addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode)(Address.apply)

  def makeCustomerInfo[E](
    name: Validation[E, PersonalName],
    emailAddress: Validation[E, EmailAddress],
    vipStatus: Validation[E, VipStatus]
  ): Validation[E, CustomerInfo] =
    Validation
      .validateWith(name, emailAddress, vipStatus)(CustomerInfo.apply)

  //
  // Smart Constructor
  //
  type SmartConstructor[A, E, B] = A => Validation[E, B]
  object SmartConstructor:

    extension [A, E, B](smartConstructor: SmartConstructor[A, E, B])

      def changeError[E1](f: E => E1): SmartConstructor[A, E1, B] =
        smartConstructor.andThen(_.mapError(f))

      def required(ifMissing: => E): SmartConstructor[Option[A], E, B] =
        optionA =>
          optionA match
            case Some(a) => smartConstructor(a)
            case None    => Validation.fail(ifMissing)

      def optional: SmartConstructor[Option[A], E, Option[B]] =
        optionA =>
          optionA match
            case Some(a) => smartConstructor(a).map(Some.apply)
            case None    => Validation.succeed(None)

  end SmartConstructor
