package io.gitlab.routis.dmmf.ordertaking.pub.internal

import io.gitlab.routis.dmmf.ordertaking.cmn.Common.*
import io.gitlab.routis.dmmf.ordertaking.pub.PlaceOrder.*
import io.gitlab.routis.dmmf.ordertaking.pub.internal.PlaceOrderLive.{
  ValidatedOrder,
  ValidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.pub.internal.ValidatePlacedOrder.*
import io.gitlab.routis.dmmf.ordertaking.pub.internal.ValidatePlacedOrder.CheckAddressExists.{
  AddressValidationError,
  CheckedAddress
}
import io.gitlab.routis.dmmf.ordertaking.pub.internal.Validations.*
import zio.prelude.Validation
import zio.{ IO, NonEmptyChunk, UIO, URLayer, ZIO }

private[internal] case class ValidatePlacedOrder(
  checkAddressExists: CheckAddressExists,
  checkProductCodeExists: CheckProductCodeExists
) extends PlaceOrderLive.Validate:

  def toCheckedAddress(
    field: FieldName,
    unvalidatedAddress: UnvalidatedAddress
  ): IO[NonEmptyChunk[ValidationError], Address] =

    def check = checkAddressExists.check(unvalidatedAddress) mapError { e =>
      val description = e match
        case AddressValidationError.AddressNotFound =>
          ValidationError.Cause("Address not found")
        case AddressValidationError.InvalidFormat   =>
          ValidationError.Cause("Address invalid format")
      NonEmptyChunk.single(fieldError(field)(description))
    }

    def validate(checkedAddress: CheckedAddress) =
      ZIO
        .fromEither(toAddress(checkedAddress))
        .mapError(nestErrors(field))
    def ensureNotNull                            = Validation.fromOptionWith(missingField(field))(Option(unvalidatedAddress))

    ZIO.logSpan(s"${field}") {
      for
        _              <- ensureNotNull.toZIOWithAllErrors
        checkedAddress <- check
        address        <- validate(checkedAddress)
        _              <- ZIO.log("Valid")
      yield address
    }

  def toProductCode(
    field: FieldName,
    unvalidated: String
  ): IO[NonEmptyChunk[ValidationError], ProductCode] =

    def exists(productCode: ProductCode) =
      checkProductCodeExists
        .check(productCode)
        .flatMap(exists =>
          if exists then ZIO.succeed(productCode)
          else
            ZIO.fail(
              NonEmptyChunk
                .single(fieldError(field)(ValidationError.Cause(s"Doesn't exist $productCode")))
            )
        )

    def validate = makeProductCode.requiredField(field, unvalidated).toZIOWithAllErrors

    for
      productCode     <- validate
      existingProduct <- exists(productCode)
    yield existingProduct

  def toValidatedOrderLine(
    unvalidated: UnvalidatedOrderLine
  ): IO[NonEmptyChunk[ValidationError], ValidatedOrderLine] =
    type EitherVV[A] = Either[NonEmptyChunk[ValidationError], A]
    def sync(
      maybeProductCode: EitherVV[ProductCode]
    ): Either[NonEmptyChunk[ValidationError], ValidatedOrderLine] =
      val orderLineId =
        OrderLineId.make.requiredField("orderLineId", unvalidated.orderLineId)

      // if maybeProductCode is left then whatever the quantity we return failed
      // otherwise we check the quantity
      val quantityConstructor: SmartConstructor[Double, String, OrderQuantity] =
        maybeProductCode.fold(
          _ => _ => Validation.fail("Cannot be validated due to invalid product code"),
          pc => OrderQuantity.forProduct(pc)
        )

      val productCode = maybeProductCode.toValidation
      val quantity    = quantityConstructor.requiredField("quantity", unvalidated.quantity)
      Validation
        .validateWith(orderLineId, productCode, quantity)(ValidatedOrderLine.apply)
        .toEither

    for
      productCode <-
        toProductCode(field = "productCode", unvalidated = unvalidated.productCode).either
      orderLine   <- ZIO.fromEither(sync(productCode))
    yield orderLine

  def validateOrder(
    unvalidated: UnvalidatedOrder
  ): IO[PlaceOrderError.ValidationFailure, ValidatedOrder] =

    def lines(field: FieldName) =
      val maybeLines                                                  = Option(unvalidated.lines).flatMap(NonEmptyChunk.fromIterableOption)
      val ensureNotNullNotEmpty                                       = Validation
        .fromOptionWith(fieldError(field)(ValidationError.Cause("Missing or empty")))(maybeLines)
        .toZIOWithAllErrors
      def vol(unvalidatedOrderLine: UnvalidatedOrderLine, index: Int) =
        val errorMapper = indexFieldError(field, index)
        ZIO.logSpan(s"$field-$index") {
          toValidatedOrderLine(unvalidatedOrderLine)
            .mapError(es => es.map(errorMapper))
            .foldZIO(
              e => ZIO.log("Error") *> ZIO.fail(e),
              line => ZIO.log("Valid") *> ZIO.succeed(line)
            )
        }

      (for
        nonEmptyLines   <- ensureNotNullNotEmpty
        linesValidation <- ZIO.collectAllPar(nonEmptyLines.zipWithIndex.map(vol))
      yield linesValidation).either

    type EitherNEC[A] = Either[NonEmptyChunk[ValidationError], A]
    def assemble(
      shippingAddress: EitherNEC[Address],
      billingAddress: EitherNEC[Address],
      lines: EitherNEC[NonEmptyChunk[ValidatedOrderLine]]
    ) =
      val orderId      = makeOrderId.requiredField("orderId", unvalidated.orderId)
      val customerInfo = (toCustomerInfo(_: UnvalidatedCustomerInfo).toValidation)
        .requiredField("customerInfo", unvalidated.unvalidatedCustomerInfo)
      Validation
        .validateWith(
          orderId,
          customerInfo,
          shippingAddress.toValidation,
          billingAddress.toValidation,
          lines.toValidation
        )(ValidatedOrder.apply)

    ZIO.logSpan(s"validateOrder-${unvalidated.orderId}") {
      (for
        shippingAddressFiber <-
          toCheckedAddress("shippingAddress", unvalidated.shippingAddress).either.fork
        billingAddressFiber  <-
          toCheckedAddress("billingAddress", unvalidated.billingAddress).either.fork
        linesFiber           <- lines("line").fork
        shippingAddress      <- shippingAddressFiber.join
        billingAddress       <- billingAddressFiber.join
        lines                <- linesFiber.join
        validatedOrder       <- assemble(shippingAddress, billingAddress, lines).toZIOWithAllErrors
        _                    <- ZIO.log("Valid")
      yield validatedOrder).mapError(PlaceOrderError.ValidationFailure.apply)
    }

private object ValidatePlacedOrder:

  def toCustomerInfo(
    unvalidated: UnvalidatedCustomerInfo
  ): Either[NonEmptyChunk[ValidationError], CustomerInfo] =
    val personalName = toPersonalName(unvalidated.firstName, unvalidated.lastName).toValidation
    val emailAddress = makeEmailAddress.requiredField("emailAddress", unvalidated.emailAddress)
    val vipStatus    = makeVipCode.requiredField("vipStatus", unvalidated.vipStatus)
    Validation
      .validateWith(personalName, emailAddress, vipStatus)(CustomerInfo.apply)
      .toEither

  def toAddress(checkedAddress: CheckedAddress): Either[NonEmptyChunk[ValidationError], Address] =
    val unvalidated  = checkedAddress.unvalidatedAddress
    val addressLine1 = makeString50.requiredField("addressLine1", unvalidated.addressLine1)
    val addressLine2 = makeString50.optionalField("addressLine2", unvalidated.addressLine2)
    val addressLine3 = makeString50.optionalField("addressLine3", unvalidated.addressLine3)
    val addressLine4 = makeString50.optionalField("addressLine4", unvalidated.addressLine4)
    val city         = makeString50.requiredField("city", unvalidated.city)
    val zipCode      = makeZipCode.requiredField("zipCode", unvalidated.zipCode)
    Validation
      .validateWith(addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode)(
        Address.apply
      )
      .toEither

  def toPersonalName(
    unvalidatedFirstName: String,
    unvalidatedLastName: String
  ): Either[NonEmptyChunk[ValidationError], PersonalName] =
    val firstName = makeString50.requiredField("firstName", unvalidatedFirstName)
    val lastName  = makeString50.requiredField("lastName", unvalidatedLastName)
    Validation
      .validateWith(firstName, lastName)(PersonalName.apply)
      .toEither

  //
  //
  //
  trait CheckProductCodeExists:
    def check(productCode: ProductCode): UIO[Boolean]

  //
  //
  //
  trait CheckAddressExists:

    import CheckAddressExists.*

    def check(unvalidatedAddress: UnvalidatedAddress): IO[AddressValidationError, CheckedAddress]

  object CheckAddressExists:
    case class CheckedAddress(unvalidatedAddress: UnvalidatedAddress)

    enum AddressValidationError:
      case InvalidFormat, AddressNotFound
