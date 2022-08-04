package io.gitlab.routis.dmmf.ordertaking

import io.gitlab.routis.dmmf.ordertaking.pub.PlaceOrder.*
import zio.IO
import zio.prelude.Validation

trait Application:

  import Dto.{ OrderDto, PlaceOrderErrorDto }
  def placeOrder(unvalidatedOrder: OrderDto): IO[PlaceOrderErrorDto, PlaceOrderEvents]

  object Dto:
    import io.gitlab.routis.dmmf.ordertaking.cmn.Common.*

    case class CustomerInfoDto(
      firstName: String,
      lastName: String,
      emailAddress: String,
      vipStatus: String
    )

    case class AddressDto(
      addressLine1: String,
      addressLine2: String,
      addressLine3: String,
      addressLine4: String,
      city: String,
      zipCode: String
    )

    case class OrderLineDto(orderLineId: String, productCode: String, quantity: Double)

    case class OrderDto(
      orderId: String,
      customerInfo: CustomerInfoDto,
      shippingAddress: AddressDto,
      billingAddress: AddressDto,
      lines: List[OrderLineDto],
      promotionCode: String
    )

    trait PlaceOrderErrorDto

    //
    // Mappings
    //

    def fromDomain(domain: CustomerInfo): CustomerInfoDto =
      CustomerInfoDto(
        firstName = domain.name.fistName.value,
        lastName = domain.name.lastName.value,
        emailAddress = EmailAddress.unwrap(domain.emailAddress),
        vipStatus = domain.vipStatus.name()
      )
    extension (dto: CustomerInfoDto)
      def toUnvalidated: UnvalidatedCustomerInfo     =
        UnvalidatedCustomerInfo(
          firstName = dto.firstName,
          lastName = dto.lastName,
          emailAddress = dto.emailAddress,
          vipStatus = dto.vipStatus
        )
      def toDomain: Validation[String, CustomerInfo] =
        for
          name         <-
            for
              firstName <- Validation
                             .fromEither(Option(dto.firstName).toRight("Missing"))
                             .flatMap(String50.make)
              lastName  <- Validation
                             .fromEither(Option(dto.lastName).toRight("Missing"))
                             .flatMap(String50.make)
            yield PersonalName(firstName, lastName)
          emailAddress <- Validation
                            .fromEither(Option(dto.emailAddress).toRight("Missing"))
                            .flatMap(EmailAddress.make)
          vipStatus    <- Validation
                            .fromEither(Option(dto.vipStatus).toRight("Missing"))
                            .flatMap(VipStatus.make)
        yield CustomerInfo(name, emailAddress, vipStatus)

    def fromDomain(domain: Address): AddressDto =
      AddressDto(
        addressLine1 = domain.addressLine1.value,
        addressLine2 = domain.addressLine2.map(_.value).orNull,
        addressLine3 = domain.addressLine3.map(_.value).orNull,
        addressLine4 = domain.addressLine4.map(_.value).orNull,
        city = domain.city.value,
        zipCode = ZipCode.unwrap(domain.zipCode)
      )
    extension (dto: AddressDto)
      def toUnvalidated: UnvalidatedAddress     =
        UnvalidatedAddress(
          addressLine1 = dto.addressLine1,
          addressLine2 = dto.addressLine2,
          addressLine3 = dto.addressLine3,
          addressLine4 = dto.addressLine4,
          city = dto.city,
          zipCode = dto.zipCode
        )
//      def toDomain: Validation[String, Address] =
//        for
//          addressLine1 <- String50.make.required("Missing")(dto.addressLine1)
//          addressLine2 <- Validation
//                            .fromOption(Option(dto.addressLine2))
//                            .fold()
//                            .fold(Validation.succeed(None), String50.make)
//          addressLine3 <- Option(dto.addressLine3)
//                            .fold(Validation.succeed(None), String50.make)
//          addressLine4 <- Option(dto.addressLine4)
//                            .fold(Validation.succeed(None), String50.make)
//          city         <- Validation
//                            .fromEither(Option(dto.city).toRight("Missing city"))
//                            .flatMap(String50.make)
//          zipCode      <- Validation
//                            .fromEither(Option(dto.zipCode).toRight("Missing zipCode"))
//                            .flatMap(ZipCode.make)
//        yield Address(
//          addressLine1 = addressLine1,
//          addressLine2 = addressLine2,
//          addressLine3 = addressLine3,
//          addressLine4 = addressLine4,
//          city = city,
//          zipCode = zipCode
//        )
