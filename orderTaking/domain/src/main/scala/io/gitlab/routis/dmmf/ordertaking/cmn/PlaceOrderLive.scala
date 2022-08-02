package io.gitlab.routis.dmmf.ordertaking.cmn
import io.gitlab.routis.dmmf.ordertaking.cmn.Common.*
import io.gitlab.routis.dmmf.ordertaking.cmn.PlaceOrder.UnvalidatedAddress
import io.gitlab.routis.dmmf.ordertaking.cmn.PlaceOrderLive.*
import io.gitlab.routis.dmmf.ordertaking.cmn.PlaceOrderLive.CheckAddressExists.CheckedAddress
import io.gitlab.routis.dmmf.ordertaking.cmn.Validations.requiredField
import zio.prelude.Validation
import zio.{ IO, NonEmptyChunk, ZIO }

case class PlaceOrderLive(
  private val checkAddressExists: CheckAddressExists,
  private val checkProductCodeExists: CheckProductCodeExists
) extends PlaceOrder:

  override def placeOrder(
    unvalidatedOrder: PlaceOrder.UnvalidatedOrder
  ): IO[PlaceOrder.PlaceOrderError, PlaceOrder.PlaceOrderEvents] = ???

object PlaceOrderLive:
  import PlaceOrder.*
  import Validations.*

  import CheckAddressExists.*

  def toCustomerInfo(
    unvalidated: UnvalidatedCustomerInfo
  ): Validation[ValidationError, CustomerInfo] =
    Validation.validateWith(
      toPersonalName(unvalidated.firstName, unvalidated.lastName),
      EmailAddress.make.requiredField("emailAddress", unvalidated.emailAddress),
      VipStatus.make.requiredField("vipStatus", unvalidated.vipStatus)
    )(CustomerInfo.apply)

  def toAddress(checkedAddress: CheckedAddress): Validation[ValidationError, Address] =
    val unvalidated = checkedAddress.unvalidatedAddress
    Validation.validateWith(
      String50.make.requiredField("addressLine1", unvalidated.addressLine1),
      String50.make.optionalField("addressLine2", unvalidated.addressLine2),
      String50.make.optionalField("addressLine3", unvalidated.addressLine3),
      String50.make.optionalField("addressLine4", unvalidated.addressLine4),
      String50.make.requiredField("city", unvalidated.city),
      ZipCode.make.requiredField("zipCode", unvalidated.zipCode)
    )(Address.apply)

  def toCheckedAddress(
    field: FieldName,
    unvalidatedAddress: UnvalidatedAddress
  ): ZIO[CheckAddressExists, Nothing, Validation[ValidationError, Address]] =
    (for
      checkAddressExists <- ZIO.service[CheckAddressExists]
      checkedAddress     <- checkAddressExists.check(unvalidatedAddress) mapError { e =>
                              val description = e match
                                case AddressValidationError.AddressNotFound =>
                                  "Address not found"
                                case AddressValidationError.InvalidFormat   =>
                                  "Address invalid format"
                              zio.NonEmptyChunk.single(fieldError(field)(description))
                            }
      validAddress       <- toAddress(checkedAddress).mapError(nestError(field)).toZIOWithAllErrors
    yield validAddress).toValidation

  def toPersonalName(
    firstName: String,
    lastName: String
  ): Validation[ValidationError, PersonalName] =
    Validation.validateWith(
      String50.make.requiredField("firstName", firstName),
      String50.make.requiredField("lastName", lastName)
    )(PersonalName.apply)

  def toProductCode(
    unvalidated: String
  ): ZIO[CheckProductCodeExists, Nothing, Validation[String, ProductCode]] =
    ???

  //
  //
  //
  trait CheckProductCodeExists:
    def check(productCode: ProductCode): ZIO[Any, Nothing, Boolean]

  //
  //
  //
  trait CheckAddressExists:
    import CheckAddressExists.*
    def check(
      unvalidatedAddress: UnvalidatedAddress
    ): ZIO[Any, AddressValidationError, CheckedAddress]

  object CheckAddressExists:
    case class CheckedAddress(unvalidatedAddress: UnvalidatedAddress)

    enum AddressValidationError:
      case InvalidFormat, AddressNotFound

object App extends zio.ZIOAppDefault:
  val unvalidatedAddress                     =
    UnvalidatedAddress("Ethnikis Antistaseos 81A", "Vrilissia", null, null, "Athens", "15235")
  val wrong                                  =
    UnvalidatedAddress(
      "Ethnikis Antistaseos 81A" + "                                         ",
      "Vrilissia",
      null,
      null,
      "Athens",
      "15235     "
    )
  val checkAddressExists: CheckAddressExists = u => ZIO.succeed(CheckedAddress(u))
  override def run                           =
    PlaceOrderLive
      .toCheckedAddress("billingAddress", wrong)
      .debug("here")
      .provideSomeLayer(zio.ZLayer.succeed(checkAddressExists))
