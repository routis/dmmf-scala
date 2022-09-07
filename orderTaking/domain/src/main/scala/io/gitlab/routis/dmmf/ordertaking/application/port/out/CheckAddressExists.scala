package io.gitlab.routis.dmmf.ordertaking.application.port.out

import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.UnvalidatedAddress
import zio.IO
trait CheckAddressExists:

  import CheckAddressExists.*

  def check(unvalidatedAddress: UnvalidatedAddress): IO[AddressValidationError, CheckedAddress]

object CheckAddressExists:
  case class CheckedAddress(unvalidatedAddress: UnvalidatedAddress)

  enum AddressValidationError:
    case InvalidFormat, AddressNotFound