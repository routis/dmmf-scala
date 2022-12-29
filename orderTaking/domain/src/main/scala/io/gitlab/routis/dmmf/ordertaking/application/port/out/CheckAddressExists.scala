package io.gitlab.routis.dmmf.ordertaking.application.port.out

import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrderUseCase.UnvalidatedAddress
import zio.IO
import CheckAddressExists.*

/**
 * Checks whether a given address is valid and exists
 */
trait CheckAddressExists extends (UnvalidatedAddress => IO[AddressValidationError, CheckedAddress])

object CheckAddressExists:
  case class CheckedAddress(unvalidatedAddress: UnvalidatedAddress)

  enum AddressValidationError:
    case InvalidFormat, AddressNotFound
