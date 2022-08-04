package io.gitlab.routis.dmmf.ordertaking.pub

import zio.IO
import PlaceOrder.UnvalidatedAddress
trait CheckAddressExists:

  import CheckAddressExists.*

  def check(unvalidatedAddress: UnvalidatedAddress): IO[AddressValidationError, CheckedAddress]

object CheckAddressExists:
  case class CheckedAddress(unvalidatedAddress: UnvalidatedAddress)

  enum AddressValidationError:
    case InvalidFormat, AddressNotFound
