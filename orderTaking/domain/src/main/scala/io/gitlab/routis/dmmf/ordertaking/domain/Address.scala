package io.gitlab.routis.dmmf.ordertaking.domain

case class Address(
  addressLine1: String50,
  addressLine2: Option[String50],
  addressLine3: Option[String50],
  addressLine4: Option[String50],
  city: String50,
  country: Country,
  zipCode: ZipCode
)
object Address:
  import zio.prelude.Validation
  def make[E](
    addressLine1: => Validation[E, String50],
    addressLine2: => Validation[E, Option[String50]],
    addressLine3: => Validation[E, Option[String50]],
    addressLine4: => Validation[E, Option[String50]],
    city: => Validation[E, String50],
    country: => Validation[E, Country],
    zipCode: => Validation[E, ZipCode]
  ): Validation[E, Address] =
    Validation
      .validateWith(addressLine1, addressLine2, addressLine3, addressLine4, city, country, zipCode)(Address.apply)
