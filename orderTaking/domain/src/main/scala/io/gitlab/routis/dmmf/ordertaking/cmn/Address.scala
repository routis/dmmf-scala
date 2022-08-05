package io.gitlab.routis.dmmf.ordertaking.cmn

case class Address(
  addressLine1: String50,
  addressLine2: Option[String50],
  addressLine3: Option[String50],
  addressLine4: Option[String50],
  city: String50,
  zipCode: ZipCode
)
