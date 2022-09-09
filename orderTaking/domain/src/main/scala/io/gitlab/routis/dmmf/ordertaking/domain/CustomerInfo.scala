package io.gitlab.routis.dmmf.ordertaking.domain

final case class CustomerInfo(name: PersonalName, emailAddress: EmailAddress, vipStatus: VipStatus)
object CustomerInfo:
  import zio.prelude.Validation
  def make[E](
    name: => Validation[E, PersonalName],
    emailAddress: => Validation[E, EmailAddress],
    vipStatus: => Validation[E, VipStatus]
  ): Validation[E, CustomerInfo] =
    Validation
      .validateWith(name, emailAddress, vipStatus)(CustomerInfo.apply)
