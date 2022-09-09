package io.gitlab.routis.dmmf.ordertaking.domain

final case class PersonalName(fistName: String50, lastName: String50)
object PersonalName:
  import zio.prelude.Validation
  def make[E](firstName: Validation[E, String50], lastName: Validation[E, String50]): Validation[E, PersonalName] =
    Validation.validateWith(firstName, lastName)(PersonalName.apply)
