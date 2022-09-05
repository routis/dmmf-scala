package io.gitlab.routis.dmmf.ordertaking.domain

final case class CustomerInfo(name: PersonalName, emailAddress: EmailAddress, vipStatus: VipStatus)
