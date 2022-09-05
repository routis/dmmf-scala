package io.gitlab.routis.dmmf.ordertaking.domain

enum VipStatus extends Enum[VipStatus]:
  case Normal, Vip

object VipStatus:
  import zio.prelude.Validation

  def make(name: String): Validation[String, VipStatus] =
    val res = name match
      case "Normal" => Option(Normal)
      case "Vip"    => Option(Vip)
      case _        => None
    Validation.fromOptionWith("Not a valid VipStatus")(res)
