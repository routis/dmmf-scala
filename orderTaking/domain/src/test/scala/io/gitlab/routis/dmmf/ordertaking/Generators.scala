package io.gitlab.routis.dmmf.ordertaking

import com.github.javafaker.Faker
import io.gitlab.routis.dmmf.ordertaking.application.port.in.PlaceOrder.{
  UnvalidatedAddress,
  UnvalidatedCustomerInfo,
  UnvalidatedOrder,
  UnvalidatedOrderLine
}
import io.gitlab.routis.dmmf.ordertaking.domain.{ PersonalName, VipStatus, ZipCode }
import zio.{ URIO, ZIO, ZLayer }
import zio.stream.ZStream
import zio.test.{ Gen, Sample }

import scala.util.matching.Regex

object Generators:

  lazy val faker: ZLayer[Any, Nothing, Faker] =
    ZLayer {
      ZIO.attempt(new Faker()).orDie
    }

  lazy val zipCodeStringGen: Gen[Faker, String] = FakerGen.fakeValueGen(_.address().zipCode().take(5))
  lazy val emailAddressStr: Gen[Faker, String]  = FakerGen.fakeValueGen(_.internet().emailAddress())
  lazy val unvalidatedCustomerInfoGen: Gen[Faker, UnvalidatedCustomerInfo] =
    for
      name         <- FakerGen.fakeValueGen(_.name())
      emailAddress <- emailAddressStr
    yield UnvalidatedCustomerInfo(
      firstName = name.firstName().s50,
      lastName = name.lastName().s50,
      emailAddress = emailAddress,
      vipStatus = VipStatus.Normal.name()
    )
  lazy val unvalidatedAddressGen: Gen[Faker, UnvalidatedAddress] =
    for
      address <- FakerGen.fakeValueGen(_.address())
      country <- FakerGen.fakeValueGen(_.country().countryCode2())
    yield UnvalidatedAddress(
      addressLine1 = address.streetAddress().s50,
      addressLine2 = null,
      addressLine3 = null,
      addressLine4 = null,
      city = address.city().s50,
      country = "GR",
      zipCode = address.zipCode().take(5)
    )

  lazy val productCodeStrGen: Gen[Faker, String] =
    for
      flag <- Gen.boolean
      code <- if flag then gizmoCodeGen else widgetCodeGen
    yield code

  lazy val gizmoCodeGen: Gen[Faker, String] =
    val r = "G\\d{3}".r
    FakerGen.fakeRegexifyGen(r)

  lazy val widgetCodeGen: Gen[Faker, String] =
    val r = "W\\d{4}".r
    FakerGen.fakeRegexifyGen(r)

  lazy val unvalidatedOrderLineGen: Gen[Faker, UnvalidatedOrderLine] =
    for
      orderLineId <- Gen.string
      productCode <- productCodeStrGen
      quantity    <- Gen.double(1, 100)
    yield UnvalidatedOrderLine(orderLineId = orderLineId, productCode = productCode, quantity = quantity)

  lazy val unvalidatedOrderGen: Gen[Faker, UnvalidatedOrder] =
    for
      orderId         <- Gen.string
      customerInfo    <- unvalidatedCustomerInfoGen
      shippingAddress <- unvalidatedAddressGen
      lines           <- Gen.listOf1(unvalidatedOrderLineGen)
    yield UnvalidatedOrder(
      orderId,
      customerInfo,
      shippingAddress = shippingAddress,
      billingAddress = shippingAddress,
      lines = lines,
      promotionCode = null
    )
  extension (s: String) private def s50: String = s.take(50)
  object FakerGen:

    def fakeRegexifyGen(r: Regex): Gen[Faker, String] =
      val rString = r.regex
      FakerGen.fakeValueGen(_.regexify(rString))
    def fakeValueGen[A](getter: Faker => A): Gen[Faker, A] =
      Gen.fromZIO(fakeValue(getter))
    def fakeValue[A](getter: Faker => A): URIO[Faker, A] =
      ZIO.service[Faker].map(getter(_))
