package io.gitlab.routis.dmmf.ordertaking.domain

import com.github.javafaker.Faker
import io.gitlab.routis.dmmf.ordertaking.Generators
import zio.test.Assertion.{ equalTo, isLeft, isRight }
import zio.test.{ assert, check, Gen, Spec }

object DomainSpec extends zio.test.ZIOSpecDefault:

  override def spec: Spec[Any, Nothing] =
    domainSpec.provideLayerShared(Generators.faker)

  def domainSpec = zipCodeSpec + productCodeSpec

  val zipCodeSpec: Spec[Faker, Nothing] =
    suite("ZipCodeSpec")(
      test("make with valid input should return a ZipCode") {
        check(Generators.zipCodeStringGen)(str => assert(ZipCode.make(str).toEither)(isRight(equalTo(str))))
      },
      test("make with invalid") {
        check(Gen.string)(str => assert(ZipCode.make(str).toEither)(isLeft))
      }
    )

  val productCodeSpec: Spec[Faker, Nothing] =
    def makeWithValidCode(validCodes: Gen[Faker, String])(label: String) = test(label) {
      check(validCodes) { str =>
        val maybeProduct = ProductCode.make(str).map(_.value).toEither
        assert(maybeProduct)(isRight(equalTo(str)))
      }
    }

    suite("ProductCode")(
      makeWithValidCode(Generators.gizmoCodeGen)("make with valid gizmo codes"),
      makeWithValidCode(Generators.widgetCodeGen)("make with valid widget codes"),
      test("make() with invalid") {
        check(Gen.string) { str =>
          val maybeProduct = ProductCode.make(str).map(_.value).toEither
          assert(maybeProduct)(isLeft)
        }
      }
    )
