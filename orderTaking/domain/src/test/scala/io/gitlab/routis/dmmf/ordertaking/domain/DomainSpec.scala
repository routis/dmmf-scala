package io.gitlab.routis.dmmf.ordertaking.domain

import com.github.javafaker.Faker
import io.gitlab.routis.dmmf.ordertaking.Generators
import io.gitlab.routis.dmmf.ordertaking.domain.DomainSpec.test
import zio.test.Assertion.{ equalTo, isLeft, isRight }
import zio.test.{ assert, check, Gen, Spec }

object DomainSpec extends zio.test.ZIOSpecDefault:

  def spec: Spec[Any, Nothing] =
    domainSpec.provideLayerShared(Generators.faker)

  def domainSpec: Spec[Faker, Nothing] = zipCodeSpec + productCodeSpec

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
    suite("ProductCodeSpec")(
      test("make with valid gizmo or widget codes") {
        check(Generators.gizmoCodeGen ++ Generators.widgetCodeGen) { str =>
          assert(ProductCode.make(str).map(_.value).toEither)(isRight(equalTo(str)))
        }
      },
      test("make() with invalid") {
        check(Gen.string) { str =>
          assert(ProductCode.make(str).toEither)(isLeft)
        }
      }
    )
