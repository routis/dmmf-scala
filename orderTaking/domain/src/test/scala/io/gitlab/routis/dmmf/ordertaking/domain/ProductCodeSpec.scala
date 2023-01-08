package io.gitlab.routis.dmmf.ordertaking.domain

import io.gitlab.routis.dmmf.ordertaking.Generators
import io.gitlab.routis.dmmf.ordertaking.domain.ZipCodeSpec.{suite, test}
import zio.test.*

object ProductCodeSpec extends ZIOSpecDefault :
  override def spec: Spec[Any, Nothing] = suite("ProductCode")(
    test("make() with valid gizmo code") {
      check(Generators.gizmoCodeGen) { str =>
        assert(ProductCode.make(str).toEither.map(_.value))(Assertion.equalTo(Right(str)))
      }
    },
    test("make() with valid widget code") {
      check(Generators.gizmoCodeGen) { str =>
        assert(ProductCode.make(str).toEither.map(_.value))(Assertion.equalTo(Right(str)))
      }
    },
    test("make() with invalid") {
      check(Gen.string)(str => assertTrue(ProductCode.make(str).toOption.isEmpty))
    }

  ).provideLayerShared(Generators.faker)
