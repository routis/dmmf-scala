package io.gitlab.routis.dmmf.ordertaking.application.port.out

import io.gitlab.routis.dmmf.ordertaking.domain.ProductCode
import zio.UIO

trait CheckProductCodeExists:
  def check(productCode: ProductCode): UIO[Boolean]
