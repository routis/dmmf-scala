package io.gitlab.routis.dmmf.ordertaking.pub

import io.gitlab.routis.dmmf.ordertaking.cmn.ProductCode
import zio.UIO

trait CheckProductCodeExists:
  def check(productCode: ProductCode): UIO[Boolean]
