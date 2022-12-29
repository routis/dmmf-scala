package io.gitlab.routis.dmmf.ordertaking.application.port.out

import io.gitlab.routis.dmmf.ordertaking.domain.ProductCode
import zio.UIO

trait CheckProductCodeExists extends (ProductCode => UIO[Boolean])
